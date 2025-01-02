--[[
     A Pandoc 2 lua filter to deal with custom environment in bookdown
--]]

-- REQUIREMENTS: Load shared lua function - see `shared.lua` in rmarkdown for more details.
--  * pandocAvailable()
--  * pandoc_type() function (backward compatible type() after 2.17 changes)
--  * print_debug()
dofile(os.getenv 'RMARKDOWN_LUA_SHARED')

--[[
  About the requirement:
  * PANDOC_VERSION -> 2.1
]]
if (not pandocAvailable {2,1}) then
    io.stderr:write("[WARNING] (latex-div.lua) requires at least Pandoc 2.1. Lua Filter skipped.\n")
    return {}
end

-- START OF THE FILTER'S FUNCTIONS --

-- theorem types available to be used
local theorem_abbr = {
    theorem = 'thm',
    lemma = 'lem',
    corollary = 'cor',
    proposition = 'prp',
    conjecture = 'cnj',
    definition = 'def',
    example = 'exm',
    exercise = 'exr',
    hypothesis = 'hyp'
}

-- other special proof envs
local proof_label = {
    proof = 'Proof',
    remark = 'Remark',
    solution = 'Solution'
}

-- create a unique id for a div with none provided
local counter = 0
local function unlabeled_div()
    counter = counter + 1
    return "unlabeled-div-"..(counter)
end

-- return [name] for latex, and (name) for html
local function get_name(format, options)
    local name = options["name"]
    if not name then return "" end
    local template = {latex = "[%s]", html = " (%s)"}
    name = string.format(template[format],  name)
    print_debug("name -> ", name)
    -- remove data-name from option
    options["name"] = nil
    return name
end

-- Create a label for referencing - only for theorem like env
local function create_label(env_type, id)
    if (env_type.type ~= "theorem") then return nil end
    label = string.format("%s:%s", theorem_abbr[env_type.env], id)
    print_debug("label for reference -> ", label)
    return label
end

-- Get metadata specific to bookdown for this filter
Meta = function(m)
    bookdownmeta = m.bookdown
    if (bookdownmeta and bookdownmeta.language and bookdownmeta.language.label) then
        -- For internationalization feature of bookdown
        for k,v in pairs(bookdownmeta.language.label) do
            if (pandoc_type(v) == 'Inlines' and proof_label[k] ~= nil) then
                -- remove any undesired space (3 or less)
                proof_label[k] = pandoc.utils.stringify(v):gsub("%.?%s?%s?%s?$", "")
                print_debug("Translation-> "..k..":", proof_label[k])
            end
        end
    end
end

-- Modify Pandoc AST for supported custom environment
Div = function (div)
    local classes = div.classes
    -- we do nothing if no classes
    if (#classes == 0) then
        print_debug("No classes in the Div.")
        return div
    end
    print_debug("Div classes -> " , classes)

    -- checking if the class is one of the supported custom environment
    local env_type = {type = nil, env = nil}
    for i,v in ipairs(classes) do
        if (theorem_abbr[v] ~= nil) then
            env_type.type = "theorem"
            env_type.env = v
            break
        elseif (proof_label[v] ~= nil) then
            env_type.type = "proof"
            env_type.env = v
            break
        end
    end
    -- classes is not a supported one, we return as is
    if not env_type.env then
        print_debug("Not a bookdown supported custom class")
        return div
    end
    print_debug("Found types -> ", env_type)

    -- get the id if it exists - it will we use to build label for reference
    local id = div.identifier
    print_debug("id -> ", id)
    -- remove unwanted identifier on the div, as it will be on the span
    div.identifier = ""

    -- get the attributes
    local options = div.attributes
    if (options["data-latex"] ~= nil or options["latex"] ~= nil) then
        -- so that latex-divs.lua in rmarkdown does not activate
        print("[WARNING] data-latex attribute can't be used with one of bookdown custom environment. It has been removed.")
        options["data-latex"] = nil
        options["latex"] = nil
    end

    if (FORMAT:match 'latex' or FORMAT:match 'beamer') then
        -- build the name
        local name = get_name('latex', options)
        -- build the label string for theorem env type
        -- For LaTeX, only insert \label{} if an id as been provided explicitly
        local label_part
        if #id ~= 0 and env_type.type == "theorem" then
            local label = create_label(env_type, id)
            label_part = string.format("\\protect\\hypertarget{%s}{}\\label{%s}", label, label)
        end
        -- build the env string
        local beginEnv = string.format('\\begin{%s}%s\n%s', env_type.env, name, label_part or "")
        local endEnv = string.format('\\end{%s}', env_type.env)

        -- similar to latex-div.lua in rmarkdown:
        --   if the first and last div blocks are paragraphs then we can
        --   bring the environment begin/end closer to the content
        if div.content[1].t == "Para" and div.content[#div.content].t == "Para" then
            table.insert(div.content[1].content, 1, pandoc.RawInline('tex', beginEnv))
            table.insert(div.content[#div.content].content, pandoc.RawInline('tex', '\n' .. endEnv))
        else
            if (div.content[1].t ~= "Para") then
            -- required trick to get correct alignment
              beginEnv = beginEnv.."\\leavevmode"
            end
            table.insert(div.content, 1, pandoc.RawBlock('tex', beginEnv))
            table.insert(div.content, pandoc.RawBlock('tex', endEnv))
        end
    elseif (FORMAT:match 'html' or FORMAT:match 'slidy') then
        -- if div is already processed by eng_theorem, it would also modify it.
        -- we can ignore knowing how eng_theorem modifies options$html.before2
        -- It can be Plain or Para depending if a name was used or not.
        -- MAYBE NOT VERY RELIABLE THOUGH
        if (div.content[1].t == "Plain" or div.content[1].t == "Para") then
            for i,el in pairs(div.content[1].content) do
                if (el.t == "Span" and el.classes[1] == env_type.env) then
                    print_debug("Already processed by knitr engine.")
                    return div
                end
            end
        end

        -- build the name
        local name = get_name('html', options)
        -- if no id, one is generated so that bookdown labelling mechanism works
        if #id == 0 then id = unlabeled_div() end
        -- build a label - only used to theorem type
        local label = create_label(env_type, id)

        -- inserted the correct span depending on the environment type
        local span
        if (env_type.type == "theorem") then
            span = pandoc.Span(
                pandoc.Strong(string.format("(#%s)%s ", label, name)),
                pandoc.Attr(label, {env_type.env})
            )
        elseif (env_type.type == "proof") then
            span = pandoc.Span({
                pandoc.Emph(pandoc.Str(proof_label[env_type.env])),
                pandoc.Str(name),
                pandoc.Str("."),
                pandoc.Space()
            },
                pandoc.Attr(id, {env_type.env})
            )
        end
        if (div.content[1].t == "Para") then
          -- add to the first block of the div, and not as first block, only if a Para
          table.insert(div.content[1].content, 1, span)
        else
          -- Otherwise add as its own Para
          table.insert(div.content, 1, pandoc.Para(span))
        end
    end

    return div
end

-- only run filter for supported format
if (FORMAT:match 'html' or FORMAT:match 'slidy' or FORMAT:match 'latex' or FORMAT:match 'beamer') then
    return {{Meta = Meta}, {Div = Div}}
else
    print_debug("Lua Filter skipped. Output format not supported:", FORMAT)
    return {}
end
