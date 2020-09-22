--[[
     A Pandoc 2 lua filter to deal with custom environment in bookdown
--]]

-- theorem types available to be used
local theorem_abbr = {
    theorem = 'thm',
    lemma = 'lem',
    corollary = 'cor',
    proposition = 'prp',
    conjecture = 'cnj',
    definition = 'def',
    example = 'exm',
    exercise = 'exr'
}

-- other special proof envs
local proof_label = {
    proof = 'Proof',
    remark = 'Remark',
    solution = 'Solution'
}

-- for debuging purpose
local debug_mode = os.getenv("DEBUG_PANDOC_LUA") == "TRUE"
function print_debug(label,obj,iter)
    obj = obj or nil
    iter = iter or pairs
    label = label or ""
    label = "DEBUG (from custom-environment.lua): "..label
    if (debug_mode) then
        if not obj then
          print(label)
        elseif (type(obj) == "string") then
            print(label.." "..obj)
        elseif type(obj) == "table" then
            for k,v in iter(obj) do
                print(label.."id:"..k.. " val:"..v)
            end
        end
    end
    return nil
end

-- create a unique id for a div with none provided
local counter = 0
function unlabeled_div()
    counter = counter + 1 
    return "unlabeled-div-"..(counter)
end

-- Get metadata specific to bookdown for this filter
Meta = function(m) 
    bookdownmeta = m.bookdown
    if (bookdownmeta ~= nil) then
        -- For internationalization feature of bookdown
        if (bookdownmeta.language ~= nil) then
            if (bookdownmeta.language.label ~= nil) then
                for k,v in pairs(bookdownmeta.language.label) do
                    if (type(v) == 'table' and v.t == 'MetaInlines' and proof_label[k] ~= nil) then
                        -- remove any undesired space (3 or less)
                        proof_label[k] = pandoc.utils.stringify(v):gsub("%.?%s?%s?%s?$", "")
                        print_debug("Translation-> "..k..":", proof_label[k])
                    end
                end
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
    -- if no id, one is generated so that bookdown labelling mechanism works
    if #id == 0 then id = unlabeled_div() end
    print_debug("id -> ", id)
    -- remove unwanted identifier on the div, as it will be on the span
    div.identifier = ""

    -- get the attributes
    local options = div.attributes
    if (options["data-latex"] ~= nil) then 
        -- so that latex-divs.lua in rmarkdown does not activate
        print("[WARNING] data-latex attribute can't be used with one of bookdown custom environment. It has been removed.")
        options["data-latex"] = nil
    end

    -- return [name] for latex, and (name) for html
    function get_name(format, options)
        local name = options["name"]
        if (name == nil) then return "" end
        local template
        if (format == 'latex') then
            template = "[%s]"
        elseif (format == 'html') then
            template = " (%s)"
        end
        name = string.format(template,  name)
        print_debug("name -> ", name)
        -- remove data-name
        options["name"] = nil
        return name
    end
    
    -- create the custom environment
    local label
    -- Create a label for referencing - only for theorem like env
    if (env_type.type == "theorem") then
        label = string.format("%s:%s", theorem_abbr[env_type.env], id)
    end
    print_debug("label for reference -> ", label)

    -- TODO: should we support beamer also ?
    if (FORMAT:match 'latex') then
        local label_part 
        if label then
            label_part = string.format( "\n\\protect\\hypertarget{%s}{}\\label{%s}", label, label)
        end
        local name = get_name('latex', options)
        table.insert(
            div.content, 1,
            pandoc.RawBlock('tex', string.format('\\begin{%s}%s%s', env_type.env, name, label_part or ""))
        )
        table.insert(
            div.content,
            pandoc.RawBlock('tex', string.format('\\end{%s}', env_type.env))
        )
    elseif (FORMAT:match 'html') then
        local name = get_name('html', options)

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

        -- inserted the correct span depending on the environment type
        local span
        if (env_type.type == "theorem") then
            span = pandoc.Span(
                pandoc.Strong(string.format("(#%s)%s ", label, name)),
                {id = label, class = env_type.env}
            )
        elseif (env_type.type == "proof") then
            span = pandoc.Span({
                pandoc.Emph(pandoc.Str(proof_label[env_type.env])),
                pandoc.Str(name),
                pandoc.Str("."),
                pandoc.Space()
            },
                {id = id, class = env_type.env}
            )
        end
        -- add to the first block of the div, and not as first block
        table.insert(div.content[1].content, 1, span)
    end

    return div
end

return {{Meta = Meta}, {Div = Div}}