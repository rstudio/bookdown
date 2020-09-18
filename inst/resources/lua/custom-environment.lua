--[[
     A Pandoc 2 lua filter to deal with custom environment in bookdown
--]]

-- theroem types available to be used
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

-- other special envs
local proof_label = {
    proof = 'Proof',
    remark = 'Remark',
    solution = 'Solution'
}

-- just for debuging purpose
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
    -- checking if classes correponds to a custom one
    local classes = div.classes
    -- we do nothing if no classes
    if (#classes == 0) then
        print_debug("No classes in the Div.")
        return div
    end

    print_debug("Div classes -> " , classes)

    local theorem_type = {}
    local proof_type = {}
    for i,v in ipairs(classes) do
        if (theorem_abbr[v] ~= nil) then theorem_type[i] = v end
        if (proof_label[v] ~= nil) then proof_type[i] = v end
    end
    -- test
    print_debug("Found types -> ", theorem_type)

    -- classes is not a supported one, we return as is
    if (#theorem_type == 0 and #proof_type == 0) then
        print_debug("Not a bookdown supported custom class")
        return div
    end
    -- get the theorem type as string
    if (#theorem_type > 1 or #proof_type > 1) then
        -- warn if special case of more than one corresponding classes
        print("[WARNING] Only one custom environment can be used. Keeping the first.")
    end
    theorem_type = theorem_type[1]
    proof_type = proof_type[1]
    print_debug("theorem type selected -> ", theorem_type)
    print_debug("proof type selected -> ", proof_type)

    -- get the id if it exists - it will we use to build label for reference
    local id = div.identifier
    print_debug("id found -> ", id)
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
        local name = options["data-name"]
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
        options["data-name"] = nil
        return name
    end

    if (theorem_type ~= nil) then
        print_debug("Enter Theorem part.")
        local label = ""
        if (#id ~= 0) then
            -- build label
            label = string.format("%s:%s", theorem_abbr[theorem_type], id)
            print_debug("label for reference -> ", label)
        end

        -- create the custom environment

        -- TODO: should we support beamer also ?
        if (FORMAT:match 'latex') then
            
            local label_part = ''
            if (#label ~= 0) then
                -- if no label referencing won't work but you can't reference without a label
                -- so no one will try
                label_part = string.format( "\n\\protect\\hypertarget{%s}{}\\label{%s}", label, label)
            end

            local name = get_name('latex', options)
            
            table.insert(
                div.content, 1,
                pandoc.RawBlock('tex', string.format('\\begin{%s}%s%s', theorem_type, name, label_part))
            )
            table.insert(
                div.content,
                pandoc.RawBlock('tex', string.format('\\end{%s}', theorem_type))
            )
        end

        if (FORMAT:match 'html') then
            local name = get_name('html', options)

            -- if div is already processed by eng_theorem, it would also modify it. 
            -- we can ignore knowing how eng_theorem modifies options$html.before2
            -- It can be Plain or Para depending if a name was used or not.
            -- NOT VERY RELIABLE THOUGH
            if (div.content[1].t == "Plain" or div.content[1].t == "Para") then
                for i,el in pairs(div.content[1].content) do
                    if (el.t == "Span" and el.classes[1] == theorem_type) then
                        print_debug("Already processed by knitr engine.")
                        return div
                    end
                end
            end

            if (#label == 0) then
                print("[WARNING] An id needs to be set in the custom divs for correct rendering. Please add one to one of your "..theorem_type.." Fenced Divs.")
                print_debug("No #id means the div is not processed by the lua filter.")
            else
                table.insert(
                    -- add to the first block of the div, and not as first block
                    div.content[1].content, 1,
                    pandoc.Span(
                        pandoc.Strong(string.format("(#%s)%s ", label, name)),
                        {id = label, class = theorem_type}
                    )
                )
                )
            end
        end
    end

    if (proof_type ~= nil) then
        print_debug("Enter Proof part.")
        -- create the custom environment

        -- TODO: should we support beamer also ?
        if (FORMAT:match 'latex') then

            local name = get_name('latex', options)
            
            table.insert(
                div.content, 1,
                pandoc.RawBlock('tex', string.format('\\begin{%s}%s', proof_type, name))
            )
            table.insert(
                div.content,
                pandoc.RawBlock('tex', string.format('\\end{%s}', proof_type))
            )
        end

        if (FORMAT:match 'html') then
            local name = get_name('html', options)

            print_debug("html name -> ", name)

            -- if div is already processed by eng_proof, it would also modify it. 
            -- we can ignore knowing how eng_proof modifies options$html.before2
            -- NOT VERY RELIABLE THOUGH
            if (div.content[1].t == "Plain") then
                for i,el in pairs(div.content[1].content) do
                    if (el.t == "Span" and el.classes[1] == proof_type) then
                        print_debug("Already processed by knitr engine.")
                        return div
                    end
                end
            end
            table.insert(
                -- add to the first block of the div, and not as first block
                div.content[1].content, 1,
                    pandoc.Span({
                        pandoc.Emph(pandoc.Str(proof_label[proof_type])),
                        pandoc.Str(name),
                        pandoc.Str("."),
                        pandoc.Space()
                    },
                        {id = id, class = proof_type}
                    )
            )
        end
    end
    return div
end

return {{Meta = Meta}, {Div = Div}}
