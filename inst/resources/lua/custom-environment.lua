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
local theorem_names = {}
for k in pairs(theorem_abbr) do
  table.insert(theorem_names, k)
end

-- just for debuging purpose
local debug_mode = os.getenv("DEBUG_PANDOC_LUA") == "TRUE"

function print_debug(obj, label, iter)
    iter = iter or pairs
    label = label or ""
    label = "DEBUG: "..label
    if (debug_mode) then
        if (type(obj) == "string") then
            print(label, obj)
        else
            if type(obj) == "table" then
                for k,v in iter(obj) do
                    print(label, k, v)
                end
            end
        end
    end
    return nil
end

-- Modify Pandoc AST for supported custom environment
Div = function (div)
    -- checking if classes correponds to a custom one
    local classes = div.classes
    -- we do nothing if no classes
    if (#classes == 0) then
        print_debug("No classes in the Div")
        return div
    end

    print_debug(classes, "Div classes ->")

    local theorem_type = {}
    for i,v in ipairs(classes) do
        if (theorem_abbr[v] ~= nil) then theorem_type[i] = v end
    end
    -- test
    print_debug(theorem_type, "Found types ->")

    -- classes is not a supported one, we return as is
    if (#theorem_type == 0) then
        print_debug("Type not supported")
        return div
    end
    -- get the theorem type as string
    if (#theorem_type ~= 1) then
        -- warn if special case of more than one corresponding classes
        print("[WARNING] Only one custom environment can be used. Keeping the first.")
    end
    theorem_type = theorem_type[1]
    print_debug(theorem_type, "type selected ->")

    -- get the id if it exists - it will we use to build label for reference
    local id = div.identifier
    print_debug(id, "id found ->")
    -- remove unwanted identifier on the div, as it will be on the span
    div.identifier = ""

    local label = ""
    if (#id ~= 0) then
        -- build label
        label = string.format("%s:%s", theorem_abbr[theorem_type], id)
        print_debug(label, "label for reference ->")
    end

    -- get the attributes
    local options = div.attributes


    function get_name(format, options)
        local name = options["data-name"]
        if (name == nil) then return "" end
        local template
        if (format == 'latex') then
            template = "[%s]"
        elseif (format == 'html') then
            template = "(%s)"
        end
        name = string.format(template,  name)
        print_debug(name, "name ->")
        -- remove data-name
        options["data-name"] = nil
        return name
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

        if (#label == 0) then
            print("[WARNING] An id needs to set in the custome divs for correct rendering")
        else
            table.insert(
                div.content, 1,
                pandoc.Para(
                    pandoc.Span(
                        pandoc.Strong("(#%s) %s", label, name),
                        {id = label, class = theorem_type}
                    )
                )
            )
        end
    end

    return div
end