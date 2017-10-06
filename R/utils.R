#' @import stats utils

bookdown_file = function(...) {
  system.file(..., package = 'bookdown', mustWork = TRUE)
}

# find the y[j] closest to x[i] with y[j] > x[i]; x and y have been sorted
next_nearest = function(x, y, allow_eq = FALSE) {
  n = length(x); z = integer(n)
  for (i in seq_len(n)) z[i] = y[if (allow_eq) y >= x[i] else y > x[i]][1]
  z
}

# change the filename extension
with_ext = function(x, ext) {
  n1 = length(x); n2 = length(ext); r = '([.][[:alnum:]]+)?$'
  if (n1 * n2 == 0) return(x)
  i = !(grepl('^[.]', ext) | ext == '')
  ext[i] = paste0('.', ext[i])

  if (all(ext == '')) ext = ''
  if (length(ext) == 1) return(sub(r, ext, x))

  if (n1 > 1 && n1 != n2) stop("'ext' must be of the same length as 'x'")
  mapply(sub, r, ext, x, USE.NAMES = FALSE)
}

# counters for figures/tables
new_counters = function(type, rownames) {
  base = matrix(
    0L, nrow = length(rownames), ncol = length(type),
    dimnames = list(rownames, type)
  )
  list(
    inc = function(type, which) {
      base[which, type] <<- base[which, type] + 1L
    }
  )
}

# set some internal knitr options
set_opts_knit = function(config) {
  # use labels of the form (\#label) in knitr
  config$knitr$opts_knit$bookdown.internal.label = TRUE
  # when the output is LaTeX, force LaTeX tables instead of default Pandoc tables
  # http://tex.stackexchange.com/q/276699/9128
  config$knitr$opts_knit$kable.force.latex = TRUE
  config
}

readUTF8 = function(input) {
  x = readLines(input, encoding = 'UTF-8', warn = FALSE)
  i = invalidUTF8(x)
  n = length(i)
  if (n > 0) warning(
    'The file ', input, ' is not encoded in UTF-8. These lines contain invalid ',
    'UTF-8 characters: ', paste(c(head(i), if (n > 6) '...'), collapse = ', ')
  )
  x
}

writeUTF8 = function(text, ...) {
  writeLines(enc2utf8(text), ..., useBytes = TRUE)
}

# which lines are invalid UTF-8
invalidUTF8 = function(x) {
  which(is.na(iconv(x, 'UTF-8', 'UTF-8')))
}

get_base_format = function(format) {
  if (is.character(format)) {
    format = eval(parse(text = format))
  }
  if (!is.function(format)) stop('The output format must be a function')
  format
}

load_config = function() {
  if (length(opts$get('config')) == 0 && file.exists('_bookdown.yml')) {
    # store the book config
    opts$set(config = yaml::yaml.load_file('_bookdown.yml'))
  }
  opts$get('config')
}

book_filename = function(config = load_config(), fallback = TRUE) {
  if (is.character(config[['book_filename']])) {
    config[['book_filename']][1]
  } else if (fallback) '_main'
}

source_files = function(format = NULL, config = load_config(), all = FALSE) {
  # a list of Rmd chapters
  files = list.files(
    '.', '[.]Rmd$', ignore.case = TRUE, recursive = isTRUE(config[['rmd_subdir']])
  )
  if (length(config[['rmd_files']]) > 0) {
    files = config[['rmd_files']]
    if (is.list(files)) {
      files = if (all && is.null(format)) unlist(files) else files[[format]]
    }
  } else {
    files = files[grep('^[^_]', basename(files))]  # exclude those start with _
    index = match('index', with_ext(files, ''))
    # if there is a index.Rmd, put it in the beginning
    if (!is.na(index)) files = c(files[index], files[-index])
  }
  check_special_chars(files)
}

output_dirname = function(dir, config = load_config(), create = TRUE) {
  if (is.null(dir)) {
    dir2 = config[['output_dir']]
    if (!is.null(dir2)) dir = dir2
  }
  if (is.null(dir)) dir = '_book'
  if (length(dir)) {
    if (create) dir_create(dir)
    # ignore dir that is just the current working directory
    if (same_path(dir, getwd(), mustWork = FALSE)) dir = NULL
  }
  dir
}

dir_exists = function(x) utils::file_test('-d', x)

# mark directories with trailing slashes
mark_dirs = function(x) {
  i = dir_exists(x)
  x[i] = paste0(x[i], '/')
  x
}

clean_empty_dir = function(dir) {
  if (!dir_exists(dir)) return()
  files = list.files(dir, all.files = TRUE, recursive = TRUE)
  if (length(files) == 0) unlink(dir, recursive = TRUE)
}

merge_chapters = function(files, to, before = NULL, after = NULL, orig = files) {
  # in the preview mode, only use some placeholder text instead of the full Rmd
  preview = opts$get('preview'); input = opts$get('input_rmd')
  content = unlist(mapply(files, orig, SIMPLIFY = FALSE, FUN = function(f, o) {
    x = readUTF8(f)
    x = if (preview && !(o %in% input)) create_placeholder(x) else {
      insert_code_chunk(x, before, after)
    }
    c(x, '', paste0('<!--chapter:end:', o, '-->'), '')
  }))
  if (preview && !(files[1] %in% input))
    content = c(fetch_yaml(readUTF8(files[1])), content)
  unlink(to)
  writeUTF8(content, to)
  Sys.chmod(to, '644')
}

match_dashes = function(x) grep('^---\\s*$', x)

create_placeholder = function(x) {
  h = grep('^# ', x, value = TRUE)  # chapter title
  h1 = grep(reg_part, h, value = TRUE)  # part title
  h2 = grep(reg_app, h, value = TRUE)   # appendix title
  h3 = setdiff(h, c(h1, h2))
  h4 = grep('^#{2,} ', x, value = TRUE)  # section/subsection/... titles
  c(
    '', placeholder(head(h1, 1)), placeholder(head(h2, 1)),
    placeholder(h3[1], '# Placeholder'), '', h4
  )
}

# x1: the title; x2: placeholder if title is empty
placeholder = function(x1, x2 = NULL) {
  if (length(x1) && !is.na(x1)) c(x1, '\nPlaceholder\n') else x2
}

fetch_yaml = function(x) {
  i = match_dashes(x)
  if (length(i) >= 2) x[(i[1]):(i[2])]
}

insert_code_chunk = function(x, before, after) {
  if (length(before) + length(after) == 0) return(x)
  if (length(x) == 0 || length(match_dashes(x[1])) == 0) return(c(before, x, after))
  i = match_dashes(x)
  if (length(i) < 2) {
    warning('There may be something wrong with your YAML frontmatter (no closing ---)')
    return(c(before, x, after))
  }
  # insert `before` after the line i[2], i.e. the second ---
  c(append(x, before, i[2]), after)
}

insert_chapter_script = function(config, where = 'before') {
  script = config[[sprintf('%s_chapter_script', where)]]
  if (is.character(script)) {
    c('```{r include=FALSE, cache=FALSE}', unlist(lapply(script, readUTF8)), '```')
  }
}

check_special_chars = function(filename) {
  reg = getFromNamespace('.shell_chars_regex', 'rmarkdown')
  for (i in grep(reg, filename)) warning(
    'The filename "', filename[i], '" contains special characters. ',
    'You may rename it to, e.g., "', gsub(reg, '-', filename[i]), '".'
  )
  if (!is.null(i)) stop('Filenames must not contain special characters')
  filename
}

Rscript = function(args, ...) {
  system2(file.path(R.home('bin'), 'Rscript'), args, ...)
}

Rscript_render = function(file, ...) {
  args = shQuote(c(bookdown_file('scripts', 'render_one.R'), file, ...))
  if (Rscript(args) != 0) stop('Failed to compile ', file)
}

clean_meta = function(meta_file, files) {
  meta = readRDS(meta_file)
  for (i in setdiff(names(meta), files)) meta[[i]] = NULL
  meta = setNames(meta[files], files)  # order by input filenames
  for (i in files) if (is.null(meta[[i]])) meta[[i]] = basename(with_ext(i, '.md'))
  saveRDS(meta, meta_file)
  meta
}

# remove HTML tags and remove extra spaces
strip_html = function(x) {
  x = gsub('<!--.*?-->', '', x)  # remove comments
  x = gsub('<[^>]+>', '', x)
  x = gsub('\\s{2,}', ' ', x)
  x
}

# remove the <script><script> content and references
strip_search_text = function(x) {
  x = gsub('<script[^>]*>(.*?)</script>', '', x)
  x = gsub('<div id="refs" class="references">.*', '', x)
  x = strip_html(x)
  x
}

# quote a string and escape backslashes/double quotes
json_string = function(x, toArray = FALSE) {
  knitr:::json_vector(x, toArray)
}

# manipulate internal options
opts = knitr:::new_defaults(list(config = list()))

dir_create = function(path) {
  dir_exists(path) || dir.create(path, recursive = TRUE)
}

# a wrapper of file.path to ignore `output_dir` if it is NULL
output_path = function(...) {
  dir = opts$get('output_dir')
  if (is.null(dir)) file.path(...) else file.path(dir, ...)
}

local_resources = function(x) {
  grep('^(f|ht)tps?://.+', x, value = TRUE, invert = TRUE)
}

#' Continously preview the HTML output of a book using the \pkg{servr} package
#'
#' When any files are modified or added to the book directory, the book will be
#' automatically recompiled, and the current HTML page in the browser will be
#' refreshed. This function is based on \code{servr::\link[servr]{httw}()} to
#' continuously watch a directory.
#'
#' For \code{in_session = TRUE}, you will have access to all objects created in
#' the book in the current R session: if you use a daemonized server (via the
#' argument \code{daemon = TRUE}), you can check the objects at any time when
#' the current R session is not busy; otherwise you will have to stop the server
#' before you can check the objects. This can be useful when you need to
#' interactively explore the R objects in the book. The downside of
#' \code{in_session = TRUE} is that the output may be different with the book
#' compiled from a fresh R session, because the state of the current R session
#' may not be clean.
#'
#' For \code{in_sesion = FALSE}, you do not have access to objects in the book
#' from the current R session, but the output is more likely to be reproducible
#' since everything is created from new R sessions. Since this function is only
#' for previewing purposes, the cleanness of the R session may not be a big
#' concern. You may choose \code{in_session = TRUE} or \code{FALSE} depending on
#' your specific applications. Eventually, you should run \code{render_book()}
#' from a fresh R session to generate a reliable copy of the book output.
#' @param dir The root directory of the book (containing the Rmd source files).
#' @param output_dir The directory for output files; see
#'   \code{\link{render_book}()}.
#' @param preview Whether to render the modified/added chapters only, or the
#'   whole book; see \code{\link{render_book}()}.
#' @param in_session Whether to compile the book using the current R session, or
#'   always open a new R session to compile the book whenever changes occur in
#'   the book directory.
#' @param ... Other arguments passed to \code{servr::\link[servr]{httw}()} (not
#'   including the \code{handler} argument, which has been set internally).
#' @export
serve_book = function(
  dir = '.', output_dir = '_book', preview = TRUE, in_session = TRUE, ...
) {
  # when this function is called via the RStudio addin, use the dir of the
  # current active document
  if (missing(dir) && requireNamespace('rstudioapi', quietly = TRUE)) {
    context_fun = tryCatch(
      getFromNamespace('getSourceEditorContext', 'rstudioapi'),
      error = function(e) rstudioapi::getActiveDocumentContext
    )
    path = context_fun()[['path']]
    if (!(is.null(path) || path == '')) dir = dirname(path)
  }
  owd = setwd(dir); on.exit(setwd(owd), add = TRUE)
  if (missing(output_dir) || is.null(output_dir)) {
    on.exit(opts$restore(), add = TRUE)
    output_dir = load_config()[['output_dir']]
  }
  if (is.null(output_dir)) output_dir = '_book'
  if (missing(preview)) preview = getOption('bookdown.preview', TRUE)
  output_format = first_html_format()
  rebuild = function(..., preview_ = preview) {
    files = grep('[.]R?md$', c(...), value = TRUE, ignore.case = TRUE)
    i = match(sans_ext(book_filename()), sans_ext(basename(files)))
    if (!is.na(i)) files = files[-i]
    i = grep('[.](knit|utf8)[.]md$', files)
    if (length(i)) files = files[-i]
    if (length(files) == 0) return()
    # if the output dir has been deleted, rebuild the whole book
    if (!dir_exists(output_dir)) preview_ = FALSE
    if (in_session) render_book(
      files, output_format, output_dir = output_dir, preview = preview_,
      envir = globalenv()
    ) else {
      args = shQuote(c(
        bookdown_file('scripts', 'servr.R'), output_format, output_dir, preview_, files
      ))
      if (Rscript(args) != 0) stop('Failed to compile ', paste(files, collapse = ' '))
    }
  }
  rebuild('index.Rmd', preview_ = FALSE)  # build the whole book initially
  servr::httw('.', ..., site.dir = output_dir, handler = rebuild)
}

# can only preview HTML output via servr, so look for the first HTML format
first_html_format = function() {
  fallback = 'bookdown::gitbook'
  if (!file.exists('index.Rmd')) return(fallback)
  formats = rmarkdown::all_output_formats('index.Rmd', 'UTF-8')
  formats = grep('gitbook|html', formats, value = TRUE)
  if (length(formats) == 0) fallback else formats[1]
}

sans_ext = knitr:::sans_ext

same_path = function(f1, f2, ...) {
  normalizePath(f1, ...) == normalizePath(f2, ...)
}

in_dir = knitr:::in_dir

# base64 encode resources in url("")
base64_css = function(css, exts = 'png', overwrite = FALSE) {
  x = readUTF8(css)
  r = sprintf('[.](%s)$', paste(exts, collapse = '|'))
  m = gregexpr('url\\("[^"]+"\\)', x)
  regmatches(x, m) = lapply(regmatches(x, m), function(ps) {
    if (length(ps) == 0) return(ps)
    ps = gsub('^url\\("|"\\)$', '', ps)
    sprintf('url("%s")', sapply(ps, function(p) {
      if (grepl(r, p) && file.exists(p)) knitr::image_uri(p) else p
    }))
  })
  if (overwrite) writeUTF8(x, css) else x
}

files_cache_dirs = function(dir = '.') {
  if (!dir_exists(dir)) return(character())
  out = list.files(dir, '_(files|cache)$', full.names = TRUE)
  out = out[dir_exists(out)]
  out = out[basename(out) != '_bookdown_files']
  out
}

# file.rename() does not work if target directory is not empty, so we just copy
# everything from `from` to `to`, and delete `from`
move_dir = function(from, to) {
  if (!dir_exists(to)) return(file.rename(from, to))
  if (file.copy(list.files(from, full.names = TRUE), to, recursive = TRUE))
    unlink(from, recursive = TRUE)
}

move_dirs = function(from, to) mapply(move_dir, from, to)

existing_files = function(x, first = FALSE) {
  x = x[file.exists(x)]
  if (first) head(x, 1) else x
}

existing_r = function(base, first = FALSE) {
  x = apply(expand.grid(base, c('R', 'r')), 1, paste, collapse = '.')
  existing_files(x, first)
}

html_or_latex = function(format) {
  if (grepl('(html|gitbook|epub)', format)) return('html')
  if (grepl('pdf', format)) return('latex')
  switch(format, tufte_book2 = 'latex', tufte_handout2 = 'latex')
}

verify_rstudio_version = function() {
  if (requireNamespace('rstudioapi', quietly = TRUE) && rstudioapi::isAvailable()) {
    if (!rstudioapi::isAvailable('0.99.1200')) warning(
      'Please install a newer version of the RStudio IDE: ',
      'https://www.rstudio.com/products/rstudio/download/'
    )
  } else if (!rmarkdown::pandoc_available('1.17.2')) warning(
    "Please install or upgrade Pandoc to at least version 1.17.2; ",
    "or if you are using RStudio, you can just install RStudio 1.0+."
  )
}

str_trim = function(x) gsub('^\\s+|\\s+$', '', x)

`%n%` = knitr:::`%n%`

output_md = function() getOption('bookdown.output.markdown', FALSE)

# a theorem engine for knitr (can also be used for lemmas, definitions, etc)
eng_theorem = function(options) {
  type = options$type %n% 'theorem'
  if (!(type %in% names(theorem_abbr))) stop(
    "The type of theorem '", type, "' is not supported yet."
  )
  options$type = type
  label = paste(theorem_abbr[type], options$label, sep = ':')
  html.before2 = sprintf('(\\#%s) ', label)
  name = options$name; to_md = output_md()
  if (length(name) == 1) {
    if (to_md) {
      html.before2 = paste(html.before2, sprintf('(%s) ', name))
    } else {
      options$latex.options = sprintf('[%s]', name)
      html.before2 = paste(html.before2, sprintf('\\iffalse (%s) \\fi{} ', name))
    }
  }
  options$html.before2 = sprintf(
    '<span class="%s" id="%s"><strong>%s</strong></span>', type, label, html.before2
  )
  process_block(options, to_md)
}

# a proof engine for unnumbered math environments
eng_proof = function(options) {
  type = options$type %n% 'proof'
  if (!(type %in% names(label_names_math2))) stop(
    "The type of proof '", type, "' is not supported yet."
  )
  options$type = type
  label = label_prefix(type, label_names_math2)
  name = options$name; to_md = output_md()
  if (length(name) == 1) {
    if (!to_md) options$latex.options = sprintf('[%s]', sub('[.]\\s*$', '', name))
    r = '^(.+?)([[:punct:][:space:]]+)$'  # "Remark. " -> "Remark (Name). "
    if (grepl(r, label)) {
      label1 = gsub(r, '\\1', label)
      label2 = paste0(' (', name, ')', gsub(r, '\\2', label))
    } else {
      label1 = label; label2 = ''
    }
    label = sprintf('<em>%s</em>%s', label1, label2)
  } else {
    label = sprintf('<em>%s</em>', label)
  }
  options$html.before2 = sprintf(
    '<span class="%s">%s</span> ', type, label
  )
  if (!to_md) options$html.before2 = paste('\\iffalse{}', options$html.before2, '\\fi{}')
  process_block(options, to_md)
}

process_block = function(options, md) {
  if (md) {
    code = options$code
    code = knitr:::pandoc_fragment(code)
    r = '^<p>(.+)</p>$'
    if (length(code) > 0 && grepl(r, code[1])) code[1] = gsub(r, '\\1', code[1])
    options$code = code
  }
  knitr:::eng_block2(options)
}

register_eng_math = function(envs, engine) {
  knitr::knit_engines$set(setNames(lapply(envs, function(env) {
    function(options) {
      options$type = env
      engine(options)
    }
  }), envs))
}
