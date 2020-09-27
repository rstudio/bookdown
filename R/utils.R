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

# set common format config
common_format_config = function(
  config, format, file_scope = getOption('bookdown.render.file_scope', FALSE)
) {

  # provide file_scope if requested
  if (file_scope) config$file_scope = md_chapter_splitter

  # set output format
  config$bookdown_output_format = format

  # use labels of the form (\#label) in knitr
  config$knitr$opts_knit$bookdown.internal.label = TRUE
  # when the output is LaTeX, force LaTeX tables instead of default Pandoc tables
  # http://tex.stackexchange.com/q/276699/9128
  config$knitr$opts_knit$kable.force.latex = TRUE
  config
}

get_base_format = function(format, options = list()) {
  if (is.character(format)) format = eval(parse(text = format))
  if (!is.function(format)) stop('The output format must be a function')
  # make sure named elements in `options` have corresponding named arguments in
  # the format function, unless the function has the ... argument
  nms = names(formals(format))
  if (!('...' %in% nms)) options = options[names(options) %in% c(nms, '')]
  do.call(format, options)
}

load_config = function() {
  if (length(opts$get('config')) == 0 && file.exists('_bookdown.yml')) {
    # store the book config
    opts$set(config = rmarkdown:::yaml_load_file('_bookdown.yml'))
  }
  opts$get('config')
}

book_filename = function(config = load_config(), fallback = TRUE) {
  if (is.character(config[['book_filename']])) {
    config[['book_filename']][1]
  } else if (fallback) '_main'
}

source_files = function(format = NULL, config = load_config(), all = FALSE) {
  subdir = config[['rmd_subdir']]; subdir_yes = isTRUE(subdir) || is.character(subdir)
  # a list of Rmd chapters
  files = list.files('.', '[.]Rmd$', ignore.case = TRUE)
  # content in subdir if asked
  subdir_files = unlist(mapply(
    list.files,
    if (is.character(subdir)) subdir else '.', '[.]Rmd$', ignore.case = TRUE,
    recursive = subdir_yes, full.names = is.character(subdir), USE.NAMES = FALSE
  ))
  subdir_files = setdiff(subdir_files, files)
  files = c(files, subdir_files)
  # if rmd_files is provided, use those files in addition to those under rmd_subdir
  if (length(files2 <- config[['rmd_files']]) > 0) {
    # users should specify 'docx' as the output format name for Word, but let's
    # make 'word' an alias of 'docx' to avoid further confusion:
    # https://stackoverflow.com/q/63678601/559676
    if ('word' %in% names(files2) && identical(format, 'docx')) format = 'word'
    if (is.list(files2)) files2 = if (all) unlist(files2) else files2[[format]]
    # add those files to subdir content if any
    files = if (subdir_yes) c(files2, subdir_files) else files2
  }
  # exclude files that start with _, and the merged file
  files = files[grep('^[^_]', basename(files))]
  files = setdiff(files, with_ext(book_filename(config), c('.md', '.Rmd')))
  files = unique(gsub('^[.]/', '', files))
  index = 'index' == with_ext(files, '')
  # if there is a index.Rmd, put it in the beginning
  if (any(index)) files = c(files[index], files[!index])
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
    if (same_path(dir, getwd())) dir = NULL
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
  if (is.null(dir) || !dir_exists(dir)) return()
  files = list.files(dir, all.files = TRUE, recursive = TRUE)
  if (length(files) == 0) unlink(dir, recursive = TRUE)
}

merge_chapters = function(files, to, before = NULL, after = NULL, orig = files) {
  # in the preview mode, only use some placeholder text instead of the full Rmd
  preview = opts$get('preview'); input = opts$get('input_rmd')
  content = unlist(mapply(files, orig, SIMPLIFY = FALSE, FUN = function(f, o) {
    x = read_utf8(f)
    # if a chapter is short enough (<= 30 lines), just include the full chapter for preview
    preview = preview && length(x) >= getOption('bookdown.preview.cutoff', 30)
    x = if (preview && !(o %in% input)) create_placeholder(x) else {
      insert_code_chunk(x, before, after)
    }
    c(x, '', paste0('<!--chapter:end:', o, '-->'), '')
  }))
  if (preview && !(files[1] %in% input))
    content = c(fetch_yaml(read_utf8(files[1])), content)
  unlink(to)
  write_utf8(content, to)
  Sys.chmod(to, '644')
}

# split a markdown file into a set of chapters
md_chapter_splitter = function(file) {
  x = read_utf8(file)

  # get positions of the chapter delimiters (r_chap_pattern defined in html.R)
  if (length(pos <- grep(r_chap_pattern, x)) <= 1) return()
  pos = c(0, pos)

  # get the filenames
  names = gsub(r_chap_pattern, '\\1', x[pos])

  # extract the chapters and pair them w/ the names
  lapply(seq_along(names), function(i) {
    i1 = pos[i] + 1
    i2 = pos[i + 1]
    list(name = names[i], content = x[i1:i2])
  })
}

match_dashes = function(x) grep('^---\\s*$', x)

create_placeholder = function(x) {
  # filter out fenced code blocks (which may contain #'s that are comments)
  x = x[xfun::prose_index(x)]
  h = grep('^# ', x, value = TRUE)  # chapter title
  h1 = grep(reg_part, h, value = TRUE)  # part title
  h2 = grep(reg_app, h, value = TRUE)   # appendix title
  h3 = setdiff(h, c(h1, h2))
  h4 = grep('^#{2,} ', x, value = TRUE)  # section/subsection/... titles
  c('', head(h1, 1), head(h2, 1), placeholder(h3), '', h4)
}

# add a placeholder paragraph
placeholder = function(x) {
  if (length(x)) c(x[1], '\nPlaceholder\n')
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
  script = get_chapter_script(config, where)
  if (is.character(script)) {
    c('```{r include=FALSE, cache=FALSE}', script, '```')
  }
}

get_chapter_script = function(config, where) {
  script = config[[sprintf('%s_chapter_script', where)]]
  unlist(lapply(script, read_utf8))
}

merge_chapter_script = function(config, where) {
  if (!is.character(script <- get_chapter_script(config, where)) || length(script) == 0)
    return('')
  f = tempfile(fileext = '.R')
  write_utf8(script, f)
  f
}

check_special_chars = function(filename) {
  reg = rmarkdown:::.shell_chars_regex
  for (i in grep(reg, filename)) warning(
    'The filename "', filename[i], '" contains special characters. ',
    'You may rename it to, e.g., "', gsub(reg, '-', filename[i]), '".'
  )
  if (!is.null(i)) stop('Filenames must not contain special characters')
  filename
}

Rscript = function(...) xfun::Rscript(...)

Rscript_render = function(file, ...) {
  args = shQuote(c(bookdown_file('scripts', 'render_one.R'), file, ...))
  if (Rscript(args) != 0) stop('Failed to compile ', file)
}

source_utf8 = function(file) {
  if (file == '') return()
  eval(xfun::parse_only(read_utf8(file)), envir = globalenv())
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
  x = gsub('[[:space:]]', ' ', x)
  x
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

# write out reference keys to _book/reference-keys.txt (for the RStudio visual
# editor to autocomplete \@ref())
write_ref_keys = function(x) {
  # this only works for books rendered with bookdown::render_book() (and not for
  # rmarkdown::render())
  if (is.null(preview <- opts$get('preview'))) return()
  # collect reference keys from parse_fig_labels() and parse_section_labels()
  if (is.null(d <- opts$get('output_dir'))) return()
  p = ref_keys_path(d)
  if (file.exists(p)) x = unique(c(xfun::read_utf8(p), x))
  xfun::write_utf8(x, p)
}

ref_keys_path = function(d = opts$get('output_dir')) {
  file.path(d, 'reference-keys.txt')
}

#' Continuously preview the HTML output of a book using the \pkg{servr} package
#'
#' When any files are modified or added to the book directory, the book will be
#' automatically recompiled, and the current HTML page in the browser will be
#' refreshed. This function is based on \code{servr::\link[servr:httd]{httw}()}
#' to continuously watch a directory.
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
#' @param quiet Whether to suppress output (e.g., the knitting progress) in the
#'   console.
#' @param ... Other arguments passed to \code{servr::\link[servr:httd]{httw}()}
#'   (not including the \code{handler} argument, which has been set internally).
#' @export
serve_book = function(
  dir = '.', output_dir = '_book', preview = TRUE, in_session = TRUE, quiet = FALSE, ...
) {
  # when this function is called via the RStudio addin, use the dir of the
  # current active document
  if (missing(dir) && requireNamespace('rstudioapi', quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    path = rstudioapi::getSourceEditorContext()[['path']]
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
      envir = globalenv(), quiet = quiet
    ) else {
      args = shQuote(c(
        bookdown_file('scripts', 'servr.R'), output_format, output_dir, preview_,
        quiet, files
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
  formats = rmarkdown::all_output_formats('index.Rmd')
  formats = grep('gitbook|html', formats, value = TRUE)
  if (length(formats) == 0) fallback else formats[1]
}

# base64 encode resources in url("")
base64_css = function(css, exts = 'png', overwrite = FALSE) {
  x = read_utf8(css)
  r = sprintf('[.](%s)$', paste(exts, collapse = '|'))
  m = gregexpr('url\\("[^"]+"\\)', x)
  regmatches(x, m) = lapply(regmatches(x, m), function(ps) {
    if (length(ps) == 0) return(ps)
    ps = gsub('^url\\("|"\\)$', '', ps)
    sprintf('url("%s")', sapply(ps, function(p) {
      if (grepl(r, p) && file.exists(p)) knitr::image_uri(p) else p
    }))
  })
  if (overwrite) write_utf8(x, css) else x
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

target_format = function(format) {
  if (grepl('(html|gitbook)', format)) return('html')
  if (grepl('pdf', format)) return('latex')
  if (grepl('epub_', format)) return('epub')
  if (grepl('word_', format)) return('docx')
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

pandoc2.0 = function() rmarkdown::pandoc_available('2.0')

# remove the body of the LaTeX document; only keep section headers and
# figure/table captions
strip_latex_body = function(x, alt = '\nThe content was intentionally removed.\n') {
  i = which(x == '\\mainmatter')
  if (length(i) == 0) i = which(x == '\\begin{document}')
  x1 = head(x, i[1])  # preamble (or frontmatter)
  x2 = tail(x, -i[1]) # body
  i = grep('^\\\\(part|chapter|(sub)*section)\\*?\\{', x2)  # headers
  x2[i] = sub('}}$', '}\n', x2[i])  # get rid of the closing } from \hypertarget{
  x2[i] = paste0(x2[i], alt)
  i = c(i, grep('^\\\\bibliography', x2))
  # extract figure/table environments
  envs = list(
    fig = c('figure', '.*(\\\\caption\\{.+})\\\\label\\{fig:.+}.*'),
    tab = c('table', '^(\\\\caption\\{\\\\label\\{tab:.+}).*')
  )
  for (j in names(envs)) {
    r = envs[[j]][2]; i2 = grep(r, x2); env = envs[[j]][1]
    x2[i2] = sprintf('\\begin{%s}%s\\end{%s}\n', env, gsub(r, '\\1', x2[i2]), env)
    i = c(i, i2)
  }
  c(x1, x2[sort(i)], '\\end{document}')
}
