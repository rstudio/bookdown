bookdown_file = function(...) {
  system.file(..., package = 'bookdown')
}

# find the y[j] closest to x[i] with y[j] > x[i]
next_nearest = function(x, y) {
  n = length(x); m = length(y); z = integer(n)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (y[j] > x[i]) {
        z[i] = y[j]
        break
      }
    }
  }
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
  config$knitr$opts_knit$bookdown.table.latex = TRUE
  config
}

readUTF8 = function(input) {
  readLines(input, encoding = 'UTF-8', warn = FALSE)
}

writeUTF8 = function(text, ...) {
  writeLines(enc2utf8(text), ..., useBytes = TRUE)
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
    opts$set(config = yaml::yaml.load_file(('_bookdown.yml')))
  }
  opts$get('config')
}

book_filename = function(config = load_config()) {
  if (is.character(config[['book_filename']])) {
    config[['book_filename']][1]
  } else '_main'
}

source_files = function(format = NULL, config = load_config(), all = FALSE) {
  # a list of Rmd chapters
  files = list.files('.', '[.]Rmd$', ignore.case = TRUE)
  if (is.character(config[['rmd_files']])) {
    files = config[['rmd_files']]
    if (is.list(files)) {
      files = if (all && is.null(format)) unlist(files) else files[[format]]
    }
  } else {
    files = grep('^[^_]', files, value = TRUE)  # exclude those start with _
    index = match('index', with_ext(files, ''))
    # if there is a index.Rmd, put it in the beginning
    if (!is.na(index)) files = c(files[index], files[-index])
  }
  check_special_chars(files)
}

output_dirname = function(dir, config = load_config(), use_default = TRUE, create = TRUE) {
  if (use_default) {
    dir2 = config[['output_dir']]
    if (!is.null(dir2)) dir = dir2
  }
  if (length(dir)) {
    if (create) dir_create(dir)
    # ignore dir that is just the current working directory
    if (same_path(dir, getwd(), mustWork = FALSE)) dir = NULL
  }
  dir
}

merge_chapters = function(files, to, before = NULL, after = NULL, orig = files) {
  # in the preview mode, only use some placeholder text instead of the full Rmd
  preview = opts$get('preview'); input = opts$get('input_rmd')
  content = unlist(mapply(files, orig, SIMPLIFY = FALSE, FUN = function(f, o) {
    x = readUTF8(f)
    # add the knit field to the YAML frontmatter of the Rmd document
    if (length(x) && x[1] != '---' && length(grep('^knit: ', x)) == 0) {
      writeUTF8(c('---', 'knit: bookdown::preview_chapter', '---\n', x), f)
    }
    if (preview && !(o %in% input)) x = create_placeholder(x)
    x = c(before, x, after)
    c(x, '', paste0('<!--chapter:end:', o, '-->'), '')
  }))
  if (preview) content = c(create_placeholder(readUTF8(files[1]), FALSE), content)
  writeUTF8(content, to)
}

create_placeholder = function(x, header = TRUE) {
  h = grep('^# ', x, value = TRUE)  # chapter title
  h = c('', if (length(h)) h[1] else '# Placeholder')
  i = grep('^---\\s*$', x)
  c(if (length(i) >= 2) x[(i[1]):(i[2])], if (header) h)
}

insert_chapter_script = function(config, where = 'before') {
  script = config[[sprintf('%s_chapter_script', where)]]
  if (is.character(script)) {
    c('```{r include=FALSE}', unlist(lapply(script, readUTF8)), '```')
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

Rscript = function(args) {
  system2(file.path(R.home('bin'), 'Rscript'), args)
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

# remove HTML tags
strip_html = function(x) gsub('<[^>]+>', '', x)

# quote a string and escape backslashes/double quotes
json_string = function(x, toArray = FALSE) {
  json_vector(x, toArray)
}

json_vector = function(x, toArray = FALSE, quote = TRUE) {
  if (quote) {
    x = gsub('(["\\])', "\\\\\\1", x)
    x = gsub('[[:space:]]', " ", x)
    if (length(x)) x = paste0('"', x, '"')
  }
  if (toArray) paste0('[', paste(x, collapse = ', '), ']') else x
}

# manipulate internal options
opts = knitr:::new_defaults(list(config = list()))

dir_create = function(path) {
  utils::file_test('-d', path) || dir.create(path, recursive = TRUE)
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
#' @param dir The root directory of the book (containing the Rmd source files).
#' @param output_dir The directory for output files; see
#'   \code{\link{render_book}()}.
#' @param preview Whether to render the modified/added chapters only, or the
#'   whole book; see \code{\link{render_book}()}.
#' @param ... Other arguments passed to \code{servr::\link[servr]{httw}()} (not
#'   including the \code{handler} argument, which has been set internally).
#' @export
serve_book = function(dir = '.', output_dir = NULL, preview = TRUE, ...) {
  # when this function is called via the RStudio addin, use the dir of the
  # current active document
  if (missing(dir) && requireNamespace('rstudioapi', quietly = TRUE)) {
    path = rstudioapi::getActiveDocumentContext()[['path']]
    if (!(is.null(path) || path == '')) dir = dirname(path)
  }
  owd = setwd(dir); on.exit(setwd(owd), add = TRUE)
  if (is.null(output_dir)) {
    on.exit(opts$restore(), add = TRUE)
    output_dir = load_config()[['output_dir']]
  }
  if (is.null(output_dir)) output_dir = '.'
  rebuild = function(..., preview_ = preview) {
    files = grep('[.]R?md$', c(...), value = TRUE, ignore.case = TRUE)
    files = files[dirname(files) == '.']
    if (length(files) == 0) return()
    # if the output dir has been deleted, rebuild the whole book
    if (!utils::file_test('-d', output_dir)) preview_ = FALSE
    args = shQuote(c(bookdown_file('scripts', 'servr.R'), output_dir, preview_, files))
    if (Rscript(args) != 0) stop('Failed to compile ', paste(files, collapse = ' '))
  }
  rebuild('index.Rmd', preview_ = FALSE)  # build the whole book initially
  servr::httw('.', ..., site.dir = output_dir, handler = rebuild)
}

# a simple JSON serializer
tojson = function(x) {
  if (is.null(x)) return('null')
  if (is.logical(x)) {
    if (length(x) != 1 || any(is.na(x)))
      stop('Logical values of length > 1 and NA are not supported')
    return(tolower(as.character(x)))
  }
  if (is.character(x) || is.numeric(x)) {
    return(json_vector(x, length(x) != 1 || inherits(x, 'AsIs'), is.character(x)))
  }
  if (is.list(x)) {
    if (length(x) == 0) return('{}')
    return(if (is.null(names(x))) {
      json_vector(unlist(lapply(x, tojson)), TRUE, quote = FALSE)
    } else {
      nms = paste0('"', names(x), '"')
      paste0('{\n', paste(nms, unlist(lapply(x, tojson)), sep = ': ', collapse = ',\n'), '\n}')
    })
  }
  stop('The class of x is not supported: ', paste(class(x), collapse = ', '))
}

same_path = function(f1, f2, ...) {
  normalizePath(f1, ...) == normalizePath(f2, ...)
}
