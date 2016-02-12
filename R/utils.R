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
  n1 = length(x); n2 = length(ext)
  if (n1 == 0) return(x)
  if (n2 == 1) sub('[.][[:alnum:]]+$', ext, x) else {
    if (n1 > 1 && n1 != n2) stop("'ext' must be of the same length as 'x'")
    mapply(sub, '[.][[:alnum:]]+$', ext, x)
  }
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

merge_chapters = function(files, to, before = NULL, after = NULL) {
  content = unlist(lapply(files, function(f) {
    x = readUTF8(f)
    # add the knit field to the YAML frontmatter of the Rmd document
    if (length(x) && x[1] != '---' && length(grep('^knit: ', x)) == 0) {
      writeUTF8(c('---', 'knit: "bookdown::render_book"', '---\n', x), f)
    }
    x = c(before, x, after)
    c(x, '', paste0('<!--chapter:end:', f, '-->'), '')
  }))
  writeUTF8(content, to)
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
  saveRDS(meta, meta_file)
  meta
}

# remove HTML tags
strip_html = function(x) gsub('<[^>]+>', '', x)

# quote a string and escape backslashes/double quotes
json_string = function(x, toArray = FALSE) {
  x = gsub('(["\\])', "\\\\\\1", x)
  x = gsub('[[:space:]]', " ", x)
  x = paste0('"', x, '"')
  if (toArray) paste0('[', paste(x, collapse = ','), ']') else x
}

# manipulate internal options
opts = knitr:::new_defaults(list(config = list()))

dir_create = function(path) {
  utils::file_test('-d', path) || dir.create(path, recursive = TRUE)
}

local_resources = function(x) {
  grep('^(f|ht)tps?://.+', x, value = TRUE, invert = TRUE)
}

#' Continously preview the HTML output of a book
#'
#' When any files are modified or added to the book directory, the book will be
#' automatically recompiled, and the current HTML page in the browser will be
#' refreshed. This function is based on \code{servr::\link[servr]{httw}()} to
#' continuously watch a directory.
#' @param dir The root directory of the book (containing the Rmd source files).
#' @param output_dir The directory for output files; see
#'   \code{\link{render_book}()}.
#' @param ... Other arguments passed to \code{servr::\link[servr]{httw}()} (not
#'   including the \code{handler} argument, which has been set internally).
#' @export
serve_book = function(dir = '.', output_dir = NULL, ...) {
  owd = setwd(dir); on.exit(setwd(owd), add = TRUE)
  if (is.null(output_dir)) {
    on.exit(opts$restore(), add = TRUE)
    output_dir = load_config()[['output_dir']]
  }
  if (is.null(output_dir)) output_dir = '.'
  servr::httw('.', ..., site.dir = output_dir, handler = function(...) {
    files = grep('[.]R?md$', c(...), value = TRUE, ignore.case = TRUE)
    files = files[dirname(files) == '.']
    if (length(files) == 0) return()
    args = shQuote(c(bookdown_file('scripts', 'servr.R'), output_dir, files))
    if (Rscript(args) != 0) stop('Failed to compile ', paste(files, collapse = ' '))
  })
}
