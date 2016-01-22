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
  sub('[.][[:alnum:]]+$', ext, x)
}

# counters for figures/tables
new_counters = function(type, len) {
  base = matrix(0L, nrow = len, ncol = length(type), dimnames = list(NULL, type))
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
  if (file.exists('_config.yml')) yaml::yaml.load_file('_config.yml') else list()
}

merge_chapters = function(files, to, before = NULL, after = NULL) {
  content = unlist(lapply(files, function(f) {
    x = c(before, readUTF8(f), after)
    id = with_ext(f, '')  # base filename (without extension)
    c(x, '', paste0('<!--chapter:end:', id, '-->'), '')
  }))
  writeLines(enc2utf8(content), to, useBytes = TRUE)
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
