#' The EPUB e-book format
#'
#' Convert a book to the EPUB format, which is is an e-book format supported by
#' many readers, such as Amazon Kindle Fire and iBooks on Apple devices.
#' @param fig_width,fig_height,dev,fig_caption Figure options (width, height,
#'   the graphical device, and whether to render figure captions).
#' @param number_sections Whether to number sections.
#' @param toc,toc_depth Whether to generate a table of contents, and its depth.
#' @param stylesheet A character vector of paths to CSS stylesheets to be
#'   applied to the eBook.
#' @param cover_image The path to a cover image.
#' @param metadata The path to the EPUB metadata file.
#' @param chapter_level The level by which the e-book is split into separate
#'   \dQuote{chapter} files.
#' @param epub_version Whether to use version 3 or 2 of EPUB.
#' @param md_extensions A character string of Pandoc Markdown extensions.
#' @param pandoc_args A vector of additional Pandoc arguments.
#' @note Figure/table numbers cannot be generated if sections are not numbered
#'   (\code{number_sections = FALSE}).
#' @export
epub_book = function(
  fig_width = 5, fig_height = 4, dev = 'png', fig_caption = TRUE,
  number_sections = TRUE, toc = FALSE, toc_depth = 3, stylesheet = NULL,
  cover_image = NULL, metadata = NULL, chapter_level = 1,
  epub_version = c('epub3', 'epub'), md_extensions = NULL, pandoc_args = NULL
) {

  epub_version = match.arg(epub_version)
  args = c(
    pandoc_args,
    if (number_sections) '--number-sections',
    if (toc) '--toc',
    if (!missing(toc_depth)) c('--toc-depth', toc_depth),
    if (!is.null(cover_image)) c('--epub-cover-image', cover_image),
    if (!is.null(metadata)) c('--epub-metadata', metadata),
    if (!missing(chapter_level)) c('--epub-chapter-level', chapter_level)
  )
  if (is.null(stylesheet)) css = NULL else {
    css = rmarkdown::pandoc_path_arg(epub_css(stylesheet))
    args = c(args, '--epub-stylesheet', css)
  }

  from_rmarkdown = getFromNamespace('from_rmarkdown', 'rmarkdown')
  from = from_rmarkdown(fig_caption, md_extensions)

  config = rmarkdown::output_format(
    knitr = rmarkdown::knitr_options_html(fig_width, fig_height, NULL, FALSE, dev),
    pandoc = rmarkdown::pandoc_options(epub_version, from, args, ext = '.epub'),
    pre_processor = function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
      intermediate_html = with_ext(input_file, 'tmp.html')
      on.exit(unlink(intermediate_html), add = TRUE)
      rmarkdown::pandoc_convert(
        input_file, 'html', from, intermediate_html, TRUE, c(args, '--section-divs')
      )
      x = readUTF8(intermediate_html)
      figs = parse_fig_labels(x, !number_sections)
      # resolve cross-references and update the Markdown input file
      content = resolve_refs_md(
        readUTF8(input_file), c(figs$ref_table, parse_section_labels(x))
      )
      writeUTF8(content, input_file)
      NULL
    },
    post_processor = function(metadata, input, output, clean, verbose) {
      unlink(css)
      if (is.null(opts$get('output_dir'))) return(output)
      output2 = output_path(output)
      file.rename(output, output2)
      output2
    }
  )
  config$bookdown_output_format = 'epub'
  config = set_opts_knit(config)
  config
}

resolve_refs_md = function(content, ref_table) {
  ids = names(ref_table)
  # replace (\#fig:label) with Figure x.x:
  for (i in grep('^(<p class="caption|<caption>|Table:)', content)) {
    for (j in ids) {
      if (grepl(j, content[i], fixed = TRUE)) {
        type = ifelse(grepl('^fig:', j), 'Figure', 'Table')
        content[i] = sub(
          sprintf('\\(\\\\#%s\\)', j),
          sprintf('<span id="%s"></span>%s %s: ', j, type, ref_table[j]),
          content[i]
        )
        break
      }
    }
  }
  # remove labels in figure alt text (it will contain \ like (\#fig:label))
  content = gsub('"\\(\\\\#(fig:[-[:alnum:]]+)\\)', '"', content)

  # look for \@ref(label) and resolve to actual figure/table/section numbers
  m = gregexpr(' \\\\@ref\\(([-:[:alnum:]]+)\\)', content)
  refs = regmatches(content, m)
  regmatches(content, m) = lapply(refs, ref_to_number, ref_table)
  content
}

# manually base64 encode images in css: https://github.com/jgm/pandoc/issues/2733
epub_css = function(files, output = tempfile('epub', fileext = '.css')) {
  css = unlist(lapply(files, function(css) {
    in_dir(dirname(css), base64_css(basename(css)))
  }))
  writeUTF8(css, output)
  output
}

#' A wrapper function to convert EPUB to the Mobipocket format
#'
#' This function simply calls the command line tool \command{kindlegen} provided
#' by Amazon to convert EPUB e-books to the Mobipocket format (\file{.mobi}).
#' @param epub The path to a \code{.epub} file (e.g. created from the
#'   \code{\link{epub_book}()} format). If missing, it is automatically guessed
#'   from the book configurations.
#' @param exec The path to the executable \command{kindlegen}, which can be
#'   downloaded from
#'   \url{http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765211}.
#' @return The path of the \file{.mobi} file if the conversion is successful.
#' @export
kindlegen = function(epub, exec = Sys.which('kindlegen')) {
  if (exec == '') stop(
    'Cannot find the executable KindleGen. You may download it from ',
    'http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000765211'
  )
  if (missing(epub)) {
    on.exit(opts$restore(), add = TRUE)
    config = load_config()
    main = with_ext(book_filename(config), 'epub')
    epub = file.path(output_dirname('_book', config), main)
  }
  if (!file.exists(epub)) stop('The EPUB file ', epub, ' does not exist')
  mobi = with_ext(epub, 'mobi')
  unlink(mobi)
  system2(exec, shQuote(epub))
  if (!file.exists(mobi)) stop('Failed to convert epub to mobi')
  mobi
}
