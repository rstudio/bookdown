#' HTML book built with bootstrap4.
#'
#' @description
#' This output format is designed to give a clean reading experience.
#' Main features:
#'
#' * A per section-search engine that helps you quickly find what you're
#'   looking for.
#'
#' * A sidebar that includes a within-chapter table of contents that
#'   dynamically updates so it's easy to keep track of where you are
#'   within the chapter.
#'
#' * Thoughtful typography to make the contents as easy as possible to read,
#'   regardless of the size of your device. A sticky header gets out of your
#'   way when reading, but is easily accessible if you need it.
#'
#' * In-line footnotes.
#'
#' * R syntax highlighting and autolinking by
#'   [downlit](http://downlit.r-lib.org/) is paired with a accessible
#'   theme designed by Alison Hill.
#'
#' * The ability to customise colours and fonts through
#'   [bootstraplib](https://rstudio.github.io/bootstraplib)
#'
#' @section Limitations:
#'
#' * `bs_book4()` is designed specifically for books that use one chapter per
#'   page.
#'
#' @param theme A named list or [bootstraplib::bs_theme()] object.
#'   The default, `bs4_book_theme()` resets the base font size to 1rem, to
#'   make reading easier. A named list will be passed to `bs4_book_theme()`,
#'   making it possible to specify theme settings in the yaml metadata.
#' @param repo Link to repository where book is hosted. Currently assumes
#'   GitHub and that the book is in the root directory of the repo.
#' @param lib_dir,pandoc_args,extra_dependencies,... Passed on to
#'   [rmarkdown::html_document()].
#' @export
bs4_book <- function(
                     theme = bs4_book_theme(),
                     repo = NULL,
                     ...,
                     lib_dir = "libs",
                     pandoc_args = NULL,
                     extra_dependencies = NULL
                     ) {
  check_packages(c("bootstraplib", "downlit", "jsonlite", "xml2"))

  # Allow theme specification in yaml metadata
  if (!inherits(theme, "bs_theme")) {
    theme <- do.call(bs4_book_theme, theme)
  }

  config <- rmarkdown::html_document(
    toc = FALSE,
    number_sections = TRUE,
    anchor_sections = FALSE,
    self_contained = FALSE,
    theme = NULL,
    template = bookdown_file("templates", "bs4_book.html"),
    pandoc_args = pandoc_args2(pandoc_args),
    lib_dir = lib_dir,
    extra_dependencies = c(bs4_book_dependency(theme), extra_dependencies),
    ...
  )

  post <- config$post_processor # in case a post processor have been defined
  config$post_processor <- function(metadata, input, output, clean, verbose) {
    if (is.function(post)) {
      output <- post(metadata, input, output, clean, verbose)
    }

    output2 <- bs4_book_build(output, repo = repo, lib_dir = lib_dir)

    if (clean && file.exists(output) && !same_path(output, output2)) {
      file.remove(output)
    }
    output2
  }

  # config <- common_format_config(config, "html")
  config
}

#' @export
#' @rdname bs4_book
bs4_book_theme <- function(...) {
  bootstraplib::bs_theme(..., "font-size-base" = "1rem")
}

bs4_book_build <- function(output = "bookdown.html",
                           repo = NULL,
                           lib_dir = "libs",
                           output_dir = opts$get("output_dir")
                           ) {
  move_files_html(output, lib_dir)

  rmd_index <- new.env(parent = emptyenv())

  output2 <- split_chapters(
    output = output,
    build = function(...) bs4_book_page(..., rmd_index = rmd_index),
    number_sections = TRUE,
    split_by = "chapter",
    split_bib = FALSE
  )
  move_files_html(output2, lib_dir)

  rmd_index <- vapply(as.list(rmd_index), force, character(1))

  bs4_chapters_tweak(output,
    repo = repo,
    rmd_index = rmd_index,
    output_dir = output_dir
  )

  output2
}

build_toc <- function(output) {
  html <- xml2::read_html(output)

  headings <- xml2::xml_find_all(html, ".//h1|.//h2|.//h3")
  toc <- data.frame(
    tag = xml2::xml_name(headings),
    id = xml2::xml_attr(xml2::xml_find_first(headings, "parent::div"), "id"),
    num = xml2::xml_attr(headings, "number"),
    text = xml2::xml_text(headings),
    stringsAsFactors = FALSE
  )
  if (requireNamespace("tibble", quietly = TRUE)) {
    toc <- tibble::as_tibble(toc)
  }

  # Strip numbers from heading text
  toc$text <- ifelse(
    is.na(toc$num),
    toc$text,
    substr(toc$text, nchar(toc$num) + 2, nchar(toc$text))
  )

  # Determine hierarchy
  toc$level <- unname(c("h1" = 1, "h2" = 2, "h3" = 3)[toc$tag])
  toc$tag <- NULL
  is_part <- grepl("\\(PART\\*?\\)", toc$text)
  is_appendix <- grepl("\\(APPENDIX\\*?\\)", toc$text)

  toc$level[is_part | is_appendix] <- 0
  toc$text[is_part] <- gsub("\\(PART\\*?\\) ", "", toc$text[is_part])
  toc$text[is_appendix] <- gsub("\\(APPENDIX\\*?\\) ", "", toc$text[is_appendix])

  # Figure book structure
  new_chapter <- toc$level == 1
  toc$chapter <- cumsum(new_chapter)
  toc$part <- cumsum(is_part)

  chapter_files <- tapply(toc$id, toc$chapter, "[[", 1)
  chapter_files[[1]] <- "index"
  toc$file_name <- paste0(chapter_files[as.character(toc$chapter)], ".html")
  toc$file_name[toc$level == 0] <- NA

  toc
}

bs4_book_page = function(head,
                         toc,
                         chapter,
                         link_prev,
                         link_next,
                         rmd_cur,
                         html_cur,
                         foot,
                         rmd_index = NULL) {
  rmd_index[[html_cur]] <- rmd_cur
  paste(c(head, toc, chapter, foot), collapse = '\n')
}

bs4_book_dependency <- function(theme) {
  assets <- bookdown_file("resources", "bs4_book")

  c(
    bootstraplib::bs_theme_dependencies(theme),
    list(
      htmltools::htmlDependency(
        name = "bs4_book",
        version = "1.0.0",
        src = assets,
        stylesheet = c("bootstrap-toc.css", "bs4_book.css"),
        script = c("bootstrap-toc.js", "bs4_book.js", "headroom.js")
      )
    )
  )
}


# HTML manip --------------------------------------------------------------

bs4_chapters_tweak <- function(output,
                               rmd_index = NULL,
                               repo = NULL,
                               output_dir = opts$get("output_dir")) {
  toc <- build_toc(output)

  files <- toc[!duplicated(toc$file_name) & !is.na(toc$file_name), ]
  files$path <- file.path(output_dir, files$file_name)

  index <- vector("list", nrow(files))

  for (i in seq_len(nrow(files))) {
    path <- files$path[[i]]
    html <- xml2::read_html(path, encoding = "UTF-8")

    tweak_tables(html)
    tweak_chapter(html)
    tweak_anchors(html)
    tweak_footnotes(html)
    tweak_navbar(html, toc, basename(path), rmd_index = rmd_index, repo = repo)
    downlit::downlit_html_node(html)

    sections <- xml2::xml_find_all(html, ".//div[contains(@class, 'section')]")
    h1 <- xml_text1(xml2::xml_find_first(html, "//h1"))
    index[[i]] <- lapply(sections, bs4_index_data,
      chapter = h1,
      path = basename(path)
    )

    xml2::write_html(html, path, format = FALSE)
  }

  index <- unlist(index, recursive = FALSE, use.names = FALSE)
  jsonlite::write_json(
    index,
    file.path(output_dir, "search.json"),
    auto_unbox = TRUE
  )
}

tweak_chapter <- function(html) {
  num <- xml2::xml_find_all(html, ".//h1//span[@class='header-section-number']")
  xml2::xml_text(num) <- gsub("Chapter ", "", xml2::xml_text(num))
}

tweak_footnotes <- function(html) {
  container <- xml2::xml_find_all(html, ".//div[@class='footnotes']")
  if (length(container) != 1) {
    return()
  }

  # Find id and contents
  footnotes <- xml2::xml_find_all(container, ".//li")
  id <- xml2::xml_attr(footnotes, "id")
  xml2::xml_remove(xml2::xml_find_all(footnotes, "//a[@class='footnote-back']"))
  contents <- vapply(footnotes, FUN.VALUE = character(1), function(x) {
    as.character(xml2::xml_children(x))
  })

  # Add popover attributes to links
  for (i in seq_along(id)) {
    links <- xml2::xml_find_all(html, paste0(".//a[@href='#", id[[i]], "']"))
    xml2::xml_attr(links, "href") <- NULL
    xml2::xml_attr(links, "id") <- NULL
    xml2::xml_attr(links, "tabindex") <- "0"
    xml2::xml_attr(links, "data-toggle") <- "popover"
    xml2::xml_attr(links, "data-content") <- contents[[i]]
  }

  # Delete container
  xml2::xml_remove(container)
}

tweak_anchors <- function(html) {
  headings <- xml2::xml_find_all(html, "(.//h1|.//h2|.//h3|.//h4|.//h5|.//h6)")
  id <- xml2::xml_attr(xml2::xml_find_first(headings, "parent::div"), "id")

  anchor <- paste0(
    "<a class='anchor' aria-label='anchor' href='#", id, "'>",
    "<i class='fas fa-link'></i>",
    "</a>"
  )

  for (i in seq_along(id)) {
    xml2::xml_add_child(headings[[i]], xml2::read_xml(anchor[[i]]))
  }
}

# Ensure all tables have class="table"
tweak_tables <- function(html) {
  table <- xml2::xml_find_all(html, ".//table")

  if (length(table) > 0) {
    class <- xml2::xml_attr(table, "class")
    xml2::xml_attr(table[is.na(class)], "class") <- "table table-sm"
  }

  invisible()
}

tweak_navbar <- function(html, toc, active = "", rmd_index = NULL, repo = NULL) {

  # Source links ------------------------------------------------------------
  repo_node <- xml2::xml_find_first(html, ".//a[@id='book-repo']")
  if (is.null(repo)) {
    # Remove parent <li>
    xml2::xml_remove(xml2::xml_parent(repo_node))
  } else {
    xml2::xml_attr(repo_node, "href") <- repo
  }

  edit_note <- xml2::xml_find_first(html, ".//a[@id='book-edit']")
  if (is.null(repo) || !active %in% names(rmd_index)) {
    # Remove parent <li>
    xml2::xml_remove(xml2::xml_parent(edit_note))
  } else {
    edit_url <- paste0(repo, "/edit/master/", rmd_index[[active]])
    xml2::xml_attr(edit_note, "href") <- edit_url
  }

  # TOC ---------------------------------------------------------------------
  nav <- toc[toc$level %in% 0:1, ]
  nav <- nav[!duplicated(nav$file_name) | is.na(nav$file_name), ]

  is_active <- nav$file_name == active
  class <- ifelse(is_active, "dropdown-item active", "dropdown-item")
  a <- paste0(
    "<a class='", class, "' href='", nav$file_name, "'>",
    nav$text,
    "</a>"
  )
  a[is.na(nav$file_name)] <- paste0(
    "</div>",
    "<div class='book-part'>",
    "<div class='dropdown-divider'></div>",
    "<h6 class='dropdown-header'>", nav$text[is.na(nav$file_name)], "</h6>"
  )

  to_insert <- paste0(
    "<div class='dropdown-menu' aria-labelledby='navbar-toc'>\n",
    "<div class='book-toc'>",
    "<div class='book-part'>",
    paste0("  ", a, "\n", collapse = ""),
    "</div>",
    "</div>",
    "</div>\n"
  )

  dropdown <- xml2::xml_find_first(html, ".//div[@id='book-toc']")
  xml2::xml_replace(dropdown, xml2::read_xml(to_insert))

  # Prev/next chapter -------------------------------------------------------
  cur <- which(is_active)
  if (length(cur) != 1) {
    return()
  }

  node_prev <- xml2::xml_find_first(html, ".//div[@id='book-chapter-prev']")
  if (cur > 1) {
    i <- cur - 1L
    link <- paste0("<a href='", nav$file_name[[i]], "'>", nav$text[[i]], "</a>")
    xml2::xml_add_child(node_prev, xml2::read_xml(link))
  }

  node_next <- xml2::xml_find_first(html, ".//div[@id='book-chapter-next']")
  if (cur < nrow(nav)) {
    i <- cur + 1L
    link <- paste0("<a href='", nav$file_name[[i]], "'>", nav$text[[i]], "</a>")
    xml2::xml_add_child(node_next, xml2::read_xml(link))
  }
}


# index -------------------------------------------------------------------

bs4_index_data <- function(node, chapter, path) {
  children <- xml2::xml_find_all(node,
    "./*[not(self::div and contains(@class, 'section'))]"
  )
  if (length(children) == 0 || !is_heading(children[[1]])) {
    return()
  }

  all <- function(...) paste0("descendant-or-self::", c(...), collapse = "|")
  text_path <- all("p", "li", "caption", "figcaption", "dt", "dd")
  code_path <- all("pre")

  code <- xml2::xml_find_all(children, code_path)
  text <- xml2::xml_find_all(children, text_path)

  list(
    path = path,
    id = xml2::xml_attr(node, "id"),
    chapter = chapter,
    heading = xml_text1(children[[1]]),
    text = strip_stop_words(xml_text1(text)),
    code = xml_text1(code)
  )
}

xml_text1 <- function(x) {
  paste0(xml2::xml_text(x), collapse = "")
}

is_heading <- function(node) {
  xml2::xml_name(node) %in% c("h1", "h2", "h3", "h4", "h5")
}

strip_stop_words <- function(x) {
  # paste(tidytext::get_stopwords()$word, collapse = "|")
  pattern <- "\\b(i|me|my|myself|we|our|ours|ourselves|you|your|yours|yourself|yourselves|he|him|his|himself|she|her|hers|herself|it|its|itself|they|them|their|theirs|themselves|what|which|who|whom|this|that|these|those|am|is|are|was|were|be|been|being|have|has|had|having|do|does|did|doing|would|should|could|ought|i'm|you're|he's|she's|it's|we're|they're|i've|you've|we've|they've|i'd|you'd|he'd|she'd|we'd|they'd|i'll|you'll|he'll|she'll|we'll|they'll|isn't|aren't|wasn't|weren't|hasn't|haven't|hadn't|doesn't|don't|didn't|won't|wouldn't|shan't|shouldn't|can't|cannot|couldn't|mustn't|let's|that's|who's|what's|here's|there's|when's|where's|why's|how's|a|an|the|and|but|if|or|because|as|until|while|of|at|by|for|with|about|against|between|into|through|during|before|after|above|below|to|from|up|down|in|out|on|off|over|under|again|further|then|once|here|there|when|where|why|how|all|any|both|each|few|more|most|other|some|such|no|nor|not|only|own|same|so|than|too|very|will)\\b ?"
  gsub(pattern, "", x, ignore.case = TRUE)
}

# helpers -----------------------------------------------------------------

check_packages <- function(pkgs) {
  inst <- vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
  if (all(inst)) {
    return()
  }

  stop(
    "Must install the following packages to use bs4_book():\n",
    paste0("* ", pkgs[!inst], "\n"),
    call. = FALSE
  )
}

preview_book <- function(path = ".", output = "bookdown::bs4_book") {
  old <- setwd(path)
  on.exit(setwd(old))

  render_book("index.Rmd",
    output_format = output,
    quiet = TRUE,
    clean = FALSE,
    envir = globalenv()
  )

  unlink(file.path(tempdir(), "_book"))
  file.copy("_book", tempdir(), recursive = TRUE)

  browseURL(file.path(tempdir(), "_book/index.html"))
}
