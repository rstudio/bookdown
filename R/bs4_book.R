#' Bootstrap4 output format
#'
#' @inheritParams html_chapters
#' @param fig_caption,number_sections,self_contained,lib_dir,pandoc_args ...
#'   Arguments to be passed to \code{rmarkdown::\link{html_document}()}
#'   (\code{...} not including \code{toc}, and \code{theme}).
#' @param config A list of configuration options for the gitbook style, such as
#'   the font/theme settings.
#' @export
#' @examples
#' withr::with_dir("inst/examples", render_book("index.Rmd", bs4_book(), quiet = TRUE, clean = FALSE))
#' bs4_book_build("inst/examples/bookdown.html")
bs4_book <- function(
                     theme = bs4_book_theme(),
                     ...,
                     lib_dir = "libs",
                     pandoc_args = NULL,
                     extra_dependencies = NULL
                     ) {
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

    output2 <- bs4_book_build(output, lib_dir = lib_dir)

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
                           lib_dir = "libs",
                           output_dir = opts$get("output_dir")
                           ) {
  move_files_html(output, lib_dir)
  output2 <- split_chapters(
    output = output,
    build = bs4_book_page,
    number_sections = TRUE,
    split_by = "chapter",
    split_bib = FALSE
  )
  move_files_html(output2, lib_dir)

  toc <- build_toc(output)
  chapters <- file.path(output_dir, setdiff(unique(toc$file_name), NA))

  for (chapter in chapters) {
    bs4_chapter_tweak(chapter, toc)
  }

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
    class = xml2::xml_attr(headings, "class"),
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
  is_part <- grepl("(PART)", toc$text)
  is_appendix <- grepl("(APPENDIX)", toc$text)

  toc$level[is_part | is_appendix] <- 0
  toc$text[is_part] <- gsub("\\(PART\\) ", "", toc$text[is_part])
  toc$text[is_appendix] <- gsub("\\(APPENDIX\\) ", "", toc$text[is_appendix])

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
                         foot) {
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

bs4_chapter_tweak <- function(path, toc) {
  html <- xml2::read_html(path, encoding = "UTF-8")

  tweak_tables(html)
  tweak_chapter(html)
  tweak_anchors(html)
  tweak_footnotes(html)
  tweak_navbar(html, toc, basename(path))
  downlit::downlit_html_node(html)

  xml2::write_html(html, path, format = FALSE)
  path
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

tweak_navbar <- function(html, toc, active = "") {
  nav <- subset(toc, level %in% 0:1)
  nav <- subset(nav, !class %in% "title")
  nav <- nav[!duplicated(nav$file_name), ]
  nav

  is_active <- nav$file_name == active
  class <- ifelse(is_active, "dropdown-item active", "dropdown-item")
  a <- paste0(
    "<a class='", class, "' href='", nav$file_name, "'>",
    nav$text,
    "</a>"
  )
  a[is.na(nav$file_name)] <- paste0("<div class='dropdown-divider'></div><h6 class='dropdown-header'>", nav$text[is.na(nav$file_name)], "</h6>")

  to_insert <- paste0(
    "<div class='dropdown-menu' aria-labelledby='navbar-toc'>\n",
    paste0("  ", a, "\n", collapse = ""),
    "</div>\n"
  )

  dropdown <- xml2::xml_find_first(html, ".//div[@id='toc-nav']")
  xml2::xml_replace(dropdown, xml2::read_xml(to_insert))
}
