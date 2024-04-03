#' HTML book built with bootstrap4
#'
#' @description
#' This output format is built with [Bootstrap](https://getbootstrap.com),
#' using carefully crafted features to provide a clean reading experience whether
#' you are on a phone, tablet, or desktop. To read more about this format, see:
#' \url{https://bookdown.org/yihui/bookdown/html.html#bs4-book}
#'
#' @param theme A named list or [bslib::bs_theme()] object.
#'   The default, `bs4_book_theme()`, resets the base font size to 1rem to
#'   make reading easier and uses a primary colour with greater contrast
#'   against the background.
#' @param repo Either link to repository where book is hosted, used to generate
#'   view source and edit buttons or a list with repository `base` link, default
#'   `branch`, `subdir` and `icon`
#'   (see "Specifying the repository" in \url{https://bookdown.org/yihui/bookdown/html.html#bootstrap4-style}).
#' @param lib_dir,pandoc_args,extra_dependencies,split_bib,... Passed on to [rmarkdown::html_document()].
#' @param template Pandoc template to use for rendering. Pass `"default"` to use
#'   the bookdown default template; pass a path to use a custom template. The
#'   default template should be sufficient for most use cases. For advanced user
#'   only, in case you want to develop a custom template, we highly recommend to
#'   start from the default template:
#'   <https://github.com/rstudio/bookdown/blob/master/inst/templates/bs4_book.html>.
#'   Otherwise, some feature may not work anymore.
#' @param footnotes_inline By default, footnotes will be set inline and shown on
#'   hover. Set to `FALSE` to keep footnotes at the bottom of the page with
#'   links.
#'
#' @export
#' @md
bs4_book <- function(theme = bs4_book_theme(),
                     repo = NULL,
                     ...,
                     lib_dir = "libs",
                     pandoc_args = NULL,
                     extra_dependencies = NULL,
                     template = 'default',
                     split_bib = FALSE,
                     footnotes_inline = TRUE) {
  check_packages(bs4_book_deps())
  bs4_check_dots(...)

  # Allow theme specification in yaml metadata
  if (!inherits(theme, "bs_theme")) {
    theme <- do.call(bs4_book_theme, theme)
  }

  # check for custom template
  if (identical(template, 'default')) {
    template <- bookdown_file('templates', 'bs4_book.html')
  }

  config <- rmarkdown::html_document(
    toc = FALSE,
    number_sections = TRUE,
    anchor_sections = FALSE,
    self_contained = FALSE,
    theme = NULL,
    template = template,
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

    output2 <- bs4_book_build(output, repo = repo, lib_dir = lib_dir,
                              split_bib = split_bib, footnotes_inline = footnotes_inline)

    if (clean && file.exists(output) && !same_path(output, output2)) {
      file.remove(output)
    }
    output2
  }

  config <- common_format_config(config, "html")
  config
}

#' @export
#' @rdname bs4_book
#' @param primary Primary colour: used for links and background of footer.
#' @param version Passed to [bslib::bs_theme()]. This should not be changed as
#'  [bs4_book()] has been designed to work with Bootstrap version 4 for now.
#' @md
bs4_book_theme <- function(primary = "#0068D9", version = 4, ...) {
  bslib::bs_theme(...,
    version = 4,
    primary = primary,
    "font-size-base" = "1rem",
  )
}

bs4_book_build <- function(output = "bookdown.html",
                           repo = NULL,
                           lib_dir = "libs",
                           output_dir = opts$get("output_dir"),
                           split_bib = FALSE,
                           footnotes_inline = TRUE) {
  move_files_html(output, lib_dir)

  rmd_index <- new.env(parent = emptyenv())
  output2 <- split_chapters(
    output = output,
    build = function(...) bs4_book_page(..., rmd_index = rmd_index),
    global_numbering = FALSE,
    split_by = "chapter",
    split_bib = split_bib
  )

  move_files_html(output2, lib_dir)

  if (is.null(output_dir)) {
    output_dir <- "_book"
  }

  if (isTRUE(opts$get("preview"))) {
    bs4_chapter_tweak(
      output2,
      repo = repo,
      rmd_index = setNames(opts$get("input_rmd"), output2),
      toc = build_toc(output2),
      footnotes_inline = footnotes_inline
    )
  } else {
    bs4_chapters_tweak(output,
      repo = repo,
      rmd_index = unlist(as.list(rmd_index)),
      output_dir = output_dir,
      footnotes_inline = footnotes_inline
    )
  }

  output2
}

build_toc <- function(output) {
  html <- xml2::read_html(output)

  main <- xml2::xml_find_first(html, ".//main")
  headings <- xml2::xml_find_all(main, ".//h1|.//h2|.//h3")

  number <- xml2::xml_find_first(headings, ".//span[@class='header-section-number']")

  toc <- data.frame(
    tag = xml2::xml_name(headings),
    id = xml2::xml_attr(xml2::xml_find_first(headings, "parent::div"), "id"),
    num = xml2::xml_text(number),
    text = htmltools::htmlEscape(xml2::xml_text(headings)),
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

  # Re-label appendixes
  if (any(is_appendix)) {
    app <- toc[
      seq_along(is_appendix) > which(is_appendix)[[1]] &
        toc$level == 1 &
        !is.na(toc$num),
    ]
    app$label <- LETTERS[seq_len(nrow(app))]
    # TODO: make less of a hack
    for (i in seq_len(nrow(app))) {
      toc$num <- sub(paste0("^", app$num[[i]], "\\b"), app$label[[i]], toc$num)
    }
  }

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

bs4_book_page <- function(head,
                          toc,
                          chapter,
                          link_prev,
                          link_next,
                          rmd_cur,
                          html_cur,
                          foot,
                          rmd_index = NULL) {
  rmd_index[[html_cur]] <- rmd_cur
  paste(c(head, toc, chapter, foot), collapse = "\n")
}

bs4_book_dependency <- function(theme) {
  assets <- bookdown_file("resources", "bs4_book")

  c(
    bslib::bs_theme_dependencies(theme),
    list(
      htmltools::htmlDependency(
        name = "bs4_book",
        version = "1.0.0",
        src = assets,
        stylesheet = c("bs4_book.css"),
        script = c("bs4_book.js")
      )
    )
  )
}


# HTML manip --------------------------------------------------------------

bs4_chapters_tweak <- function(output,
                               rmd_index = NULL,
                               repo = NULL,
                               output_dir = opts$get("output_dir"),
                               footnotes_inline = TRUE) {
  toc <- build_toc(output)
  files <- toc[!duplicated(toc$file_name) & !is.na(toc$file_name), ]
  files$path <- file.path(output_dir, files$file_name)

  index <- vector("list", nrow(files))
  for (i in seq_len(nrow(files))) {
    path <- files$path[[i]]
    message("Tweaking ", path)
    index[[i]] <- bs4_chapter_tweak(path, toc, rmd_index = rmd_index, repo = repo, footnotes_inline = footnotes_inline)
  }
  # tweak 404.html ---
  path_404 <- file.path(output_dir, "404.html")
  if (file.exists(path_404)) {
    message("Tweaking ", path_404)
    bs4_chapter_tweak(path_404, toc, rmd_index = rmd_index, repo = repo, footnotes_inline = footnotes_inline)

  }
  index <- unlist(index, recursive = FALSE, use.names = FALSE)

  jsonlite::write_json(
    index,
    file.path(output_dir, "search.json"),
    auto_unbox = TRUE
  )
}

bs4_chapter_tweak <- function(path, toc, rmd_index = NULL, repo = NULL, footnotes_inline = TRUE) {
  text <- xfun::file_string(path)

  # Convert ANSI escape to \u2029 since control characters are ignored in XML2
  text <- gsub("\033", "&#8233;", text, fixed = TRUE, useBytes = TRUE)
  Encoding(text) <- "UTF-8" # gsub with useBytes inhibits marked encoding
  html <- xml2::read_html(text, encoding = "UTF-8")

  tweak_tables(html)
  tweak_chapter(html)
  tweak_anchors(html)
  tweak_chunks(html)
  if (isTRUE(footnotes_inline)) tweak_footnotes(html)
  tweak_part_screwup(html)
  tweak_navbar(html, toc, basename(path), rmd_index = rmd_index, repo = repo)
  tweak_metadata(html, path)
  if (basename(path) == "404.html") tweak_404(html)
  downlit::downlit_html_node(html)

  xml2::write_html(html, path, format = FALSE)

  sections <- xml2::xml_find_all(html, ".//div[contains(@class, 'section')]")
  h1 <- xml_text1(xml2::xml_find_first(html, "//main//h1"))
  lapply(sections, bs4_index_data,
    chapter = h1,
    path = basename(path)
  )
}

tweak_404 <- function(html) {
  sidebar_toc <- xml2::xml_find_all(html, ".//div[contains(@class, 'sidebar')]/nav[@id='toc']")
  xml2::xml_remove(sidebar_toc)
}

tweak_chapter <- function(html) {
  num <- xml2::xml_find_all(html, ".//h1//span[@class='header-section-number']")
  xml2::xml_text(num) <- gsub("Chapter ", "", xml2::xml_text(num))
}

tweak_part_screwup <- function(html) {
  sidebar <- xml2::xml_find_first(html, "//div[contains(@class, 'sidebar-chapter')]")
  parent <- xml2::xml_parent(sidebar)

  if (xml2::xml_attr(parent, "class") == "row") {
    return()
  }

  # It's been put in the wrong place and we need to repair it
  main <- xml2::xml_find_first(html, "//main")
  xml2::xml_add_sibling(main, sidebar)
  xml2::xml_remove(sidebar)
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
    paste0(as.character(xml2::xml_children(x)), collapse = "")
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

tweak_chunks <- function(html) {
  # Want to surround inline images in special div, instead of a p, so that
  # we can set overflow-x. But don't want for floating images, since they
  # already have a container

  img <- xml2::xml_find_all(html, ".//img")
  parent <- xml2::xml_parent(img)
  n_children <- xml2::xml_find_num(parent, "count(*)")

  inline <- xml2::xml_name(parent) == "p" & n_children == 1
  xml2::xml_name(parent[inline]) <- "div"
  xml2::xml_attr(parent[inline], "class") <- "inline-figure"

  # Need to do the same for inline tables, but they don't have an existing
  # parent, so we need to insert one
  table <- xml2::xml_find_all(html, ".//table")
  toplevel <- xml2::xml_find_chr(table, "name(..)") == "div"

  for (table in table[toplevel]) {
    wrapper <- xml2::read_xml("<div class='inline-table'></div>")
    xml2::xml_add_child(wrapper, table)
    xml2::xml_replace(table, wrapper)
  }
}

tweak_anchors <- function(html) {
  main <- xml2::xml_find_first(html, ".//main")
  headings <- xml2::xml_find_all(main, "(.//h1|.//h2|.//h3|.//h4|.//h5|.//h6)")
  id <- xml2::xml_attr(xml2::xml_find_first(headings, "parent::div"), "id")

  headings <- headings[!is.na(id)]
  id <- id[!is.na(id)]

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
  if (!is.null(repo) && length(repo) == 1) {
    repo <- list(
      base = repo,
      branch = "master",
      subdir = NULL
    )
  }

  # Source links ------------------------------------------------------------
  if (!is.null(repo) && active %in% names(rmd_index)) {
    if (!is.null(repo$subdir)) {
      repo$subdir <- paste0(repo$subdir, "/")
    }

    repo_edit <- paste0(repo$base, "/edit/", repo$branch, "/", repo$subdir, rmd_index[[active]])
    repo_view <- paste0(repo$base, "/blob/", repo$branch, "/", repo$subdir, rmd_index[[active]])
  } else {
    repo_edit <- NULL
    repo_view <- NULL
  }

  if (!is.null(repo$base)) {
    icon <- repo$icon %n%
      ifelse(grepl("github\\.com", repo$base), "fab fa-github", "fab fa-gitlab")
    template_link_icon(html, ".//a[@id='book-repo']", icon)
    template_link_icon(html, ".//a[@id='book-source']", icon)
    template_link_icon(html, ".//a[@id='book-edit']", icon)
  }

  template_link(html, ".//a[@id='book-repo']", repo$base)
  template_link(html, ".//a[@id='book-source']", repo_view)
  template_link(html, ".//a[@id='book-edit']", repo_edit)

  # Within chapter nav --------------------------------------------------
  head <- toc[toc$file_name == active & toc$level > 0 & !is.na(toc$id), ]
  if (nrow(head) > 0) {
    link <- paste0(
      "<a class='nav-link' href='#", head$id, "'>",
      nav_num(head$num), head$text,
      "</a>"
    )
    n <- length(link)
    this_level <- head$level
    this_level[this_level == 1] <- 2 # Treat chapter like 2.0
    next_level <- c(this_level[-1], NA)
    prev_level <- c(NA, this_level[-n])

    prefix <- rep("<li>", length(link))
    prefix[prev_level > this_level] <- "</ul></li>\n<li>"
    suffix <- rep("</li>", length(link))
    suffix[next_level > this_level] <- "\n<ul class='nav navbar-nav'>"

    closing <- rep("</ul></li>", max(0, this_level[[n]] - 2))
    suffix[[n]] <- paste0("</li>", paste0(closing, collapse = ""))

    nav <- paste0(
      "<ul class='nav navbar-nav'>\n",
      paste(prefix, link, suffix, "\n", collapse = ""),
      "</ul>\n"
    )

    node <- xml2::xml_find_first(html, ".//div[@id='book-on-this-page']")
    xml2::xml_replace(node, xml2::read_xml(nav))
  }

  # TOC ---------------------------------------------------------------------
  nav <- toc[toc$level %in% 0:1, ]
  nav <- nav[!duplicated(nav$file_name) | is.na(nav$file_name), ]

  is_active <- nav$file_name == active
  class <- ifelse(is_active, "active", "")
  a <- paste0(
    "<li><a class='", class, "' href='", nav$file_name, "'>",
    nav_num(nav$num), nav$text,
    "</a></li>"
  )
  a[is.na(nav$file_name)] <- paste0(
    "<li class='book-part'>", nav$text[is.na(nav$file_name)], "</li>"
  )

  to_insert <- paste0(
    "<ul class='book-toc list-unstyled'>\n",
    paste0("  ", a, "\n", collapse = ""),
    "</ul>\n"
  )

  dropdown <- xml2::xml_find_first(html, ".//div[@id='book-toc']")
  xml2::xml_replace(dropdown, xml2::read_xml(to_insert))

  # Prev/next chapter -------------------------------------------------------
  # Need to ignore entries without files for cross-links
  nav2 <- nav[!is.na(nav$file_name), ]
  cur <- which(nav2$file_name == active)

  if (length(cur) > 0 && cur > 1) {
    i <- cur - 1L
    chapter_prev <- paste0(
      "<div class='prev'>",
      "<a href='", nav2$file_name[[i]], "'>",
      nav_num(nav2$num[[i]]), nav2$text[[i]],
      "</a>",
      "</div>"
    )
  } else {
    chapter_prev <- "<div class='empty'></div>"
  }

  if (length(cur) > 0 && cur < nrow(nav2)) {
    i <- cur + 1L
    chapter_next <- paste0(
      "<div class='next'>",
      "<a href='", nav2$file_name[[i]], "'>",
      nav_num(nav2$num[[i]]), nav2$text[[i]],
      "</a>",
      "</div>"
    )
  } else {
    chapter_next <- "<div class='empty'></div>"
  }

  chapter_nav <- paste0(
    "<div class='chapter-nav'>\n",
    chapter_prev, "\n",
    chapter_next, "\n",
    "</div>\n"
  )
  main <- xml2::xml_find_first(html, ".//main")
  xml2::xml_add_child(main, xml2::read_xml(chapter_nav))
}

nav_num <- function(x) {
  ifelse(is.na(x), "", paste0("<span class='header-section-number'>", x, "</span> "))
}

# Assume links are always inside a container that should be removed
# if there's no link
template_link <- function(html, xpath, href) {
  node <- xml2::xml_find_first(html, xpath)
  if (is.null(href)) {
    xml2::xml_remove(xml2::xml_parent(node))
  } else {
    xml2::xml_attr(node, "href") <- href
  }
}

tweak_metadata <- function(html, path) {
  file <- basename(path)

  # Fix generator
  generator <- xml_find_meta_name(html, 'generator')
  bookdown_string <- sprintf("bookdown %s with bs4_book()", packageVersion("bookdown"))
  set_content(generator, bookdown_string)

  # Check there are descriptions, add them if not
  general_description <- xml_find_meta_name(html, 'description')

  # Add the nodes if they were missing by default
  if (length(general_description) == 0) {
    head <- xml2::xml_find_first(html, '//head')
    default_description <- "A book created with bookdown."
    if (file == "index.html") message("TODO: Add a description field in the YAML metadata of index.Rmd.")
    xml2 <- xml2::xml_add_child(head, "meta", name= "description", content = default_description)
    xml2 <- xml2::xml_add_child(head, "meta", property = "og:description", content = default_description)
    xml2 <- xml2::xml_add_child(head, "meta", name = "twitter:description", content = default_description)
  }

  # index page is the only one not modified - yaml provided description by user is used.
  if (file == "index.html") return(invisible())

  # Fix og:url
  og_url <- xml_find_meta_property(html, 'og:url')
  base_url <- xml2::xml_attr(og_url, "content")
   if (!grepl("/$", base_url)) base_url <- paste0(base_url, "/")
  set_content(og_url, paste0(base_url, file))

  # Fix descriptions if possible
  general_description <- xml_find_meta_name(html, 'description')
  twitter_description <- xml_find_meta_name(html, 'twitter:description')
  og_description <- xml_find_meta_property(html, 'og:description')

  contents <- copy_html(xml2::xml_find_first(html, "//main[@id='content']"))
  xml2::xml_remove(xml2::xml_find_first(contents, "//h1"))
  xml2::xml_remove(xml2::xml_find_first(contents, "//div[@class='chapter-nav']"))
  text <- xml2::xml_text(contents)
  text <- gsub("\\\n", " ", text)
  text <- gsub("  ", " ", text)
  text <- gsub("^[[:space:]]+", "", text)
  text <- gsub("[[:space:]]+$", "", text)
  if (nzchar(text)) {
    words <- unlist(strsplit(text, " "))
    no_char <- cumsum(unlist(lapply(words, function(x) {nchar(x) + 1})))
    max_n <- max(which(no_char <= 197), 1)
    description_string <- paste(words[1: max_n], collapse = " ")
    if (max_n != length(words)) {
      description_string <- paste0(description_string, "...")
    }
    set_content(og_description, description_string)
    set_content(twitter_description, description_string)
    set_content(general_description, description_string)
  }

}

# https://github.com/ropensci/tinkr/blob/935ed21439230228f07f26161a507812d0fc76c3/R/to_md.R#L68
# TODO: replace by xml_clone() when it exists (https://github.com/r-lib/xml2/issues/341)
copy_html <- function(html) {
  xml2::read_html(as.character(html))
}

template_link_icon <- function(html, xpath, icon) {
  icon_node <- xml2::xml_child(xml2::xml_find_first(html, xpath))
  xml2::xml_attr(icon_node, "class") <- icon
}

# index -------------------------------------------------------------------

bs4_index_data <- function(node, chapter, path) {
  children <- xml2::xml_find_all(
    node,
    "./*[not(self::div and contains(@class, 'section'))]"
  )
  if (length(children) == 0 || !is_heading(children[[1]])) {
    return()
  }

  all <- function(...) paste0("descendant-or-self::", c(...), collapse = "|")
  text_path <- all("p", "li", "caption", "figcaption", "dt", "dd", "div[contains(@class, 'line-block')]")
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
  if (is_empty(index <- get_index_file())) {
    stop(sprintf('`index.Rmd` was not found in path = "%s".', path))
  }
  render_book(index,
    output_format = output,
    quiet = TRUE,
    clean = FALSE,
    envir = globalenv()
  )

  unlink(file.path(tempdir(), "_book"))
  file.copy("_book", tempdir(), recursive = TRUE)

  browseURL(file.path(tempdir(), "_book/index.html"))
}

bs4_check_dots <- function(...) {
  dot_names <- names(substitute(...()))

  fixed <- c(
    "anchor_sections",
    "number_sections",
    "self_contained",
    "toc",
    "toc_depth",
    "toc_float"
  )
  for (arg in fixed) {
    if (arg %in% dot_names) {
      stop(
        "`bs4_book()` does not support customisation of `", arg, "`",
        call. = FALSE
      )
    }
  }

  if ("highlight" %in% dot_names) {
    stop(
      "`bs4_book()` does not currently support the `highlight` argument.\n",
      "You'll need to use css directly to customise the colour scheme",
      call. = FALSE
    )
  }
}

set_content <- function(node, content) {
  xml2::xml_set_attr(node, "content", content)
}

xml_find_meta_property <- function(html, property){
    xml2::xml_find_first(html, sprintf('//meta[@property="%s"]', property))
}

xml_find_meta_name <- function(html, property){
    xml2::xml_find_first(html, sprintf('//meta[@name="%s"]', property))
}

# these dependencies are required to use bs4_book() but are suggested deps
# of bookdown. Hence the need to check they are available
# TODO: remove this and the check in bs4_book when we add them as Imports (if we do it)
bs4_book_deps <- function() {
  c("bslib", "downlit", "jsonlite", "xml2")
}
