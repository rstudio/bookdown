# CHANGES IN bookdown VERSION 0.7

## MINOR CHANGES

- Added a new dependency **tinytex** to build PDF, and **xfun** for some utility functions.

- Added the ability to share documents on LinkedIn with the `gitbook` output format (thanks, @WeeBeasties, #523, https://stackoverflow.com/q/48494097/559676).

# CHANGES IN bookdown VERSION 0.6

## NEW FEATURES

- Added an argument `config_file` to `render_book()` so that one can specify a custom config file; the default config file is still `_bookdown.yml` (thanks, @stephlocke, #465).

- Added a global option `bookdown.preview.cutoff` (defaults to 30) for the preview mode `render_book(preview = TRUE)`: when the number of lines of a chapter is smaller or equal to this number, the full chapter is included in the preview; otherwise, only the section titles are extracted from the chapter for preview.

## BUG FIXES

- Various compatibility issues with Pandoc 2.0 (thanks, @maxheld83 #479, @jerrythomas #481, @Hantabaru #483, @dataopt #504, and #478).

- `split_by = 'section'` does not work completely correctly for the HTML output formats like `gitbook` (thanks, @dataopt, #502).

# CHANGES IN bookdown VERSION 0.5

## NEW FEATURES

- Added support for two more environments: Exercises and Solutions (thanks, @dshuman1, #423).

- If the Rmd file merged from all chapters exists and you are sure it can be safely deleted, you can set an option `delete_merged_file` to `true` in `_bookdown.yml` (thanks, @dmenne, #442).

## BUG FIXES

- The book cannot be properly rendered when the option `book_filename` in `_bookdown.yml` contains a dot (thanks, @pinusm, #410).

- Proof and Remark blocks do not work well for EPUB output (thanks, @mamaciasq, #443).

- When `split_by = 'section'` for the `gitbook` output format, navigation buttons are missing on the page before the appendix (thanks, @dataopt, #409).

## MINOR CHANGES

- The label prefix for Example blocks was changed from `ex:` to `exm:`.

# CHANGES IN bookdown VERSION 0.4

## NEW FEATURES

- Added special syntax for unnumbered part headers: `# (PART\*)`. Numbered parts should be written after `# (PART)` as before (thanks, @brooksambrose, http://stackoverflow.com/q/43688902/559676).

- The `gitbook` output format also supports `abstract` in YAML now (thanks, @maxheld83, #311).

- For the `gitbook` output format, the `downloads` option in `config` supports `rmd` now (e.g. `download: ["pdf", "epub", "rmd"]`) if the edit link has been specified and is a link to Github (thanks, @coatless, #330).

- You can set the global R option `bookdown.post.latex` via `options()` to be a function to post-process the LaTeX output of the `pdf_book` format; see `?bookdown::pdf_book` for details (thanks, @nicksolomon, #373).

## BUG FIXES

- The HTML output file is not moved to the output directory when `split_by = 'none'` in `bookdown::gitbook` or `bookdown::html_book` (http://stackoverflow.com/q/40976073/559676).

- The YAML option `includes: before_body` works correctly for `gitbook` output now (thanks, @benmarwick, #267).

- Theorem environments are not defined for LaTeX output unless a `theorem` block is present (thanks, @JeffreyRacine, #291).

- For `remark` and `proof` blocks, the chunk option `name` did not work correctly for non-LaTeX output (thanks, @ugroempi, #347).

- Some text references do not work for HTML and Word output (thanks, @ugroempi, #363).

- The option `chapter_name` in `_bookdown.yml` does not work when it is specified as a function (thanks, @tzerk, 0c05c3828be).

- External assets such as fonts/css files should never be wiped when rendering a book to HTML (thanks, @nicholaelaw, #398).

## MINOR CHANGES

- The `daemon` argument was removed from `serve_book()`, but you can still pass it to `servr::httw()` via the `...` argument.

- Added a small space (padding) to the right of section numbers in `gitbook` output, so that the section numbers are better separated from the titles (thanks, @aronatkins, #367).

# CHANGES IN bookdown VERSION 0.3

## NEW FEATURES

- Added a Github button in the group of sharing buttons on the toolbar. By default, this button is not displayed. You have to set `github: yes` under `sharing` in the `gitbook` configurations (https://bookdown.org/yihui/bookdown/html.html) and specify your Github repo using the top-level option `github-repo` in the YAML metadata of `index.Rmd`, e.g. `github-repo: rstudio/bookdown`.

- The appendix heading will be preserved in `bookdown::html_document2` output, e.g. if you have `# (APPENDIX) Appendix {-}` in your document, you will see the heading `Appendix` in the output. Previously it was removed.

- Parts in HTML output are also be numbered using roman numerals like LaTeX/PDF output.

## BUG FIXES

- Wrong part titles were inserted to the table of contents of PDF output (thanks, @florisvdh, #243).

- Cross-references for appendices in `html_document2` output did not work (thanks, @florisvdh, #245).

- Part titles were not correctly processed when they were longer than 20 characters in PDF output (thanks, @florisvdh, #246).

# CHANGES IN bookdown VERSION 0.2

## NEW FEATURES

- Added arguemnts `toc_unnumberred`, `toc_appendix`, `toc_bib`, and `quote_footer` to `pdf_book()`.

- Added support for cross-referencing equations in multi-page HTML output and EPUB; see https://bookdown.org/yihui/bookdown/ for the syntax (thanks, @deleeuw, #85).

- Rmd files can live in subdirectories if you use the Merge-and-Knit approach (the default), and they will be found if the configuration option `rmd_subdir` is true in `_bookdown.yml` (thanks, @leobuchignani, #205).

## MAJOR CHANGES

- The `force_knit` argument of `render_book()` was removed (to avoid confusion when switching output formats).

## MINOR CHANGES

- The merged R Markdown file will not be deleted if rendering failed so you can debug with this file (http://stackoverflow.com/q/38883222/559676).

- The configurations `edit: text` and `chapter_name` have been moved from the top-level options to the sub-options of `language: ui` in `_bookdown.yml`. See https://bookdown.org/yihui/bookdown/internationalization.html

## BUG FIXES

- Figures are not correctly numbered in Word output using the `bookdown::word_document2()` format (thanks, @byzheng, #158).

- For the "Knit and Merge" approach (`new_session: yes` in `_bookdown.yml`), certain parts like figures may not show up when switching from one output format to another (e.g. from HTML to LaTeX).

- The `rmd_files` option in `_bookdown.yml` does not work when it is a list of `html` and `latex` options (thanks, @ismayc, #177).

- Math expressions does not appear in the table of contents when the output format is `gitbook` (thanks, @philomonk, #204).

- Footnotes of multiple paragraphs are not displayed on the current page (thanks, @axitdn, #234).

- The output format `pdf_document2()` also works with articles now when an R Markdown document contains bookdown-specific headers, such as parts or appendix headers (http://stackoverflow.com/q/40529798/559676).

# CHANGES IN bookdown VERSION 0.1

## NEW FEATURES

- Initial CRAN release.
