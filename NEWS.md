# CHANGES IN bookdown VERSION 0.21

## BUG FIXES

- Make sure `search_index.json` contains valid characters for the JSON format (thanks, @wlandau, #913).

## MINOR CHANGES

- The `--file-scope` behavior introduced in bookdown v0.20 is now disabled by default. This is due to broken TOC links for duplicate section names (e.g., "Exercises"; see #909) that have automatically generated identifiers.

# CHANGES IN bookdown VERSION 0.20

## NEW FEATURES

- If `delete_merged_file` is set to `false` in `_bookdown.yml`, the merged (Rmd or md) file will not be deleted after the book is rendered (thanks, ilse pit, https://stackoverflow.com/q/61973608/559676).

- Numeric footnotes duplicated across chapters are now automatically renumbered. This is done by passing the `--file-scope` argument to pandoc (and having it operate on split out individual chapters of the target .md file rather than a combined file). This behavior can be toggled off by setting `options(bookdown.render.file_scope = FALSE)`.

## BUG FIXES

- Fixed a JS issue in `gitbook` when it is used with jQuery 3.x (thanks, @afkegel, #895).

## MINOR CHANGES

- Removed the `encoding` argument from `bookdown::render_book()`. This argument has always been ignored in **bookdown**.

# CHANGES IN bookdown VERSION 0.19

## BUG FIXES

- Multiline footnotes are now correctly rendered in HTML output (thanks, @jtbayly,  @cderv, #876).

- Text references do not work for `theorem` environments (thanks, @ssp3nc3r, rstudio/tufte#75).

- When both `rmd_subdir` and `rmd_files` are provided in the config file `_bookdown.yml`, only the files specified in `rmd_files` are now selected in addition to files under `rmd_subdir`. In the previous version, all files under the root directory are selected (thanks, @Gnossos #885, @cderv #886).

- When `rmd_subdir` is provided in `_bookdown.yml`, the subdirectories were always alphabetically ordered instead of following the order of elements in `rmd_subdir` (thanks, @Rothdyt, #736).

## MAJOR CHANGES

- Files with a leading `_` in their names are always ignored, even if they are specified in `rmd_files` in `_bookdown.yml`. Such files under subdirectories are also always ignored (#886).

# CHANGES IN bookdown VERSION 0.18

## NEW FEATURES

- Added an output format `context_document2`, based on the newly developed `rmarkdown::context_document` (see rstudio/rmarkdown#1713, rstudio/rmarkdown#1715, and rstudio/rmarkdown#1725; thanks @jooyoungseo, @atusy, and @RLesur).

## BUG FIXES

- `render_book()` works correctly with `output_dir = "."` now (thanks, @julianre, @cderv, #857).

- Cross-referencing works correctly now with `gitbook` when using `split_by: section` or `split_by: section+number` (thanks, @ThierryO, @cderv, #787).

- When using the Knit-and-Merge approach to compile a book (`new_session: true` in `_bookdown.yml`) and the fields `before_chapter_script` and/or `after_chapter_script` are configured in `_bookdown.yml`, the original Rmd files are no longer touched (thanks, @clauswilke #405 and @bob-carpenter https://stackoverflow.com/q/50554196/559676), and the scripts specified in `before/after_chapter_script` are no longer inserted into the Rmd files (they are read and evaluated separately), so the line numbers will be correct in case of **knitr** errors (thanks, @arencambre, #852).

# CHANGES IN bookdown VERSION 0.17

## NEW FEATURES

- Added an output format `github_document2`, which is a wrapper function based on `markdown_document2` using `rmarkdown::github_document` as the base format (thanks, @jooyoungseo, #831).

## BUG FIXES

- Fixed cross-reference issues with Pandoc 2.9+. Note that Pandoc 2.8 is not supported since it had a fairly short lifespan, but Pandoc below v2.8 or above v2.9 is still supported (thanks, @N0rbert @RLesur, #832; @jooyoungseo #845).

- For output formats like `pdf_book`, unused arguments passed to `base_format` will be discarded (thanks, @jooyoungseo, #790).

- For the sake of backward-compatibility, prevent the commands `\frontmatter`, `\mainmatter`, and `\backmatter` from being automatically added to the LaTeX output when the Pandoc version is higher than 2.7, because **bookdown** users may have added these commands by themselves (thanks, @remlapmot, rstudio/rmarkdown#1721).

- Fixed the issues yihui/bookdown-chinese#29 and yihui/bookdown-chinese#30. Such issues can occurr on Windows when there are multibyte characters in the section headers, and users will run into the error "Error in file.exists(f): file name conversion problem - name too long?" (thanks, @kongdd @JiaxiangBU @gaospecial and other users who reported the same issue such as https://twitter.com/matsuchiy/status/1186653559405727744 and https://d.cosx.org/d/420961).

## MINOR CHANGES

- The default value of `base_format` in the `markdown_document2` format is `rmarkdown::md_document` now. Previously the default value is missing.

# CHANGES IN bookdown VERSION 0.16

## NEW FEATURES

- You can also add a "view" button on the GitBook toolbar, similar to the "edit" and "history" buttons, which shows the page's `.Rmd` source file on GitHub. Unlike "edit", "view" does not require the reader to login to GitHub and fork the repo (thanks, @jtr13, #806).

- For `gitbook` output, the font setting button can be removed via `fontsettings: false` in the `config` option. Similarly, the info button can be removed by `info: false` in `config` (thanks, @mnazarov, #788).

- It is possible to customize the prefixes of appendix titles in `gitbook` output now (the default is still `A`, `B`, `C`, ..., and now you can change them to something like `Appendix A`, `Appendix B`, ...); see the documentation at https://bookdown.org/yihui/bookdown/internationalization.html (thanks, @WerthPADOH, #783).

- Added `html_fragment2`, `html_notebook2`, `html_vignette2`, `ioslides_presentation2`, `slidy_presentation2`, and `beamer_presentation2` for cross-referencing capabilities on top of **rmarkdown** output formats (thanks, @jooyoungseo, #789 #823).

## BUG FIXES

- For the `gitbook` output format, disabling the `sharing` menu or buttons works again (thanks, @lwjohnst86, #812).

- For the `gitbook` output format, toc collapsed by section now works with accents in chapter titles (thanks, @glimmerphoenix, @cderv, #819) 

- For output formats `word_document2`, `powerpoint_presentation2`, and `odt_document2`, `$$` is no longer added to equation environments when they are inside fenced code blocks (thanks, @N0rbert, #814).

# CHANGES IN bookdown VERSION 0.15

## BUG FIXES

- Sharing to Facebook and Twitter is possible again. Google+ sharing has been disabled (with a warning) as this service no longer exists (thanks, @cderv, #802).

- When using Pandoc 2.7.3 or later, footnotes are now placed again at the end of each chapter (#798, #801, thanks @cderv).

- gitbook toolbar is not missing any more when rendering books with Pandoc 2.x and using `self_contained = TRUE` (thanks, @Pindar777, @RLesur, @cderv, #739).

# CHANGES IN bookdown VERSION 0.14

## NEW FEATURES

- Added `rtf_document2` (thanks, @jooyoungseo, #768).

- Added copy to clipboard buttons to code blocks in the `gitbook` output format (thanks, @behrman #775, @RLesur #776).

## BUG FIXES

- Images specified in `toc: before:` of the `gitbook` format are not copied to the output directory (thanks, @dcossyleon, #763).

# CHANGES IN bookdown VERSION 0.13

## NEW FEATURES

- Added `odt_document2` and `powerpoint_presentation2` (thanks, @atusy, #742).

- Added `markdown_document2` which enables to use cross references in an arbitrary format specified in `base_format` argument (e.g., `markdown_document2(base_format = prettydoc::html_pretty)`) (thanks, @atusy, #742).

# CHANGES IN bookdown VERSION 0.12

## MINOR CHANGES

- Reverted #706 and removed the `clean_highlight_tags` argument in `html_document2()`; **bookdown** will no longer clean up the HTML tags of the syntax-highlighted code blocks.

## BUG FIXES

- The `gitbook` format failed to work with Pandoc 2.7.3 (thanks, @varemo @jwbowers @serine @RLesur, #733).

# CHANGES IN bookdown VERSION 0.11

## BUG FIXES

- The fix for https://stackoverflow.com/q/56061122/559676 in the previous version was incorrect, causing `rmd_files` to fail when it is a character vector of Rmd filenames (thanks, Joyce Robbins and Hadley Wickham, https://stackoverflow.com/q/56118663/559676).

# CHANGES IN bookdown VERSION 0.10

## NEW FEATURES

- Added an argument `clean_highlight_tags` to `html_document2()` (thanks, @atusy, #706).

- For HTML output formats such as `gitbook`, the abstract title (if the abstract is provided) can be customized via the field `abstract-title` in the YAML frontmatter (thanks, @XiangyunHuang, #715).

## BUG FIXES

- Split reference sections in `gitbook` ignored the sorting definition of the citation style (thanks @GegznaV #661, @crsh #674).

- For the `gitbook` output format, the content doesn't get the focus when the page is loaded, which makes it fail to respond to keystrokes such as PageUp/PageDown/Up/Down (thanks, @darshanbaral #691, @aronatkins #699).

- For the `gitbook` output format, when searching for keywords in code blocks, the automatic scroll to keywords doesn't work (thanks, @colearendt, #700).

- The search keyword no longer persists across page loads for different books in the `gitbook` format (thanks, @aronatkins, #695).

- The keybindings `Up` and `Down` (or `Enter`) in the search input of the `gitbook` output format work across all pages now; previously they only work on the current page (thanks, @dsblank, #657).

- When performing searching, the `gitbook` sidebar will only display relevant TOC items that actually take users to the HTML pages containing the search keyword. Previously, some TOC items do not really take users to the search destination but an anchor on a page instead, which can be confusing (thanks, @aronatkins, #696).

- Hyphenated words will be correctly highlighted in the search results now if spaces are used in the search keyword instead of dashes, e.g., you can search for `hand-off` using the keyword `hand off` (thanks, @aronatkins, #701).

- When `rmd_files` is configured in `_bookdown.yml`, `render_book()` will fail if the output format is not HTML or LaTeX (thanks, Ladislas Nalborczyk, https://stackoverflow.com/q/56061122/559676).

- The colon after figure/table numbers is missing in Word and EPUB output (thanks, @upton9265, #618).

- Multiple labels on the same line are allowed for Word output (thanks, @mdlincoln @h-k-kan @brooksambrose, #538).

## MINOR CHANGES

- Added alt/hover text to icons on the `gitbook` toolbar (thanks, @aronatkins, #698).

- Added an Information button to the `gitbook` toolbar to show the keybindings that are otherwise difficult for users to discover without reading the **bookdown** book (thanks, @aronatkins, #697).

- Added information about the keybindings Enter/Up/Down to the placeholder text and tooltip of the search input in `gitbook` output (thanks, @pyltime, #660).

# CHANGES IN bookdown VERSION 0.9

## BUG FIXES

- The tags for OpenGraph titles in HTML output were not properly closed.

# CHANGES IN bookdown VERSION 0.8

## NEW FEATURES

- Added Conjecture to the list of theorem environments.

- In addition to `rmd_subdir: true`, which searches all subdirectories, you can now provide a list of subdirectories to be recursively searched (#242).

- Added an argument `template` to `gitbook()` and `epub_book()` (thanks, @ThierryO, #570).

- Added an argument `table_css` to `gitbook()` to allow customized css for tables. (thanks, @haozhu233, #642)

- You can also add a "history" button on the Gitbook toolbar, which is similar to the "edit" button, but shows the GIT history of a source file instead (thanks, @maelle #638, @noamross, #639).

- Added a `quiet` argument to `serve_book()`, so that users can suppress stdout with `bookdown::serve_book(quiet = TRUE)` (thanks, @hammer, #633).

- For HTML output, the title of the current chapter or section will be added to the page title (in the `<title>` tag). This will give readers more information when reading the results from search engines or Twitter cards. Previously, all pages would have identical titles (thanks, @benwhalley and @batpigandme, #544).

## BUG FIXES

- HTML output formats such as `gitbook` and `html_document2` won't work when only unnumbered parts (i.e., `# (PART\*)`) are used (thanks, @tjmahr, #575).

- Previously the `rmd_files` parameter in `_bookdown.yml` would override `rmd_subdir`, but now both parameters can be used simultaneously (thanks, @ellisvalentiner, #600).

- Resources with URL encoded special characters are now correctly copied to the output directory (thanks, @AshesITR, #622).

- `serve_book()` can now be used without error when rstudioapi is installed but RStudio is not being used (thanks, @jimhester, #637).

- Text references via `(ref:label)` for `bookdown::pdf_document()` doesn't always work (thanks, @brendanf, #616).

# CHANGES IN bookdown VERSION 0.7

## MAJOR CHANGES

- The label prefix for Example blocks must be `exm:` (`ex:` is no longer supported).

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
