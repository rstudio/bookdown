# CHANGES IN bookdown VERSION 0.42
- New option in `gitbook` font settings menu to control line spacing (thanks, @hayden-MB, #1479).

# CHANGES IN bookdown VERSION 0.41

- New `mathjax-config` option for `bs4_book` and `gitbook` to control MathJax config string (thanks, @bwu62, #1472). The option can be set either in the YAML metadata or as a variable in `pandoc_args`. Currently tested and supported settings:
  - If empty, defaults to original `TeX-MML-AM_CHTML` which renders all equations in common HTML.
  - If set to `TeX-AMS-MML_HTMLorMML` renders equations in HTML + CSS (which may look nicer for some equations).
  - If set to `TeX-MML-AM_SVG` renders equations in SVG.

- Fixed the bug that `render_book()` fails due to `file.rename()` being unable to rename files across different disk volumes (thanks, @Giqles @katrinabrock, #804).

# CHANGES IN bookdown VERSION 0.40

- Footnotes are not rendered correctly when `katex` is used to render LaTeX math expressions (thanks, @pbreheny, #1470).

# CHANGES IN bookdown VERSION 0.39

- Fixed a bug that `bs4_book()` errors on generating document description. The error occured when the beggining of the document is a very long sentence without spaces (> 197 characters), which typically happens in CJK languages (thanks, @atusy, #1463).

# CHANGES IN bookdown VERSION 0.38

- Fixed a bug that `gitbook()` may generate an empty search index on certain platforms (thanks, @UlvHare, #1454).

# CHANGES IN bookdown VERSION 0.37

- Custom config files passed to the `config_file` argument of `render_book()` are no longer temporarily renamed to `_bookdown.yml` (thanks, @debruine, #1307).

- Do not move all `_files` directories temporarily to the `_bookdown_files` directory when calling `render_book()` (thanks, @steeleb, #1307).

# CHANGES IN bookdown VERSION 0.36

- Fix an issue with parsing resources from raw HTML code (thanks, @lennylin, https://community.rstudio.com/t/bookdown-image-with-a-weblink/172542)

- R 4.3.x would error if multiple files are passed to `render_book()` without an `output_format` specified (thanks, @slodge-work, #1442).

# CHANGES IN bookdown VERSION 0.35

- Search configuration in `bs4_book()` will now return more results as [Field-Length Norm](https://fusejs.io/concepts/scoring-theory.html#field-length-norm) is now ignored. This means the length of the search fields do not matter anymore in the scoring. Previous configuration was ignoring some search results being considered with a too low score (thanks, @jtbayly, #1431).

# CHANGES IN bookdown VERSION 0.34

- Fix an issue with CSL using hanging indent style in `gitbook()` (thanks, @pablobernabeu, #1422).

- Fix cross referencing of figures and tables in `epub_book()` format by correctly adding an anchor id at the caption level (thanks, @muschellij2, #766, @tstratopoulos, @jasonmosborne, #1399, @N0rbert, rstudio/bookdown-demo#42).

- Adapt an `epub_book()` internal command-line argument passed to Pandoc for changes from version 3.0 and above (#1425).

- Fix an issue with Pandoc 2.19 not rendering math by default in `epub3` format (#1417).

# CHANGES IN bookdown VERSION 0.33

- `extra_dependencies` in `gitbook()` is now correctly working (thanks, @ThierryO, #1408).

# CHANGES IN bookdown VERSION 0.32

- The defunct `kindlegen()` has been removed from this package.

- Theorem and Proof environments are now supported again in HTML slide format `slidy_presentation2()` (thanks, @urx449, #1398).

# CHANGES IN bookdown VERSION 0.31

- This package requires R >= 3.5.0 now.

# CHANGES IN bookdown VERSION 0.30

- Support specific markdown content like list or code chunk inside Theorem and Proof special environments (#1371).

- Fix regression about special usage of **bookdown** project not using `index.Rmd` as main file. It is recommended to use `index.Rmd` in all projects, but workflow has been improved for other cases (thanks, @otoomet, #1349).

# CHANGES IN bookdown VERSION 0.29

- The argument `code_folding` works for the `gitbook()` output format now (thanks, @atusy, #1368).

- Setting `toc_depth` or `toc_float` in `bs4_book()` will now throw an error like `toc` to make it clear that TOC is not an opt-out choice and can't be customize (thanks, @karlmay88, #1377).

# CHANGES IN bookdown VERSION 0.28

- Fix fontawesome 4.7 CSS that is included with `gitbook()` format styling. Now new icons (like `fa-usb`) are correctly available as expected (thanks, @snipfoo, #1353).

- Fix an issue with clipboard button in `gitbook()` (thanks, @chadyuu, #1358).

# CHANGES IN bookdown VERSION 0.27

- Fix `fence_theorems()` so that `output` is not ignored anymore. With previous version, when `output` was different than `NULL`, the result was written to `input`, ignoring `output` value. From now on, set `input` and `output` to the same file if you want to overwrite (thanks, @Scinawa, #1342). 

- Tweak `bs4_book()` default CSS for better support of python chunk highlighting (thanks, @briandk, #1333).

- Fix an issue with guessing output format when no `output_format` is provided in `render_book()` for files in a `rmd_subdir` folder (thanks, @shivam7898, #1331).

- Fix the issue of the invisible `gitbook` toolbar on iPad (thanks, @mpereira-dev, #60).

# CHANGES IN bookdown VERSION 0.26

- Fix issues with TOC in `gitbook()` and Pandoc 2.18 (thanks, @avraam-inside, #1326, #1329).

- Fix an issue with per-format `rmd_files` config when no `output_format` was provided in `render_book()` (thanks, @ellessenne, #1323).

- Added support for theorem/proof environments back for Word/EPUB/ODT output (thanks, @N0rbert, #1313).

- `kindlegen()` is defunct now.

# CHANGES IN bookdown VERSION 0.25

## NEW FEATURES

- Set option `bookdown.theorem.enabled = FALSE` to opt-out **bookdown** special Theorem and Proof environment syntax. `options(bookdown.theorem.enabled = FALSE)` must be called before the function to render the book, e.g in a project's `.Rprofile`. This can be useful for advanced users who only want PDF output and needs to handle themselves the environment using LaTeX directly, or [Custom Blocks](https://bookdown.org/yihui/rmarkdown-cookbook/custom-blocks.html) syntax without **bookdown** interfering (thanks, @finkelshtein, #1285).

- `bs4_book()` gains a `footnotes_inline` argument. Set to `FALSE` to opt-out the default behavior of moving footnotes inline to show on hover (thanks, @Pindar777, #1253).

## BUG FIXES

- Fix styling of bibliography for `bs4_book()` (thanks, @Selbosh, #1277).

- Fix an issue with Pandoc 2.17 and internationalization of Proof-like environment (#1302).

- Fix an issue with Pandoc 2.17 and cross referencing sections in non HTML format (thanks, @N0rbert, #1301).

- Fix an issue with Pandoc 2.15 and footnote relocation in each chapter (#1275).

- Fix an issue with `html_book()` and `toc.css` not working correctly with recent pandoc (thanks, @florisvdh, #1268).

- Fix an issue with unneeded `header-attr.js` inserted by **rmarkdown** while **bookdown** already cleans attributes (thanks, @salim-b, #865).

## MAJOR CHANGES

- The `theorem` and `proof` **knitr** engines no longer use the `block2` **knitr** engine to create theorem/proof environments, but write out fenced `Div`s instead, which is [the new syntax](https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#theorems) for these environments. Note that this means these environments are no longer supported in EPUB output (they work only in HTML and PDF), but hopefully the support will be back in the future (thanks, @deleeuw, #1178).

- The tag `<meta property="og:url">` has been removed from the default HTML template and the `gitbook` template (thanks, @jtbayly, #970).

# CHANGES IN bookdown VERSION 0.24

## MAJOR CHANGES

- The default search engine for `gitbook` has been changed from `lunr` to `fuse`. If you want to switch back to `lunr`, you may set:

  ```yaml
  output:
    bookdown::gitbook:
      config:
        search:
          engine: lunr
  ```

## MINOR CHANGES

- Reverted the fix for #1223 since it only affects a specific version of Pandoc (2.14.1). If this issue affects you, please see #1223 for workarounds.

- `bs4_book()` now has the `template` argument like `gitbook()` (thanks, @shinneuro, #1247).

## BUG FIXES

- `extra_dependencies` in `gitbook()` will now be appended after Gitbook's dependencies so that it does not get overridden (thanks, @ThierryO, @linogaliana, #1101, #1248).

- Fix an issue with Fenced Divs for Theorem & Proof environments in the Lua filter (thanks, @tchevri, #1233).

- `gitbook(self_contained = TRUE)` was slow when the output contains base64-encoded images (thanks, @king2bob, #1236).

- The search config `search: true` or `search: false` for `gitbook` throws a misleading error (thanks, @GegznaV, #1238).

# CHANGES IN bookdown VERSION 0.23

## NEW FEATURES

- This version has included a new RStudio template project to start an HTML book in `bookdown::gitbook` or `bookdown::bs4_book`. Template projects can be created using the RStudio IDE menu "New Project", or using one of the two new functions, `create_gitbook()` or `create_bs4_book()`, to easily create the template that you want to start with from within the R console (#225, #1123, #1201).

- Added an argument `global_numbering` to most output format functions in this package to control the figure/table numbering scheme (thanks, @elfunesto #948, @Kodiologist #1057). If `TRUE`, number figures and tables globally throughout a document (e.g., Figure 1, Figure 2, ...). If `FALSE`, number them sequentially within sections (e.g., Figure 1.1, Figure 1.2, ..., Figure 5.1, Figure 5.2, ...). Previously, this numbering scheme was hard-coded internally according to the `number_sections` argument (`global_numbering = !number_sections`). Now the two arguments have become independent, e.g., you can use `global_numbering = TRUE` with `number_sections = TRUE`.

- For HTML book formats, a default `404.html` page will now be created if none exists already. This page can be customized by adding a `_404.md` or `_404.Rmd` file which will be rendered to HTML and inserted in the book. Most web serving platforms (e.g. Netlify, GH Pages, etc.) will use this file named `404.html` in the root as a custom error page. Otherwise, like browsers do, a default 404 page is shown. For context, a 404 error indicates that the file can’t be found, and it happens when a browser can’t find a requested web page. This could happen with your online book if you shared a link to a section but change the name of this section leading to a change in url (#1035).

- The `bookdown::gitbook` output format now supports an alternative search engine, namely `fuse.js`, which has several advantages over `lunr.js`, the previous search engine for `gitbook`. Using `fuse.js` will fix a number of long-standing issues such as #734, #735, and #792. To enable `fuse.js`, set the search engine to be `fuse` in `gitbook`'s config in YAML, e.g.,

  ```yaml
  output:
    bookdown::gitbook:
      config:
        search:
          engine: fuse
  ```

  Depending on user feedback, we may set `fuse` to be the default search engine in a future version of **bookdown**. We will appreciate your testing and feedback!

- `bs4_book(splib_bib = TRUE)` can now be specified to have the same effect as in `gitbook()`. References will be shown at the end of each chapter and not only at the end of the book. This is useful with `bs4_book()` when a citation style not supporting footnotes is used because in that case, references are not shown inline in popups (thanks, @shirdekel, #1185).

- In `bs4_book()`, improvement regarding copy button:
  * It has now a light icon instead of a text with white background (#1192). 
  * It will no more show on output block code when knitr's option is `collapse = FALSE` (#1197).
  * It will now be placed correctly on the right side of the code block, with a light color which gets darker on hover so that it is less obtrusive when overlapping text in block with long lines  (#1204). If you want to customize part of the UI to change this default behavior, you can do it using a custom css with `bs4_book()`.

- In `bs4_book()`, copy button has now a light icon instead of a text with white background (#1192). 

- `bs4_book()` has now some `<meta>` tags that allows sharing a published book on social media. `cover-image`, `url`, `title` and `description` set in YAML will be used in `index.html` and then modified to be adapted per HTML page (#1034). 

- `repo` specification in `bs4_book()` can now be done in a more flexible way: base url, branch name, subdir and icon can be specify. See `?bookdown::bs4_book()` for details (thanks, @maelle, #1036).

- `epub_version` argument in `epub_book()` can now be set to `epub2` to create EPUB book of version 2. This follows an old change for default behavior in Pandoc 2.0 where the alias `epub` defaults to `epub3` and no more `epub2` (thanks, @jtbayly, #1150).

- [Theorem and Proof environment](https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#theorems) can now be used with `beamer_presentation2()` using fenced Div syntax like this
  ````markdown
  ::: {.theorem #label name="My Theorem"}
  Content
  :::
  ````
  
  However, as _beamer_ defines its own LaTeX theorem environments, **bookdown** won't add any definition in preamble as it is doing with `pdf_book()`. This means user will have to define the ones supported by **bookdown** and not yet defined by _beamer_. Special environment from _beamer_ (like `fact`) needs to be used with usual [Custom Blocks syntax](https://bookdown.org/yihui/rmarkdown-cookbook/custom-blocks.html). See related issues for examples in their discussions thread (thanks, @XiangyunHuang, #1143, #1145).

  This change comes with several small improvements in `custom-enviromnent.lua` for `latex` and `beamer` format, including a new option `bookdown.theorem.preamble` to opt-out **bookdown** addition of theorems and proofs definitions in LaTeX preamble. Set it to `FALSE` if you have conflict with some specific format for example (like #1001).

## MINOR CHANGES

- Updated the jQuery library to v3.x, which is now imported from the R package **jquerylib** (thanks, @mterente #693, @cooknl #882).

## BUG FIXES

- Removed the requirement for `.html` filenames to be alphanumeric, which fixes a common error "Automatically generated filenames contain duplicated ones: -" (thanks, @psychelzh #605, @AzureRabbit #902, @carloslederman #1000, Ritsu Kitagawa https://stackoverflow.com/q/60014350/559676, Shrek Tan).

- Fix an issue with `bookdown_site()` where the comment in `site:` line key was not supported (thanks, @LDSamson, #1194).

- Figure reference links now point correctly to the top of figures (thanks, @GuillaumeBiessy, #1155).

- When the `site` field is quoted in `index.Rmd`'s YAML data (i.e., `site: "bookdown::bookdown_site"`), **bookdown** fails to identify the root directory of the book (thanks, @dchiu911, #1160).

- The figure/table labels are no longer duplicated in Word output generated from Pandoc 2.14.1 (thanks, @dewittpe, #1223).

- When a book has multiple authors, the CSS styles for each author were inlined in the `<p>` tags, and hence are hard to override. Now the class `multi-author` is applied to each individual author's `<p>` tag, and the CSS rules are defined on this class instead (thanks, @robjhyndman, #1170).

- Style change in `bs4_book()` where code block inside callout blocks will have their background fill the whole width of the bordered block (#1175).

- In `bs4_book()`, math in footnotes is now rendered (@mine-cetinkaya-rundel, #1026)

- Fix an issue with `bs4_book()` where text written using [Line Block](https://bookdown.org/yihui/rmarkdown-cookbook/indent-text.html) was not found in search (thanks, @dmklotz, #1141).

# CHANGES IN bookdown VERSION 0.22

## NEW FEATURES

- New `bs4_book()` theme - see `?bs4_book` for details about this new format (thanks, @hadley, #996).

- `render_book()` can now take a directory as input, i.e `render_book("book_dir")`, to render in this directory by using the `index.Rmd` file if it exists. The default is now to look for `input.Rmd` is the current working directory. Previously, filename must have been provided (`render_book()` is now equivalent to `render_book("index.Rmd")`) (#990).

- `hypothesis` environment is now supported among [Theorems and Proof](https://bookdown.org/yihui/bookdown/markdown-extensions-by-bookdown.html#theorems) (thanks, @shirdekel, #1102).

- In `_bookdown.yaml`, `label` fields `fig`, `tab` and `eq` can now take a function as `ui` fields `chapter_name` and `appendix`. This function takes the reference number as only argument and must return a character to be used as full label. The default is a string prepended before the reference number. This new feature gives more flexibility to change the default for other language, e.g append the label name after the number. See updated doc about [Internationalization](https://bookdown.org/yihui/bookdown/internationalization.html)(thanks, Tamás Ferenc, #1114)

- Using the 'Knit' button now also works with a Rmd file in a sub-directory of the book project (when `rmd_subdir` is used in `_bookdown.yml`) (#1122)

- WhatsApp sharing feature has been added for `gitbook()` using `sharing` key  in `config`. This enables sharing the bookdown URL in WhatsApp on Mobile and on Desktop (thanks, @prdm0, #1125).

## BUG FIXES

- Adapt CSS in `gitbook()` and `html_book()` for correct displaying of `<details><summary>` content (thanks, @maelle, #971)

- When `split_bib = TRUE`, references in footnotes are now also correctly relocated in the chapter (thanks, @jimhshen, #952)

- In `pdf_book()`, `toc_bib = TRUE` now works with _natbib_ and _biblatex_ as `citation_package` (thanks, @qifei9, @umarcor, #450).

- CSS dependencies like `url('truetype/Spectral-ExtraLight.ttf')` (with single quotes) are now correctly identified and moved (thanks, @RLesur, #991).

- `fenced_theorems()` now correctly transforms implicit label in chunk header to a fenced divs id. (thanks, @enixam, #982)

- References are now correctly relocated with Pandoc 2.11 when `split_bib = TRUE` in `gitbook()`. New citation processing in Pandoc 2.11 will also add new classes to the divs, and these are preserved when relocating. This allows for references styling using CSS (#981).

- The new syntax for theorem and proof environments introduced in **bookdown** requires Pandoc >= 2.3 instead of 2.0 (thanks, @markhymers, #979 #980).

- `serve_book()` will refresh correctly now when using subdirectories with `rmd_subdir` (thanks, @shenfei, #834).

- Added the same CSS as in default Pandoc's template for when a CSL is used (#1045).   

- Properly support multiple authors in the `gitbook()` output format (thanks, @adamvi, #1095).

- No more warnings are thrown when passing several input files to `render_book(preview = TRUE)` (#1091).

- Correctly remove reference IDs of tables (e.g `(#tab:lab)`) generated by custom function (like `gt::gt()`) (#1099).

- In sepia or night mode, the background color during sidebar transition is now correct and no more white (#1100).

- Fix an issue in `bs4_book()` with encoding on system non-UTF8 by default (#1027).

## MINOR CHANGES

- `anchor_sections = TRUE` becomes the default for `bookdown::gitbook()`.

# CHANGES IN bookdown VERSION 0.21

## NEW FEATURES

- Add the `number_sections` argument to `markdown_document2()` and its family. This allows to have now figure references numbered by chapters in these formats, like `word_document2()` or `odt_document2()` for example. This argument default to `TRUE` like `html_document2()` and  `pdf_document2()`. Set it to `number_sections = FALSE` to get the same output as previous version without numbered chapters (thanks, @atusy, #756). 

- Provided an alternative way to create theorem and proof environments using Pandoc's fenced Divs. Previously, **bookdown** supports theorems and proofs in special code chunks like ```` ```{theorem}````. Now you can use the new syntax ```::: {.theorem}```. You may use the helper function `bookdown::fence_theorems()` to convert the former syntax to the latter. The main benefit of using fenced Divs is that you can write arbitrary content in a theorem environment (here "theorem" includes other environments such as lemma, corollary, and definition, etc.), such as R code chunks and inline R code, which was not possible previously. Note that this feature is only supported for LaTeX and HTML output formats at the moment. To learn more about the fenced Divs in general, you may read this section in _R Markdown Cookbook_: https://bookdown.org/yihui/rmarkdown-cookbook/custom-blocks.html (thanks, @tchevri #924, @cderv #940).

## BUG FIXES

- Correctly encode the document title when creating the Twitter sharing link from a bookdown chapter (thanks, @maelle, #934).

- Make sure `search_index.json` contains valid characters for the JSON format (thanks, @wlandau, #913).

## MAJOR CHANGES

- The `--file-scope` behavior introduced in bookdown v0.20 is now disabled by default. This is due to broken TOC links for duplicate section names (e.g., "Exercises"; see #909) that have automatically generated identifiers.

- The `clean_envir` argument of `bookdown::render_book()` has been deprecated and will be removed in the future (thanks, @jenslaufer, #932).

- The function `kindlegen()` has been deprecated, since Amazon no longer provides KindleGen. Please consider using `bookdown::calibre()` instead if you want `.mobi` output (#973).

## MINOR CHANGES

- Updated documentation for `render_book()` to make it clearer how options are set for the `output_format` parameter (thanks, @jonathan-g, #958 #930).

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

- Fixed the issues yihui/bookdown-chinese#29 and yihui/bookdown-chinese#30. Such issues can occur on Windows when there are multibyte characters in the section headers, and users will run into the error "Error in file.exists(f): file name conversion problem - name too long?" (thanks, @kongdd @JiaxiangBU @gaospecial and other users who reported the same issue such as https://twitter.com/matsuchiy/status/1186653559405727744 and https://d.cosx.org/d/420961).

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

- Added special syntax for unnumbered part headers: `# (PART\*)`. Numbered parts should be written after `# (PART)` as before (thanks, @brooksambrose, https://stackoverflow.com/q/43688902/559676).

- The `gitbook` output format also supports `abstract` in YAML now (thanks, @maxheld83, #311).

- For the `gitbook` output format, the `downloads` option in `config` supports `rmd` now (e.g. `download: ["pdf", "epub", "rmd"]`) if the edit link has been specified and is a link to Github (thanks, @coatless, #330).

- You can set the global R option `bookdown.post.latex` via `options()` to be a function to post-process the LaTeX output of the `pdf_book` format; see `?bookdown::pdf_book` for details (thanks, @nicksolomon, #373).

## BUG FIXES

- The HTML output file is not moved to the output directory when `split_by = 'none'` in `bookdown::gitbook` or `bookdown::html_book` (https://stackoverflow.com/q/40976073/559676).

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

- Added arguments `toc_unnumberred`, `toc_appendix`, `toc_bib`, and `quote_footer` to `pdf_book()`.

- Added support for cross-referencing equations in multi-page HTML output and EPUB; see https://bookdown.org/yihui/bookdown/ for the syntax (thanks, @deleeuw, #85).

- Rmd files can live in subdirectories if you use the Merge-and-Knit approach (the default), and they will be found if the configuration option `rmd_subdir` is true in `_bookdown.yml` (thanks, @leobuchignani, #205).

## MAJOR CHANGES

- The `force_knit` argument of `render_book()` was removed (to avoid confusion when switching output formats).

## MINOR CHANGES

- The merged R Markdown file will not be deleted if rendering failed so you can debug with this file (https://stackoverflow.com/q/38883222/559676).

- The configurations `edit: text` and `chapter_name` have been moved from the top-level options to the sub-options of `language: ui` in `_bookdown.yml`. See https://bookdown.org/yihui/bookdown/internationalization.html

## BUG FIXES

- Figures are not correctly numbered in Word output using the `bookdown::word_document2()` format (thanks, @byzheng, #158).

- For the "Knit and Merge" approach (`new_session: yes` in `_bookdown.yml`), certain parts like figures may not show up when switching from one output format to another (e.g. from HTML to LaTeX).

- The `rmd_files` option in `_bookdown.yml` does not work when it is a list of `html` and `latex` options (thanks, @ismayc, #177).

- Math expressions does not appear in the table of contents when the output format is `gitbook` (thanks, @philomonk, #204).

- Footnotes of multiple paragraphs are not displayed on the current page (thanks, @axitdn, #234).

- The output format `pdf_document2()` also works with articles now when an R Markdown document contains bookdown-specific headers, such as parts or appendix headers (https://stackoverflow.com/q/40529798/559676).

# CHANGES IN bookdown VERSION 0.1

## NEW FEATURES

- Initial CRAN release.
