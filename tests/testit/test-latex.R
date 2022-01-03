library(testit)

assert("insert TeX syntax for bib in toc correctly", {
  (add_toc_bib("\\anything else unchanged") %==% "\\anything else unchanged")
  (add_toc_bib(c("dummyline", "\\bibliography{bib1.bib}")) %==%
    c("dummyline", "\\bibliography{bib1.bib}\n\\addcontentsline{toc}{section}{\\bibname}"))
  (add_toc_bib("\\bibliography{bib1.bib}") %==%
    "\\bibliography{bib1.bib}\n\\addcontentsline{toc}{section}{\\bibname}")
  (add_toc_bib("\\bibliography{bib1.bib,bib2.bib}") %==%
    "\\bibliography{bib1.bib,bib2.bib}\n\\addcontentsline{toc}{section}{\\bibname}")
  (add_toc_bib("\\printbibliography") %==%
    "\\printbibliography[heading=bibintoc]")
  (add_toc_bib("\\printbibliography[title=References]") %==%
    "\\printbibliography[title=References,heading=bibintoc]")
  (add_toc_bib(c("dummyline", "\\printbibliography[title=References]")) %==%
    c("dummyline", "\\printbibliography[title=References,heading=bibintoc]"))
})
