For general questions, please ask them on StackOverflow first, using the tags `r` and `bookdown`: http://stackoverflow.com/questions/ask Please come back here only if nobody answers your question there, and let us know the URL of your StackOverflow post. If you are not sure whether to post a StackOverflow question or open a Github issue, do the former first.

For bug reports, please try the development version first: `devtools::install_github('rstudio/bookdown')`. If it still does not work, please provide a minimal, self-contained, and reproducible example by reducing your example as much as possible right before the problem goes away. By doing this, you may be able to figure out what the problem really is before reporting to me. You can attach your example as a zip file here, and screenshots are often very helpful to illustrate your issues. You may consider creating your reproducible example based on https://github.com/yihui/bookdown-minimal. Please include the following info with your bug report:

```{r}
devtools::session_info('bookdown')
rmarkdown::pandoc_version()
system('pdflatex --version')
```

To include a verbatim chunk of arbitrary text, enclose it in four backticks, especially when the text contains backticks, e.g.

````
A sample document.

```{r}
1 + 1  # a line of code
```

Another paragraph.
````

If it is just a chunk of R code (or other languages) and you want syntax highlighting, you may use three backticks to format it, e.g.

```r
rnorm(10)
```

Usually your issue will be closed after it is fixed, but sometimes it is closed only because we are not able to offer any help. It does not mean your issue is not real or bad. You can propose a fix by yourself through a pull request. Your constructive feedback is always appreciated.
