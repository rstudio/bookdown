# the HTML validation server was started in GHA
validate_html = function(files, server = Sys.getenv('W3C_MARKUP_VALIDATOR_BASEURL')) {
  if (server == '') return()
  res = lapply(files, function(f) {
    h = curl::new_handle()
    curl::handle_setform(handle = h, out = 'json', file = curl::form_file(f, 'text/html'))
    jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(server, h)$content))
  })
  # Regex patterns for errors that are known/expected and should be ignored.
  # Using regex (rather than exact strings) makes matching robust to minor
  # changes in the validator's message wording or quotation style.
  ignore_patterns = c(
    # IE CSS hack: border: solid\9 (backslash-9 hack for IE8 compatibility)
    'CSS:.*border-(top|bottom).*solid.*9.*is not a.*color.*value',
    # bookdown uses a custom "number" attribute on <div> elements
    'Attribute.*number.*not allowed on element.*div.*at this point',
    # W3C validator flags heading level skips as errors; these are valid in
    # user-authored content (e.g. when testing non-sequential headings)
    'The heading .+ follows the heading .+, skipping .+ heading levels'
  )
  res = do.call(rbind, lapply(res, function(x) {
    m <- x$messages$message[x$messages$type == 'error']
    for (pat in ignore_patterns) m <- m[!grepl(pat, m)]
    if (length(m)) data.frame(file = x$url, messages = m)
  }))
  if (NROW(res) > 0) stop(
    'HTML issues detected:\n',
    paste0('  ', res$file, ': ', res$messages, collapse = '\n')
  )
}
