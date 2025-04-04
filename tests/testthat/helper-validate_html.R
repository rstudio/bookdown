# the HTML validation server was started in GHA
validate_html = function(files, server = Sys.getenv('W3C_MARKUP_VALIDATOR_BASEURL')) {
  if (server == '') return()
  res = lapply(files, function(f) {
    h = curl::new_handle()
    curl::handle_setform(handle = h, out = 'json', file = curl::form_file(f, 'text/html'))
    jsonlite::fromJSON(rawToChar(curl::curl_fetch_memory(server, h)$content))
  })
  expected_errors = c(
    'Attribute “number” not allowed on element “div” at this point.',
    'CSS: “border-top”: “solid\\9” is not a “color” value.',
    'CSS: “border-bottom”: “solid\\9” is not a “color” value.'
  )
  res = do.call(rbind, lapply(res, function(x) {
    m <- x$messages$message[x$messages$type == 'error']
    m <- setdiff(m, expected_errors)
    if (length(m)) data.frame(file = x$url, messages = m)
  }))
  if (NROW(res) > 0) stop(
    'HTML issues detected:\n',
    paste0('  ', res$file, ': ', res$messages, collapse = '\n')
  )
}
