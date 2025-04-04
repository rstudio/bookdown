validate_html <- function(
  files,
  # using same env variable old package to keep some compatibility
  base_url = Sys.getenv('W3C_MARKUP_VALIDATOR_BASEURL')
) {
  if(base_url == '' || length(files) == 0) return(NULL)
  lapply(files, function(file) {
    h <- curl::new_handle()
    curl::handle_setform(handle = h, out = 'json', file = curl::form_file(file, "text/html"))
    jsonlite::fromJSON(rawToChar(
      curl::curl_fetch_memory(base_url, h)$content
    ))
  })
}

simplify_html_validation <- function(results) {
  if(is.null(results) || length(results) == 0) return(
    data.frame(file=character(0), messages=character(0))
  )
  expected_errors <- c(
    "Attribute “number” not allowed on element “div” at this point.",
    "CSS: “border-top”: “solid\\9” is not a “color” value.",
    "CSS: “border-bottom”: “solid\\9” is not a “color” value."
    
  )
  do.call(rbind, lapply(results,
    function(result) {
      if(is.null(result)) return(
        data.frame(file=character(0), messages=character(0))
      )
      messages <- result$messages$message[result$messages$type == 'error']
      messages <- messages[!messages %in% expected_errors]
      data.frame(
        file = if (length(messages) > 0) result$url else character(0),
        messages = messages
      )
    }
  ))
}