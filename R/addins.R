jquery_dependency = function() {
  htmltools::htmlDependency(
    'jquery', '2.2.0', bookdown_file('resources', 'jquery'),
    script = 'jquery.min.js'
  )
}

mathquill_dependency = function() {
  htmltools::htmlDependency(
    'mathquill', '0.10.0', bookdown_file('resources', 'mathquill'),
    script = c('mathquill.min.js', 'init.js'), stylesheet = 'mathquill.css'
  )
}

#' @import miniUI
#' @importFrom htmltools attachDependencies tags
mathquill = function() {
  context = rstudioapi::getActiveDocumentContext()
  shiny::runGadget(
    miniPage(
      gadgetTitleBar('Input LaTeX Math Expressions'),
      attachDependencies(
        tags$span(
          id = 'latex_input',
          context$selection[[1]]$text
        ),
        list(jquery_dependency(), mathquill_dependency())
      ),
      tags$p('LaTeX source:', style = 'margin: 1em auto 1em auto;'),
      tags$textarea(id = 'latex_source', style = 'height: 40px; min-height: 40px;')
    ),
    server = function(input, output) {
      shiny::observeEvent(input$done, {
        code = input$latex_source
        if (code != '') rstudioapi::insertText(code)
        shiny::stopApp()
      })
    },
    viewer = shiny::dialogViewer('Input LaTeX Math', height = 400)
  )
}
