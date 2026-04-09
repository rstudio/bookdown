mathquill_dependency = function() {
  htmltools::htmlDependency(
    'mathquill', '0.10.1', bookdown_file('resources', 'mathquill'),
    script = c('mathquill.min.js', 'init.js'), stylesheet = 'mathquill.css'
  )
}

local({
  context = rstudioapi::getActiveDocumentContext()
  tags = htmltools::tags
  shiny::runGadget(
    miniUI::miniPage(
      miniUI::gadgetTitleBar('Input LaTeX Math Expressions'),
      htmltools::attachDependencies(
        tags$span(
          id = 'latex_input',
          context$selection[[1]]$text
        ),
        list(bookdown:::jquery_dependency(), mathquill_dependency())
      ),
      tags$p('LaTeX source:', style = 'margin: 1em auto 1em auto;'),
      tags$textarea(
        id = 'latex_source', readonly = 'readonly',
        style = 'height: 40px; min-height: 40px;'
      )
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
})
