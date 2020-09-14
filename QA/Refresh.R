library(shiny)
library(shinyjs)

jscode <- "shinyjs.refresh = function() { location.reload(); }"

shinyApp(
  ui = tagList(
    useShinyjs(),
    extendShinyjs(text = jscode, functions = "refresh"),
    navbarPage(
      "test",
      id = "navbar",
      tabPanel(title = "tab1", "tab 1"),
      tabPanel(title = "tab2", "tab 2"),
      tabPanel(title = "Refresh", value = "refresh")
    )
  ),
  server = function(input, output, session) {
    observe({
      if (input$navbar == "refresh") {
        js$refresh();
      }
    })
  }
)
