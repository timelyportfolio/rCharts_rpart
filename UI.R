library(rCharts)

shinyUI(
  tagList(
    tags$head(
      tags$link(
        href = "http://timelyportfolio.github.io/rCharts_rpart/css/treestyle.css"
        ,rel = "stylesheet"
      )
    ),
    fluidPage(
      
      mainPanel(
        sliderInput("slider1", label = h3("Min Split"),
                    min = 1, max = 10, value = 4),
        tableOutput("test"),
        tableOutput("rpk.text"),
        showOutput("plot", "dimple"))
    )
  )
)
