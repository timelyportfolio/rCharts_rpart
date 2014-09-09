library(rCharts)

shinyUI(fluidPage(
  mainPanel(
    sliderInput("slider1", label = h3("Min Split"),
                min = 1, max = 10, value = 4),
    tableOutput("test"),
    tableOutput("rpk.text"),
    showOutput("plot", "dimple"))
            
#  showOutput("plot", "dimple")
))
