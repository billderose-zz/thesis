library(shiny)

shinyUI(
  navbarPage("Sampling",
    tabPanel("Simple Sampling",
      sidebarLayout(
        sidebarPanel(
          sliderInput("sample_size",
                      "Sample Size",
                      min = 10,
                      max = 1000,
                      value = 10),
          animate=animationOptions(interval=300, loop=T)),
        mainPanel(plotOutput("distPlot"))
      )
    )
  )
)
