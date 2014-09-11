library(shiny)
library(ggplot2)
source('~/sampling.R')

shinyServer(function(input, output) {
  
  # Generate a histogram of values samples from an exponential(5) distribution.
  # Sample size is selected by the user and the histogram and smoothed version
  # of the density are plotted against the true density.
  lambda <- 5
  output$distPlot <- renderPlot({    
    xvals <- seq(0,  5 / lambda, length = 100)
    qplot(inverse_sample(inverse_exponential, lambda, input$sample_size), geom = 'blank') +   
      geom_line(aes(y = ..density.., colour = 'Empirical'), stat = 'density', trim = TRUE) +  
      geom_histogram(aes(y = ..density..), alpha = 0.4) +     
      geom_line(aes(x = xvals, y = dexp(xvals, rate = lambda), colour = "True")) +
      scale_colour_manual(name = 'Density', values = c('red', 'blue')) +
      xlab("x ") + ggtitle(paste("Exponential, n =", input$sample_size, "lambda =", lambda))
  })
})
