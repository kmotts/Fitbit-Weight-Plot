library(shiny)


fluidPage(
  fluidRow(
    column(6, offset=3, align="center",

      

    
      plotOutput('plot')
      
      , br(),
      
      column(8, align="center",
             sliderInput(inputId="Dates", label="", min=as.Date("2016-01-01"), max=Sys.Date(), value=c(as.Date("2016-01-01"),Sys.Date()))
      ),
      
      column(4, align="left",
             checkboxInput("Legend", "Legend", FALSE),
             checkboxInput("Poly", "Polynomial", TRUE),
             checkboxInput("MovingAverage", "Moving Average", TRUE)
             
      )      
      
    )
  )
  
  
)


