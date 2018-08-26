#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyTime)
library(shinyjs)

# mandatory fields: copied from here: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/#define-mandatory

fieldsMandatory <- c("place")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }"

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
   # Application title
   titlePanel("Report Helmet Usage"),
   
   # Sidebar with questions 
   sidebarLayout(
      sidebarPanel(
         textInput(inputId = "place", label = labelMandatory("Place"), ""),
         dateInput(inputId = "date", label = "Date"),
         timeInput(inputId = "time", label = "Time of the day", seconds = FALSE, value = Sys.time()),
         numericInput(inputId = "with", label = "bikers with helmet", value = 0),
         numericInput(inputId = "with", label = "bikers without helmet", value = 0),
         radioButtons(inputId = "risk", label = "how risky is this place for bikers?", inline = T, choices = c("0 - not sure", "1", "2", "3", "4", "5 - very risky"), selected = NULL),
         radioButtons(inputId = "law", label = "are there laws for wearing helmets in this place?", inline = TRUE, choices = c("Not sure", "Yes", "No"), selected = NULL),
         actionButton(inputId = "submit", label = "Submit", class = "btn-primary")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
   
   # only enable the Submit button when the mandatory fields are validated
   observe({
     mandatoryFilled <-
       vapply(fieldsMandatory,
              function(x) {
                !is.null(input[[x]]) && input[[x]] != ""
              },
              logical(1))
     mandatoryFilled <- all(mandatoryFilled)
     
     shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
   })    
}

# Run the application 
shinyApp(ui = ui, server = server)

