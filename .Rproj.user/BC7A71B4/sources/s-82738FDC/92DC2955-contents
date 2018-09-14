library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

migrants<-read.csv("migrants.csv")

#Defining the UI for application 
#I will use a FluidPage instead of navbar
ui <- fluidPage(
  #I will Definde the Title of the app
  titlePanel("Deaths of Migrants around the World"),
  fluidRow(
    column(4,
           wellPanel(
             #Month Selection
             selectInput("month_select",
                         "month:",
                         choices = levels(migrants$Reported.Month),
                         multiple = TRUE,
                         selectize = TRUE,
                         selected = c("Jan")),
             # Year Selection
             sliderInput("yearSelect",
                         "Year:",
                         min = min(migrants$Reported.Year, na.rm = T),
                         max = max(migrants$Reported.Year, na.rm = T),
                         value = c(min(migrants$Reported.Year, na.rm = T), max(migrants$Reported.Year, na.rm = T)),
                         step = 1),
             
             actionButton("reset", "Reset Filters", icon = icon("refresh"))
           )
           )       
    ),
    column(12,
           plotlyOutput("plot1")
    ),
    column(12,
           plotlyOutput("plot2")
    ),
    column(12,
           plotlyOutput("plot3")
    ),
  fluidRow(
    DT::dataTableOutput("table")
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Starwars data
  swInput <- reactive({
    migrants <- migrants %>%
      # Slider Filter
      filter(Reported.Year >= input$yearSelect[1] & Reported.Year <= input$yearSelect[2])
    # Homeworld Filter
    if (length(input$month_Select) > 0 ) {
      migrants <- subset(migrants, Reported.Month %in% input$month_Select)
    }
    
    return(migrants)
  })
  # Point plot showing Mass, Height and Species
  output$plot1 <- renderPlotly({
    migrants <- swInput()
    ggplotly(
      ggplot(data = migrants, aes(x = Reported.Year, y = Number.Dead)) + 
        geom_point() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$plot2 <- renderPlotly({
    migrants<- swInput()
    ggplotly(
      ggplot(data = migrants, aes(x = Reported.Year, y = Number.Dead)) + 
        geom_line() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$plot3 <- renderPlotly({
    migrants <- swInput()
    ggplotly(
      ggplot(data = migrants, aes(x = Reported.Year, y = Number.Dead)) + 
        geom_histogram() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  # Data Table
  output$table <- DT::renderDataTable({
    migrants <- swInput()
    
    subset(migrants, select = c(Region, Reported.Date, Number.Dead, Total.Dead.and.Missing, Number.of.Females, Number.of.Males, Cause.of.Death))
  })
  # Updating the URL Bar
  observe({
    print(reactiveValuesToList(input))
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("migrants",Sys.Date(),".csv", sep="")
    },
    content = function(file) {
      write.csv(migrantsInputs(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "month_Select", selected = c("Jan"))
    updateSliderInput(session, "YearSelect", value = c(min(migrants$Reported.Year, na.rm = T), max(migrants$Reported.Year, na.rm = T)))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")