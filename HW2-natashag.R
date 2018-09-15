library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

migrants<-read.csv("migrants2.csv")

#Defining the UI for application 
#I will use a FluidPage instead of navbar
ui <- fluidPage(
  #I will Definde the Title of the app
  titlePanel("Deaths of Migrants around the World"),
  fluidRow(
    column(4,
           wellPanel(
             #Month Selection
             selectInput("month_Select",
                         "month:",
                         choices = levels(migrants$Reported.Month),
                         multiple = TRUE,
                         selectize = TRUE,
                         selected = c("")),
             # Year Selection
             sliderInput("yearSelect",
                         "Year:",
                         min = min(migrants$Reported.Year, na.rm = T),
                         max = max(migrants$Reported.Year, na.rm = T),
                         value = c(min(migrants$Reported.Year, na.rm = T), max(migrants$Reported.Year, na.rm = T)),
                         step = 1),
             #Cause of Death Selection
             selectInput("cause_Select", h3("Select Cause"),
                         choices= levels(migrants$Cause.of.Death),
                         selected=0),
            #Reset Button
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
    DT::dataTableOutput("table"),
    downloadButton("downloadData", "Download Migrants Deaths Data")
  )
)

# Define server logic
server <- function(input, output, session = session) {
  # Filtered Starwars data
  migrantsInput <- reactive({
    migrants <- migrants %>%
     # Slider Filter
      filter(Reported.Year >= input$yearSelect[1] & Reported.Year <= input$yearSelect[2])
    # Month and Cause of Dead Filter
    if (length(input$month_Select) > 0 | length(input$cause_Select) > 0 ) {
      migrants <- subset(migrants, Reported.Month %in% input$month_Select)
      migrants<- subset(migrants, Cause.of.Death %in% input$cause_Select)
      return(migrants)
      }
    
    #Cause of Death Filter
    #if (length(input$cause_Select) > 0 ) {
    #   migrants<- subset(migrants, Cause.of.Death %in% input$cause_Select)
    #   return(migrants)
    #   }
    
  })
  # Point plot showing Mass, Height and Species
  output$plot1 <- renderPlotly({
    migrants <- migrantsInput()
    ggplotly(
      ggplot(data = migrants, aes(x = Reported.Year, y = Number.Dead, fill= Region)) + 
        geom_bar(stat="identity"))
  })
  output$plot2 <- renderPlotly({
    migrants<- migrantsInput()
    ggplotly(
      ggplot(data = migrants, aes(x = Reported.Year, y = Minimum.Estimated.Number.of.Missing,fill=Region)) + 
        geom_bar(stat="identity"))
  })
  output$plot3 <- renderPlotly({
    migrants <- migrantsInput()
    ggplotly(
      ggplot(data = migrants, aes(x = Reported.Year, y = Number.Dead)) + 
        geom_histogram())
  })
  # Data Table
  output$table <- DT::renderDataTable({
    migrants <- migrantsInput()
    subset(migrants, select = c(Region, Reported.Date, Number.Dead, Total.Dead.and.Missing, Number.of.Females, Number.of.Males, Cause.of.Death))
  })
  # Download data in the datatable
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("migrants",Sys.Date(),".csv", sep="")
    },
    content = function(file) {
      write.csv(migrantsInput(), file)
    }
  )
  # Reset Filter Data
  observeEvent(input$reset, {
    updateSelectInput(session, "month_Select", selected = c(""))
    updateSliderInput(session, "YearSelect", value = c(min(migrants$Reported.Year, na.rm = T), max(migrants$Reported.Year, na.rm = T)))
    updateSelectInput(session,"cause_Select", selected =c (""))
    showNotification("You have successfully reset the filters", type = "message")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)