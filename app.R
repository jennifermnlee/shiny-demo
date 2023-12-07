# load packages –---------------------------------------------------------------

library(tidyverse)
library(shiny)

# load data –-------------------------------------------------------------------

weather <- read_csv("data/weather.csv")

# create app –------------------------------------------------------------------

shinyApp(
  #### UI SIDE ####
  ui = fluidPage( # fluid page means layout adjusts to screen size.
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel( # add widgets
        # select city
        selectInput(
          inputId = "city",
          label = "Select City: ",
          choices = c("Chicago", "Durham", "Sedona", "New York", "Los Angeles")
        ),
        # select variable
        selectInput( # for whatever variable they chose, select a
          inputId = "variable",
          label = "Select a variable: ",
          choices = names(weather)
        )
      ),
      mainPanel( #
        tabsetPanel(
          tabPanel(title = "Plot", plotOutput("weatherplot")),
          tabPanel(title = "Data", dataTableOutput("weatherdata")) # display a data table
        )
      )
    )
  ),
  #### SERVER SIDE ####
  server = function(input, output, session) {

    weather_city <- reactive({ # updates app when user updates input
      weather |>
      filter(city %in% input$city)
    })

    # weather variables that are numeric and not constant for selected city
    weather_vars <- reactive({
      weather_city() |>
        select(where(is.numeric)) |>
        select(where(function(x) var(x) != 0)) |>
        names()
    })

    # update the select input when weather_vars changes (drill-down selection)
    observe({
      updateSelectInput(inputId = "var", choices = weather_vars())
    })



    output$weatherplot <- renderPlot({ # curly brackets needed because tells R to run multiple lines in one go
      weather |>
        filter(city == input$city) |> # allow user's input for the city to dictate plot shown.
        ggplot(aes_string(x = "time", y = input$variable)) + # variable types of inputs are normally character strings. must convert type
        geom_line(color = "purple") +
        labs(
          title = paste(input$city, "-", input$variable) # paste() function combines characters into single string
        )
    })

    ## CREATE DATA TABLE ##
    output$weatherdata <- renderDataTable({ # don't forget curly braces!
      weather_city() |>
        select(city, time, input$variable)
    })
  }
)



# must link client to server side.
## linking happens in the output object. store as a plot, and display it in the main panel.

# Shiny App: constantly checks the state of the app. Allows for the app to almost instantly update in response to user input.






