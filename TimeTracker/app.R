rm(list = ls())
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(config)
library(DBI)
library(httr)
library(rvest)
library(jsonlite)
library(AzureAuth)
library(AzureKeyVault)
library(shiny)
library(shinythemes)

task_groups <- c(
    "Family"
    ,"Health"
    ,"Finance"
    ,"Professional"
    ,"Skills"
    ,"Hobby"
    ,"Waste"
    ,""
)

ui <- navbarPage(
    
    "Time Tracker",
    tabPanel(
        
        "Time Submission",
        fluidPage(
            
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    hr(),
                    tags$img(width = "100%", src = "clock.jpg"),
                    hr()
                ),
                
                mainPanel(
                    column(
                        width = 8,
                        h2("Enter Tasks", align = "center"),
                        fluidRow(
                            column(
                                width = 4,
                                dateInput(
                                    "sub_date",
                                    label = "Activity date:",
                                    value = today(),
                                    weekstart = 1
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "sub_start",
                                    label = "Start Time:"
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "sub_end",
                                    label = "End Time:"
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                selectInput(
                                    "sub_group",
                                    label = "Task Group:",
                                    choices = c(task_groups),
                                    selected = "",
                                    multiple = F
                                )
                            ),
                            column(
                                width = 4,
                                uiOutput("sub_subgroup")
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "sub_notes",
                                    label = "Additional Notes:"
                                )
                            )
                        )
                    ),
                    column(
                        width = 4
                    )
                )
            )
        )
        
    )
    
)

server <- function(input, output, session) {

    output$sub_subgroup <- renderUI({
        
        if (input$sub_group == "Family") {
            
            subgroup <- c("Logan", "Ash", "House", "Meals", "Walks", "Family Finance", "Improvements", "Other")
            
        } else if (input$sub_group == "Health") {
            
            subgroup <- c("Exercise", "Reading", "Language", "Other")
            
        } else if (input$sub_group == "Professional") {
            
            subgroup <- c("Coursera", "Datacamp", "New Role Effort", "Personal Website", "Blog and Social Posts", "NFL GM Game Project", "Other Projects", "Work", "Other")
            
        } else if (input$sub_group == "Skills") {
            
            subgroup <- c("Room Decoration", "Garden Work", "Woodwork and Furniture", "Other")
            
        } else if (input$sub_group == "Hobby") {
            
            subgroup <- c("PC Construction", "Guitar Learning", "Piano Learning", "Guitar Construction", "Pedal Construction", "Other")
            
        } else if (input$sub_group == "Waste") {
            
            subgroup <- c("Transit", "Social", "Gaming", "Other")
            
        } else {
            
            subgroup <- c("")
            
        }
        
        selectInput(
            "sub_subgroup",
            label = "SubGroup:",
            choices = c(subgroup, ""),
            selected = "",
            multiple = F
        )
        
    })
    
}

shinyApp(ui = ui, server = server)