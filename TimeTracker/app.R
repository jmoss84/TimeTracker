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
library(RColorBrewer)
library(AzureAuth)
library(AzureKeyVault)
library(DT)
library(shiny)
library(shinythemes)

task_groups <- c(
    "Ash"
    ,"Family"
    ,"Health"
    ,"Finance"
    ,"Professional"
    ,"Skills"
    ,"Hobby"
    ,"Waste"
    ,""
)

conf <- config::get()

theme_tsk <- function() {
    
    theme_minimal() +
    theme(
        panel.grid = element_blank()
        ,axis.title = element_text(size = 14, face = "bold")
        ,axis.text = element_text(size = 13)
        ,legend.position = "none"
    )
    
}

ui <- navbarPage(
    
    "Time Tracker",
    theme = shinytheme("simplex"),
    collapsible = T,
    
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
                        hr(),
                        h3("Main Task", align = "center"),
                        hr(),
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
                        ),
                        hr(),
                        h3("Secondary Task", align = "center"),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                dateInput(
                                    "sub_date2",
                                    label = "Activity date:",
                                    value = today(),
                                    weekstart = 1
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "sub_start2",
                                    label = "Start Time:"
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "sub_end2",
                                    label = "End Time:"
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                selectInput(
                                    "sub_group2",
                                    label = "Task Group:",
                                    choices = c(task_groups),
                                    selected = "",
                                    multiple = F
                                )
                            ),
                            column(
                                width = 4,
                                uiOutput("sub_subgroup2")
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "sub_notes2",
                                    label = "Additional Notes:"
                                )
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                actionButton(
                                    "sub_go_task",
                                    label = "Submit Task",
                                    icon = icon("check")
                                )
                            ),
                            column(
                                width = 4
                            ),
                            column(
                                width = 4
                            )
                        ),
                        hr()
                    ),
                    column(
                        width = 4,
                        column(
                            width = 12,
                            h2("Task Table", align = "center"),
                            hr(),
                            dataTableOutput("sub_tbl_tasks")
                        )
                    )
                )
            )
            
        )
    ),
    
    tabPanel(
        "Analysis"
        ,fluidPage(
            
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    hr(),
                    tags$img(width = "100%", src = "clock.jpg"),
                    hr(),
                    selectInput(
                        "ana_usr",
                        label = "User:",
                        choices = c("Jamie", "Ash"),
                        selected = "Jamie",
                        multiple = F
                    ),
                    hr()
                ),
                
                mainPanel(
                    width = 10,
                    h2("Analyses", align = "center"),
                    hr(),
                    column(
                        width = 4,
                        h3("Task Counts", align = "center"),
                        hr(),
                        plotOutput("ana_total", height = 750)
                    ),
                    column(
                        width = 4,
                        h3("Task Time", align = "center"),
                        hr(),
                        plotOutput("ana_time", height = 750)
                    )
                )
            )
            
        )
    )
    
)

server <- function(input, output, session) {
    
    con_tsk <- dbConnect(
        odbc::odbc(),
        .connection_string = conf$con_str,
        timeout = 5
    )
    
    rv <- reactiveValues(
        
        dat = dbGetQuery(
            conn = con_tsk,
            statement = "SELECT * FROM dbo.Tasks WHERE Notes <> 'Test';"
        ) %>% 
                mutate(
                    StartTime = gsub("[[:punct:]]", "", StartTime)
                    ,EndTime = gsub("[[:punct:]]", "", EndTime)
                    ,StartTimestamp = paste0(TaskDate, " ", substr(StartTime, 1, 2), substr(StartTime, 3, 4), "00.000")
                    ,StartTimestamp = as_datetime(StartTimestamp)
                    ,EndTimestamp = paste0(TaskDate, " ", substr(EndTime, 1, 2), substr(EndTime, 3, 4), "00.000")
                    ,EndTimestamp = as_datetime(EndTimestamp)
                    ,TimeSpent = difftime(EndTimestamp, StartTimestamp, units = "mins")
                )
        
    )
    
    dbDisconnect(con_tsk)
    
    refresh <- function() {
        
        updateDateInput(session, "sub_date", value = today())
        updateTextInput(session, "sub_start", value = "")
        updateTextInput(session, "sub_end", value = "")
        updateSelectInput(session, "sub_group", selected = "")
        updateSelectInput(session, "sub_subgroup", selected = "")
        updateTextInput(session, "sub_notes", value = "")
        
        updateDateInput(session, "sub_date2", value = today())
        updateTextInput(session, "sub_start2", value = "")
        updateTextInput(session, "sub_end2", value = "")
        updateSelectInput(session, "sub_group2", selected = "")
        updateSelectInput(session, "sub_subgroup2", selected = "")
        updateTextInput(session, "sub_notes2", value = "")
        
        con_tsk <- dbConnect(
            odbc::odbc(),
            .connection_string = conf$con_str,
            timeout = 5
        )
        
        rv$dat <- dbGetQuery(
            conn = con_tsk,
            statement = "SELECT * FROM dbo.Tasks WHERE Notes <> 'Test';"
        ) %>% 
            mutate(
                StartTime = gsub("[[:punct:]]", "", StartTime)
                ,EndTime = gsub("[[:punct:]]", "", EndTime)
                ,StartTimestamp = paste0(TaskDate, " ", substr(StartTime, 1, 2), substr(StartTime, 3, 4), "00.000")
                ,StartTimestamp = as_datetime(StartTimestamp)
                ,EndTimestamp = paste0(TaskDate, " ", substr(EndTime, 1, 2), substr(EndTime, 3, 4), "00.000")
                ,EndTimestamp = as_datetime(EndTimestamp)
                ,TimeSpent = difftime(EndTimestamp, StartTimestamp, units = "mins")
            )
        
        dbDisconnect(con_tsk)
        
    }

    output$sub_subgroup <- renderUI({
        
        if (input$sub_group == "Ash") {
            
            subgroup <- c("CPD - Reading", "CPD - Podcasts", "CPD - Coursework", "Fitness - Yoga", "Fitness - Walking", "Fitness - Workouts", "Self-Care - Reading", "Self-Care - TV", "Self-Care - Podcasts", "Skills - Piano", "Skills - French", "Skills - Baking", "Skills - Knitting")
            
        } else if (input$sub_group == "Family") {
            
            subgroup <- c("Logan", "Ash", "Family Time", "House", "Meals", "Walks", "Family Finance", "Improvements")
            
        } else if (input$sub_group == "Health") {
            
            subgroup <- c("Exercise", "Reading", "Audiobook", "Podcast", "Language", "Social")
            
        } else if (input$sub_group == "Professional") {
            
            subgroup <- c("Coursera", "Datacamp", "New Role Effort", "Personal Website", "Blog and Social Posts", "NFL GM Game Project", "Other Projects", "Work")
            
        } else if (input$sub_group == "Skills") {
            
            subgroup <- c("Room Decoration", "Garden Work", "Woodwork and Furniture")
            
        } else if (input$sub_group == "Hobby") {
            
            subgroup <- c("PC Construction", "Guitar Learning", "Piano Learning", "Guitar Construction", "Pedal Construction")
            
        } else if (input$sub_group == "Waste") {
            
            subgroup <- c("Transit", "Social", "Gaming")
            
        } else {
            
            subgroup <- c("")
            
        }
        
        selectInput(
            "sub_subgroup",
            label = "SubGroup:",
            choices = c(subgroup, "Other", ""),
            selected = "",
            multiple = F
        )
        
    })
    
    output$sub_subgroup2 <- renderUI({
        
        if (input$sub_group2 == "Ash") {
            
            subgroup2 <- c("CPD - Reading", "CPD - Podcasts", "CPD - Coursework", "Fitness - Yoga", "Fitness - Walking", "Fitness - Workouts", "Self-Care - Reading", "Self-Care - TV", "Self-Care - Podcasts", "Self-Care - Social", "Skills - Piano", "Skills - French", "Skills - Baking", "Skills - Knitting")
            
        } else if (input$sub_group2 == "Family") {
            
            subgroup2 <- c("Logan", "Ash", "Family Time", "House", "Meals", "Walks", "Family Finance", "Improvements")
            
        } else if (input$sub_group2 == "Health") {
            
            subgroup2 <- c("Exercise", "Reading", "Audiobook", "Podcast", "Language", "Social")
            
        } else if (input$sub_group2 == "Professional") {
            
            subgroup2 <- c("Coursera", "Datacamp", "New Role Effort", "Personal Website", "Blog and Social Posts", "NFL GM Game Project", "Other Projects", "Work")
            
        } else if (input$sub_group2 == "Skills") {
            
            subgroup2 <- c("Room Decoration", "Garden Work", "Woodwork and Furniture")
            
        } else if (input$sub_group2 == "Hobby") {
            
            subgroup2 <- c("PC Construction", "Guitar Learning", "Piano Learning", "Guitar Construction", "Pedal Construction")
            
        } else if (input$sub_group2 == "Waste") {
            
            subgroup2 <- c("Transit", "Social", "Gaming")
            
        } else {
            
            subgroup2 <- c("")
            
        }
        
        selectInput(
            "sub_subgroup2",
            label = "SubGroup:",
            choices = c(subgroup2, "Other", ""),
            selected = "",
            multiple = F
        )
        
    })
    
    observeEvent(input$sub_go_task, {
        
        data <- data.frame(
            TaskDate = input$sub_date
            ,StartTime = input$sub_start
            ,EndTime = input$sub_end
            ,TaskGroup = input$sub_group
            ,TaskSubGroup = input$sub_subgroup
            ,Notes = input$sub_notes
        )
        
        validate2 <- (
            !is.na(input$sub_start2)
            & !is.na(input$sub_group2)
            & input$sub_start2 != ""
            & input$sub_group2 != ""
        )
        
        if (validate2 == T) {
            
            data2 <- data.frame(
                TaskDate = input$sub_date2
                ,StartTime = input$sub_start2
                ,EndTime = input$sub_end2
                ,TaskGroup = input$sub_group2
                ,TaskSubGroup = input$sub_subgroup2
                ,Notes = input$sub_notes2
                ,TaskLevel = "Secondary"
            )
            
            con_tsk <- dbConnect(
                odbc::odbc(),
                .connection_string = conf$con_str,
                timeout = 5
            )
            
            dbAppendTable(
                conn = con_tsk,
                name = SQL(conf$table),
                value = data2
            )
            
            dbDisconnect(con_tsk)
            
        }
        
        con_tsk <- dbConnect(
            odbc::odbc(),
            .connection_string = conf$con_str,
            timeout = 5
        )
        
        dbAppendTable(
            conn = con_tsk,
            name = SQL(conf$table),
            value = data
        )
        
        dbDisconnect(con_tsk)
        refresh()
        
    })
    
    output$sub_tbl_tasks <- renderDataTable({
        
        DT::datatable(
            options = list(pageLength = 8, scrollX = T),
            rownames = F,
            
            rv$dat %>% 
                select(
                    -ID
                    ,-TaskLevel
                ) %>% 
                arrange(
                    desc(ImportTimestamp)
                )
            
        )
        
    })
    
    output$ana_total <- renderPlot({
        
        if (input$ana_usr == "Jamie") {
            
            maxcnt <- rv$dat %>% 
                filter(
                    TaskGroup != "Ash"
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = n()
                )
            maxcnt <- max(maxcnt$Count)
            
            rv$dat %>% 
                filter(
                    TaskGroup != "Ash"
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = n()
                ) %>% 
                mutate(
                    Tasks = paste(sep = "\n", TaskGroup, TaskSubGroup)
                ) %>% 
                ggplot() +
                geom_bar(aes(y = reorder(Tasks, Count), weight = Count, fill = TaskGroup, group = TaskGroup, alpha = Count), position = "dodge", color = "black") +
                theme_tsk() +
                labs(
                    x = "Total Tasks"
                    ,y = "Task"
                ) +
                scale_x_continuous(
                    breaks = c(seq(0, 1000, 1))
                    ,labels = c(seq(0, 1000, 1))
                ) +
                scale_fill_brewer(
                    palette = "YlGnBu"
                ) +
                coord_cartesian(
                    xlim = c(0, maxcnt)
                )
            
        } else if (input$ana_usr == "Ash") {
            
            maxcnt <- rv$dat %>% 
                filter(
                    TaskGroup == "Ash"
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = n()
                )
            maxcnt <- max(maxcnt$Count)
            
            rv$dat %>% 
                filter(
                    TaskGroup == "Ash"
                ) %>% 
                mutate(
                    Arc_Group = TaskSubGroup
                    ,TaskGroup = sapply(X = TaskSubGroup, FUN = function(x) {str_split(x, " - ")[[1]][1]})
                    ,TaskSubGroup = sapply(X = TaskSubGroup, FUN = function(x) {str_split(x, " - ")[[1]][2]})
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = n()
                ) %>% 
                mutate(
                    Tasks = paste(sep = "\n", TaskGroup, TaskSubGroup)
                ) %>% 
                ggplot() +
                geom_bar(aes(y = reorder(Tasks, Count), fill = TaskGroup, group = TaskGroup, alpha = Count), position = "dodge", color = "black") +
                theme_tsk() +
                labs(
                    x = "Total Tasks"
                    ,y = "Task"
                ) +
                scale_x_continuous(
                    breaks = c(seq(0, 1000, 1))
                    ,labels = c(seq(0, 1000, 1))
                ) +
                scale_fill_brewer(
                    palette = "Spectral"
                ) +
                coord_cartesian(
                    xlim = c(0, maxcnt)
                )
            
        }
        
    })
    
    output$ana_time <- renderPlot({
        
        if (input$ana_usr == "Jamie") {
            
            maxcnt <- rv$dat %>% 
                filter(
                    TaskGroup != "Ash"
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = sum(TimeSpent, na.rm = T)
                )
            maxcnt <- max(maxcnt$Count)
            
            rv$dat %>% 
                filter(
                    TaskGroup != "Ash"
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = sum(TimeSpent, na.rm = T)
                ) %>% 
                mutate(
                    Tasks = paste(sep = "\n", TaskGroup, TaskSubGroup)
                ) %>% 
                ggplot() +
                geom_bar(aes(y = reorder(Tasks, Count), weight = Count, fill = TaskGroup, group = TaskGroup, alpha = Count), position = "dodge", color = "black") +
                theme_tsk() +
                labs(
                    x = "Total Time (Minutes)"
                    ,y = "Task"
                ) +
                scale_x_continuous(
                    breaks = c(seq(0, 10000, 30))
                    ,labels = c(seq(0, 10000, 30))
                ) +
                scale_fill_brewer(
                    palette = "YlGnBu"
                ) +
                coord_cartesian(
                    xlim = c(0, maxcnt)
                )
            
        } else if (input$ana_usr == "Ash") {
            
            maxcnt <- rv$dat %>% 
                filter(
                    TaskGroup == "Ash"
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = sum(TimeSpent, na.rm = T)
                )
            maxcnt <- max(maxcnt$Count)
            
            rv$dat %>% 
                filter(
                    TaskGroup == "Ash"
                ) %>% 
                mutate(
                    Arc_Group = TaskSubGroup
                    ,TaskGroup = sapply(X = TaskSubGroup, FUN = function(x) {str_split(x, " - ")[[1]][1]})
                    ,TaskSubGroup = sapply(X = TaskSubGroup, FUN = function(x) {str_split(x, " - ")[[1]][2]})
                ) %>% 
                group_by(
                    TaskGroup
                    ,TaskSubGroup
                ) %>% 
                summarise(
                    Count = sum(TimeSpent, na.rm = T)
                ) %>% 
                mutate(
                    Tasks = paste(sep = "\n", TaskGroup, TaskSubGroup)
                ) %>% 
                ggplot() +
                geom_bar(aes(y = reorder(Tasks, Count), weight = Count, fill = TaskGroup, group = TaskGroup, alpha = Count), position = "dodge", color = "black") +
                theme_tsk() +
                labs(
                    x = "Total Time (Minutes)"
                    ,y = "Task"
                ) +
                scale_x_continuous(
                    breaks = c(seq(0, 10000, 30))
                    ,labels = c(seq(0, 10000, 30))
                ) +
                scale_fill_brewer(
                    palette = "Spectral"
                ) +
                coord_cartesian(
                    xlim = c(0, maxcnt)
                )
            
        }
        
    })
    
}

shinyApp(ui = ui, server = server)