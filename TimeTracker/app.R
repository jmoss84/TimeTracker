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
    "Family"
    ,"Health"
    ,"Finance"
    ,"Professional"
    ,"Skills"
    ,"Hobby"
    ,"Waste"
    ,""
)

month_starts <- seq(1, 12, 1)
for (i in 1:12) {
    if (nchar(month_starts[i]) == 1) {
        month_starts[i] <- paste0("0", month_starts[i])
    }
}
month_starts <- as_date(paste0("2021-", month_starts, "-01"))

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
    theme = shinytheme("yeti"),
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
                    width = 10,
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
        "Books",
        fluidPage(
            
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    hr(),
                    tags$img(width = "100%", src = "clock.jpg"),
                    hr()
                ),
                
                mainPanel(
                    width = 10,
                    column(
                        width = 8,
                        h2("Reading Data", align = "center"),
                        hr(),
                        h3("Enter Books", align = "center"),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                dateInput(
                                    "bok_date",
                                    label = "Completion date:",
                                    value = today(),
                                    weekstart = 1
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "bok_name",
                                    label = "Book Name:"
                                )
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    "bok_score",
                                    label = "Book Score:",
                                    value = 5,
                                    min = 0,
                                    max = 10,
                                    step = 0.1
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                selectInput(
                                    "bok_type",
                                    label = "Book Type:",
                                    choices = c("Book", "Audiobook", ""),
                                    selected = "",
                                    multiple = F
                                )
                            ),
                            column(
                                width = 4,
                                selectInput(
                                    "bok_genre",
                                    label = "Book Genre:",
                                    choices = c("Personal Development", "Professional Development", "Non-Fiction", "Autobiography", "Fantasy", "Sci-Fi", "Classic", "Other Fiction", "Other Non-Fiction", ""),
                                    selected = "",
                                    multiple = F
                                )
                            ),
                            column(
                                width = 4
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                actionButton(
                                    "bok_go_book",
                                    label = "Submit Book",
                                    icon = icon("check")
                                )
                            ),
                            column(
                                width = 4
                            ),
                            column(
                                width = 4
                            )
                        )
                    ),
                    column(
                        width = 4,
                        column(
                            width = 12,
                            h2("Book Table", align = "center"),
                            hr(),
                            dataTableOutput("bok_tbl_books")
                        )
                    )
                )
            )
            
        )
    ),
    
    tabPanel(
        "Movie Night",
        fluidPage(
            
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    hr(),
                    tags$img(width = "100%", src = "clock.jpg"),
                    hr()
                ),
                
                mainPanel(
                    width = 10,
                    column(
                        width = 8,
                        h2("Movie Data", align = "center"),
                        hr(),
                        h3("Enter Movies", align = "center"),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                dateInput(
                                    "mov_date",
                                    label = "Activity date:",
                                    value = today(),
                                    weekstart = 1
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "mov_start",
                                    label = "Start Time:"
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "mov_end",
                                    label = "End Time:"
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 6,
                                textInput(
                                    "mov_name",
                                    label = "Movie Name:",
                                    value = ""
                                )
                            ),
                            column(
                                width = 6,
                                selectInput(
                                    "mov_genre",
                                    label = "Movie Genre:",
                                    choices = c("Action", "Thriller", "Comedy", "RomCom", "Classic", "Horror", "Animated", "Drama", "Other", ""),
                                    selected = "",
                                    multiple = F
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 6,
                                numericInput(
                                    "mov_ashscore",
                                    label = "Ash's Score",
                                    value = 5,
                                    min = 0,
                                    max = 10,
                                    step = 0.1
                                )
                            ),
                            column(
                                width = 6,
                                numericInput(
                                    "mov_jamscore",
                                    label = "Jamie's Score",
                                    value = 5,
                                    min = 0,
                                    max = 10,
                                    step = 0.1
                                )
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                actionButton(
                                    "mov_go_movie",
                                    label = "Submit Movie",
                                    icon = icon("check")
                                )
                            ),
                            column(
                                width = 4
                            ),
                            column(
                                width = 4
                            )
                        )
                    ),
                    column(
                        width = 4,
                        column(
                            width = 12,
                            h2("Movie Table", align = "center"),
                            hr(),
                            dataTableOutput("mov_tbl_movies")
                        )
                    )
                )
            )
            
        )
    ),
    
    tabPanel(
        "Cooking",
        fluidPage(
            
            sidebarLayout(
                sidebarPanel(
                    width = 2,
                    hr(),
                    tags$img(width = "100%", src = "clock.jpg"),
                    hr()
                ),
                
                mainPanel(
                    width = 10,
                    column(
                        width = 8,
                        h2("Cooking Data", align = "center"),
                        hr(),
                        h3("Enter Meals", align = "center"),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                dateInput(
                                    "cok_date",
                                    label = "Activity date:",
                                    value = today(),
                                    weekstart = 1
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "cok_start",
                                    label = "Start Time:"
                                )
                            ),
                            column(
                                width = 4,
                                textInput(
                                    "cok_end",
                                    label = "End Time:"
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                textInput(
                                    "cok_name",
                                    label = "Meal Name:",
                                    value = ""
                                )
                            ),
                            column(
                                width = 4,
                                selectInput(
                                    "cok_book",
                                    label = "Recipe Book:",
                                    choices = c("Jamie Recipe", "Gino's Pasta", "Nick Naird Scottish Cookery", "What Mummy Makes", "Kitchen", "Granny's Favourites", "Other", ""),
                                    selected = "",
                                    multiple = F
                                )
                            ),
                            column(
                                width = 4,
                                numericInput(
                                    "cok_score",
                                    label = "Score",
                                    value = 5,
                                    min = 0,
                                    max = 10,
                                    step = 0.1
                                )
                            )
                        ),
                        hr(),
                        fluidRow(
                            column(
                                width = 4,
                                actionButton(
                                    "cok_go_meals",
                                    label = "Submit Meal",
                                    icon = icon("check")
                                )
                            ),
                            column(
                                width = 4
                            ),
                            column(
                                width = 4
                            )
                        )
                    ),
                    column(
                        width = 4,
                        column(
                            width = 12,
                            h2("Meal Table", align = "center"),
                            hr(),
                            dataTableOutput("cok_tbl_meals")
                        )
                    )
                )
            )
            
        )
    ),
    
    navbarMenu(
        
        "Analysis",
        
        tabPanel(
            "Tasks",
            fluidPage(
                
                sidebarLayout(
                    sidebarPanel(
                        width = 2,
                        hr(),
                        tags$img(width = "100%", src = "clock.jpg"),
                        hr()
                    ),
                    
                    mainPanel(
                        width = 10,
                        h2("Analyses", align = "center"),
                        hr(),
                        column(
                            width = 4,
                            h3("Task Groups", align = "center"),
                            plotOutput("ana_grps", height = 300),
                            hr(),
                            h3("Parenting", align = "center"),
                            plotOutput("ana_parenting", height = 300),
                            hr(),
                            h3("Active Hours Per Week", align = "center"),
                            plotOutput("ana_activehoursperweek", height = 100),
                            hr()
                        ),
                        column(
                            width = 8,
                            fluidRow(
                                column(
                                    width = 6,
                                    h3("Task Sub Groups", align = "center"),
                                    plotOutput("ana_subs", height = 550),
                                    hr()
                                ),
                                column(
                                    width = 6,
                                    h3("Daily Outlook", align = "center"),
                                    plotOutput("ana_daily", height = 550),
                                    hr()
                                )
                            ),
                            fluidRow(
                                width = 8,
                                column(
                                    width = 12,
                                    plotOutput("tska_weeklyactive", height = 200),
                                    hr()
                                )
                            )
                        )
                    )
                )
            )
        ),
        
        tabPanel(
            "Books",
            fluidPage(
                
                sidebarLayout(
                    sidebarPanel(
                        width = 2,
                        hr(),
                        tags$img(width = "100%", src = "clock.jpg"),
                        hr()
                    ),
                    
                    mainPanel(
                        width = 10,
                        h2("Analyses", align = "center"),
                        hr(),
                        fluidRow(
                            column(
                                width = 12,
                                plotOutput("tska_bookscomp", height = 125),
                                hr()
                            )
                        ),
                        fluidRow(
                            column(
                                width = 4,
                                h3("Books Per Month", align = "center"),
                                plotOutput("tska_bookspermonth", height = 225),
                                hr(),
                                h3("Books By Genre", align = "center"),
                                plotOutput("tska_bookgenres", height = 400),
                                hr()
                            ),
                            column(
                                width = 8,
                                h3("All Scores", align = "center"),
                                plotOutput("tska_bookscores", height = 700),
                                hr()
                            )
                        )
                    )
                )
                
            )
            
        ),
        
        tabPanel(
            "Movies",
            fluidPage(
                
            )
            
        ),
        
        tabPanel(
            "Cooking",
            fluidPage(
                
                sidebarLayout(
                    sidebarPanel(
                        width = 2,
                        hr(),
                        tags$img(width = "100%", src = "clock.jpg"),
                        hr()
                    ),
                    
                    mainPanel(
                        width = 10,
                        h2("Analyses", align = "center"),
                        hr(),
                        column(
                            width = 4,
                            h3("Average Meal Score", align = "center"),
                            plotOutput("tska_avgscore", height = 150),
                            hr(),
                            h3("Meals Per Week", align = "center"),
                            plotOutput("tska_meals", height = 150),
                            hr(),
                            h3("Average Mealtime", align = "center"),
                            plotOutput("tska_avgprep", height = 150),
                            hr()
                        ),
                        column(
                            width = 8,
                            h3("Meal Quality", align = "center"),
                            plotOutput("tska_mealscore", height = 300),
                            hr(),
                            h3("Prep Time", align = "center"),
                            plotOutput("tska_mealtime", height = 300),
                            hr()
                        )
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
        
        dat_tsk = dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_tasks
        ) %>% 
                mutate(
                    StartTime = gsub("[[:punct:]]", "", StartTime)
                    ,EndTime = gsub("[[:punct:]]", "", EndTime)
                    ,StartTimestamp = paste0(TaskDate, " ", substr(StartTime, 1, 2), substr(StartTime, 3, 4), "00.000")
                    ,StartTimestamp = as_datetime(StartTimestamp)
                    ,EndTimestamp = paste0(TaskDate, " ", substr(EndTime, 1, 2), substr(EndTime, 3, 4), "00.000")
                    ,EndTimestamp = as_datetime(EndTimestamp)
                    ,EndTimestamp = ifelse(EndTimestamp < StartTimestamp, EndTimestamp + days(1), EndTimestamp)
                    ,EndTimestamp = as_datetime(EndTimestamp)
                    ,TimeSpent = difftime(EndTimestamp, StartTimestamp, units = "mins")
                ),
        
        dat_mov = dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_movies
        ),
        
        dat_cok = dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_meals
        ),
        
        dat_bok = dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_books
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
        
        updateDateInput(session, "bok_date", value = today())
        updateTextInput(session, "bok_name", value = "")
        updateNumericInput(session, "bok_score", value = 5)
        updateSelectInput(session, "bok_type", selected = "")
        updateSelectInput(session, "bok_genre", selected = "")
        
        updateDateInput(session, "mov_date", value = today())
        updateTextInput(session, "mov_start", value = "")
        updateTextInput(session, "mov_end", value = "")
        updateSelectInput(session, "mov_genre", selected = "")
        updateNumericInput(session, "mov_ashscore", value = 5)
        updateNumericInput(session, "mov_jamscore", value = 5)
        
        updateDateInput(session, "cok_date", value = today())
        updateTextInput(session, "cok_start", value = "")
        updateTextInput(session, "cok_end", value = "")
        updateTextInput(session, "cok_name", value = "")
        updateSelectInput(session, "cok_book", selected = "")
        updateNumericInput(session, "cok_score", value = 5)
        
        con_tsk <- dbConnect(
            odbc::odbc(),
            .connection_string = conf$con_str,
            timeout = 5
        )
        
        rv$dat_tsk <- dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_tasks
        ) %>% 
            mutate(
                StartTime = gsub("[[:punct:]]", "", StartTime)
                ,EndTime = gsub("[[:punct:]]", "", EndTime)
                ,StartTimestamp = paste0(TaskDate, " ", substr(StartTime, 1, 2), substr(StartTime, 3, 4), "00.000")
                ,StartTimestamp = as_datetime(StartTimestamp)
                ,EndTimestamp = paste0(TaskDate, " ", substr(EndTime, 1, 2), substr(EndTime, 3, 4), "00.000")
                ,EndTimestamp = as_datetime(EndTimestamp)
                ,EndTimestamp = ifelse(EndTimestamp < StartTimestamp, EndTimestamp + days(1), EndTimestamp)
                ,EndTimestamp = as_datetime(EndTimestamp)
                ,TimeSpent = difftime(EndTimestamp, StartTimestamp, units = "mins")
            )
        
        rv$dat_mov <- dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_movies
        )
        
        rv$dat_cok <- dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_meals
        )
        
        rv$dat_bok <- dbGetQuery(
            conn = con_tsk,
            statement = conf$queries$all_books
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
            
            subgroup <- c("Work", "Coursera", "Datacamp", "New Role Effort", "Personal Website", "Blog and Social Posts", "NFL GM Game Project", "Other Projects")
            
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
            
            subgroup2 <- c("Work", "Coursera", "Datacamp", "New Role Effort", "Personal Website", "Blog and Social Posts", "NFL GM Game Project", "Other Projects")
            
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
                name = SQL(conf$tables$tasks),
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
            name = SQL(conf$tables$tasks),
            value = data
        )
        
        dbDisconnect(con_tsk)
        refresh()
        
    })
    
    output$sub_tbl_tasks <- renderDataTable({
        
        DT::datatable(
            options = list(pageLength = 8, scrollX = T),
            rownames = F,
            
            rv$dat_tsk %>% 
                select(
                    -ID
                    ,-TaskLevel
                ) %>% 
                arrange(
                    desc(ImportTimestamp)
                )
            
        )
        
    })
    
    observeEvent(input$bok_go_book, {
        
        data <- data.frame(
            BookName = input$bok_name
            ,CompletionDate = input$bok_date
            ,BookScore = input$bok_score
            ,BookType = input$bok_type
            ,BookGenre = input$bok_genre
        )
        
        validate <- (
            !is.na(input$bok_name)
            & !is.na(input$bok_genre)
            & input$bok_name != ""
            & input$bok_genre != ""
        )
        
        if (validate == T) {
            
            con_tsk <- dbConnect(
                odbc::odbc(),
                .connection_string = conf$con_str,
                timeout = 5
            )
            
            dbAppendTable(
                conn = con_tsk,
                name = SQL(conf$tables$books),
                value = data
            )
            
            dbDisconnect(con_tsk)
            
            refresh()
            
        }
        
    })
    
    output$bok_tbl_books <- renderDataTable({
        
        DT::datatable(
            options = list(pageLength = 8, scrollX = T),
            rownames = F,
            
            rv$dat_bok %>% 
                select(
                    -ID
                ) %>% 
                arrange(
                    desc(ImportTimestamp)
                )
            
        )
        
    })
    
    observeEvent(input$mov_go_movie, {
        
        data <- data.frame(
            MovieName = input$mov_name
            ,WatchDate = input$mov_date
            ,StartTime = input$mov_start
            ,EndTime = input$mov_end
            ,Genre = input$mov_genre
            ,AshScore = input$mov_ashscore
            ,JamScore = input$mov_jamscore
        )
        
        validate <- (
            !is.na(input$mov_name)
            & !is.na(input$mov_genre)
            & input$mov_name != ""
            & input$mov_genre != ""
        )
        
        if (validate == T) {
            
            con_tsk <- dbConnect(
                odbc::odbc(),
                .connection_string = conf$con_str,
                timeout = 5
            )
            
            dbAppendTable(
                conn = con_tsk,
                name = SQL(conf$tables$movies),
                value = data
            )
            
            dbDisconnect(con_tsk)
            
            refresh()
            
        }
        
    })
    
    output$mov_tbl_movies <- renderDataTable({
        
        DT::datatable(
            options = list(pageLength = 8, scrollX = T),
            rownames = F,
            
            rv$dat_mov %>% 
                select(
                    -ID
                ) %>% 
                arrange(
                    desc(ImportTimestamp)
                )
            
        )
        
    })
    
    observeEvent(input$cok_go_meals, {
        
        data <- data.frame(
            MealName = input$cok_name
            ,MealDate = input$cok_date
            ,RecipeSource = input$cok_book
            ,StartTime = input$cok_start
            ,EndTime = input$cok_end
            ,Score = input$cok_score
        )
        
        validate <- (
            !is.na(input$cok_name)
            & !is.na(input$cok_book)
            & input$cok_name != ""
            & input$cok_book != ""
        )
        
        if (validate == T) {
            
            con_tsk <- dbConnect(
                odbc::odbc(),
                .connection_string = conf$con_str,
                timeout = 5
            )
            
            dbAppendTable(
                conn = con_tsk,
                name = SQL(conf$tables$meals),
                value = data
            )
            
            dbDisconnect(con_tsk)
            
            refresh()
            
        }
        
    })
    
    output$cok_tbl_meals <- renderDataTable({
        
        DT::datatable(
            options = list(pageLength = 8, scrollX = T),
            rownames = F,
            
            rv$dat_cok %>% 
                select(
                    -ID
                ) %>% 
                arrange(
                    desc(ImportTimestamp)
                )
            
        )
        
    })
    
    output$ana_grps <- renderPlot({
        
        grps <- rv$dat_tsk %>% 
            filter(
                TaskGroup != "Ash"
            ) %>% 
            group_by(
                TaskGroup
            ) %>% 
            summarise(
                Count = sum(TimeSpent, na.rm = T)
            )
        maxcnt <- max(grps$Count, na.rm = T)
        
        grps %>% 
            ggplot() +
            geom_bar(aes(y = reorder(TaskGroup, Count), weight = Count, fill = TaskGroup, group = TaskGroup), position = "dodge", color = "black") +
            theme_tsk() +
            labs(
                x = "Total Tasks"
                ,y = ""
            ) +
            scale_fill_manual(
                values = c(
                    "Family" = "#A8B333"
                    ,"Health" = "#CCC5B9"
                    ,"Finance" = "#403D39"
                    ,"Professional" = "#252422"
                    ,"Skills" = "#EB5E28"
                    ,"Hobby" = "#F7AEF8"
                    ,"Waste" = "#B388EB"
                )
            ) +
            coord_cartesian(
                xlim = c(0, maxcnt)
            )
        
    })
    
    output$ana_subs <- renderPlot({
        
        subs <- rv$dat_tsk %>% 
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
        maxcnt <- max(subs$Count)
        
        subs %>% 
            ggplot() +
            geom_bar(aes(y = reorder(TaskSubGroup, Count), weight = Count, fill = TaskGroup, group = TaskGroup), position = "dodge", color = "black") +
            theme_tsk() +
            labs(
                x = "Total Time (Minutes)"
                ,y = ""
            ) +
            scale_fill_manual(
                values = c(
                    "Family" = "#A8B333"
                    ,"Health" = "#CCC5B9"
                    ,"Finance" = "#403D39"
                    ,"Professional" = "#252422"
                    ,"Skills" = "#EB5E28"
                    ,"Hobby" = "#F7AEF8"
                    ,"Waste" = "#B388EB"
                )
            ) +
            coord_cartesian(
                xlim = c(0, maxcnt)
            )
        
    })
    
    output$ana_daily <- renderPlot({
        
        rv$dat_tsk %>% 
            filter(
                TaskGroup != "Ash"
                ,TaskDate >= today() - 31
            ) %>% 
            mutate(
                StartHour = as.integer(substr(StartTime, 1, 2))
                ,StartMinute = as.integer(substr(StartTime, 3, 4))
                ,EndHour = as.integer(substr(EndTime, 1, 2))
                ,EndMinute = as.integer(substr(EndTime, 3, 4))
                ,StartDayMinute = (StartHour * 60) + StartMinute
                ,EndDayMinute = (EndHour * 60) + EndMinute
                ,StartDayHour = StartDayMinute / 60
                ,StartDayHour = ifelse(StartDayHour < 3.5, StartDayHour + 24, StartDayHour)
                ,EndDayHour = EndDayMinute / 60
                ,EndDayHour = ifelse(EndDayHour <= 3.5, EndDayHour + 24, EndDayHour)
                ,TaskDate = as_date(TaskDate)
            ) %>% 
            ggplot() +
            geom_vline(aes(xintercept = 7.5), color = "tomato", size = 1) +
            geom_vline(aes(xintercept = 12.5), color = "tomato", size = 1) +
            geom_vline(aes(xintercept = 17), color = "tomato", size = 1) +
            geom_vline(aes(xintercept = 20), color = "tomato", size = 1) +
            geom_vline(aes(xintercept = 24), color = "tomato", size = 1) +
            geom_segment(aes(x = StartDayHour, xend = EndDayHour, y = TaskDate, yend = TaskDate, color = TaskGroup), size = 5) +
            theme_tsk() +
            theme(
                axis.text.x = element_text(margin = margin(b = 10))
                ,axis.text.y = element_text(margin = margin(r = -20, l = 10))
                ,legend.position = "top"
                ,legend.text = element_text(size = 13)
            ) +
            labs(
                x = "Hour"
                ,y = "Date"
                ,color = ""
            ) +
            scale_x_continuous(
                breaks = c(seq(0, 24, 2))
                ,labels = c(seq(0, 24, 2))
            ) +
            scale_color_manual(
                values = c(
                    "Family" = "#A8B333"
                    ,"Health" = "#CCC5B9"
                    ,"Finance" = "#403D39"
                    ,"Professional" = "#252422"
                    ,"Skills" = "#EB5E28"
                    ,"Hobby" = "#F7AEF8"
                    ,"Waste" = "#B388EB"
                )
            )
        
    })
    
    output$ana_parenting <- renderPlot({
        
        rv$dat_tsk %>% 
            filter(
                TaskSubGroup == "Logan"
                ,!Notes %in% c("Bathtime", "")
            ) %>% 
            mutate(
                Temp_All = str_extract(Notes, "[[:digit:]][[:digit:]][[:digit:]][apt]")
                ,Temp_A = str_extract(Notes, "[[:digit:]][[:digit:]]a")
                ,Temp_A = ifelse(!is.na(Temp_All), NA, Temp_A)
                ,Temp_T = str_extract(Notes, "[[:digit:]][[:digit:]]t")
                ,Temp_T = ifelse(!is.na(Temp_All), NA, Temp_T)
                ,Temp_P = str_extract(Notes, "[[:digit:]][[:digit:]]p")
                ,Temp_P = ifelse(!is.na(Temp_All), NA, Temp_P)
                ,Plan = gsub("[[:digit:]][[:digit:]][[:digit:]]", "", Temp_All)
                ,Plan = ifelse(is.na(Plan), "", Plan)
                ,Temp_Active = as.integer(gsub("[[:alpha:]]", "", Temp_A)) / 100
                ,Temp_Active = ifelse(is.na(Temp_Active), 0, Temp_Active)
                ,Temp_TV = as.integer(gsub("[[:alpha:]]", "", Temp_T)) / 100
                ,Temp_TV = ifelse(is.na(Temp_TV), 0, Temp_TV)
                ,Temp_Passive = as.integer(gsub("[[:alpha:]]", "", Temp_P)) / 100
                ,Temp_Passive = ifelse(is.na(Temp_Passive), 0, Temp_Passive)
                ,Active = ifelse(Plan == "a", 1, Temp_Active)
                ,TV = ifelse(Plan == "t", 1, Temp_TV)
                ,Passive = ifelse(Plan == "p", 1, Temp_Passive)
                ,Active = Active * TimeSpent
                ,TV = TV * TimeSpent
                ,Passive = Passive * TimeSpent
                ,CrossCheck = Active + TV + Passive
            ) %>% 
            select(
                -c(Temp_All, Temp_A, Temp_T, Temp_P, Plan, Temp_Active, Temp_TV, Temp_Passive, CrossCheck)
            ) %>% 
            group_by(
                "Totals"
            ) %>% 
            summarise(
                Active = sum(Active, na.rm = T)
                ,TV = sum(TV, na.rm = T)
                ,Passive = sum(Passive, na.rm = T)
            ) %>% 
            pivot_longer(
                cols = -`"Totals"`
                ,names_to = "Metric"
                ,values_to = "Value"
            ) %>% 
            ggplot() +
            geom_bar(aes(x = reorder(Metric, -Value), weight = as.integer(Value), fill = Metric), width = 0.8, color = "black") +
            theme_tsk() +
            theme(
                axis.text.x = element_text(margin = margin(b = 5, t = -15))
                ,axis.text.y = element_text(margin = margin(r = -20))
            ) +
            labs(
                x = "Group"
                ,y = "Total Minutes"
            ) +
            scale_fill_manual(
                values = c(
                    "Active" = "#24B7E9"
                    ,"TV" = "#D102A9"
                    ,"Passive" = "#B9D121"
                )
            )
        
    })
    
    output$ana_activehoursperweek <- renderPlot({
        
        weeks <- isoweek(today())
        
        rv$dat_tsk %>% 
            filter(
                TaskSubGroup == "Logan"
                ,!Notes %in% c("Bathtime", "")
            ) %>% 
            mutate(
                Temp_All = str_extract(Notes, "[[:digit:]][[:digit:]][[:digit:]][apt]")
                ,Temp_A = str_extract(Notes, "[[:digit:]][[:digit:]]a")
                ,Temp_A = ifelse(!is.na(Temp_All), NA, Temp_A)
                ,Temp_T = str_extract(Notes, "[[:digit:]][[:digit:]]t")
                ,Temp_T = ifelse(!is.na(Temp_All), NA, Temp_T)
                ,Temp_P = str_extract(Notes, "[[:digit:]][[:digit:]]p")
                ,Temp_P = ifelse(!is.na(Temp_All), NA, Temp_P)
                ,Plan = gsub("[[:digit:]][[:digit:]][[:digit:]]", "", Temp_All)
                ,Plan = ifelse(is.na(Plan), "", Plan)
                ,Temp_Active = as.integer(gsub("[[:alpha:]]", "", Temp_A)) / 100
                ,Temp_Active = ifelse(is.na(Temp_Active), 0, Temp_Active)
                ,Temp_TV = as.integer(gsub("[[:alpha:]]", "", Temp_T)) / 100
                ,Temp_TV = ifelse(is.na(Temp_TV), 0, Temp_TV)
                ,Temp_Passive = as.integer(gsub("[[:alpha:]]", "", Temp_P)) / 100
                ,Temp_Passive = ifelse(is.na(Temp_Passive), 0, Temp_Passive)
                ,Active = ifelse(Plan == "a", 1, Temp_Active)
                ,TV = ifelse(Plan == "t", 1, Temp_TV)
                ,Passive = ifelse(Plan == "p", 1, Temp_Passive)
                ,Active = Active * TimeSpent
                ,TV = TV * TimeSpent
                ,Passive = Passive * TimeSpent
                ,CrossCheck = Active + TV + Passive
            ) %>% 
            select(
                -c(Temp_All, Temp_A, Temp_T, Temp_P, Plan, Temp_Active, Temp_TV, Temp_Passive, CrossCheck)
            ) %>% 
            group_by(
                "Total"
            ) %>% 
            summarise(
                ActivePerWeek = sum(Active, na.rm = T)
            ) %>% 
            mutate(
                ActivePerWeek = as.integer(ActivePerWeek / 60) / weeks
                ,ActivePerWeek = round(ActivePerWeek, 1)
                ,OnTarget = ifelse(ActivePerWeek >= 5, T, F)
            ) %>% 
            ggplot() +
            geom_text(aes(x = 1, y = 1, label = round(ActivePerWeek, 1), color = OnTarget), size = 18) +
            theme_tsk() +
            theme(
                axis.title = element_blank()
                ,axis.text = element_blank()
            ) +
            labs(
                
            ) +
            scale_color_manual(
                values = c(
                    "TRUE" = "cyan"
                    ,"FALSE" = "tomato"
                )
            )
        
    })
    
    output$tska_weeklyactive <- renderPlot({
        
        rv$dat_tsk %>% 
            filter(
                TaskSubGroup == "Logan"
                ,!Notes %in% c("Bathtime", "")
            ) %>% 
            mutate(
                Temp_All = str_extract(Notes, "[[:digit:]][[:digit:]][[:digit:]][apt]")
                ,Temp_A = str_extract(Notes, "[[:digit:]][[:digit:]]a")
                ,Temp_A = ifelse(!is.na(Temp_All), NA, Temp_A)
                ,Temp_T = str_extract(Notes, "[[:digit:]][[:digit:]]t")
                ,Temp_T = ifelse(!is.na(Temp_All), NA, Temp_T)
                ,Temp_P = str_extract(Notes, "[[:digit:]][[:digit:]]p")
                ,Temp_P = ifelse(!is.na(Temp_All), NA, Temp_P)
                ,Plan = gsub("[[:digit:]][[:digit:]][[:digit:]]", "", Temp_All)
                ,Plan = ifelse(is.na(Plan), "", Plan)
                ,Temp_Active = as.integer(gsub("[[:alpha:]]", "", Temp_A)) / 100
                ,Temp_Active = ifelse(is.na(Temp_Active), 0, Temp_Active)
                ,Temp_TV = as.integer(gsub("[[:alpha:]]", "", Temp_T)) / 100
                ,Temp_TV = ifelse(is.na(Temp_TV), 0, Temp_TV)
                ,Temp_Passive = as.integer(gsub("[[:alpha:]]", "", Temp_P)) / 100
                ,Temp_Passive = ifelse(is.na(Temp_Passive), 0, Temp_Passive)
                ,Active = ifelse(Plan == "a", 1, Temp_Active)
                ,TV = ifelse(Plan == "t", 1, Temp_TV)
                ,Passive = ifelse(Plan == "p", 1, Temp_Passive)
                ,Active = Active * TimeSpent
                ,TV = TV * TimeSpent
                ,Passive = Passive * TimeSpent
                ,CrossCheck = Active + TV + Passive
                ,Week = isoweek(TaskDate)
            ) %>% 
            select(
                -c(Temp_All, Temp_A, Temp_T, Temp_P, Plan, Temp_Active, Temp_TV, Temp_Passive, CrossCheck)
            ) %>% 
            filter(
                Week <50
            ) %>% 
            group_by(
                Week
            ) %>% 
            summarise(
                ActivePerWeek = sum(Active, na.rm = T)
            ) %>% 
            mutate(
                ActivePerWeek = as.integer(ActivePerWeek / 60)
                ,ActivePerWeek = round(ActivePerWeek, 1)
                ,OnTarget = ifelse(ActivePerWeek >= 5, T, F)
            ) %>% 
            ggplot() +
            geom_line(aes(x = Week, y = ActivePerWeek), color = "grey90", size = 0.8) +
            geom_point(aes(x = Week, y = ActivePerWeek, color = OnTarget), size = 4) +
            theme_tsk() +
            labs(
                
            ) +
            scale_x_continuous(
                breaks = c(seq(1, 52, 1))
                ,labels = c(seq(1, 52, 1))
            ) +
            scale_color_manual(
                values = c(
                    "TRUE" = "seagreen"
                    ,"FALSE" = "navy"
                )
            )
        
    })
    
    output$tska_meals <- renderPlot({
        
        weeks <- isoweek(today())
        
        rv$dat_cok %>% 
            group_by(
                1
            ) %>% 
            summarise(
                Count = n()
            ) %>% 
            mutate(
                MealsPerWeek = Count / weeks
                ,MealsPerWeekTarget = 3
                ,OnTarget = MealsPerWeek >= MealsPerWeekTarget
            ) %>% 
            ggplot() +
            geom_text(aes(x = 1, y = 1, label = round(MealsPerWeek, 1), color = OnTarget), size = 24) +
            theme_tsk() +
            theme(
                axis.title = element_blank()
                ,axis.text = element_blank()
            ) +
            scale_color_manual(
                values = c(
                    "TRUE" = "dodgerblue"
                    ,"FALSE" = "grey50"
                )
            ) +
            coord_cartesian(ylim = c(0.5, 1.5))
        
    })
    
    output$tska_avgprep <- renderPlot({
        
        rv$dat_cok %>% 
            mutate(
                Start = as_datetime(paste0(MealDate, " ", substr(StartTime, 1, 2), ":", substr(StartTime, 3, 4), ":00.000"))
                ,End = as_datetime(paste0(MealDate, " ", substr(EndTime, 1, 2), ":", substr(EndTime, 3, 4), ":00.000"))
                ,Time = difftime(End, Start, units = "mins")
            ) %>% 
            group_by(
                1
            ) %>% 
            summarise(
                Time = mean(Time, na.rm = T)
            ) %>% 
            ggplot() +
            geom_text(aes(x = 1, y = 1, label = paste0(round(Time, 0), " mins")), color = "dodgerblue", size = 24) +
            theme_tsk() +
            theme(
                axis.title = element_blank()
                ,axis.text = element_blank()
            ) +
            coord_cartesian(ylim = c(0.5, 1.5))
        
    })
    
    output$tska_avgscore <- renderPlot({
        
        rv$dat_cok %>% 
            group_by(
                1
            ) %>% 
            summarise(
                Score = mean(Score, na.rm = T)
            ) %>% 
            ggplot() +
            geom_text(aes(x = 1, y = 1, label = round(Score, 1)), color = "dodgerblue", size = 24) +
            theme_tsk() +
            theme(
                axis.title = element_blank()
                ,axis.text = element_blank()
            ) +
            coord_cartesian(ylim = c(0.5, 1.5))
        
    })
    
    output$tska_mealscore <- renderPlot({
        
        rv$dat_cok %>% 
            ggplot(aes(x = as_date(MealDate), y = Score)) +
            geom_line(aes(group = 1), color = "cyan") +
            geom_point(color = "black", size = 5) +
            geom_point(color = "cyan", size = 4) +
            theme_tsk() +
            labs(
                x = "Meal Date"
                ,y = "Score"
            ) +
            scale_y_continuous(
                breaks = c(seq(0, 10, 1))
            )
        
    })
    
    output$tska_mealtime <- renderPlot({
        
        rv$dat_cok %>% 
            mutate(
                Start = as_datetime(paste0(MealDate, " ", substr(StartTime, 1, 2), ":", substr(StartTime, 3, 4), ":00.000"))
                ,End = as_datetime(paste0(MealDate, " ", substr(EndTime, 1, 2), ":", substr(EndTime, 3, 4), ":00.000"))
                ,Time = difftime(End, Start, units = "mins")
            ) %>% 
            ggplot(aes(x = as_date(MealDate), y = Time)) +
            geom_line(aes(group = 1), color = "navy") +
            geom_point(color = "black", size = 5) +
            geom_point(color = "navy", size = 4) +
            theme_tsk() +
            labs(
                x = "Meal Date"
                ,y = "Prep Time"
            )
        
    })
    
    output$tska_bookscomp <- renderPlot({
        
        rv$dat_bok %>% 
            mutate(
                Lane = ifelse(BookType == "Book", 1, 2)
                ,CompletionDate = date(CompletionDate)
            ) %>% 
            ggplot(aes(x = CompletionDate)) +
            geom_point(aes(y = Lane, size = BookScore, color = BookType), alpha = 0.5) +
            theme_tsk() +
            theme(
                panel.grid.major.x = element_line(size = 0.8, color = "grey85")
                ,axis.title.y = element_blank()
                ,axis.text.x = element_text(margin = margin(b = 5, t = 5))
                ,axis.text.y = element_blank()
            ) +
            labs(
                x = "Date Finished"
            ) +
            scale_x_date(
                limits = c(as_date("2021-01-01"), as_date("2022-01-01"))
                ,breaks = month_starts
            ) +
            scale_color_manual(
                values = c(
                    "Book" = "tomato"
                    ,"Audiobook" = "darkorange1"
                )
            ) +
            scale_size_continuous(
                range = c(3, 10)
            ) +
            coord_cartesian(
                ylim = c(0.5, 2.5)
            )
        
    })
    
    output$tska_bookspermonth <- renderPlot({
        
        months <- month(today())
        
        rv$dat_bok %>% 
            group_by(
                BookType
            ) %>% 
            summarise(
                Count = n()
            ) %>% 
            mutate(
                BooksPerMonth = Count / months
                ,BooksPerMonthTarget = 2
                ,OnTarget = BooksPerMonth >= BooksPerMonthTarget
            ) %>% 
            ggplot() +
            geom_text(aes(x = 1, y = 1, label = paste0(BookType, "s: ", round(BooksPerMonth, 1)), color = OnTarget), size = 16) +
            theme_tsk() +
            theme(
                axis.title = element_blank()
                ,axis.text = element_blank()
                ,strip.text = element_text(size = 15, face = "bold")
            ) +
            scale_color_manual(
                values = c(
                    "TRUE" = "darkorange1"
                    ,"FALSE" = "grey50"
                )
            ) +
            coord_cartesian(ylim = c(0.25, 1.75)) +
            facet_grid(BookType~.)
        
    })
    
    output$tska_bookscores <- renderPlot({
        
        means <- rv$dat_bok %>% 
            group_by(
                BookType
            ) %>% 
            summarise(
                Mean = mean(BookScore, na.rm = T)
            )
        
        rv$dat_bok %>% 
            ggplot() +
            geom_hline(data = means, aes(yintercept = Mean), linetype = "dotted", color = "black", size = 1) +
            geom_jitter(aes(x = BookType, y = BookScore, color = BookType), width = 0.2, size = 2.5) +
            theme_tsk() +
            theme(
                panel.grid.major.y = element_line(size = 0.8, linetype = "dotted", color = "grey85")
                ,axis.title.x = element_blank()
                ,axis.text.x = element_text(size = 14, face = "bold")
                ,strip.text = element_blank()
            ) +
            labs(
                y = "Score"
            ) +
            scale_y_continuous(
                breaks = c(seq(0, 10, 1))
            ) +
            scale_color_manual(
                values = c(
                    "Audiobook" = "darkorange1"
                    ,"Book" = "tomato"
                )
            ) +
            coord_cartesian(
                ylim = c(0, 10)
            ) +
            facet_grid(.~BookType, scales = "free_x")
        
    })
    
    output$tska_bookgenres <- renderPlot({
        
        rv$dat_bok %>% 
            group_by(
                BookGenre
            ) %>% 
            summarise(
                Count = n()
            ) %>% 
            ggplot() +
            geom_bar(aes(y = BookGenre, weight = Count, fill = Count), color = "white", width = 0.4) +
            theme_tsk() +
            theme(
                axis.title.y = element_blank()
                ,axis.text.y = element_text(size = 14, margin = margin(r = -20))
            ) +
            labs(
                x = "Books"
            ) +
            scale_x_continuous(
                breaks = c(seq(0, 100, 1))
            ) +
            scale_y_discrete(
                labels = function(x) {str_wrap(x, width = 10)}
            ) +
            scale_fill_gradient(
                low = "tomato", high = "darkorange"
            )
        
    })
    
}

shinyApp(ui = ui, server = server)