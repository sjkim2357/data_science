library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(readr)
library(rmarkdown)
library(recipes)

# Data import
wages <- read.csv("http://murraylax.org/datasets/wage2.csv")

# Data preprocess
wages <- replace(wages, wages=='.', NA)
wages = as.data.frame(sapply(wages, as.numeric))
wages

# Multi-variate choices
AttributeChoices=c("MonthlyEarnings","AveWeeklyHours","IQ","Knowledge","YearsEdu",
                   "Tenure","Age","Married", "Black", "South", "Urban",
                   "NumSiblings", "BirthOrder", "MomEdu", "DadEdu")

# Server
server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    dat <- wages[1:input$myslider1,]
    ggplot(dat, aes(MonthlyEarnings)) + geom_histogram(fill="gold4")
  })
  
  
  output$plot2 <- renderPlot({
    dat <- wages[1:input$myslider1,]
    ggplot(dat, aes(IQ)) + geom_histogram(fill="cadetblue")
  })
  
  output$plot3 <- renderPlot({
    dat <- wages[1:input$myslider1,]
    ggplot(dat, aes(YearsEdu)) + geom_histogram(fill="darkgray", binwidth = 1)
  })
  
  output$plot4 <- renderPlot({
    dat <- wages[1:input$myslider1,]
    ggplot(dat, aes(YearsExperience)) + geom_histogram(fill="forestgreen", binwidth = 1)
  })
  
  
  output$myPlot1 <- 
    renderPlot({
      dat <- 
        wages |>
        filter(YearsEdu >= input$range1[1], 
               YearsEdu <= input$range1[2]) 
      
      ggplot(dat, aes(YearsEdu, MonthlyEarnings)) + geom_point(position="jitter") + stat_smooth()
    })
  
  
  output$myPlot2 <- 
    renderPlot({
      dat <- 
        wages |>
        filter(IQ >= input$range2[1], 
               IQ <= input$range2[2]) 
      
      ggplot(dat, aes(IQ, MonthlyEarnings)) + geom_point(position="jitter") + stat_smooth()
    })    
  
  
  output$myPlot3 <- 
    renderPlot({
      dat <- 
        wages |>
        filter(YearsExperience >= input$range3[1], 
               YearsExperience <= input$range3[2]) 
      
      ggplot(dat, aes(YearsExperience, MonthlyEarnings)) + geom_point(position="jitter") + stat_smooth()
    }) 
  
  
  
  output$indep <- renderUI({
    selectInput(inputId = "indep", label = "Independent Variables", 
                multiple = TRUE, choices = as.list(AttributeChoices[AttributeChoices!= input$dependent]), selected = AttributeChoices[1])
  })
  
  recipe_formula <- reactive({
    req(input$indep)
    wages %>%
      recipe() %>%
      update_role(!!!input$dependent, new_role = "outcome") %>%
      update_role(!!!input$indep, new_role = "predictor") %>%
      prep() %>% 
      formula()
  })
  
  lm_reg <- reactive(
    lm(recipe_formula(),data = wages)
  )
  
  output$RegOut = renderPrint({
    summary(lm_reg())
  })
  
  output$tableDT <- DT::renderDataTable(wages,
                                        options = list(paging=F),
                                        rownames=F,
                                        filter = "top")
  output$table <- DT::renderDataTable(DT::datatable(cust()))
  
}


# UI
ui <- dashboardPage(
  dashboardHeader(title = "Analysis of Income"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "histogram", icon = icon("calendar")),
      menuItem("Range control", tabName = "control", icon = icon("dashboard")),
      menuItem("Regression", tabName = "regression", icon = icon("th")),
      menuItem("Dataset", tabName = "dtset", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "histogram",
              h1("(Histogram) Variables affecting monthly earnings: data from 1980 for 935 individuals"),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(plotOutput("plot2", height = 250)),
                box(plotOutput("plot3", height = 250)),
                box(plotOutput("plot4", height = 250)),
                
                box(
                  title = "Controls",
                  sliderInput("myslider1", "Sample Size:", 1, 935, 400)
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "control",
              h1("Set the range of predictors about Monthly Earnings"),
              fluidRow(
                sliderInput(inputId = "range1", 
                            label = h2("YearsEdu Vs. Earnings"), 
                            min = 9,
                            max = 18,
                            value = c(12, 15)),
                
                plotOutput(outputId = "myPlot1", height = 250),
                
                sliderInput(inputId = "range2", 
                            label = h2("IQ Vs. Earnings"), 
                            min = 50,
                            max = 150,
                            value = c(80, 120)),
                
                plotOutput(outputId = "myPlot2", height = 250),
                
                
                sliderInput(inputId = "range3", 
                            label = h2("YearsExperience Vs. Earnings"), 
                            min = 1,
                            max = 23,
                            value = c(8, 15)),
                
                plotOutput(outputId = "myPlot3", height = 250)
              )
      ),
      
      # third tab content
      tabItem(tabName = "regression",
              h1("Choose your dependent and indenpendent variables for regression"),
              selectInput(inputId="dependent", label = "Dependent Variables",
                          choices = as.list(AttributeChoices)),
              uiOutput("indep"),
              verbatimTextOutput(outputId = "RegOut")
      ), 
      
      # fourth tab content
      tabItem(tabName = "dtset",
              h1("Original dataset"),
              DT::dataTableOutput("tableDT"))
    )
    
  )
)



shinyApp(ui, server)