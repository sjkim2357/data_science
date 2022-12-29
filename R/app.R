https://sjkim2357.shinyapps.io/project4/

#Brush, DT, tabPanel

library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
mydata <-read.csv("Mall_Customers.csv")

server <- function(input, output, session) {

  
  output$tableDT <- DT::renderDataTable(mydata,
                                        options = list(paging=F),
                                        rownames=F,
                                        filter = "top")
  
  
  output$scat = renderPlot({
    ggplot(mydata, aes(Age, Annual_Income)) + geom_point() + geom_smooth()
  })
  
  cust <- reactive({
    
    user_brush <- input$user_brush
    sel <- brushedPoints(mydata, user_brush)
    return(sel)
    
  })
  
  output$table <- DT::renderDataTable(DT::datatable(cust()))
  
  output$mydownload <- downloadHandler(
    filename = "target_customers.csv",
    content = function(file) {
      write.csv(cust(), file)})
  
}

ui = navbarPage(theme = shinytheme("sandstone"), title = h4(strong("Mall customer analysis project: Age & Annual income")),
                tabPanel(
                  ("Brush your target customers and download"),
                plotOutput("scat", brush = "user_brush"),
                dataTableOutput("table"),
                downloadButton(outputId = "mydownload", label = "Download Table")),
                
                tabPanel("Original dataset with underlying information",
                         DT::dataTableOutput("tableDT"))
)

shinyApp(ui = ui, server = server)

