library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(cluster)
library(shiny)
library(factoextra) # clustering algorithms & visualization
library(ggplot2) 

mydata <-read.csv("Mall_Customers.csv")
mydata2 <-mydata[,3:5]
vars <- setdiff(names(mydata2), NA)


server <- function(input, output, session) {
  
  #datatable
  output$tableDT <- DT::renderDataTable(mydata,
                                        options = list(paging=F),
                                        rownames=F,
                                        filter = "top")
  
  #Brush
  output$scat = renderPlot({
    ggplot(mydata, aes(Age, Annual_Income)) + geom_point() + geom_smooth()
  })
  
  cust <- reactive({
    
    user_brush <- input$user_brush
    sel <- brushedPoints(mydata, user_brush)
    return(sel)
    
  })
  #export
  output$table <- DT::renderDataTable(DT::datatable(cust()))
  
  output$mydownload <- downloadHandler(
    filename = "target_customers.csv",
    content = function(file) {
      write.csv(cust(), file)})
  
  #clustering
  selectedData <- reactive({
    mydata2[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters, nstart = 25)
  })
  
  output$plot1 <- renderPlot({
    fviz_cluster(clusters(), geom = "point", data = selectedData(), 
                 show.clust.cent = TRUE, show_labels = FALSE, stand=F)
  })
  
}
  

ui = navbarPage(theme = shinytheme("sandstone"), strong("Mall customer analysis project"),
                tabPanel(
                  "Brush your target customers and download",
                  plotOutput("scat", brush = "user_brush"),
                  dataTableOutput("table"),
                  downloadButton(outputId = "mydownload", label = "Download Table")),

                tabPanel(
                  "Kmeans clustering by customer's age, income, and spending score",
                  sidebarPanel(
                    selectInput('xcol', 'X Variable', vars),
                    selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                    numericInput('clusters', 'Cluster count', 3, min = 1, max = 9)
                  ),
                  mainPanel(
                    plotOutput('plot1')
                    
                  )),
                                
                tabPanel(
                  "Original dataset with underlying information",
                         DT::dataTableOutput("tableDT"))
                
)
                
shinyApp(ui = ui, server = server)