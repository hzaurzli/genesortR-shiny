library(shiny)

ui <- tagList(
  fluidPage(
    titlePanel("genesorteR (Feature Ranking for Single Cell Data)"),
    sidebarLayout(
      sidebarPanel(
        # uiOutput 做上传文件的 ui, 对应后面的 output$file1
        uiOutput('file1'),
        
        actionButton('reset', 'RESET'),
        hr(),
        sliderInput("level", "Choose cutoff",
                    min = 0, max = 1,
                    value = 0.99),
        hr(),
        downloadButton("downloadData", "Download"),
        hr(),
        h5('Developer:'),
        h6('Small runze (shiny app)'),
        br(),
        h5('Github: '),
        h6('https://github.com/hzaurzli (Small runze)'),
      ),
      mainPanel(
        h4("genesorteR table"),
        br(),
        br(),
        shinycssloaders::withSpinner(
          dataTableOutput("table")
        )
      )
    )
  )
)



server <- function(input, output, session) {
  options(shiny.maxRequestSize=1024*1024*1024^2)
  
  values <- reactiveValues(
    file = NULL
  )
  
  
  dataInput1 <- reactive({
    sessionEnvir <- sys.frame()
    if (!is.null(input$file1)) eval(parse(text = load(input$file1$datapath, sessionEnvir)))
  })
  
  
  # observeEvent(input$reset), 代表点击 RESET 时触发的动作,此时重新渲染 fileInput 的 ui
  observeEvent(input$reset, {
    values$file <- NULL
    output$file1 <- renderUI({
      fileInput("file1", "Step 1: Choose scRNA RData",
                accept=c('.RData, .Rds')
      )
    })
  }, ignoreNULL = F)

  
  output$table <- renderDataTable({
    library(genesorteR)
    
    data = dataInput1()
    
    sg = sortGenes(data$exp, data$cellType)
    mm = getMarkers(sg, quant = input$level)
    dd = data.frame(mm$gene_shannon_index[mm$markers])
    dat <<- data.frame(Gene = row.names(dd),Shannon_index = dd[,1])
    
  }, options = list(pageLength = 10))
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dat,file,row.names = T,quote = F)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

