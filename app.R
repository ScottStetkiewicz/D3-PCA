library(shinydashboard)
library(scatterD3)
library(FactoMineR)
library(RColorBrewer)
library(colorspace)

ui <- tagList(
  dashboardPage(
  dashboardHeader(title = "D3 Principal Component Analysis" ,
                  titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(id="sidebarmenu",
      menuItem("Introduction", tabName = "intro", icon = icon("dashboard")),
      menuItem("Analysis", icon = icon("arrows-alt"), tabName = "analysis"),
      conditionalPanel("input.sidebarmenu === 'analysis'",
                       selectInput("labNames", "Select Label Names",""),
                       selectInput("colVar", "Select Color Variable",""),
                       selectInput("shapeVar", "Select Shape Variable",""),
                       checkboxInput("ellipse", tagList(strong("Toggle Ellipses"))),
                       checkboxInput("do", tagList(strong(tags$em("k"),"- Means Cluster Analysis"))),
                       sliderInput("clusters", strong("Clusters :"), 2,10,3,step = 1),
                       actionButton("reset_input", "Clear Selected Points"),
                       tags$style(type='text/css', "button#reset_input { margin-left: 9px; }")
      ),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              fluidRow(column(width=6,
                              box(title=strong("D3 PCA"), width=NULL, status = "success", solidHeader = TRUE,
                                  includeMarkdown("d3pcaintro.md")
                              )),
                       column(width=6,
                              box(title="Hovering", width=NULL, status = "warning", solidHeader = TRUE,
                                  h4("Once the plot is generated, you can hover over points to reveal more information about them.  This can also be done with the items listed in the legend."
                                  )),
                              box(title="Lasso", width=NULL, status = "warning", solidHeader = TRUE,
                                  h4("If you want to select a series of points, hold down the shift key and define the area on the plot.  Any points within this area will have their label names displayed on the right of the plot. Hold Shift and click again to release the lasso."
                                  )),
                              box(title="Zoom", width=NULL, status = "warning", solidHeader = TRUE,
                                  h4("To control the zoom, scroll the mouse wheel forwards and backwards."
                                  ))
                              )
              )
      ),
      tabItem(tabName = "analysis",
        fluidRow(
          tabBox(
            id = "tabset1", width=12, height=800,
            tabPanel(strong(h3("Step 1")),
                     box(title=strong("Step 1: Upload your .CSV file using the Browse button to the right"), 
                         status = "success", solidHeader = TRUE,
                         includeMarkdown("d3pcaupload.md")
                     ),
                     box(title="Choose CSV File", status = "warning", solidHeader = TRUE,
                       column(
                         width=8, fileInput('file1', "",
                                            accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                         radioButtons('sep', NULL,
                                                        c(Comma=',',
                                                          Semicolon=';',
                                                          Tab='\t'),
                                                        'Comma', inline = TRUE),
                         radioButtons('quote', NULL,
                                                        c(None='',
                                                          'Double Quote'='"',
                                                          'Single Quote'="'"),
                                                        'Double Quote', inline = TRUE)
                       ),
                       column(width = 2, checkboxInput('header', 'Header', TRUE))
                                ),
                     box(title="Original Data", solidHeader = TRUE, status = "primary", width=12,
                         DT::dataTableOutput("contents")         
                     )
                     ),
            tabPanel(strong(h3("Step 2")),
                     box(title=strong("Step 2: Create a new table of ONLY numeric data"), 
                         status = "success", solidHeader = TRUE,
                         h4("Principal Component Analysis can only be performed on quantitative data.  All string vectors (e.g. sample names, sites, etc.) must be removed prior to analysis.")
                     ),
                     box(title="Select Quantitative Variables:", status = "warning", solidHeader = TRUE,
                         selectizeInput(
                           "variables", 
                           NULL,
                           "",
                           multiple=TRUE)
                     ),
                     box(title="Original Data", solidHeader = TRUE, status = "primary", width=12,
                         DT::dataTableOutput("contents2")         
                     )
            ),
            tabPanel(strong(h3("Step 3")),
                       column(width=6,
                         box(title=strong("Step 3: Select Input Variables"), width=NULL, 
                             status = "success", solidHeader = TRUE,
                             h4(p("Using the three 'Select:...' options on the left-hand panel, choose which variables to use for the plot aesthetics."),
                                p("If you do not want to specifically define any of these inputs, select 'None'."),
                                p("Examples are provided to the right."))
                         )
                       ),
                       column(width=6,
                         box(title="Select Label Names:", width=NULL, status = "warning", solidHeader = TRUE,
                             h4("If your data is comprised of individual samples which are represented in a column of sample names, select this column.")
                         ),
                         box(title="Select Color Variable:", width=NULL, status = "warning", solidHeader = TRUE,
                             h4("If your data is organized by discrete sites, select the column which is used to identify these sites.")
                         ),
                         box(title="Select Shape Variable:", width=NULL, status = "warning", solidHeader = TRUE,
                             h4("If your data comes from different regions or countries, select the column which is used to identify these regions or countries.")
                         )
                       )
            ),
            tabPanel(strong(h3("Plot Output")),
                     box(width=10,
                         scatterD3Output("scatterPlot", height=700)
                     ),
                     box(title = "Selected Points :", width=2, solidHeader = TRUE, status = "primary",
                         h4(tagAppendAttributes(textOutput("points_selected"), style="white-space:pre-wrap;"))
                     )
            )
          )
        )
      ),
      tabItem(tabName = "about",
              box(title=strong("Acknowledgements"), status = "success", solidHeader = TRUE,
                  includeMarkdown("d3pcaabout.md")
              ),
              box(title=strong("References"), status = "success", solidHeader = TRUE,
                  includeMarkdown("d3pcareferences.md")
              )
      )
    )
  )
)
)

server <- function(input, output, session) {
  
  inFile <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(iris)
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  
  observe({
    updateSelectizeInput(
      session,
      "variables",
      choices=names(inFile()),
      selected=c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width")
      )
  })
  
  observe({
    updateSelectInput(
      session,
      "labNames",
      choices=c("None", names(inFile())))
  })

  observe({
    updateSelectInput(
      session,
      "colVar",
      choices=c("None", names(inFile())),
      selected = "Species"
      )
  })

  observe({
    updateSelectInput(
      session,
      "shapeVar",
      choices=c("None", names(inFile())))
  })
  
  labNames <- reactive({
    if (input$labNames=="None") {rep(NA,nrow(inFile()))} else {inFile()[,input$labNames]}
  })
  
  colVar <- reactive({
    if (input$colVar=="None") {rep(NA,nrow(inFile()))} else {inFile()[,input$colVar]}
  })
  
  shapeVar <- reactive({
    if (input$shapeVar=="None") {rep(NA,nrow(inFile()))} else {inFile()[,input$shapeVar]}
  })

  selectedData <- reactive({
    data.frame(inFile()[input$variables])
  })
  
  output$contents <- DT::renderDataTable({
    inFile()
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

  output$contents2 <- DT::renderDataTable({
    selectedData()
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

  svd_comp <- reactive({

    out<-PCA(selectedData(), graph=FALSE)
    clust1<-data.frame(out$ind$coord)
    clust <- cbind(clust1$Dim.1,clust1$Dim.2)
    clust1
  })

  vector_arrows <- reactive({


    out<-PCA(selectedData(), graph=FALSE)
    cc1<-data.frame(out$ind$coord)
    cc2<-data.frame(out$var$coord)

    varcoordz1 <- (cc2$Dim.1*3)
    varcoordz2 <- (cc2$Dim.2*3)

    points <- data.frame(x = cc1$Dim.1,
                         y = cc1$Dim.2,
                         names = labNames(),
                         color = colVar(),
                         lab = rep(NA,nrow(cc1)),
                         symbol = shapeVar(),
                         type = rep("point", nrow(cc1)))

    arrows <- data.frame(x = varcoordz1,
                         y = varcoordz2,
                         names = rep(NA,nrow(cc2)),
                         color = "",
                         lab = row.names(cc2),
                         symbol = rep(NA,nrow(cc2)),
                         type = rep("arrow", nrow(cc2)))

    data1 <- rbind(points, arrows)
    data1
  })

  clusters <- reactive({
    kmeans(svd_comp(), input$clusters)
  })

  output$points_selected <- renderText(if (input$reset_input==TRUE) {NULL} else {input$selected_point})

  output$scatterPlot <- renderScatterD3({
    getPalette <- colorRampPalette(brewer.pal(12, "Paired"))
    df <- svd_comp()
    dfarrow <- vector_arrows()
    if(input$do) {df$cluster <- as.factor(clusters()$cluster)}

    tooltips <- paste0("<b>", input$colVar, "</b><br>",
                       input$labNames, "<br>")

    tooltips2 <- paste0("<b>", dfarrow$color, "</b><br>",
                       dfarrow$names, "<br>")

    if (input$do) {
      scatterD3(x = df[,1], y = df[,2],
                xlab="Component 1", ylab="Component 2",
                tooltip_text = tooltips,
                point_opacity = 0.75,
                lasso = TRUE,
                lab = labNames(),
                labels_size = 0,
                hover_size = 4,
                ellipses = if (input$ellipse==TRUE) {TRUE} else {FALSE},
                ellipses_level = .75,
                hover_opacity = 1,
                col_var = df$cluster,
                col_lab = "Cluster",
                symbol_var = shapeVar(),
                symbol_lab = input$shapeVar,
                lasso_callback = "function(sel) {
                Shiny.onInputChange(
                'selected_point', sel.data().map(function(d)
                {return d.lab}).join('\\n')
                )
    }"
                )
  } else {
scatterD3(data= dfarrow, x = x, y = y,
          type_var = dfarrow$type,
          xlab="Component 1", ylab="Component 2",
          tooltip_text = tooltips2,
          lasso = TRUE,
          lab = lab,
          ellipses = if (input$ellipse==TRUE) {TRUE} else {FALSE},
          ellipses_level = .75,
          key_var = names,
          hover_size = 4,
          hover_opacity = 1,
          col_var = color,
          col_lab = input$colVar,
          colors = if (length(unique(colVar())) <= 8) {c(rainbow_hcl(length(unique(colVar()))),"#000000")} 
                    else 
                      {c(getPalette(length(unique(colVar()))),"#000000")},
          symbol_var = symbol,
          symbol_lab = input$shapeVar,
          lasso_callback = "function(sel) {
            Shiny.onInputChange(
              'selected_point', sel.data().map(function(d)
              {return d.key_var}).join('\\n')
                              )
                            }"
)
  }
})
}

shinyApp(ui, server)