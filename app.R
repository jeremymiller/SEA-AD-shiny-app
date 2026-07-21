# I'm running this in Using R 4.3.2
# Set-up the renv.lock file. This only needs to be done once


library(shiny)
library(readxl)
library(shinydashboard)
library(shinyLP)
library(dashboardthemes)
library(dplyr)
library(data.table)
library(DT)
library(shinycssloaders)
library(arrow)
options(stringsAsFactors = F)

css <- "
div.dataTables_wrapper  div.dataTables_filter {
  width: 100%;
 float: none;
  text-align: center;
}
"

ui <- dashboardPage(

  title = 'SEA-AD Gene Trajectories',
  ##
  dashboardHeader(title = div(h3("SEA-AD", style="margin: 0;"),h4("Gene Trajectories", style="margin: 0;"))),
  
  ##
  dashboardSidebar(
    verticalLayout(
      div(),
      
      h3("Get started now"),
      
      h6("Please select an option and click open table to begin.", style='justify-self: center; padding-left: 10px; padding-right: 10px; padding-bottom: 0px;margin-bottom: 0px; text-align: center;'),
      ## Let the user filter tables to just one species
      selectInput("Level", "Taxonomy Level", choices=c("All","Class","Subclass","Supertype"), selected="Class", multiple=FALSE),
      actionButton("openTable", 
                   "Open beta coefficent table",
                   style = "color: #fff; background-color: #27ae60; border-color: #fff; padding: 10px 20px 10px px; margin: 5px 5px 5px 20px; "),
      ),
    
    HTML("<hr/>"),
    
    h4("New to this tool?"),
    
    actionButton(
      "open_video_btn",
      HTML("Watch this overview!"),
      icon = icon("video"),
      style = "font-size: 100%; 
                            padding: 5px 5px; 
                            width: 165px; /* Give it a specific width so margin: auto can work */
                            display: block; /* Make it a block element */
                            white-space: normal;",  
      onclick = paste0("window.open('https://jeremymiller.github.io/SEA-AD-shiny-app/gene_trajectory_overview.mp4', '_blank');")
    ),
    
    h4("Have suggestions?"),
    
    actionButton(
      inputId = "email1",
      icon = icon("envelope", lib = "font-awesome"),
      a("PROVIDE FEEDBACK",
        style = "color: #000000; border-color: #2e6da4; text-align: center;", 
        href = "https://app.smartsheet.com/b/form/01c0ed3a74d14135bd68620a92bbd5ef")
    ),
    
    HTML("<hr/>"),
    
    htmlOutput("text1"),
    tags$head(tags$style("#text1{color: white;
                                 font-size: 12px;
                                 text-align: center;
                                 }"
    )
    ),
    div(
      actionButton("showInfo","Show/Hide Info", style = "margin: 0px;"),
      style="display: flex; align-content: center; justify-content: center; flex-wrap: wrap; padding-top: 10%"
    )
    ),
  ##
  dashboardBody(
    
    ## NEW: favicon
    tags$head(
      tags$title("SEA-AD Gene Trajectories"),
      tags$link(rel = "icon", type = "image/x-icon", href = "SEAAD.ico"),
      tags$script(HTML("document.title = 'SEA-AD Gene Trajectories';")),
    ),
    
    ## Peak table
    fluidRow(
        tags$style(
        HTML(
          ".dataTables_wrapper .dataTables_filter {
              float: none;
              text-align: center;}
              .dataTables_wrapper .dataTables_filter input{
              text-align: center;
              width: 500px;}"
        ),
        
      
        
        ## This is a css hack to get text before the loading indicator
        ## It disappears with the loader
        HTML(".load-container::before {
              content: 'This might take a minute...';
              position: absolute;
              left: 45%;
              text-align: center;
             }
             ")
      ),
      DT::dataTableOutput('table') %>% withSpinner(color="#0dc5c1"),
      width = 12,
      height = 120,
      solidHeader = T,
      collapsible = F,
      fluidRow(
        column(width = 6,
               uiOutput("PseudoProgressionImageOverview")),
        column(width = 6,
               uiOutput("PseudoProgressionImage"))
        
      )
    ),
    htmlOutput('moreInfo'),
    tags$head(includeHTML("google-analytics.html")),
  ),
  
)

####################################################################################################
server <- function(input, output, session){
  
  output$text1 <- renderUI({
    HTML(paste("<h4 style='padding-bottom: 0px; margin-bottom: 0px;'>Pseudo progression table column names</h4>",
               "<h5 style='font-weight: bold'>All</h5> Beta coefficient across all of pseudoprogression",
               "<h5 style='font-weight: bold'>Early</h5> Beta coefficient across early pseudoprogression",
               "<h5 style='font-weight: bold'>Late</h5> Beta coefficient across late pseudoprogression",
               "<h5 style='font-weight: bold'>Mean Expression</h5> (natural log UMIs per 10k plus 1)",
               sep = "<hr/>"
        ))
  })
  
  openInfo <- reactiveVal(TRUE)
  observeEvent(input$showInfo, {
    openInfo(!openInfo())
  })
  
  infoFile <- reactive({
    if(openInfo()) {
      includeHTML('./moreinfo.html')
    } else {
      NULL
    }
  })
  
  output$moreInfo <- renderUI({
    infoFile()
  })
  
  beta_table_selector <- eventReactive(input$openTable, {
    ##
    beta_file <- read_feather('output_new.feather')
    colnames(beta_file)[which(colnames(beta_file) == "X")] <- "Row.number"
    beta_file_subset <- beta_file[,c("Gene","Taxonomy Level","Population","Effect size across all of pseudoprogression","Effect size across early pseudoprogression","Effect size across late pseudoprogression","Mean expression (natural log UMIs per 10k plus 1)","Comparative Viewer","Pseudoprogression Plot")]
    #beta_file_subset <- beta_file
    colnames(beta_file_subset) <- c("Gene","Taxonomy.Level","Population","all","early","late","Mean.expression","Comparative.Viewer","Pseudoprogression.Plots")
    
    if(input$Level == "All"){
      beta_table_show = beta_file_subset
      beta_file_subset
    }else{
      beta_tabled_selected = beta_file_subset %>% filter(Taxonomy.Level == input$Level)
      beta_tabled_selected
    }
  })
  
  observeEvent(input$openTable, {
    openInfo(FALSE)
    ## ------ Render the selected peak table
    output$table <- DT::renderDataTable({
      ##
      DT::datatable(
        data = beta_table_selector(),
        options = list(
          dom = 'tp',
          pageLength = 10
        ),
        filter = "top",
        selection = 'single',
        rownames = F,
        extensions = c(
          "SearchBuilder",
          'RowGroup',
          "Scroller"
        ),
        escape=F ## Sanitize is for handling links in data table
      )
    },
    server = TRUE)
  })
  
  output$PseudoProgressionImageOverview <- renderUI({
    if(length(input$table_rows_selected) != 0){
      s = input$table_rows_selected
      Gene <- beta_table_selector()[s, ]$Gene
      print(paste0("https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/",Gene,"/overview_subclass.jpg",collapse= ""))
      img(src = paste0("https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/",Gene,"/overview_subclass.jpg",collapse= ""),
          width = "100%",
          height = "100%")
    }
  })
  
  
  output$PseudoProgressionImage <- renderUI({
    
    if(length(input$table_rows_selected) !=0 ){
      
      s = input$table_rows_selected
      Gene <- beta_table_selector()[s, ]$Gene
      Population <- beta_table_selector()[s, ]$Population
      Taxonomy <- beta_table_selector()[s, ]$Taxonomy.Level
      
      
      #if(Population != "Class"){
      Population_edited <- strsplit(Population, "_(?!.*_)", perl  = T)[[1]][1]
      #}
      
      if(Population_edited == "Astro"){
        Population_edited_2 <- "Astrocyte"
      }else if(Population_edited == "Oligo"){
        Population_edited_2 <- "Oligodendrocyte"
      }else if(Population_edited == "Endo"){
        Population_edited_2 <- "Endothelial"
      }else if(Population_edited == "Micro-PVM"){
        Population_edited_2 <- "Microglia-PVM"
      }else if(Population_edited == "Lamp5_Lhx6"){
        Population_edited_2 <- "Lamp5 Lhx6"
      }else{
        Population_edited_2 <- gsub("/"," ",Population_edited)
      }
      
      #if(Taxonomy == "Class"){
      # img(src = "https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/AHR/overview_subclass.jpg",width = "100%", height = "100%") 
      #}else{
      if(Taxonomy != "Class"){
        img(src = paste0("https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/", Gene,"/supertype-",Population_edited_2,".jpg", collapse= ""), width = "100%", height = "100%")
      }
      
    }
  })
  
  ## ------ Download user selection table
  #output$downloadOutput <- downloadHandler(
  #  filename = function(){paste0("CERP_peaks", input$speciesInput, "_", input$datasetInput, "_", input$annotationInput, ".csv")}, 
  #  content = function(fname){
  #    write.csv(peak_table_selector()[input$table_rows_selected,], fname, row.names=F)
  # }
  #)
  
}

#####################################################################################################
shinyApp(ui, server)

#updates
# read it inside the function instead of global
# Download filtered rows/ full table link , since we already have one?
# Change column names and alignment
