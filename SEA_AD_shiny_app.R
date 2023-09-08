library(shiny)
#library(DBI) 
library(readxl)
library(shinydashboard)
library(shinyLP)
library(dashboardthemes)
library(dplyr)
library(data.table)
library(DT)
library(shinycssloaders)
options(stringsAsFactors = F)

# beta table coefficient path
# change to fread
#beta_file <- read.csv("/allen/programs/celltypes/workgroups/rnaseqanalysis/sarojaS/230810_SEA_AD_app/beta_coefficient_table.csv", header = T, sep = ",")
# Removed the plot as it will appear belo
#beta_file_subset <- beta_file[,c(-1)]
#beta_file_subset <- beta_file[c(1:50),]
#beta_file <- read.csv("/allen/programs/celltypes/workgroups/rnaseqanalysis/sarojaS/230810_SEA_AD_app/Pax6_1_beta_coeff_table_with_header.csv")
#beta_file_subset <- beta_file
#colnames(beta_file_subset) <- c("Row.number","Gene","Taxonomy.Level","Population","all","early","late","Mean.expression","Comparative.Viewer","Pseudoprogression.Plot")
##
css <- "
div.dataTables_wrapper  div.dataTables_filter {
  width: 100%;
 float: none;
  text-align: center;
}
"

ui <- dashboardPage(
  ##
  dashboardHeader(title = "SEA-AD shiny app"),
  ##
  dashboardSidebar(
    verticalLayout(
      div(),
      ## Let the user filter tables to just one species
      selectInput("Level", "Taxonomy Level", choices=c("All","Class","Subclass","Supertype"), selected=NULL, multiple=FALSE),
      actionButton("openTable", 
                   "Open beta coefficent table",
                   style = "color: #fff; background-color: #27ae60; border-color: #fff; padding: 10px 20px 10px px; margin: 5px 5px 5px 20px; "),
      #br(),
      #downloadButton("downloadOutput", 
       #              "Download selections",
       #               style = "color: #fff; background-color: #0C134F; border-color: #fff; padding: 10px 20px 10px px; margin: 5px 5px 5px 25px; "),
    ),
    br(),
    htmlOutput("text1"),
    tags$head(tags$style("#text1{color: white;
                                 font-size: 12px;
                                 text-align: center;
                                 }"
    )
    )
  ),
  ##
  dashboardBody(
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
        )
      ),
      #column(width = 12,
             #box(
        DT::dataTableOutput("table") %>% withSpinner(color="#0dc5c1"),
        width = 12,
        height = 120,
        solidHeader = T,
        collapsible = F,
      fluidRow(
        column(width = 6,
          #box(uiOutput("OverviewImage")),
          uiOutput("PseudoProgressionImageOverview")),
        column(width = 6,
           uiOutput("PseudoProgressionImage"))
        
      )
    )
  )
)

####################################################################################################
server <- function(input, output, session){
  
  output$text1 <- renderUI({
    HTML(paste("Pseudo progression table coloumn names:",
    "all: Beta coefficient across all of pseudoprogression",
    "early: Beta coefficient across early pseudoprogression",
    "late: Beta coefficient across late pseudoprogression",
    "Mean expression: (natural log UMIs per 10k plus 1)", sep = "<br>"))
    })
  
  ## ---- Handle the selector Inputs to only show valid combinations
  ## ---- Do this first to avoid weird quirks when read in new peak table.
  ## For selected Species only load Dataset
  #observe({
   # datasetChoices = peakTables.df %>% filter(genome == input$speciesInput) %>% pull(dataset)
   # updateSelectInput(session, 
    #                  "datasetInput",
     #                 choices = datasetChoices
    #)})
  
  ## For selected Species and Dataset only load levels
  #observe({
  #  annoChoices = peakTables.df %>% filter(genome == input$speciesInput & dataset == input$datasetInput) %>% pull(annotation)
   # updateSelectInput(session, 
   #                   "annotationInput",
   #                   choices = annoChoices
   # )})
  
  ## ------ Handle the selection of a new peak table, lazy evaluation
  ##
  beta_table_selector <- eventReactive(input$openTable, {
    ##
    
    beta_file <- read.csv("/allen/programs/celltypes/workgroups/rnaseqanalysis/sarojaS/230810_SEA_AD_app/beta_coefficient_table.csv", header = T, sep = ",")
    beta_file_subset <- beta_file
    colnames(beta_file_subset) <- c("Row.number","Gene","Taxonomy.Level","Population","all","early","late","Mean.expression","Comparative.Viewer","Pseudoprogression.Plot")
   
    if(input$Level == "All"){
      beta_table_show = beta_file_subset
      #beta_table_show <- read.csv("/allen/programs/celltypes/workgroups/rnaseqanalysis/sarojaS/230810_SEA_AD_app/beta_coefficient_table.csv", header = T, sep = ",") 
      #colnames(beta_table_show) <- c("Row.number","Gene","Taxonomy.Level","Population","all","early","late","Mean.expression","Comparative.Viewer","Pseudoprogression.Plot")
      ##
      beta_table_show
    }else{
    beta_tabled_selected = beta_file_subset %>% filter(Taxonomy.Level == input$Level)
      beta_table_show = beta_tabled_selected
      beta_table_show
    #Taxonomy.Level = input$Level 
    # beta_table_show <- read.csv(paste0("/allen/programs/celltypes/workgroups/rnaseqanalysis/sarojaS/230810_SEA_AD_app/beta_coefficient_table_",Taxonomy.Level,".csv", collapse = ""), header = T, sep = ",") 
    #colnames(beta_table_show) <- c("Row.number","Gene","Taxonomy.Level","Population","all","early","late","Mean.expression","Comparative.Viewer","Pseudoprogression.Plot")
    
    }
  })
  
  observeEvent(input$openTable, {
    ## ------ Render the selected peak table
    output$table <- DT::renderDataTable({
      ##
      DT::datatable(
        data = beta_table_selector(),
        options = list(
          dom = 'tp',
          #search = list(smart = TRUE),
          #searchBuilder = TRUE,
          #scrollY = "70vh",
          #scrollX = TRUE,
          #scroller = TRUE,
          #deferRender = T,
          #dom = 'Bfrtip',
          #plain = T,
          #processing = TRUE,
          #elipsis = T,
          #select = list(style = 'os', items = 'row'),
          pageLength = 10
          #dom = 't'
          #lengthMenu = list(c( 5,1000, 5000, -1), c( '5','1000','5000','All'))
          #language = list(searchPlaceholder = "Genomic coordinate, Cell population, Rank")
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
  
  
  #output$OverviewImage <- renderUI({
  #  s = input$table_rows_selected
  #  img(src = "https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/AHR/overview.jpg",width = "100%", height = "100%") 
  #})
  
  output$PseudoProgressionImageOverview <- renderUI({
    if(length(input$table_rows_selected) != 0){
      img(src = "https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/AHR/overview_subclass.jpg",width = "100%", height = "100%") 
  }else{
    renderText({
      "Select class please"
    })
    
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
     Population_edited_2 <- Population_edited
   }
    
    
    
   
    print(Population_edited)
    #if(Taxonomy == "Class"){
     # img(src = "https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/AHR/overview_subclass.jpg",width = "100%", height = "100%") 
    #}else{
    if(Taxonomy != "Class"){
    img(src = paste0("https://sea-ad-single-cell-profiling.s3.amazonaws.com/MTG/RNAseq/pseudoprogression-plots/", Gene,"/supertype-",Population_edited_2,".jpg", collapse= ""), width = "100%", height = "100%")
    }else{
      renderText({
        "Select subclass or supertype please"
      })
    }
   
    }else{
      renderText({
        "Select subclass or supertype please"
      })
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
