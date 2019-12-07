####################################################################################
####### GFC WRAPPER
####### SEPAL shiny application
####### FAO Open Foris SEPAL project
####### remi.dannunzio@fao.org
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or
# software or in the documentation accompanying it, for program maintenance and
# upgrading as well as for any # damage that may arise from them. FAO also declines
# any responsibility for updating the data and assumes no responsibility for errors
# and omissions in the data provided. Users are, however, kindly asked to report any
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2019/02/19
## gfc-wrapper / server
####################################################################################


####################################################################################
####### Start Server

shinyServer(function(input, output, session) {
  ####################################################################################
  ##################### Choose language option             ###########################
  ####################################################################################
  output$chosen_language <- renderPrint({
    if (input$language == "English") {
      source("scripts/text_english.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("en")
    }
    if (input$language == "") {
      source("scripts/text_english.R", 
             local = TRUE, 
             encoding = "UTF-8")
      #print("fr")
    }
    
  })
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store filepath    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  
  volumes <- c('Home' = Sys.getenv("HOME"),
               volumes)
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  

  ##################################################################################################################################
  ############### GET A REACTIVE VALUE
  v <- reactiveValues(threshold = FALSE,
                      country   = FALSE)
  

  ##################################################################################################################################
  ############### Select input file 
  shinyFileChoose(
    input,
    'aoi_file',
    filetype = c(
      'shp',
      'sqlite',
      'gdb',
      'kml'
    ),
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  ################################# Display the file path
  aoi_file_path <- reactive({
    validate(need(input$aoi_file, "Missing input: Please select the zone file"))
    df <- parseFilePaths(volumes, input$aoi_file)
    file_path <- as.character(df[, "datapath"])
    nofile <- as.character("No file selected")
    if (is.null(file_path)) {
      cat(nofile)
    } else{
      cat(file_path)
    }
    file_path
  })
  
  ##################################################################################################################################
  ############### DISPLAY THE AOI FILE PATH
  output$filepath <- renderText({
    req(input$aoi_file)
    aoi_file_path()
  })
  
  
  ##################################################################################################################################
  ############### Insert the START button
  output$ProcessButton <- renderUI({
    req(input$aoi_file)
    actionButton('ProcessButton', textOutput('process_button'))
  })
  

  ##################################################################################################################################
  ############### Insert the DISPLAY MAP button
  output$DisplayMapButton <- renderUI({
    req(input$aoi_file)
    actionButton('DisplayMapButton', textOutput('display_map_button'))
  })
  
  
  ##################################################################################################################################
  ############### Insert the STATISTICS button
  output$StatButton <- renderUI({
    req(process())
    actionButton('StatButton', textOutput('stat_button'))
  })
  
  
  ##################################################################################################################################
  ############### Make the AOI reactive
  make_aoi <- reactive({
    req(input$aoi_file)
    
    aoi_file_path <- aoi_file_path()
    
    base        <- basename(as.character(aoi_file_path))
    countrycode <- substr(base,1,nchar(base)-4)
    
    source("scripts/b0_custom_aoi_app.R",  local=T, echo = TRUE)
    
    v$country <- aoi_shp
    
  })
  
  # threshold <- reactive({
  #   v$threshold <- input$threshold
  #   input$threshold
  # })
  
  ##################################################################################################################################
  ############### DOWNLOAD DATA
  process <- eventReactive(input$ProcessButton,
                             {
                               req(input$ProcessButton)
                               req(input$aoi_file)
                               
                               req(make_aoi())
                               
                               threshold   <- input$threshold
                               base        <- basename(as.character(aoi_file_path()))
                               countrycode <- substr(base,1,nchar(base)-4)
                               
                               aoi_name   <- paste0(aoi_dir,"aoi_",countrycode)
                               aoi_shp    <- paste0(aoi_name,".shp")
                               aoi_field <-  "id_aoi"
                               
                               aoi <- readOGR(make_aoi())
                               (bb    <- extent(aoi))
                               
                               source("scripts/b1_download_merge.R",  local=T, echo = TRUE)
                               source("scripts/b2_make_map_threshold.R",  local=T,echo = TRUE)
                               source("scripts/b3_compute_areas.R",  local=T,echo = TRUE)
                               source("scripts/b4_make_mspa_ready_mask.R",  local=T,echo = TRUE)
                               
                               list.files(gfc_dir)
                             })
  


  ############### Display the results as map
  output$display_res <- renderPlot({
    req(input$DisplayMapButton)

    threshold   <- input$threshold
    base        <- basename(as.character(aoi_file_path()))
    countrycode <- substr(base,1,nchar(base)-4)
    
    print('Check: Display the map')
    
    plot(raster(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")))
    
    
  })
  
  ##################################################################################################################################
  ############### Display parameters
  output$parameterSummary <- renderText({
    #req(input$input_file)
    #print(paste0("Parameters are : ",parameters()))
  })
  
  # ##################################################################################################################################
  # ############### Display time
  # output$message <- renderTable({
  #   req(prims_data())
  #   
  #   data <- prims_data()
  #   
  #   head(data)
  # })
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
