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
  ############### Insert the MERGE button
  output$MergeButton <- renderUI({

    actionButton('MergeButton', textOutput('merge_button'))
  })
  
  ##################################################################################################################################
  ############### Insert the MAP button
  output$MapButton <- renderUI({
    req(merge_tiles())
    actionButton('MapButton', textOutput('map_button'))
  })
  
  ##################################################################################################################################
  ############### Insert the DISPLAY MAP button
  output$DisplayMapButton <- renderUI({
    req(input$country_code)
    actionButton('DisplayMapButton', textOutput('display_map_button'))
  })
  
  ##################################################################################################################################
  ############### Insert the STATISTICS button
  output$StatButton <- renderUI({
    req(merge_tiles())
    actionButton('StatButton', textOutput('stat_button'))
  })
  
  
  ##################################################################################################################################
  ############### Make the AOI reactive
  make_aoi <- reactive({
    req(input$country_code)
    countrycode <- input$country_code
    
    source("scripts/b0_get_aoi.R",  local=T, echo = TRUE)
    
    v$country <- aoi_shp
    
  })
  
  threshold <- reactive({
    v$threshold <- input$threshold
    input$threshold
  })
  
  ##################################################################################################################################
  ############### DOWNLOAD DATA
  merge_tiles <- eventReactive(input$MergeButton,
                             {
                               req(input$MergeButton)
                               req(input$country_code)
                               req(make_aoi())
                               
                               threshold   <- input$threshold
                               countrycode <- input$country_code
                               
                               source("scripts/b1_download_merge.R",  local=T, echo = TRUE)
                               
                               list.files(gfc_dir)
                             })
  

  ##################################################################################################################################
  ############### DOWNLOAD DATA
  generate_map <- eventReactive(input$MapButton,
                               {
                                 req(input$MapButton)
                                 req(input$country_code)
                                 req(make_aoi())
                                 req(merge_tiles())
                                 
                                 threshold   <- input$threshold
                                 countrycode <- input$country_code
                                 aoi_name    <- paste0(aoi_dir,'GADM_',countrycode)
                                 aoi_shp     <- make_aoi()
                                 aoi_field   <-  "id_aoi"
                                 
                                 source("scripts/b2_make_map_threshold.R",  local=T,echo = TRUE)
                                  
                                 paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")
                               })
  
  ############### Display the results as map
  output$display_res <- renderPlot({
    req(input$DisplayMapButton)
    
    threshold   <- input$threshold
    countrycode <- input$country_code

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
