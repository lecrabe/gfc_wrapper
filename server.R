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
## Last update: 2020/03/12
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
  options(echo = T)
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store aoi_message    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  aoi_vol <- setNames(paste0(normalizePath("~"),"/gfc_wrapper/data/aoi/"),"AOI")
  
  volumes <- c(aoi_vol,
               'Home' = Sys.getenv("HOME"),
               volumes
               )
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  
  ##################################################################################################################################
  ############### GET A REACTIVE VALUE
  v <- reactiveValues(threshold = FALSE,
                      country   = FALSE)
  
  
  ##################################################################################################################################
  ############### Insert the Customized AOI button
  output$aoi_select_custom <- renderUI({
    req(input$aoi_type == "custom")
    
    shinyFilesButton(id = 'aoi_custom_file',
                     label = "Area of interest",  #htmlOutput('t2_b1_button'), TO TRY TO IMPLEMENT
                     title = "Browse", #htmlOutput('select_a_file'),
                     multiple = FALSE)
    
    
    
  })
  
  
  ##################################################################################################################################
  ############### Select input file 
  shinyFileChoose(
    input,
    'aoi_custom_file',
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
  
  
  ##################################################################################################################################
  ############### Insert the GADM AOI button
  output$aoi_select_gadm <- renderUI({
    req(input$aoi_type == "gadm")
    
    selectizeInput(
      'country_code',
      textOutput('text_choice_country'),
      choices = setNames(getData('ISO3')[,1],
                         getData('ISO3')[,2]),
      options = list(
        placeholder = '',#Please select a country from the list below',#htmlOutput('t6_b2_button1_field'),
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  
  
  ##################################################################################################################################
  ############### Insert the leaflet
  output$leafmap <- renderLeaflet({
    req(input$aoi_type == "draw")
    
    leaflet() %>%
      setView(0,0,2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addDrawToolbar(#editOptions = editToolbarOptions(),
        circleOptions = F,
        polylineOptions = F,
        markerOptions = F,
        circleMarkerOptions = F,
        singleFeature = T)
  })
  
  # # Start of Drawing
  # observeEvent(input$leafmap_draw_start, {
  #   print("Start of drawing")
  #   print(input$leafmap_draw_start)
  # })
  # 
  # # Stop of Drawing
  # observeEvent(input$leafmap_draw_stop, {
  #   print("Stopped drawing")
  #   print(input$leafmap_draw_stop)
  # })
  # 
  # # New Feature
  # observeEvent(input$leafmap_draw_new_feature, {
  #   print("New Feature")
  #   print(input$leafmap_draw_new_feature)
  # })
  # 
  # # Edited Features
  # observeEvent(input$leafmap_draw_edited_features, {
  #   print("Edited Features")
  #   print(input$leafmap_draw_edited_features)
  # })
  # 
  # # Deleted features
  # observeEvent(input$leafmap_draw_deleted_features, {
  #   print("Deleted Features")
  #   print(input$leafmap_draw_deleted_features)
  # })
  
  # Listen for draw_all_features which is called anytime features are created/edited/deleted from the map
  observeEvent(input$leafmap_draw_all_features, {
    print("All Features")
    print(input$leafmap_draw_all_features)
    
  })
  
  ##################################################################################################################################
  ############### Store the drawn geometry as reactive
  drawn <- reactive({
    validate(need(input$leafmap_draw_all_features, "Draw area of interest"))
    list <- input$leafmap_draw_all_features
  })
  
  ##################################################################################################################################
  ############### Spatialize the drawn feature
  drawn_geom <- reactive({
    validate(need(input$leafmap_draw_all_features, "Draw area of interest"))
    list <- input$leafmap_draw_all_features
    
    poly_type   <- list$features[[1]]$properties$feature_type
    nb_vertices <- length(list$features[[1]]$geometry$coordinates[[1]])
    
    lp <- list()
    
    poly <- Polygons(list(Polygon(cbind(
      sapply(1:nb_vertices,function(x){list$features[[1]]$geometry$coordinates[[1]][[x]][[1]]}),
      sapply(1:nb_vertices,function(x){list$features[[1]]$geometry$coordinates[[1]][[x]][[2]]})
    )
    )),
    list$features[[1]]$properties$`_leaflet_id`)
    lp <- append(lp,list(poly))
    
    ## Transform the list into a SPDF 
    spdf <- SpatialPolygonsDataFrame(
      SpatialPolygons(lp,1:length(lp)),
      data.frame(list$features[[1]]$properties$`_leaflet_id`),
      match.ID = F
    )
    proj4string(spdf) <- CRS("+init=epsg:4326")
    
    spdf
  })
  
  
  ##################################################################################################################################
  ############### Plot the geometry (check only)
  output$leafmap_drawn <- renderPlot({
    req(input$leafmap_draw_all_features)
    plot(drawn_geom())
  })
  
  
  ##################################################################################################################################
  ############### Print the geometry type (check)
  output$leafmap_message <- renderTable({
    req(drawn())
    list <- drawn()
    poly_type   <- list$features[[1]]$properties$feature_type
    print(poly_type)
  }
  )
  
  ################################# Display the file info
  aoi_info <- reactive({
    req(input$aoi_type)
    
    if(input$aoi_type == "custom"){
      validate(need(input$aoi_custom_file, "Missing input: select the AOI file"))
      df <- parseFilePaths(volumes, input$aoi_custom_file)
      file_info <- as.character(df[, "datapath"])
      nofile <- as.character("No file selected")
      if (is.null(file_info)) {
        cat(nofile)
      } else{
        cat(file_info)
      }
      
    }
    if(input$aoi_type == "gadm"){
      file_info <- input$country_code
    }
    
    if(input$aoi_type == "draw"){
      file_info   <- drawn()$features[[1]]$properties$feature_type
    }
    
    file_info
    
  })
  
  ##################################################################################################################################
  ############### DISPLAY THE AOI FILE PATH
  output$aoi_message_ui <- renderText({
    req(input$aoi_type)
    aoi_info()
  })
  
  
  ##################################################################################################################################
  ############### Insert the START button
  output$ProcessButton <- renderUI({
    req(input$aoi_type)
    actionButton('ProcessButton', textOutput('process_button'))
  })
  
  
  ##################################################################################################################################
  ############### Insert the DISPLAY MAP button
  output$DisplayMapButton <- renderUI({
    req(input$aoi_type)
    actionButton('DisplayMapButton', textOutput('display_map_button'))
  })
  
  
  ##################################################################################################################################
  ############### Insert the STATISTICS button
  output$StatButton <- renderUI({
    req(input$aoi_type)
    actionButton('StatsCalc', textOutput('stat_button'))
  })
  
  ##################################################################################################################################
  ############### GET THE BASENAME
  the_basename <- reactive({
    req(input$aoi_type)
    
    if(input$aoi_type == "custom"){
      aoi_file_path  <- aoi_info()
      base           <- basename(as.character(aoi_file_path))
      the_basename   <- paste0(substr(base,1,nchar(base)-4))
    }
    
    if(input$aoi_type == "gadm"){
      the_basename   <- paste0("aoi_",input$country_code)
    }
    
    if(input$aoi_type == "draw"){
      list <- drawn()
      the_basename <- paste0("aoi_manual_",unlist(list$features[[1]]$properties$`_leaflet_id`))
    }
    
    the_basename
  })
  
  
  ##################################################################################################################################
  ############### Make the AOI reactive
  make_aoi <- reactive({
    req(input$aoi_type)
    req(the_basename())
    
    the_basename <- the_basename()
    
    if(input$aoi_type == "custom"){
      aoi_file_path  <- aoi_info()
      source("scripts/b0_custom_aoi_app.R",  local=T, echo = TRUE)
    }
    
    if(input$aoi_type == "gadm"){
      countrycode <- as.character(input$country_code)
      source("scripts/b0_gadm_aoi_app.R",  local=T, echo = TRUE)
    }
    
    if(input$aoi_type == "draw"){
      drawn_aoi   <- drawn_geom()
      source("scripts/b0_draw_aoi_app.R",  local=T, echo = TRUE)
    }
    
    v$country <- aoi_shp
    
  })
  
  
  ##################################################################################################################################
  ############### DOWNLOAD DATA
  process <- eventReactive(input$StatsCalc,
                           {
                             req(input$StatsCalc)
                             req(input$aoi_type)
                             
                             req(make_aoi())
                             
                             threshold    <- input$threshold
                             the_basename <- the_basename()
                             #progress_file <- progress_file()
                             
                             aoi_name   <- paste0(aoi_dir,the_basename)
                             aoi_shp    <- paste0(aoi_name,".shp")
                             aoi_field <-  "id_aoi"
                             
                             aoi <- readOGR(make_aoi())
                             (bb    <- extent(aoi))
                             
                             #system(paste0('echo "Preparing data..." > ', progress_file))
                             
                             
                             withProgress(message = paste0('Downloading GFC data'),
                                          value = 0,
                                          {if(!file.exists(paste0(gfc_dir,"gfc_",the_basename,"_",types[4],".tif"))){
                                            source("scripts/b1_download_merge.R",  local=T, echo = TRUE)
                                          }
                                          })
                             
                             
                             
                             withProgress(message = paste0('Combine layers into a map'),
                                          value = 0,{
                                            if(!file.exists(paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct.tif"))){
                                              source("scripts/b2_make_map_threshold.R",  local=T,echo = TRUE)
                                              
                                            }
                                          })
                             
                             
                             withProgress(message = paste0('Compute statistics'),
                                          value = 0,{
                                            if(!file.exists(paste0(stt_dir,"stats_",the_basename,"_",threshold,".txt"))){
                                              source("scripts/b3_compute_areas.R",  local=T,echo = TRUE)
                                            }
                                          })
                             
                             
                             withProgress(message = paste0('Generate fragmentation mask'),
                                          value = 0,{
                                            if(!file.exists(paste0(gfc_dir,"mask_mspa_gfc_",the_basename,"_",threshold,".tif"))){
                                              source("scripts/b4_make_mspa_ready_mask.R",  local=T,echo = TRUE)
                                            }
                                          })
                             
                             list.files(gfc_dir)
                           })
  
  ############# Create the raw statistics table
  raw_stats <- reactive({
    req(input$StatsCalc)
    req(process())
    threshold   <- input$threshold
    the_basename <- the_basename()
    
    print('Check: Display the stats')
    
    t <- read.table(paste0(stt_dir,"stats_",the_basename,"_",threshold,".txt"))
    
    names(t) <- c("class","pixel","area")
    
    t
  })
  
  
  ############# Create the statistics table
  stats <- reactive({
    req(raw_stats())
    
    t <- raw_stats()
    
    tt <- t[t$class > 20,]
    tt <- rbind(tt,colSums(t[t$class %in% 1:20,]))
    tt[nrow(tt),1] <- 45
    
    codes <- data.frame(cbind(c(30,40,45,50,51),
                              c("Forest","Non-Forest","Loss","Gain","Gain-Loss")))
    names(codes) <- c("class","class_name")
    
    ttt <- merge(tt,codes,by.x="class",by.y="class",all.x=T)
    ttt <- arrange(ttt,class)
    
    tttt <- ttt[,c("class_name","area")]
    names(tttt) <- c("Class","Area (ha)")
    
    tttt
    
  })
  
  
  
  ############### Display the STATS
  output$display_stats <- renderTable({
    req(stats())
    stats()
  })
  
  ############### Display the STATS
  output$display_loss_graph <- renderPlot({
    req(raw_stats())
    t <- raw_stats()
    tt <- t[t$class < 20 & t$class > 0,]
    tt$year <- 2000+tt$class
    barplot(area ~ year,tt,xlab = "Year",ylab ="Tree cover loss (Ha)")
  })
  
  ############### Display the results as map
  output$display_res <- renderPlot({
    req(input$DisplayMapButton)
    
    threshold   <- input$threshold
    the_basename <- the_basename()
    
    print('Check: Display the map')
    
    gfc <- raster(paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct.tif"))
    aoi <- spTransform(readOGR(make_aoi()),proj4string(gfc))
    
    plot(gfc)
    plot(aoi,add=T,border="yellow")
    
  })
  
  ##################################################################################################################################
  ############### Button to download the stats file (csv)
  output$ui_download_stats <- renderUI({
    req(stats())
    downloadButton('download_stats',
                   label = textOutput('download_csv_button'))
  })
  
  ##################################################################################################################################
  ############### Enable to download the stats (csv)
  output$download_stats <- downloadHandler(
    filename = function() {
      paste0("stats_",the_basename(), ".csv")
    },
    content  = function(xx) {
      to_export <- raw_stats()
      write.csv(to_export, xx, row.names = FALSE)
    }
  )

  
  ##################################################################################################################################
  ############### Button to download the tif file
  output$ui_download_gfc_map <- renderUI({
    req(process())
    req(input$DisplayMapButton)
    downloadButton('download_gfc_map',
                   label = textOutput('download_map_button'))
  })
  ##################################################################################################################################
  ############### Enable to download the map (.tif)
  output$download_gfc_map <- downloadHandler(
    filename = function() {
      paste0(the_basename(), ".tif")
    },
    content  = function(xx) {
      to_export <- raster(paste0(gfc_dir,"gfc_",the_basename(),"_",input$threshold,"_map_clip_pct_geo.tif"))
      writeRaster(to_export, xx)
    }
  )
  
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
