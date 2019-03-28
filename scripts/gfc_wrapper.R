countrycode <- aoi_list[1]

## Download data for CAFI countries 
for(countrycode in aoi_list){
  
  if(!file.exists(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"))){
    
    
    aoi   <- getData('GADM',
                     path=gfcdwn_dir,
                     country= countrycode,
                     level=0)

    aoi <- spTransform(aoi,CRS('+init=epsg:4326'))
    (bb    <- extent(aoi))

    aoi_name   <- paste0(aoi_dir,'GADM_',countrycode)
    aoi_shp    <- paste0(aoi_name,".shp")
    aoi_field <-  "id_aoi"
    aoi@data[,aoi_field] <- row(aoi)[,1]

    writeOGR(obj = aoi,
             dsn = aoi_shp,
             layer = aoi_name,
             driver = "ESRI Shapefile",
             overwrite_layer = T)

    tiles <- calc_gfc_tiles(aoi)

    proj4string(tiles) <- proj4string(aoi)

    tiles <- tiles[aoi,]

    download_tiles(tiles,
                   gfcdwn_dir,
                   images = types)

    ### Find the suffix of the associated GFC data for each tile
    tmp         <- data.frame(1:length(tiles),rep("nd",length(tiles)))
    names(tmp)  <- c("tile_id","gfc_suffix")

    for (n in 1:length(tiles)) {
      gfc_tile <- tiles[n, ]
      min_x <- bbox(gfc_tile)[1, 1]
      max_y <- bbox(gfc_tile)[2, 2]
      if (min_x < 0) {min_x <- paste0(sprintf("%03i", abs(min_x)), "W")}
      else {min_x <- paste0(sprintf("%03i", min_x), "E")}
      if (max_y < 0) {max_y <- paste0(sprintf("%02i", abs(max_y)), "S")}
      else {max_y <- paste0(sprintf("%02i", max_y), "N")}
      tmp[n,2] <- paste0("_", max_y, "_", min_x, ".tif")
    }

    ### Store the information into a SpatialPolygonDF
    df_tiles <- SpatialPolygonsDataFrame(tiles,tmp,match.ID = F)
    rm(tmp)

    prefix <- "Hansen_GFC-2018-v1.6"
    suffix <- df_tiles@data$gfc_suffix
    tilesx <- substr(suffix,2,nchar(suffix)-4)

    ### MERGE THE TILES TOGETHER, FOR EACH LAYER SEPARATELY and CLIP TO THE BOUNDING BOX OF THE COUNTRY
    for(type in types){
      print(type)

      to_merge <- paste0(prefix,"_",
                         type,"_",
                         tilesx,
                         ".tif")

      if(!file.exists(paste0(gfc_dir,"gfc_",countrycode,"_",type,".tif"))){


        system(sprintf("gdalbuildvrt -te %s %s %s %s %s %s",
                       floor(bb@xmin),
                       floor(bb@ymin),
                       ceiling(bb@xmax),
                       ceiling(bb@ymax),
                       paste0(tmp_dir,"tmp_merge_",type,".vrt"),
                       paste0(gfcdwn_dir,to_merge,collapse = " ")
        ))

        system(sprintf("gdal_translate -ot Byte -projwin %s %s %s %s -co COMPRESS=LZW %s %s",
                       floor(bb@xmin),
                       ceiling(bb@ymax),
                       ceiling(bb@xmax),
                       floor(bb@ymin),
                       paste0(tmp_dir,"tmp_merge_",type,".vrt"),
                       paste0(gfc_dir,"gfc_",countrycode,"_",type,".tif")
        ))

        print(to_merge)
      } #### END OF EXISTS MERGE

    } #### END OF MERGE TILES BY TYPE


    #################### COMBINATION INTO NATIONAL SCALE MAP
    system(sprintf("gdal_calc.py -A %s -B %s -C %s -D %s --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
                   paste0(gfc_dir,"gfc_",countrycode,"_",types[1],".tif"),
                   paste0(gfc_dir,"gfc_",countrycode,"_",types[2],".tif"),
                   paste0(gfc_dir,"gfc_",countrycode,"_",types[3],".tif"),
                   paste0(gfc_dir,"gfc_",countrycode,"_",types[4],".tif"),
                   paste0(tmp_dir,"tmp_gfc_map_",countrycode,".tif"),

                   paste0("(A<=",threshold,")*((C==1)*50 + (C==0)*30)+", ### NON FOREST
                          "(A>", threshold,")*",
                          "((C==1)*(",
                          "(B>0)*  51 +",           ### GAIN+LOSS
                          "(B==0)* 50 )+",          ### GAIN
                          "(C==0)*(",
                          "(B>0)*B+",               ### LOSS
                          "(B==0)* 40 ))"           ### FOREST STABLE
                   )
    ))

    #############################################################
    ### CROP TO COUNTRY BOUNDARIES
    system(sprintf("python %s/oft-cutline_crop.py -v %s -i %s -o %s -a %s",
                   scriptdir,
                   aoi_shp,
                   paste0(tmp_dir,"tmp_gfc_map_",countrycode,".tif"),
                   paste0(tmp_dir,"tmp_gfc_map_clip_",countrycode,".tif"),
                   aoi_field
    ))

    ################################################################################
    #################### REPROJECT IN EA PROJECTION
    ################################################################################
    system(sprintf("gdalwarp -t_srs \"%s\" -overwrite -ot Byte -multi -co COMPRESS=LZW %s %s",
                   proj,
                   paste0(tmp_dir,"tmp_gfc_map_clip_",countrycode,".tif"),
                   paste0(tmp_dir,"tmp_gfc_map_clip_prj_",countrycode,".tif")
    ))


    ################################################################################
    #################### Add pseudo color table to result
    ################################################################################
    system(sprintf("(echo %s) | oft-addpct.py %s %s",
                   paste0(gfc_dir,"color_table.txt"),
                   paste0(tmp_dir,"tmp_gfc_map_clip_prj_",countrycode,".tif"),
                   paste0(tmp_dir,"tmp_gfc_map_clip_prj_pct",countrycode,".tif")
    ))

    ################################################################################
    #################### COMPRESS
    ################################################################################
    system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
                   paste0(tmp_dir,"tmp_gfc_map_clip_prj_pct",countrycode,".tif"),
                   paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")
    ))
    
    ###############################################################################
    ################### COMPUTE AREAS
    ###############################################################################
    hist <- pixel_count(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"))
    write.table(hist,paste0(stt_dir,"stats_",countrycode,"_",threshold,".txt"),row.names = F,col.names = F)
    
    } #### END OF FILE EXISTS
  
}#### END OF COUNTRY

############################################################
system(sprintf("rm -f %s",
               paste0(tmp_dir,"tmp*")))


for(countrycode in aoi_list){
  
  png(file= paste0(stt_dir,countrycode,"_",threshold,"loss_area.png") ,
      width= 1000,
      height=400)
  
  df  <- pixel_count(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"))
  pix <-  res(raster(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")))[1]
  
  df1 <- merge(df,codes,by.x="V1",by.y="my_classes",all.x=T)[,c("my_labels","V1","buckets")]
  names(df1) <- c("classes","codes","pixels")
  df1$area_ha <- df1$pixels*pix*pix/10000000
  df1$rate    <- round(df1$area_ha / (df1[df1$classes == "forest","area_ha"] + sum(df1[grep("loss_",df1$classes),]$area_ha)) * 100,2)
  
  loss <- df1[grep("loss_",df1$classes),]
 
  barplot(loss$area_ha,
          names.arg=substr(loss$classes,6,9),
          ylab="Area (1000 ha)",
          # ylim=c(0,1500),
          main=paste0("Annual loss ",countrycode))
  
  # barplot(loss$rate,
  #         names.arg=substr(loss$classes,6,9),
  #         ylab="%",
  #         ylim=c(0,0.8),
  #         main=paste0("Taux ",countrycode))
  
  dev.off()
}

