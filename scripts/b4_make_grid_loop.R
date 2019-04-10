nb_iter <- 100
i <- 1

master <- data.frame(matrix(nrow = 0,ncol=23))
names(master) <- c(paste("year",2000+(1:max_year),sep="_"),
                   "total","intensity","sampling","iter","offset")

aoi_proj <- spTransform(aoi,proj)

for(rand_off in runif(nb_iter,0,1)){
  ###############################################################################
  ################### GET THE BASE GRID
  ###############################################################################
  samplepoints              <- SpatialPoints(spsample(aoi_proj,
                                                      type="regular",
                                                      cellsize=spacing,
                                                      #offset=c(0,0)
                                                      offset = c(rand_off,rand_off)
  ))
  

  proj4string(samplepoints) <- proj4string(aoi_proj)
  pts                       <- samplepoints[aoi_proj,]
  
  spdf         <- SpatialPointsDataFrame(coords = pts@coords,
                                         data   = data.frame(pts),
                                         proj4string = CRS(proj4string(pts)))
  
  names(spdf)  <- c("xcoord","ycoord")
  
  spdf$code    <- extract(raster(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")),
                          spdf)
  df <- spdf@data
  
  ################# Create a new dataset containing all levels of longitude
  lon_fact <- data.frame(
    cbind(
      levels(as.factor(df$xcoord)),
      1:length(levels(as.factor(df$xcoord)))
    )
  )
  
  ################# Create a new dataset containing all levels of latitude
  lat_fact <- data.frame(
    cbind(
      levels(as.factor(df$ycoord)),
      1:length(levels(as.factor(df$ycoord)))
    )
  )
  
  ################# Add both columns to df
  df<-merge(df,lon_fact,by.x = "xcoord",by.y='X1')
  df<-merge(df,lat_fact,by.x = "ycoord",by.y='X1')
  
  names(df)<-c("latitude",
               "longitude",
               "code",
               "lon_fact",
               "lat_fact")
  
  df$lon_fact <- as.numeric(df$lon_fact)
  df$lat_fact <- as.numeric(df$lat_fact)
  
  
  out <- data.frame(
    sapply(1:max_year,function(y){
      sapply(classes,function(x){
        estimate(x,y)*tcov_area
      })
    })
  )
  
  ################# Apply function estimate to cumulated years for all sub-sampling
  out$all_years <- sapply(classes,function(x){
    all_estimate(x)*tcov_area
  })
  
  ################# Change names
  names(out)<-c(paste("year",2000+(1:max_year),sep="_"),"total")
  
  ################# Add a column with number of samples corresponding to each level
  out$intensity <- sapply(classes,function(x){nombre(x)})
  out$sampling  <- classes
  
  ################# Create, to the same format, a line corresponding to target areas of loss
  ################# The before-last number corresponds to column "intensity", with the number of pixels used
  out[length(classes)+1,] <- c(hist[(hist$code > 0 & hist$code < 30),"pixels"]*pixel*pixel/10000,
                               loss_area,
                               sum(hist[(hist$code == 40 | (hist$code > 0 & hist$code < 30)),"pixels"]),
                               0)
  out$iter <- i
  out$offset <- rand_off
  i <- i+1
  
  master <- rbind(master,out)
}

out    <- data.frame(sapply(c(paste("year",2000+(1:max_year),sep="_"),"total","intensity"),
                            function(x){tapply(master[,x],master$sampling,mean)}))

out_sd <- data.frame(sapply(c(paste("year",2000+(1:max_year),sep="_"),"total","intensity"),
                            function(x){tapply(master[,x],master$sampling,sd)}))

out$sampling   <- as.numeric(row.names(out))
out_sd$sampling <- as.numeric(row.names(out_sd))

write.csv(master,paste0(stt_dir,"iter_",countrycode,"_100_master.csv"),row.names = F)
write.csv(out,paste0(stt_dir,"iter_",countrycode,"_100_mean.csv"),row.names = F)
write.csv(out_sd,paste0(stt_dir,"iter_",countrycode,"_100_sd.csv"),row.names = F)
