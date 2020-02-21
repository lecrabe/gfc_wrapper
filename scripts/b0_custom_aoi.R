setwd(aoi_dir)

aoi <- readOGR(paste0(aoi_dir,"cocoa_project_ghana_dissolved.shp"))
(bb    <- extent(aoi))
names(aoi)
aoi_name   <- paste0(aoi_dir,"zone_ghana")
aoi_shp    <- paste0(aoi_name,".shp")
aoi_field <-  "id_aoi"
aoi@data[,aoi_field] <- row(aoi)[,1]

writeOGR(obj = aoi,
         dsn = aoi_shp,
         layer = aoi_name,
         driver = "ESRI Shapefile",
         overwrite_layer = T)
