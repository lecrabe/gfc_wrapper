aoi   <- getData('GADM',
                 path=gfcdwn_dir,
                 country= countrycode,
                 level=0)

print(countrycode)

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