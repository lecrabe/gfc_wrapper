###############################################################################
################### COMPUTE AREAS
###############################################################################
#map <- paste0(gfc_dir,"glad_check_",countrycode,"_",threshold,".tif")
map <- paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")

hist <- pixel_count(map)
names(hist) <- c("code","pixels")

pixel     <- res(raster(map))[1]
hist$area <- hist$pixels*pixel*pixel/10000

write.table(hist,paste0(stt_dir,"stats_",countrycode,"_",threshold,".txt"),row.names = F,col.names = F)

tcov_area <- sum(hist[hist$code == 40 | (hist$code > 0 & hist$code < 30),"area"])
loss_area <- sum(hist[(hist$code > 0 & hist$code < 30),"area"])

###############################################################################
################### REPROJECT ZONES IN EA PROJECTION
###############################################################################
system(sprintf("ogr2ogr -t_srs \"%s\" %s %s",
               proj,
               paste0(tmp_dir,"aoi_shp_proj_",countrycode,".shp"),
               aoi_shp
))

#############################################################
### compute stats by zone
system(sprintf("python %s/oft-zonal_large_list.py -um %s -i %s -o %s -a %s",
               scriptdir,
               paste0(tmp_dir,"aoi_shp_proj_",countrycode,".shp"),
               map,
               paste0(tmp_dir,"tmp_stats_gfc_map_clip_",countrycode,".txt"),
               aoi_field
))

codes     <- aoi@data

df        <- read.table(paste0(tmp_dir,"tmp_stats_gfc_map_clip_",countrycode,".txt"))
df        <- df[,colSums(df) != 0]
names(df) <- c("zone_id","total","no_data",paste0("loss_",1:max_year),"non_forest","forest","gain","gain_loss") 

df$zones <- codes$ID

d0 <- df[,c("zones","zone_id")]
d0[,2+1:(max_year+6)] <- df[,1+1:(max_year+6)]*pixel*pixel/10000
write.csv(d0,paste0(stt_dir,"stats_gfc_map_clip_",countrycode,".csv"),row.names = F)

#############################################################
### RASTERIZE ZONES
system(sprintf("python %s/oft-rasterize_attr.py -v %s -i %s -o %s -a %s",
               scriptdir,
               paste0(tmp_dir,"aoi_shp_proj_",countrycode,".shp"),
               map,
               paste0(tmp_dir,"aoi_shp_proj_",countrycode,".tif"),
               aoi_field
))

################################################################################
#################### CLUMP
################################################################################
system(sprintf("oft-clump %s %s",
               paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"),
               paste0(gfc_dir,"tmp_clump_gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")
))


################################################################################
#################### MAP HISTOGRAM ON CLUMPS
################################################################################
system(sprintf("oft-stat -um %s -i %s -o %s",
               paste0(gfc_dir,"tmp_clump_gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"),
               paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"),
               paste0(stt_dir,"clump_stats_gfc_",countrycode,"_",threshold,".txt")
))

################################################################################
#################### ZONE HISTOGRAM ON CLUMPS
################################################################################
system(sprintf("oft-his -um %s -i %s -o %s -maxval %s",
               paste0(gfc_dir,"tmp_clump_gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"),
               paste0(tmp_dir,"aoi_shp_proj_",countrycode,".tif"),
               paste0(stt_dir,"clump_stats_aoi_shp.txt"),
               13
))

codes     <- aoi@data

dz <- read.table(paste0(stt_dir,"clump_stats_aoi_shp.txt"))
dm <- read.table(paste0(stt_dir,"clump_stats_gfc_",countrycode,"_",threshold,".txt"))[,1:3]

dz <- arrange(dz,V1)
dm <- arrange(dm,V1)

summary(dm$V1-dz$V1)

df <- cbind(dm,dz[3:16])
names(df) <- c("patch_id","patch_size","class","nodata",codes$ID)
head(df)

dl0012 <- df[df$class %in% 1:12 ,]
dl1216 <- df[df$class %in% 13:16,]
hist(dl1216$patch_size)
nrow(dl1216[dl1216$patch_size <6,])
nrow(dl0012[dl0012$patch_size <6,])

write.csv(df,paste0(stt_dir,"stats_clump_",countrycode,".csv"),row.names = F)
