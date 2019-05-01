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
