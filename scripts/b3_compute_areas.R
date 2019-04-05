###############################################################################
################### COMPUTE AREAS
###############################################################################
hist <- pixel_count(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif"))
write.table(hist,paste0(stt_dir,"stats_",countrycode,"_",threshold,".txt"),row.names = F,col.names = F)
names(hist) <- c("code","pixels")

pixel     <- res(raster(paste0(gfc_dir,"gfc_",countrycode,"_",threshold,"_map_clip_pct.tif")))[1]

tcov_area <- sum(hist[hist$code == 40 | (hist$code > 0 & hist$code < 30),"pixels"])*pixel*pixel/10000
loss_area <- sum(hist[(hist$code > 0 & hist$code < 30),"pixels"]*pixel*pixel/10000)
