###############################################################################
################### COMPUTE AREAS
###############################################################################
map <- paste0(gfc_dir,"glad_check_",countrycode,"_",threshold,".tif")

hist <- pixel_count(map)
names(hist) <- c("code","pixels")

pixel     <- res(raster(map))[1]
hist$area <- hist$pixels*pixel*pixel/10000

df <- merge(hist,legend,by.x="code",by.y="code",all.x=T)

df <- df[df$code != 0,]

df$percent <- round(df$area/sum(df$area)*100,2)

df <- df[,c("code","fnf_gfc_2000","fnf_glad_2010","chg_gfc","agree","pixels","area","percent")]
tapply(df$percent,df$agree,sum)

write.csv(df,paste0(stt_dir,"stats_",countrycode,"_",threshold,".csv"),row.names = F)


