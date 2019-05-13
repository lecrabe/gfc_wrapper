plot(bb)
plot(aoi,add=T)

xs <- floor(bb@xmin):ceiling(bb@xmax)
ys <- floor(bb@ymin):ceiling(bb@ymax)

rooturl <- "https://download.geoservice.dlr.de/FNF50/files/"

for(y in ys){
  
  lat <- paste0(gsub("0","N",gsub("1","N",gsub(pattern = "-1","S",sign(y)))),sprintf("%02d", abs(y)))
  
  for(x in xs){
    
    lon <- paste0(gsub("0","E",gsub("1","E",gsub(pattern = "-1","W",sign(x)))),sprintf("%03d", abs(x)))
    LON_dir <- paste0(gsub("0","E",gsub("1","E",gsub(pattern = "-1","W",sign(x)))),sprintf("%03d",floor(abs(x)/10)*10),"/")
    
    url <- paste0(rooturl,lat,"/",LON_dir,"TDM_FNF_20_",lat,lon,".zip")
    zip <- paste0(tan_dir,"TDM_FNF_20_",lat,lon,".zip")
    
    if(!file.exists(paste0(tan_dir,"TDM_FNF_20_",lat,lon,".tiff"))){
    
    system(sprintf("wget -O %s %s --auth-no-challenge",
                   zip,
                   url))
    
    system(sprintf("unzip -o %s -d %s",
                   zip,
                   paste0(tan_dir,"tmp")))
    
    system(sprintf("mv %s %s",
                   paste0(tan_dir,"tmp/FNF/","TDM_FNF_20_",lat,lon,".tiff"),
                   paste0(tan_dir,"TDM_FNF_20_",lat,lon,".tiff")
                   ))
    
    system(sprintf("rm -f -r %s",
                   paste0(tan_dir,"tmp/*")
    ))
    
    system(sprintf("rm -f -r %s",
                   zip
    ))
    }
  }
}

#############################################################
### MERGE TILES
# system(sprintf("gdal_merge.py -v -co COMPRESS=LZW -o %s  %s",
#                paste0(tan_dir,"tmp_tdm_",countrycode,".tif"),
#                paste0(tan_dir,"TDM_FNF_20_","*.tiff")
# ))

system(sprintf("gdalbuildvrt %s  %s",
               paste0(tan_dir,"tmp_tdm_",countrycode,".vrt"),
               paste0(tan_dir,"TDM_FNF_20_","*.tiff")
))

#############################################################
### CROP TO COUNTRY BOUNDARIES
system(sprintf("python %s/oft-cutline_crop.py -v %s -i %s -o %s -a %s",
               scriptdir,
               aoi_shp,
               paste0(tan_dir,"tmp_tdm_",countrycode,".vrt"),
               paste0(tan_dir,"tmp_tdm_clip_",countrycode,".tif"),
               aoi_field
))

################################################################################
#################### Add pseudo color table to result
################################################################################
system(sprintf("(echo %s) | oft-addpct.py %s %s",
               paste0(gfc_dir,"color_table_tandem.txt"),
               paste0(tan_dir,"tmp_tdm_clip_",countrycode,".tif"),
               paste0(tan_dir,"tmp_tdm_clip_pct_",countrycode,".tif")
))

################################################################################
#################### COMPRESS
################################################################################
system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
               paste0(tan_dir,"tmp_tdm_clip_pct_",countrycode,".tif"),
               paste0(tan_dir,"fnf_tandem_",countrycode,".tif")
))

#################### DISPLAY
plot(raster(paste0(tan_dir,"fnf_tandem_",countrycode,".tif")))


#################### CLEAN
system(sprintf("rm -f -r %s",
               paste0(tan_dir,"TDM_*.tiff")
))

#################### CLEAN
system(sprintf("rm -f -r %s",
               paste0(tan_dir,"tmp_*.*")
))
