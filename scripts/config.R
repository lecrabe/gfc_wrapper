
root <- "~" 
setwd(root)
root       <- paste0(getwd(),'/')
gfcdwn_dir <- paste0(root,"downloads/gfc/2018/")
rootdir     <- paste0(root,"gfc_wrapper/")
scriptdir   <- paste0(rootdir,"scripts/")
data_dir    <- paste0(rootdir,"data/")
tmp_dir     <- paste0(rootdir,"tmp/")
gfc_dir       <- paste0(data_dir,"gfc/")
aoi_dir       <- paste0(data_dir,"aoi/")
stt_dir       <- paste0(data_dir,"stat/")

dir.create(scriptdir,showWarnings = F)
dir.create(gfcdwn_dir,recursive=T,showWarnings = F)
dir.create(data_dir,showWarnings = F)
dir.create(gfc_dir,showWarnings = F)
dir.create(aoi_dir,showWarnings = F)
dir.create(stt_dir,showWarnings = F)
dir.create(tmp_dir,showWarnings = F)

#################### load packages
source(paste0(scriptdir,'packages.R'))

#################### load parameters
source(paste0(scriptdir,'parameters.R'))

## Get List of Countries 
(gadm_list  <- data.frame(getData('ISO3')))

#################### CREATE A COLOR TABLE FOR THE OUTPUT MAP
my_classes <- c(0,1:max_year,30,40,50,51)
my_labels  <- c("no data",paste0("loss_",2000+1:max_year),"non forest","forest","gains","gains+loss")
codes <- data.frame(cbind(my_labels,my_classes))

loss_col <- colorRampPalette(c("yellow", "darkred"))
nonf_col <- "lightgrey"
fore_col <- "darkgreen"
gain_col <- "lightgreen"
ndat_col <- "black"
gnls_col <- "purple"

my_colors  <- col2rgb(c(ndat_col,
                        loss_col(max_year),
                        nonf_col,
                        fore_col,
                        gain_col,
                        gnls_col))

pct <- data.frame(cbind(my_classes,
                        my_colors[1,],
                        my_colors[2,],
                        my_colors[3,]))

write.table(pct,paste0(gfc_dir,"color_table.txt"),row.names = F,col.names = F,quote = F)

types       <- c("treecover2000","lossyear","gain","datamask")

pixel_count <- function(x){
  info    <- gdalinfo(x,hist=T)
  buckets <- unlist(str_split(info[grep("bucket",info)+1]," "))
  buckets <- as.numeric(buckets[!(buckets == "")])
  hist    <- data.frame(cbind(0:(length(buckets)-1),buckets))
  hist    <- hist[hist[,2]>0,]
}
