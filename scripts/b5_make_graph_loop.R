rel <- data.frame(cbind(out_av$sampling,out_av$intensity,out_av$total,out_sd$total))

names(rel)  <- c("sampling","intensity","mean","sd")

rel$diff <- rel$mean-rel[rel$sampling == 0,"mean"]


the_plot <- ggplot(rel,aes(x=intensity,
                           y=diff /1000))+
  
  geom_line()+
  
  labs(
    title=countrycode,
    x="Sampling intensity (points in forest)",
    y="Difference (1000 ha)") +
  
  geom_errorbar(aes(ymin=(diff-sd)/1000, ymax=(diff+sd)/1000), colour="black", width=.1) 

scale <- scale_x_continuous(
  breaks = c(0,1,10,100,1000,10000,100000,1000000,1e+8),
  labels = c(0,1,10,100,1000,10000,100000,1000000,1e+8),
  limits = c(1,1e+8),
  trans  = "log10")

the_plot + scale
