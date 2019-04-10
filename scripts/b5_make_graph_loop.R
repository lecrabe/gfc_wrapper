rel <- data.frame(cbind(out$sampling,out$intensity,out$total,out_sd$total))
names(rel)  <- c("sampling","intensity","mean","sd")

rel$diff <- rel$mean-rel[rel$sampling == 0,"mean"]


the_plot <- ggplot(rel,aes(x=intensity,
                               #log(intensity,10),
                               y=diff))+
  geom_line()+
  #ylim(-0.0000001,.0000001)+
  
  labs(list(
    title=countrycode,
    x="Sampling intensity",
    y="Est/Target ratio")) +
  
  geom_errorbar(aes(ymin=diff-sd, ymax=diff+sd), colour="black", width=.1) 

scale <- scale_x_continuous(
  breaks = c(0,1,10,100,1000,10000,100000,1000000,1e+8),
  labels = c(0,1,10,100,1000,10000,100000,1000000,1e+8),
  limits = c(1,1e+8),
  trans  = "log10")


  
the_plot + scale
