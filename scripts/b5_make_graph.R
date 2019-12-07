rel <- out

for(the_col in c(paste("year",2000+(1:max_year),sep="_"),"total")){
  rel[,the_col] <- out[,the_col] - out[nrow(out),the_col]
}

out_col  <- melt(rel,"intensity")

the_plot <- ggplot(out_col,aes(x=intensity,
                               #log(intensity,10),
                               y=value,
                               group=variable))+
  geom_line(aes(colour=variable))+
  #ylim(-0.0000001,.0000001)+
  
  labs(list(
    title=countrycode,
    x="Sampling intensity",
    y="Est/Target ratio"))

scale <- scale_x_continuous(
  breaks = c(0,1,10,100,1000,10000,100000,1000000,1e+11),
  labels = c(0,1,10,100,1000,10000,100000,1000000,1e+11),
  limits = c(1,1e+13),
  trans  = "log10")

the_plot + scale
