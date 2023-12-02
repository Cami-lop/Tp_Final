mapa<-ggplot(data=febrero_porcentaje_df,aes(Longitudes,Latitudes))
mapa<-mapa+coord_quickmap(xlim=c(-70,-55),ylim=c(-45,-30),expand=F)
mapa<-mapa+geom_contour_fill(aes(z=Porcentaje))
mapa<-mapa+theme_bw() +theme(strip.background=element_rect(fill="grey92"), plot.subtitle=element_text(hjust= 0.5,size=10), axis.text =element_text(size=7,colour ="black"), axis.title =element_text(size=8))
mapa<-mapa+borders(colour= "grey2",size=1) +
  labs(x = "Longitudes",
       y = "Latitudes",
       fill = "Porcentaje de lluvia",
       title = "Probabilidad de lluvia en Buenos Aires ",
       subtitle = "Periodo de datos de 1996 a 2023",
       caption="Elaborado por Camila Lopez"
  )
mapa