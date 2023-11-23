library(metR)
require(ggplot2)
p<-ggplot(data=febrero_porcentaje_df,aes(Longitudes,Latitudes))
p<-p+borders(colour= "grey22",size=0.3) +ylab("Latitudes")+ xlab("Longitudes")
p<-p+coord_quickmap(xlim=c(-65,-57),ylim=c(-40,-33),expand=F)
p<-p+geom_point(data=febrero_porcentaje_df,aes(x=Longitudes,y=Latitudes,color=Porcentaje),size=3)
p<-p+theme_bw() +theme(strip.background=element_rect(fill="grey92"), plot.subtitle=element_text(hjust= 0.5,size=10), axis.text =element_text(size=7,colour ="black"), axis.title =element_text(size=8))
p

#me muestar la prov de bs as me faltan los datos