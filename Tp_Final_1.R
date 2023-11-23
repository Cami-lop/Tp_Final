#Tp Final Camila Lopez 
#Abro el archivo ncdf 
getwd()
setwd("C:/Users/Fernando Cabrera/Desktop/Cami_Labo/Tp_Final/")
#setwd("C:/Users/camil/OneDrive/Escritorio/Cami_Labo/Tp_Final/")
require(ncdf4)  #llamo a la libreria que voy a necesitar 
#abro el archivo
archivo<-nc_open("C:/Users/Fernando Cabrera/Desktop/Cami_Labo/Tp_Final/daily_data_buenos_aires_province_1993-2023.nc")
archivo
# 4 dimen - variable lwe_precipitation_rate
pp<-ncvar_get(archivo,varid="precip") #array de dime 9 lon/8 lat / 9892 dias 
pp[which(pp == -9999)]<-NA # veo el datos faltante en la informacion del archivo y lo reemplazo con NA
#1- tiempo cada dia desde 1970-01-01 00:00:00 0:00
tiempo<-ncvar_get(archivo,varid="time")
#2-longitud 
lon<-ncvar_get(archivo,varid="longitude")
#3-latitud
lat<-ncvar_get(archivo,varid="latitude")

#Lista con las 10 ciudades 
ciudades<-list()
datos_ciudades<-read.table("Ciudades.txt") #lee los datos del txt

for (i in  1:10){
  ciudad<-list("Nombre"= datos_ciudades[i,1], #preguntar si me conviene sacarlo
                 "Latitud"=as.numeric(datos_ciudades[i,2]), #preguntar lo de hacerlo numerico
                 "Longitud"=(360+(as.numeric(datos_ciudades[i,3])))
  )
 ciudades[[i]]<-ciudad 
}

names(ciudades)<-list("Bella Vista","Ezeiza","La Plata","Pergamino","San Nicolas","Bahia Blanca","Ayacucho","Monte Chingolo","Magdalena","Pinamar")
#Calculos los puntos cercanos a cada ciudad
puntos_cercanos <- list()
lon_cerca <- c()
lat_cerca <- c()
for (i in 1:10) {
  pos_cercana <- which.min(abs(lon - ciudades[[i]][[3]]))
  pas_cercana <- which.min(abs(lat - ciudades[[i]][[2]]))
  
  lon_cerca[i] <- lon[pos_cercana]
  lat_cerca[i] <- lat[pas_cercana]
  
  puntos_cercanos[[i]] <- list("Ciudad" = datos_ciudades[i, 1],
                               "Latitud" = lat_cerca[i],
                               "Longitud" = lon_cerca[i])
}
names(puntos_cercanos)<-list("Bella Vista","Ezeiza","La Plata","Pergamino","San Nicolas","Bahia Blanca","Ayacucho","Monte Chingolo","Magdalena","Pinamar")
#lo convierto en un dataframe 
puntos_df<-data.frame()
for (i in 1:10) {
  fila<-data.frame("Nombre"=c(puntos_cercanos[[i]][[1]]),
                        "Longitudes"=c(puntos_cercanos[[i]][[3]]),
                        "Latitudes"=c(puntos_cercanos[[i]][[2]]))
  puntos_df<-rbind(puntos_df,fila)
}
#guardo el archivo 
write.table(puntos_df, "dataciudades_pr.txt", sep = "\t", row.names = FALSE)
###############################################################################
require(lubridate) #llamo a las librerias que voy a necesitar 
tiempos_leg<- as.Date(tiempo,origin="1970-01-01 00:00:00 ")
head(tiempos_leg) #desde el 01/10/96 // se repite
tail(tiempos_leg) #hasta el 31/07/23 #son 26 a?os y  meses
#como se repite borro los datos que no quiero
n<-seq(2,184,2) #posiciones repetidas 
pp2<-pp
pp2<-pp[,,-n]
tiempo_legibles<-tiempos_leg[-n]
#array donde guardo mi informacion
media<-array(data=NA,dim=c(9,8,366))
desvio<-array(data=NA, dim=c(9,8,366))

dias<-seq(ymd("2020-01-01"),ymd("2020-12-31"),1)
dias_num<-1:366
d<-format(tiempo_legibles,"%m %d")

for(dia in dias_num){
  media[,,dia]<-apply(pp2[,,which(d==format(dias[dia],"%m %d"))],c(1,2),FUN=mean,na.rm =T ) #estan prdenadas desde el 01/01 al 31/12
  desvio[,,dia]<-apply(pp2[,,which(d==format(dias[dia],"%m %d"))],c(1,2),FUN=sd,na.rm =T )
}

#grafico de la media y desvio para cada ciudad
#ggplot necesita que sea un df 
attach(puntos_df)
estadisticos_df<-data.frame()
for ( i in 1:nrow(puntos_df)){

 fila<-data.frame("Nombre"= c(puntos_df[[i,1]]),
                  "Media"=c(media[which(lon == puntos_df[[i,2]]),which(lat == puntos_df[[i,3]]),]),
                  "Desvio"=c( desvio[which(lon == puntos_df[[i,2]]),which(lat == puntos_df[[i,3]]),])
                     )
 estadisticos_df<-rbind.data.frame(estadisticos_df,fila)
}
#Agrego la columna fecha
dd<-c(rep(format(dias,"%m-%d"),10))
estadisticos_df$Fecha<-dd
#####Para agregar el desv por encima y por debajo 
estadisticos_df$Desv_sum=estadisticos_df$Media+estadisticos_df$Desvio
estadisticos_df$Desv_rest=estadisticos_df$Media-estadisticos_df$Desvio
estadisticos_df$Numero=c(rep(1:366,10))
require(ggplot2)  
g <- ggplot(estadisticos_df, aes(x = Numero, y = Media, group=Nombre)) +
  geom_ribbon(estadisticos_df,mapping=aes(ymin=Desv_rest,ymax=Desv_sum),fill="pink",alpha=0.8)+
  geom_line(aes(color = Nombre), size = 0.3, alpha = 3) +
  facet_wrap(.~Nombre,nrow = 5) +
  labs(title = "Media diaria de precipitacion ") +
  scale_x_continuous(name="Fecha",labels = estadisticos_df$Fecha[seq(1,366,by=50)],breaks = estadisticos_df$Numero[seq(1,366,by=50)])
#cada 45 dias queda bastante bien apesar de que hay fechas que no se ven 
#cada 50 dias para que quede lindo 
g
############# porcentaje de a?os lluviosos de todos los 4 de febrero para cada punto de grilla #############
feb<-array(data=NA,dim=c(9,8,27))
f<-dmy("04/02/2002")
feb<-pp2[,,which(d==format(f,"%m %d"))] #me quede con todos los 04/02  
feb[which(feb>0)]<-1                    #aplico ceros y unos
febrero<-apply(feb,c(1,2),sum)          #sumo los tiempos
febrero_porcentaje<-(febrero/27)*100    #porcentaje 
colnames(febrero_porcentaje)<-lat       #para verificar 
rownames(febrero_porcentaje)<-lon
datos<-as.vector(febrero_porcentaje) #por columna 
latt<-c()
lonn<-c(rep(lon-360,8)) #por cuestiones practicas cambie las longitudes 
#armo dataframe con las long y las lat 
for (i in 1:8){
  latitud<-c(rep(lat[i],9))
  print(latitud)
  latt<-c(latt,latitud)
}

febrero_porcentaje_df<-data.frame("Longitudes"=lonn,
                                  "Latitudes"=latt,
                                  "Porcentaje"=datos)
attach(febrero_porcentaje_df)
#Mapa 
library(metR)
require(metR)
require(ggplot2)
mapa<-ggplot(data=febrero_porcentaje_df,aes(Longitudes,Latitudes))
mapa<-mapa+borders(colour= "grey22",size=0.3) +ylab("Latitudes")+ xlab("Longitudes")
mapa<-mapa+coord_quickmap(xlim=c(-65,-57),ylim=c(-40,-33),expand=F)
mapa<-mapa+geom_contour_fill(aes(z=Porcentaje))
mapa<-mapa+geom_point(data=febrero_porcentaje_df,aes(x=Longitudes,y=Latitudes,color=Porcentaje),size=3)
mapa<-mapa+theme_bw() +theme(strip.background=element_rect(fill="grey92"), plot.subtitle=element_text(hjust= 0.5,size=10), axis.text =element_text(size=7,colour ="black"), axis.title =element_text(size=8))
mapa
  

