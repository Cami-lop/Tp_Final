#Tp Final Camila Lopez 
#Abro el archivo ncdf 
getwd()
#setwd("C:/Users/camil/OneDrive/Escritorio/Cami_Labo/Tp_Final/")
setwd("/home/clinux01/Escritorio/CamiLabo/Tp_Final/")
require(ncdf4)  #llamo a la libreria que voy a necesitar 
#abro el archivo
archivo<-nc_open("/home/clinux01/Escritorio/CamiLabo/Tp_Final/daily_data_buenos_aires_province_1993-2023.nc")
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
tail(tiempos_leg) #hasta el 31/07/23 #son 26 años y  meses
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
  #dias[dia]
  #dato<-mean(pp2[,,which(format(dia,"%m %d")==d)])
  #print(format(dias[dia],"%m %d"))
  #print(which(d==format(dia,"%m %d")))
  media[,,dia]<-apply(pp2[,,which(d==format(dias[dia],"%m %d"))],c(1,2),FUN=mean) #estan prdenadas desde el 01/01 al 31/12
  desvio[,,dia]<-apply(pp2[,,which(d==format(dias[dia],"%m %d"))],c(1,2),FUN=sd)
}

#grafico de la media y desvio para cada ciudad // un panel por ciudad 
#ggplot necesita que sea un df 
attach(puntos_df)
bellavista<-media[which(lon == puntos_df[[1,2]]),which(lat == puntos_df[[1,3]]),]
estadisticos_df<-data.frame()
for ( i in 1:nrow(puntos_df)){

 fila<-data.frame("Nombre"= c(puntos_df[[i,1]]),
                  "Media"=c(media[which(lon == puntos_df[[i,2]]),which(lat == puntos_df[[i,3]]),]),
                  "Desvio"=c( desvio[which(lon == puntos_df[[i,2]]),which(lat == puntos_df[[i,3]]),])
                     )
 estadisticos_df<-rbind.data.frame(estadisticos_df,fila)
}

  

