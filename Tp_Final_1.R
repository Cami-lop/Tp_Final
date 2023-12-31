#Tp Final Camila Lopez 
getwd()
setwd("C:/Users/camil/OneDrive/Escritorio/Cami_Labo/Tp_Final/")
require(ncdf4)  #llamo a la libreria que voy a necesitar 
#abro el archivo
archivo<-nc_open("C:/Users/camil/OneDrive/Escritorio/Cami_Labo/Tp_Final/daily_data_buenos_aires_province_1993-2023.nc")
archivo
##################################PUNTO A#######################################
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
  ciudad<-list("Nombre"= datos_ciudades[i,1], 
                 "Latitud"=as.numeric(datos_ciudades[i,2]),
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
#########################################PUNTO B################################
require(lubridate) #llamo a las librerias que voy a necesitar 
tiempos_leg<- as.Date(tiempo,origin="1970-01-01 00:00:00 ")
head(tiempos_leg) #desde el 01/10/96 // se repite
tail(tiempos_leg) #hasta el 31/07/23 #son 26 anos y  meses
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
################################################################################
#grafico de la media y desvio para cada ciudad
attach(puntos_df) #ggplot necesita que sea un df 
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
#Para agregar el desv por encima y por debajo 
estadisticos_df$Desv_sum=estadisticos_df$Media+estadisticos_df$Desvio
estadisticos_df$Desv_rest=estadisticos_df$Media-estadisticos_df$Desvio
estadisticos_df$Numero=c(rep(1:366,10))
require(ggplot2) #llamo a la libreria que voy a necesitar 

g <- ggplot(estadisticos_df, aes(x = Numero, y = Media, group=Nombre)) +
     geom_ribbon(estadisticos_df,mapping=aes(ymin=Desv_rest,ymax=Desv_sum),fill="pink",alpha=0.5)+
     geom_line(aes(color = Nombre), size = 0.3, alpha = 3) +
     facet_wrap(.~Nombre,nrow = 5) +
     labs(title = "Media diaria de precipitacion ",subtitle = "Periodo de datos de 1996 a 2023",caption="Elaborado por Camila Lopez") +
     scale_x_continuous(name="Fecha",labels = estadisticos_df$Fecha[seq(1,366,by=50)],breaks = estadisticos_df$Numero[seq(1,366,by=50)])
#cada 50 dias para que quede lindo 
g

######################################PUNTO C###################################
#porcentaje de anios lluviosos de todos los 4 de febrero para cada punto de grilla
#se puede hacer general pidiendo que se ingrese la fecha por consola y verificarla
feb<-array(data=NA,dim=c(9,8,27))
f<-dmy("04/02/2002")
feb<-pp2[,,which(d==format(f,"%m %d"))] #me quede con todos los 04/02  
feb[which(feb>0)]<-1                    #aplico ceros y unos
febrero<-apply(feb,c(1,2),sum)          #sumo los tiempos
febrero_porcentaje<-(febrero/27)*100    #porcentaje 
colnames(febrero_porcentaje)<-lat       #para verificar 
rownames(febrero_porcentaje)<-lon
datos<-as.vector(febrero_porcentaje)    #por columna 
latt<-c()
lonn<-c(rep(lon-360,8)) #por cuestiones practicas cambie las longitudes 

for (i in 1:8){ #armo dataframe con las long y las lat 
  latitud<-c(rep(lat[i],9))
  #print(latitud)
  latt<-c(latt,latitud)
}

febrero_porcentaje_df<-data.frame("Longitudes"=lonn,
                                  "Latitudes"=latt,
                                  "Porcentaje"=datos)
attach(febrero_porcentaje_df)
#Mapa 
#librerias que necesito 
library(metR)
require(metR)
require(ggplot2)
mapa<-ggplot(data=febrero_porcentaje_df,aes(Longitudes,Latitudes))
mapa<-mapa+coord_quickmap(xlim=c(-70,-55),ylim=c(-45,-30),expand=F)
mapa<-mapa+geom_contour_fill(aes(z=-Porcentaje))
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

#################################PUNTO D########################################
#funcion que dada una ciudad y un dia muestre porcentaje de años lluviosos
porcentaje_cuadrilla_df <- data.frame()  #lugar vacio
for (i in 1:10) {
  pos_cercana <- which.min(abs(lon - ciudades[[i]][[3]]))
  pas_cercana <- which.min(abs(lat - ciudades[[i]][[2]]))
  
  lon_cerca[i] <- lon[pos_cercana]
  lat_cerca[i] <- lat[pas_cercana]
  
  puntos_cercanos[[i]] <- list("Ciudad" = datos_ciudades[i, 1],
                               "Latitud" = lat_cerca[i],
                               "Longitud" = lon_cerca[i]-360) #de puntos cercanos cambio las longitudes

}
calcula_porcentaje <- function(ciudad, dia_especifico) { 
  fecha_c<-paste(dia_especifico,"02","2023")
  f <- as.Date(fecha_c, format = "%d%m%Y") #la fecha en formato fecha
  fecha<-format(f,"%m %d")
  mes<-substr(fecha,1,2)
  dia<-substr(fecha,4,5)
  febrero_f <- pp2[, , which(d == fecha)]# Obtener datos de febrero_f para la fecha dada

  febrero_f[which(febrero_f > 0)] <- 1
  
  # Calcular el porcentaje de días de lluvia para cada cuadrilla
  porcentaje_fc <- apply(febrero_f, c(1, 2), sum)
  porcentaje_cuadrilla <- (porcentaje_fc / 27) * 100
  datos<-as.vector(porcentaje_cuadrilla)
  #vectores de latitud y longitud
  latt <- rep(lat, each = 9)
  lonn <- rep(lon-360, 8)
  
  porcentaje_cuadrilla_df <- data.frame("Longitudes" = lonn,
                                        "Latitudes" = latt,
                                        "Porcentaje" = datos)
  
  # el porcentaje correspondiente a la ciudad y fecha específicas
  index_quiero <- which((porcentaje_cuadrilla_df$Latitudes == puntos_cercanos[[ciudad]][[2]]) &
                          (porcentaje_cuadrilla_df$Longitudes == puntos_cercanos[[ciudad]][[3]]))
  
  Porcentaje <- porcentaje_cuadrilla_df$Porcentaje[index_quiero]
  
  if (length(index_quiero) == 0) {
    stop("No se encontró la ciudad en las coordenadas proporcionadas.")
  }
  if (any(is.na(as.numeric(dia))) || any(as.numeric(dia) < 1) || any(as.numeric(dia) > 29)) { #Febrero va del 1 al 29
    stop("Fecha incorrecta.")
  }
  resultado<-as.numeric(round(Porcentaje,2))
  #print(paste("El porcentaje de años lluviosos para",ciudad,"el dia",dia,"del mes",mes,"es de",resultado,"%"))
  
  return(resultado) #devuelve el porcentaje calculado
}

# Llamada a la función
calcula_porcentaje("Ramos Mejia", "14") #no funciona la ciudad es incorrecta

calcula_porcentaje("Bella Vista", "31") #no funciona la fecha es incorrecta

calcula_porcentaje("Bella Vista", "04") 
####################################PUNTO E#####################################
porcentajes <- list()
resultado <- c()
fechas_largas <- c()
dias_ordenado<-c()
pos_min<-c()
fecha_ideal <- function(dd) {
  menos <- c(1, 2, 3)
  if(dd== 1 & dd<=3){
    fecha_mas <- c(dd + menos)
    dias_ordenado <- sort(c(fecha_mas))
  } else if (dd>=26 & dd<=29){
    fecha_menos <- c(dd - menos)
    dias_ordenado <- sort(c(fecha_menos))
  }
  else{
  fecha_menos <- c(dd - menos)
  fecha_mas <- c(dd + menos)
  dias_ordenado <- sort(c(fecha_menos, fecha_mas))
  }
  Porcentaje <- list()
  
  for (i in 1:length(dias_ordenado)) {
    
    for (c in 1:10) {
      ciudad <- ciudades[[c]][[1]]
      resul <- calcula_porcentaje(ciudad, dias_ordenado[i])
      resultado <- c(resultado, resul)
      
      Porcentaje[[c]] <- list("Ciudad" = ciudad,
                              "Fecha" = dias_ordenado[i],
                              "resultado" = resul)
    }
  }
  
  # la posición del mínimo, manejando  NA
  pos_min <- which.min(sapply(Porcentaje, function(x) ifelse(is.na(x$resultado), Inf, x$resultado)))
  
  #hay más de un mínimo?
  if (length(pos_min) > 1) {
    cat("Hay múltiples fechas con el mismo porcentaje mínimo. Puede manejar esto según tus necesidades.")
  }
  
  # información del mínimo
  ciudad_final <- Porcentaje[[pos_min]]$Ciudad
  Fecha_final <- Porcentaje[[pos_min]]$Fecha
  
  producto <- paste("El día con menos porcentaje de años lluviosos es el ", Fecha_final, "de Febrero en la ciudad", ciudad_final)
  return(producto)
}

fecha_ideal(1)

fecha_ideal(31) #Fecha incorrecta 







