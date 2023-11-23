titulo<-c("Bella Vista","Ezeiza","La Plata","Pergamino","San Nicolas"," Bahia Blanca","Ayacucho","Monte Chingolo","Magdalena","Pinamar" )
# Crear una secuencia para seleccionar filas
j <- seq(366, 3660, 366)

# Crear una lista para almacenar los gráficos
graficos <- list()

# Bucle para crear gráficos
for (k in 1:10) {
  # Seleccionar las filas correspondientes a la secuencia actual
  subset_df <- estadisticos_df[((k - 1) * 366 + 1):(j[k] - 1), ]
  
  # Crear el gráfico con ggplot
  g <- ggplot(subset_df, aes(x = Fecha, y = Media)) +
    geom_line(aes(color = Nombre), size = 3, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, aes(color = Nombre)) +
    facet_wrap(~Nombre) +
    labs(title = titulo[k])
  
  # Almacenar el gráfico en la lista
  graficos[[k]] <- g
}

# Imprimir todos los gráficos
lapply(graficos, print)
#falta ver como hacerque sea una figura con 10 graficos y lo del desvio