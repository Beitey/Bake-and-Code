# Librerias ---------------------------------------------------------------
require(tidyverse)



# Abriendo base de datos: -------------------------------------------------

dat = rio::import("Data Set/Bakery Sales.csv") %>% tibble()

dat = dat[!is.na(dat$total),] #Se eliminan ventas inexistentes 

View(dat)

dias_ordenados = c('Mon', 'Tues', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')

# Gráfico de Ventas por día
dat  %>%
  ggplot() + aes(x = factor(`day of week`, level = dias_ordenados)) + 
  geom_bar(stat = "count", fill = "#336688") +
  theme_minimal() +
  labs(title = "Numero de ventas por dias de la semana",
       y = "Numero de Ventas",
       x = "Dias de la Semana") + ggx::gg_("Center the title please")

# Gráfico de Ganancias por día

grafico_1 = dat  %>%
  ggplot() +
  aes(x = factor(`day of week`, level = dias_ordenados), weight = total) +
  geom_bar(fill = "#900000") +
  labs(title = "Ganancias por dias de la semana",
       y = "Monto ganado",
       x = "Dias de la Semana") +
  theme_minimal() + ggx::gg_("Center the title please") 

grafico_1 + scale_y_continuous(labels = comma)
  
  
# Se Verifican sus valores:
Ganancias_por_dia = c(0, 0, 0, 0, 0, 0, 0) # Se crea un vector con la ganancia
                                           # total del día

for (day in 1:length(dat$`day of week`)){
  dia = as.character(dat[2][day,]) # Obtiene el día
  pos = which(dia == dias_ordenados) # Obtiene la posicion de los dias ordenados
  # Se agrega la ganancia al vector creado en la posicion del dia
  Ganancias_por_dia[pos] = Ganancias_por_dia[pos] + as.numeric(dat[3][day,])
}


names(Ganancias_por_dia) = dias_ordenados
Ganancias_por_dia



# Creo la columna Hora para analizar las horas mas concurridas:

dat = dat %>% mutate(Hora = str_sub(dat$datetime, start = 12L, end = 13L))
    
grafico_2 = dat %>%
  ggplot() +
  aes(x = Hora, weight = total) +
  geom_bar(fill = "#6510a1") +
  labs(title = "Ganancias por Hora",
       y = "Monto ganado",
       x = "Hora del día") +
  theme_minimal() + ggx::gg_("Center the title please")

grafico_2 + scale_y_continuous(labels = comma)





