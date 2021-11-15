# Librerias ---------------------------------------------------------------
require(tidyverse)
require(scales)
require(ggthemes)

# Abriendo base de datos: -------------------------------------------------

dat = rio::import("Data_Set/Bakery Sales.csv") %>% tibble()

dat = dat[!is.na(dat$total),] #Se eliminan ventas inexistentes 

dias_ordenados = c('Mon', 'Tues', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun')
dias_esp_ord = c('Lun', 'Mar', 'Mie', 'Jue', 'Vie', 'Sab', 'Dom')

# Funciones ---------------------------------------------------------------

esp_days = function(datos, dias_ordenados, dias_esp_ord){
  dias_español = c()
  for (dia in datos){
    pos = which(dia == dias_ordenados)
    dias_español = c(dias_español, dias_esp_ord[pos])
    }
  return(dias_español)
}



# Transformaciones --------------------------------------------------------

# Días a español
dat = dat %>% mutate(dias = esp_days(dat$`day of week`, dias_ordenados, dias_esp_ord))


# Gráficos ----------------------------------------------------------------

# Gráfico de Ventas por día
grafico_1 = dat  %>%
  ggplot() + aes(x = factor(dias, level = dias_esp_ord)) + 
  geom_bar(stat = "count", fill = "#336688") +
  labs(title = "Número de ventas por días de la semana",
       y = "Número de Ventas",
       x = "Días de la Semana")+ 
  ggthemes::theme_base() +
  ggx::gg_("Center the title please")

# Gráfico de Ganancias por día

grafico_2 = dat  %>%
  ggplot() +
  aes(x = factor(dias, level = dias_esp_ord), weight = total) +
  geom_bar(fill = "#900000") +
  labs(title = "Ganancias por días de la semana",
       subtitle = "Ganancia en Wones Sur Coreanos",
       y = "Monto ganado",
       x = "Días de la Semana") +
  ggthemes::theme_base() +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  ggx::gg_("Center the title please") 

grafico_2 = grafico_2 + scale_y_continuous(labels = comma)
  
  
# Gráfico Promedio ponderado de ganancias:

Ganancias_por_dia = c(0, 0, 0, 0, 0, 0, 0) # Se crea un vector con la ganancia
                                           # total del día

for (day in 1:length(dat$`day of week`)){
  dia = as.character(dat[2][day,]) # Obtiene el día
  pos = which(dia == dias_ordenados) # Obtiene la posicion de los dias ordenados
  # Se agrega la ganancia al vector creado en la posicion del dia
  Ganancias_por_dia[pos] = Ganancias_por_dia[pos] + as.numeric(dat[3][day,])
}

# Promedio del dia ponderado por venta
names(Ganancias_por_dia) = dias_ordenados
Ganancias_por_dia
Ventas_por_dia = plyr::count(dat$dias) 

Porcentaje = c(0, 0, 0, 0, 0, 0, 0)# Se crea un vector con la ganancia
                                    # ponderada del día

for (day in 1:length(dias_esp_ord)){
  pos = which(Ventas_por_dia$x == dias_esp_ord[day])
  Porcentaje[day] = as.numeric(Ganancias_por_dia[day])/ Ventas_por_dia$freq[pos]
}


porcent = as.data.frame(cbind(dias_esp_ord, format(Porcentaje, digits = 2)))

grafico_3 = porcent %>% ggplot() + 
  aes(x = factor(dias_esp_ord, level = dias_esp_ord), V2) +
  geom_bar(stat = "Identity", fill = "#03bac7") +
  labs(title = "Promedio ponderado de Ganancias por Dia",
       subtitle = "Ganancia en Wones Sur Coreanos",
       y = "Monto ganado",
       x = "Días de la Semana") +
  ggthemes::theme_base() +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  ggx::gg_("Center the title please") 




# Creo la columna Hora para analizar las horas mas concurridas:
horas = seq(18, 21, by = 1)
dat_h = dat %>% mutate(Hora = str_sub(dat$datetime, start = 12L, end = 13L))


# Para lograr un eje x continuo se agregan datos fantasmas que no afectan 
## a los resultados

vec = vector("numeric", 27)
vec[2] = "Tues" ; vec[4] = ""; vec[c(5:27)] = NA
for (i in horas){
  datos_fantasma = c(vec, "Mar", i)
  dat_h = rbind(dat_h, datos_fantasma)
}

# Gráfico de ganancias por horas:
grafico_4 = dat_h %>%
  ggplot() +
  aes(x = Hora) +
  geom_bar(fill = "#6510a1") +
  labs(title = "Ganancias por Horario",
       subtitle = "Ganancia en Wones Sur Coreanos",
       y = "Monto ganado",
       x = "Hora del día") +
  ggthemes::theme_base() +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  ggx::gg_("Center the title please") 

grafico_4 = grafico_4 + scale_y_continuous(labels = comma)

# Grafico 5 Top productos

# Conteo de los productos (columna 6 a 27)
Productos = c()
for (i in 6:27){
  conteo = plyr::count(dat[i])
  total = 0
  for (j in 1:length(conteo$freq)) {
    if (!is.na(conteo[j,1])){
      total = total + (conteo[j,1] * conteo[j,2])
    }
  }
  fil = cbind(names(dat[i]), total)
  Productos = rbind(Productos, fil)
}

Productos = Productos %>% as.data.frame()
colnames(Productos) = c("Producto", "Ventas")  

Vendidos = c() #Ventas
for (i in 1:22){
  prod = Productos[i,]
  for (j in 1:prod$Ventas){
    Vendidos = rbind(Vendidos, as.character(prod$Producto))
  }
}
Vendidos = as.data.frame(Vendidos)

grafico_5 = Vendidos %>% ggplot() + 
  aes(x = reorder(V1, V1 ,function(x)+length(x))) + 
  geom_bar(fill = "#3F9E91") +
  labs(title = "Numero de ventas por Producto",
       y = "Número de Ventas",
       x = "Productos") + 
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  ggthemes::theme_base() +
  ggx::gg_("Center the title please")

