# Librerias ---------------------------------------------------------------
require(tidyverse)
require(scales)
require(ggthemes)
require(gtsummary)
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

# Productos a español

## Obtengo Productos

# Conteo de los productos (columna 6 a 27)
Productos = c()
for (i in 5:27){
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

# Productos en español
Productos_esp = c("Pan de mantequilla y Frijoles rojos",
                  "Pan Blanco", "Tarta de Durazno", "Café americano", 
                  "Croissant", "Café latte", "Croissant de Tiramisú", 
                  "Croissant cuvierto de chocolate Varhona", "Pan de Chocolate",
                  "Croissant rellenado con crema de Almendras", 
                  "Croque Monsieur", "Mad Garlic", "Té con leche", 
                  "Pastel de Chocolate", "Pandoro", "Cheese Cake", "Limonada", 
                  "Queque de Naranja", "Panecillo con Salchicha", 
                  "Café de Vainilla latte", "Jugo de Frutos Rojos",
                  "Tiramisú", "Galletas de Merengue")

Bebestibles = c("Café americano", 
                "Café latte", "Té con leche", "Limonada",
                "Café de Vainilla latte", "Jugo de Frutos Rojos")

Productos = cbind(Productos, Productos_esp)


Vendidos_com = c() #Ventas
Vendidos_beb = c()
for (i in 1:22){
  prod = Productos[i,]
  for (j in 1:prod$Ventas){
    if (prod$Productos_esp %in% Bebestibles){
      Vendidos_beb = rbind(Vendidos_beb, as.character(prod$Productos_esp))
    }else{
      Vendidos_com = rbind(Vendidos_com, as.character(prod$Productos_esp))
    }
  }  
}

Vendidos_beb = as.data.frame(Vendidos_beb)
Vendidos_com = as.data.frame(Vendidos_com)


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
  labs(title = "Cantidad de pedidos por Horario",
       y = "Cantidad de pedidos",
       x = "Hora del día") +
  ggthemes::theme_base() + 
  ggx::gg_("Center the title please") 



# Grafico 5 Top productos Bebestibles y Comestibles


grafico_5 = Vendidos_beb %>% ggplot() + 
  aes(x = reorder(V1, V1 ,function(x)+length(x))) + 
  geom_bar(fill = "#3F9E91") +
  labs(title = "Numero de ventas por Producto Bebestibles",
       y = "Número de Ventas",
       x = "Productos") + 
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  ggthemes::theme_base() +
  ggx::gg_("Center the title please")


grafico_6 = Vendidos_com %>% ggplot() + 
  aes(x = reorder(V1, V1 ,function(x)+length(x))) + 
  geom_bar(fill = "#3F9E91") +
  labs(title = "Numero de ventas por Producto Comestibles",
       y = "Número de Ventas",
       x = "Productos") + 
  coord_flip(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") +
  ggthemes::theme_base() +
  ggx::gg_("Center the title please")

# Dulce vs Salado

dat_h = dat_h %>% mutate(Tipo = NA)

# Posiciones dulces y salados
Azucarados = c(7, 11, 12, 13, 18, 19, 20, 22, 26, 27)
Salados = c(5, 6, 9, 14, 23)

## Reviso si el pedido fue mas Dulce que salado (sin contar bebestibles)
for (i in 1:length(dat_h$total)){
  salado = 0
  dulce = 0
  for (j in 5:27){
    if (!is.na(dat_h[i,j])){
      if (j %in% Azucarados){
        dulce = dulce + as.numeric(dat_h[i,j])
      }else{
        salado = salado + as.numeric(dat_h[i,j])
      }
    }
  }
  
  # En caso de empate se usará dulce puesto a que los bebestibles son dulces
  if (salado > dulce){
    dat_h$Tipo[i] = "Salado"
  }else{
    dat_h$Tipo[i] = "Dulce"
  }
}

grafico_7 = dat_h %>%
  ggplot() +
  aes(x = Hora, fill = Tipo) +
  geom_bar() +
  labs(title = "Pedidos por Horario",
       subtitle = "Separado por dulce o salado",
       y = "Cantidad Pedida",
       x = "Hora del día") +
  ggthemes::theme_base() +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  ggx::gg_("Center the title please") 


# Grafico 8
# Top Productos por hora.
dat_h = dat_h %>% mutate(`Top Comestible` = NA) %>% 
  mutate(`Top Bebestible` = NA)

comestibles = c(5, 6, 9, 11, 13) #Top 5 comestibles
names_com = c("Pan de mantequilla y Frijoles rojos", "Pan Blanco", "Croissant",
              "Croissant de Tiramisú", "Pan de Chocolate")
bebestibles = c(8, 10, 17, 21, 24, 25) # Bebestibles
names_beb = c("Café americano", 
              "Café latte", "Té con leche", "Limonada",
              "Café de Vainilla latte", "Jugo de Frutos Rojos")
for (i in 1:length(dat_h$total)){
  com = 0
  com_cant = 0
  
  for (j in comestibles){
    if (!is.na(dat_h[i,j])){
      if (dat_h[i,j] > com_cant){ #En caso de empate gana el primero.
        com = j
        com_cant = dat_h[i,j]
      }
    }
  }
  
  if (!com == 0){
    p = which(com == comestibles)
    com = names_com[p]
  }else{
    com = "No Aplica"
  }
  
  beb = 0
  beb_cant = 0
  for (j in bebestibles){
    if (!is.na(dat_h[i,j])){
      if (dat_h[i,j] > beb_cant){ #En caso de empate gana el primero.
        beb = j
        beb_cant = dat_h[i,j]
      }
    }
  }
  
  if (!beb == 0){
    p = which(beb == bebestibles)
    beb = names_beb[p]
  }else{
    beb = "No Aplica"
  }

  dat_h$`Top Comestible`[i] = com
  dat_h$`Top Bebestible`[i] = beb
}


grafico_8 = dat_h %>% filter(!`Top Bebestible` == "No Aplica")%>%
  ggplot() +
  aes(x = Hora, fill = `Top Bebestible`) +
  geom_bar() +
  labs(title = "Pedidos por Horario",
       subtitle = "Separado por bebestibles",
       y = "Cantidad Pedida",
       x = "Hora del día") +
  ggthemes::theme_base() +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  ggx::gg_("Center the title please") 


grafico_9 = dat_h %>% filter(!`Top Comestible`=="No Aplica") %>% 
  filter(Hora<18) %>% 
  ggplot() +
  aes(x = Hora, fill = `Top Comestible`) +
  geom_bar() +
  labs(title = "Pedidos por Horario",
       subtitle = "Separado por Top 5 Comestibles",
       y = "Cantidad Pedida",
       x = "Hora del día") +
  ggthemes::theme_base() +
  theme(plot.subtitle = element_text(hjust = 0.5)) + 
  ggx::gg_("Center the title please") 

theme_gtsummary_language(
  language = "es",
  decimal.mark = ",",
  big.mark = "."
)

tbl_1 = dat_h %>% filter(!`Top Bebestible`=="No Aplica") %>% 
  filter(Hora<18) %>% select(Hora, `Top Bebestible`) %>% 
  tbl_summary(by=Hora, label = list(`Top Bebestible`~"Productos")) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Hora**") %>% 
  modify_header(update = list(label~"")) %>% 
  modify_footnote(all_stat_cols()~"Ventas en porcentajes")

tbl_2 = dat_h %>% filter(!`Top Comestible`=="No Aplica") %>% 
  filter(Hora<18) %>% select(Hora, `Top Comestible`) %>% 
  tbl_summary(by=Hora, label = list(`Top Comestible`~"Productos")) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Hora**") %>% 
  modify_header(update = list(label~"")) %>% 
  modify_footnote(all_stat_cols()~"Ventas en porcentajes")


