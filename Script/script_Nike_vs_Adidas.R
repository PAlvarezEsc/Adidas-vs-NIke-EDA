########################## Proyecto Adidas vs Nike ##########################

### Preparar


## Cargar Librerias

library(dplyr)
library(tidyverse)
library(ggplot2)
library(skimr)
library(visdat)
library(inspectdf)
library(patchwork)

## Cargar base de datos

dataset<- read.csv("Adidas_Nike_dataset.csv")


### Procesar

head(dataset, n = 10)

# elliminamos "descripccion", "Product.ID" y "Last.Visited" , ya que molesta y no se utilizarán

dataset <- dataset %>% select(- Description)
dataset <- dataset %>% select(- Product.ID)
dataset <- dataset %>% select(- Last.Visited)

# revisamos la estructura de los datos

glimpse(dataset)

# Ajuste de las variables


# Brand hay q cambiarlo a factor

dataset$Brand <- as.factor(dataset$Brand)

# Rating hay q pasarlo a numerico

dataset$Rating <- as.numeric(dataset$Rating)

# Reviews hay q pasarlo a numerico

dataset$Reviews <- as.numeric(dataset$Reviews)

glimpse(dataset)

# cambiar los nombres en la variable brand

levels(dataset$Brand) # vemos los nombres a  reemplazar

# Nuevos Nombres

mynewnames <- c("Adidas Adidas ORIGINALS" = "Adidas", 
                "Adidas CORE / NEO" = "Adidas", 
                "Adidas ORIGINALS" = "Adidas", 
                "Adidas SPORT PERFORMANCE" = "Adidas",
                "Nike" = "Nike")
# reemplazamos

levels(dataset$Brand) <- mynewnames[levels(dataset$Brand)]

glimpse(dataset)

# Identificar y tratar NA

any(is.na(dataset))
inspect_na(dataset) %>% show_plot() # no NA

# Identificar y tratar duplicados

any(duplicated(dataset))

sum(duplicated(dataset))

dataset <- dataset %>% distinct() 


## "Analisis descriptivo de los datos"

# volvemos a visualizar

head(dataset, n=10)


# primera vision de los datos

skim(dataset)

# tenemos que son 3268 observaciones y 7 variables
# 2 variables de tipo "caracter" y 5 "numericas
# No hay variables agrupadas
# no hay datos perdidos (NA)
# posiblementte hayan outliers

names(dataset)


# resumen visual de los datos

general <- vis_dat(dataset)
tipos_de_datos <- inspect_types(dataset) %>% show_plot()  # Explorar tipos de datos
inspeccion_categoricos <- inspect_cat(dataset) %>% show_plot() # Explorar datos categoricos
inspeccion_numericos <- inspect_num(dataset) %>% show_plot() # explorar los numericos

plot_resumen <- tipos_de_datos + general +
  labs(title = "Dataset description" )
plot_resumen
# la mayoria de las variables del dataframe son datos de tipo integer o numericos  y 
# solo una variable son categoricas  y 1 es de tipo cadena de caracteres.

inspeccion_categoricos  # el mayor procentaje de los datos obtenidos son de la marca adidas
inspeccion_numericos    # las variables tienen una distribucion unimodal pero se nota la presencia 
                        # de outliers en las variables.



### Analizar

## Establecer relaciones

corPlot(dataset[, c("Listing.Price", "Sale.Price", "Discount", "Rating", "Reviews")],
        min.length = 3)

dev.off()


## Analizando las variables Categóricas


## Productos disponibles en el mercado

productos_mercado<- dataset %>% 
                        dplyr::count(Brand)

productos_mercado

# observamos que Adidas tiene mas productos en el mercado que nike.


## Analizando Las Variables Numericas


# Listing price

# visualizamos la distribucion de datos

ggplot(data = dataset, aes(x = Brand, y = Listing.Price , fill = Brand)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(outlier.shape = NA , width = 0.05) +
  scale_y_continuous(breaks=c(1000, 5000, 10000, 15000, 20000, 25000, 30000)) +
  scale_fill_manual(values = c("purple1", "#FFFF00"))+
  labs(title = "Listing Price by Brand")+
  theme_classic()

# Los precios adidas, en su mayoria se distribuyen entre los $4k y $9k con dos picks en los $5K y $7k.
# los precios Nike poseen una distribucion unimodal con un pick en los $1k.
# El precio de lista es menor en los productos nike con respecto a los productos adidas.


# Sale price

ggplot(data = dataset, aes(x = Brand, y = Sale.Price , fill = Brand)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(outlier.shape = NA , width = 0.05) +
  scale_fill_manual(values = c("springgreen1", "#FF6347"))+
  scale_y_continuous(breaks=c(1000, 5000, 10000, 15000, 20000, 25000, 30000)) +
  labs(title = "Sale price by Brand")+
  theme_classic()

dataset %>%
  group_by(Brand) %>%
  summarise ( 'Mean Sale Price' = mean(Sale.Price))


# Parece que Adidas generalmente ofrece sus productos más baratos que Nike.
# Los precios de venta de Adidas tienen un pick cercano a los $2K mientras que Nike está más cercano
# a los $6K.
# Los precios de venta de los productos Nike son mas simetricos coparados con los precios de venta adidas,
#que son altamente variables.


# Discount

# Al filtrar los valores de descuento, parece que Nike no ofrece ninguno, pero al examinar los precios de
# lista y de venta, parece que Nike ofrece algunos. Entonces, para una comparación justa, calculo y 
# completo los valores de Nike.

df.dsc <- dataset %>%                # creamos el dataframe solo con descuentos vs marca
          select(- Product.Name,
                 - Listing.Price,
                 - Sale.Price,
                 - Rating,
                 - Reviews)

df.dsc <- df.dsc %>% distinct()     #eliminamos replicados

df.dsc


# Se observa que adidas trata bien a sus clientes y les ofrece descuentos mientras nike no suele ofrecer 
# descuetos en sus productos.


# Rating

df.rat2 <- dataset %>%                # creamos el dataframe solo con descuentos vs marca
  select(- Discount)

df.rat2 <- df.rat2 %>% distinct()     #eliminamos replicados

df.rat2 <- df.rat2[df.rat2$Rating > 0 & df.rat2$Brand != 0, ] # eliminamos los ceros

df.rat2

df.rat2 %>%
  group_by(Brand) %>%
  summarise ( 'Mean Rating' = mean(Rating),
              'Mean Listing.Price' = mean(Listing.Price),
              'Mean Sale.Price' = mean(Sale.Price),
              'Mean Reviews' = mean(Reviews))


df.rat2 %>%
  group_by(Brand) %>%
  summarise ( 'Mean Rating' = mean(Rating))


ggplot(data = df.rat2, aes(x = Brand, y = Rating , fill = Brand)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(outlier.shape = NA , width = 0.05) +
  scale_fill_manual(values = c("#7FFF00", "#0000FF"))+
  labs(title = "Rating by Brand")+
  theme_classic()

# Las evaluaciones de los productos Nike son bastante asimetricos y distribuidos a valores altos.
# Nike tiene  mejores evaluaciones que Adidas.
#Adidas tiene menores evaluaciones que Nike, pero sus evaluaciones son mas uniformes que los productos Nike.


# Comparando Reviews vs Rating

resumen<- df.rat2 %>%
  group_by(Brand) %>%
  summarise ( 'Mean Rating' = mean(Rating),
              'Mean Listing.Price' = mean(Listing.Price),
              'Mean Sale.Price' = mean(Sale.Price),
              'Mean Reviews' = mean(Reviews))

resumen

rating<- ggplot(data = resumen, aes(x = Brand, y =`Mean Rating` , fill = Brand)) +
  geom_col() +
  scale_fill_manual(values = c("#00FFFF", "#FF8C00"))+
  labs(title = "Rating by Brand", x = "Brand", y = "Rating")+
  theme_classic()
reviews<- ggplot(data = resumen, aes(x = Brand, y = `Mean Reviews` , fill = Brand)) +
  geom_col() +
  scale_fill_manual(values = c("#00FFFF", "#FF8C00"))+
  labs(title = "Reviews by Brand", x = "Brand", y = "Reviews")+
  theme_classic()

rating + reviews
