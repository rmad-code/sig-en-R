#### Sig en R ####

library(raster)
library(rworldxtra)
library(sf)
library(tidyverse)

data("countriesHigh")

## trasformamos la bd para tener la nueva bd en formato más "agradable"

mundo <- countriesHigh %>% 
  st_as_sf()

# revisando la clase de datos de cada base

class(countriesHigh)    # "SpatialPolygonsDataFrame", "sp": formato antiguo

mundo %>% class()            # "sf": simple fixture, "data.frame"

## dado el cambio de formato y al ser la nueva bd un data.frame podemos manejarlo de manera similar

## graficando el archivo: empleando ggplot con el uso de nuevo geom_sf y nueva sintaxis ##

ggplot()+ geom_sf(data = mundo)

# Revisando las variables de "mundo"
mundo %>% colnames()

ggplot()+ geom_sf(data = mundo, aes(fill=POP_EST))   # genera un relleno al mapa con estimadores de pobl (numerico)

# haciendo fill (relleno) con variable categorica: region, continente

ggplot()+ geom_sf(data = mundo, aes(fill=REGION))

## Generando filtros:

# buscamos los valores unicos desde la variable continente

mundo$continent %>% unique()

## Construimos un nuevo poligono SA (sudamerica)

SA <- mundo %>% dplyr::filter(continent=="South America and the Caribbean")

ggplot()+ geom_sf(data = SA, aes(fill=POP_EST)) # estimacion pob para continente

## se pueden tomar más de una capa

ggplot()+ geom_sf(data = mundo)+ 
  geom_sf(data = SA, fill="red")+
  theme_bw()                              # sirve para destacar un lugar

## destacamos un pais de sudamerica

Peru <- SA %>% dplyr::filter(ADMIN=="Peru")

ggplot()+ geom_sf(data = SA)+ 
  geom_sf(data= Peru, fill= "yellow")

## Usando la funcion mutate para modificar las variables del df

# creamos una nueva variable: poblacion en millones

SA <- SA %>% mutate(Pob_mill= POP_EST/1000000)

ggplot()+ geom_sf(data = SA, aes(fill=Pob_mill))

ggplot()+ geom_sf(data = SA, aes(fill=Pob_mill))+ theme_bw()

# agregando escala de colores

ggplot()+ geom_sf(data = SA, aes(fill=Pob_mill))+ 
  theme_bw()+ scale_fill_viridis_c()

# cambiando el nombre de la escala

ggplot()+ geom_sf(data = SA, aes(fill=Pob_mill))+ 
  theme_bw()+ scale_fill_viridis_c(name= "Población en millones")

# reubicando la escala 

ggplot()+ geom_sf(data = SA, aes(fill=Pob_mill))+ 
  theme_bw()+ 
  scale_fill_viridis_c(name= "Población en millones")+
  theme(legend.position = "bottom")

## Generamos el PIB per capita usando dos variables: GDP_MD_EST / POP_EST

SA <- SA %>% mutate(Pob_mill= POP_EST/1000000, PIB_per_cap= GDP_MD_EST/POP_EST)

# notar que despues de ejecutar la linea 90 SA tendra 54 variables

ggplot()+ geom_sf(data = SA, aes(fill= PIB_per_cap))+
  scale_fill_viridis_c(name= "PIB")+
  theme_bw()

## en la grafica se aprecia la existencia de territorio(s) con PIB sobre 0.06,
# para identificarlos podemos hace:

SA[SA$PIB_per_cap>= 0.06,]

SA %>% dplyr::filter(PIB_per_cap== max(PIB_per_cap))

SA %>% dplyr::filter(PIB_per_cap==max(PIB_per_cap)) %>% pull(NAME)

# usando la forma tradiional o la sintaxis tidy vemos que el resultado es Bermuda, 
# con la linea de codigo 105, se obtiene el nombre de forma directa

# volviendo la observación en df

Max_per_cap <- SA %>% dplyr::filter(PIB_per_cap== max(PIB_per_cap))

Max_per_cap$ADMIN

## Guardando el archivo shape para leerlo en otro programa 

## Exportar el shapefile con solo algunas variables de nuestro interes 

SA <- SA %>% dplyr::select(PIB_per_cap, Pob_mill, ADMIN.1, SOVEREIGNT, GLOCAF)

write_sf(SA, "SA.shp")

## Leer un archivo shapefile

SA2 <- read_sf("SA.shp")

## Notese que en algunos casos  
# los nombres de las columnas se acortan para poder ser manejados por el programa (ej. esri)

colnames(SA)
SA2 %>% colnames()

## graficando SA2 

ggplot()+ geom_sf(data = SA2, aes(fill=GLOCAF))

# usando distintos themes

ggplot()+ geom_sf(data = SA2, aes(fill=GLOCAF))+
  theme_classic()

#### Descargar datos vectoriales de raster ####

# Optener los codigos de cada pais con la funcion getData

?getData

# guardamos los codigos de los paises en un dataframe

DF <- getData("ISO3")
# para el caso de Peru el codigo es PER

# usaremos la funcion getData("GADM") el argumento representa los datos administrativos
# podemos sacar un pais en sus diferentes niveles administrativos (region, provincia, etc)

Peru_raster<- getData(name = "GADM", country="PER", level=1) %>% st_as_sf()

## graficando 

ggplot()+
  geom_sf(data = Peru_raster, aes(fill=NAME_1))+
  scale_fill_viridis_d()+
  theme(legend.position = "none")

# para generar el mapa de Peru por provincias subimos el level en la bd

Peru_raster_prov <- getData(name = "GADM", country="PER", level=2) %>% st_as_sf()

# graficando

ggplot()+
  geom_sf(data=Peru_raster_prov, aes(fill=NAME_2))+
  scale_color_viridis_c()+
  theme(legend.position = "none")

# construyendo puntos sobre el raster de peru (lat -5, lon -76)

DF <- data.frame(lon=c(-76, -76), lat=c(-5, -4), Punto =c("P1", "P0"))

# Transformamos el DF en un objeto espacial

DF <- data.frame(lon=c(-76, -76), lat=c(-5, -4), Punto =c("P1", "P0")) %>% 
  st_as_sf(coords=c(1,2), crs="+proj=longlat +datum=WGS84 +no_defs")

# Para determinar la proyeccion del mapa al que se añadiran los puntos
# debemos correr el nombre de la base de datos que genera  el mapa (Peru_raster_prov) y revisar el 
# apartado CRS y copiarlo en  el codigo del DF

## graficando los puntos:

ggplot()+
  geom_sf(data=Peru_raster_prov)+
  geom_sf(data=DF, aes(color=Punto))

## eligiendo solo algunas provincias de Peru

Norte_oriente<- Peru_raster_prov %>% 
  dplyr::filter(NAME_1 %in% c("Amazonas", "Lambayeque", "Cajamarca"))

ggplot()+
  geom_sf(data = Norte_oriente, aes(fill=NAME_2))+
  scale_fill_viridis_d()


#### Raster #### 

Prec <- getData(name = "worldclim", res=10,  var= "prec")

Prec

plot(Prec)

# cambiando el fondo de la vista

plot(Prec, colNA="black")

# subseteo de stacks 

Invierno_sur <- Prec[[c(6,7,8)]]

plot(Invierno_sur)

plot(Invierno_sur, colNA="black")

#### Operaciones simples con Raster ####

# sumaremos las precipitaciones de los meses de invierno en el hemisferio sur

Inv_sur_total <- Prec[[6]]+ Prec[[7]]+ Prec[[8]]

plot(Inv_sur_total, colNA="black")

# se puede hacer un resumen de la prec anual al sumar todos los meses 

PP_total <- sum(Prec)

plot(PP_total, colNA="black")

#### Cortar un raster (en base a otro mapa) ####


Peru_prec<- PP_total %>% crop(Peru_raster)

plot(Peru_prec)

## para que se vea la forma de peru añadimos la funcion mask()
# siempre hacer primero el crop (corte) y luego el mask (mascara)

Peru_prec<- PP_total %>% crop(Peru_raster) %>% mask(Peru_raster)

plot(Peru_prec, colNA="black")

## Extraeremos la precipitacion anual en los puntos generados en el DF

raster::extract(Peru_prec, DF)

# si vemos la clase de objeto que es lo generado por el codigo anterior:

raster::extract(Peru_prec, DF) %>% class()

# al ser numerico, lo podemos añadir al DF

DF$Prec<- raster::extract(Peru_prec, DF)

# Extraigamos la precipitacion anual para los puntos de DF

Prec_anual_ptos<- raster::extract(Prec, DF)

Prec_anual_ptos

# como vemos el codigo genera una matrix con la prec en cada mes para cada punto de DF

## luego, lo transformamos en data.frame

Prec_anual_ptos %>% as.data.frame()

### unimos lo anterior a DF

Prec_anual_ptos %>% as.data.frame() %>% bind_cols(DF)

DF_1<- Prec_anual_ptos %>% as.data.frame() %>% bind_cols(DF)


# se puede apreciar en la comprobacion que existe una pequeña diferencia en las 
# precipitaciones entre la anual y la suma de las mensuales
sum(DF_1[1, c(1:12)])

sum(DF_1[2, c(1:12)])

#### Exportar y leer un raster ####
# se le da la extension "grd" por ser la mas nativa, overwrite es por si se 
# desea sobreescribir

writeRaster(Peru_prec, "Peru_prec.grd", overwrite=T)

# leer el raster

Peru_prec_2 <- raster("Peru_prec.grd")

# comprondo el archivo 

plot(Peru_prec_2)

# en el caso de que se tratara de varias capas como en un geostick
# lo unico que se hace para leer el archivo es cambiar el raster por stack

#### Proyecciones ####
# la funcion proj4string() permite determinar la proyeccion o sistema 
# de coordenadas de referencia de un archivo

proj4string(Peru_prec_2)

### no se puede evaluar cierta caracteristica si se tiene pixeles de areas
# diferentes

# revisar Projection wizard: permite elegir un lugar del mundo y cambiar 
# su proyeccion y elegir el tipo de proyeccion


## supongamos que cambiaremos las propiedades de proyeccion a equal area


# hacemos una reproyeccion

Peru_prec_Equal <- projectRaster(Peru_prec, crs = "+proj=tcea +lon_0=-75.9375 +datum=WGS84 +units=m +no_defs")

plot(Peru_prec_Equal, colNA="black")

# observemos la diferencia en los graficos

plot(Peru_prec, colNA="black")

plot(Peru_prec_Equal, colNA="black")

#### Graficar raster y shapefiles juntos ####

Peru_prec_DF <- Peru_prec_2 %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame()

# revisando el objeto creado

head(Peru_prec_DF) # devuelve al prec, lon y lat

# cambiamos la variable layer a precipitacion (prec)

Peru_prec_DF <- Peru_prec_2 %>% as("SpatialPixelsDataFrame") %>% 
  as.data.frame() %>% rename(prec=layer)

head(Peru_prec_DF)

# graficando con ggplot

ggplot()+
  geom_tile(data = Peru_prec_DF, aes(x=x, y=y, fill= prec))+
  geom_sf(data = Peru_raster_prov, alpha=0)+
  scale_fill_viridis_c()+
  xlab("")+ ylab("")+ theme_bw()











