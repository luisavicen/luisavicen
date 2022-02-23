install.packages("ggmap")
install.packages("rgdal")
install.packages("rstudio/leaflet")
install.packages("raster")
library(sf) #para trabajar con datos geo
library(ggmap) #Para visualizar mapas lindos
library(tidyverse)
library(leaflet) #para mapas interactivos 
library(rgdal)
library(sp)
library(dplyr)

#Cargo el shapefile
marchiquita_raw <- st_read("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/069_parcela/110101.shp")
#En la consola les va a devolver algo como esto:
#Reading layer `Ejes_518' from data source `C:\Users\Usuario\Documents\Code\R\Amartia\shp\Ejes_518.shp' using driver `ESRI Shapefile'
#Simple feature collection with 3804 features and 6 fields
#Geometry type: LINESTRING
#Dimension:     XY
#Bounding box:  xmin: 5696414 ymin: 5804400 xmax: 5727983 ymax: 5873912
#Projected CRS: POSGAR 94 / Argentina 5
#Fijensé la proyección (POSGAR 94). Son coordenadas planas, despues les cuento bien de que se trata
#https://ramsac.ign.gob.ar/posgar07_pg_web/documentos/Informe_sobre_codigos_oficiales_EPSG.pdf
#Debemos cambiar la proyección para que poder ubicarlo en coordenadas geográficas (google maps, etc.)
marchiquita_geo <- st_transform(marchiquita_raw, crs = 4326)
#4326 es el id de la proyección que usa, por ejemplo, google. (WGS 84 pueden encontrarlo también
#https://en.wikipedia.org/wiki/World_Geodetic_System)
plot(marchiquita_geo)
#Visualizamos para ver como nos quedó
mar_chiquita_plot <- leaflet() %>%
  addTiles() %>% 
  setView(lng = -57.4507217, lat=-37.7579979,zoom=11) %>% 
  addPolygons(data=marchiquita_geo,weight=1,col = 'blue') 
mar_chiquita_plot

#manzanas
marchiquita_raw2 <- st_read("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/069_manzana/110102.shp")
marchiquita_geo2 <- st_transform(marchiquita_raw2, crs = 4326)
plot(marchiquita_geo2)
mar_chiquita_plot2 <- leaflet() %>%
  addTiles() %>% 
  setView(lng = -57.4507217, lat=-37.7579979,zoom=11) %>% 
  addPolygons(data=marchiquita_geo2,weight=1,col = 'red') 
mar_chiquita_plot2

#circunscripcion
marchiquita_raw3 <- st_read("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/069_circunscripcion/110107.shp")
marchiquita_geo3 <- st_transform(marchiquita_raw3, crs = 4326)
plot(marchiquita_geo3)
mar_chiquita_plot3 <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>% 
  setView(lng = -57.4507217, lat=-37.7579979,zoom=11) %>% 
  addPolygons(data=marchiquita_geo3,weight=0,col = 'green') %>%
  addMarkers(data=mchiq_shp, label = ~nombre.est) %>% 
  addCircles(radius=200, color="green")%>% #verde para zona de exclusión
  addCircles(radius=500, color = "orange") 
mar_chiquita_plot3

prueba5 <- leaflet%>%
  addTiles() %>% 
  setView(lng = -57.4507217, lat=-37.7579979,zoom=11) %>% 
  addPolygons(data=marchiquita_geo3, weight=0,col = 'green') %>%
  addMarkers(data=mchiq_shp, label = ~nombre.est) %>% 
  addCircles(data=mchiq_shp, radius=200, color="green") %>% #verde para zona de exclusión
  addCircles(data=mchiq_shp, radius=500, color = "orange")
prueba5

#establecimientos educativos
escuelas_shp <- readOGR("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/establecimientos-educativos/shapefile/establecimientos-educativos.shp", use_iconv=TRUE, encoding = "UTF-8")
escuelas_shp <- spTransform(escuelas_shp, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#Para ver qué datos tiene y poder filtrar por Mar Chiquita
head(escuelas_shp)
#Filtro por Mar Chiquita y por escuelas que no sean urbanas (esto ultimo puede ir o no)
mchiq_shp <- escuelas_shp%>%
  subset(partido == "Mar Chiquita" & ubicación!= "Urbano")
#Qué datos me quedaron
head(mchiq_shp)
mchiq_shp@data$ubicación

######MAPEO######
actual <- leaflet (mchiq_shp, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>% 
  addTiles() %>%  #addTiles() nos da la capa base del mapa. si lo dejamos vacío agrega tiles default de OpenStreetMap
  #Agregamos marcadores
  addMarkers(label = ~nombre.est) %>% 
  addCircles(radius=200, color="green")%>% #verde para zona de exclusión
  addCircles(radius=500, color = "orange") #naranja para zona de amortiguamiento
actual


#Mapa con ordenanza deseada en 6 años
futuro <-leaflet (mchiq_shp, options = leafletOptions(minZoom = 10, maxZoom = 18)) %>% 
  addTiles() %>% 
  #Agregamos marcadores
  addMarkers(label = ~nombre.est)%>%
  addCircles(radius = 500, color = "blue")%>%
  addCircles(radius = 750, color = "red")
futuro


#mapa manzanas
marchiquita_raw2 <- st_read("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/069_manzana/110102.shp")
marchiquita_geo2 <- st_transform(marchiquita_raw2, crs = 4326)
plot(marchiquita_geo2)
mar_chiquita_plot2 <- leaflet() %>%
  addTiles() %>% 
  setView(lng = -57.4507217, lat=-37.7579979,zoom=11) %>% 
  addPolygons(data=marchiquita_geo2,weight=1,col = 'green') 
mar_chiquita_plot2



####CURSOS DE AGUA ####
cursos_agua <- readOGR("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/cursos-agua/shapefile/cursos-agua.shp", encoding = "UTF-8" )

cursos_agua <- spTransform(cursos_agua, CRS("+proj=longlat +datum=WGS84 +no_defs"))

cursos_agua <- leaflet() %>% #Llamamos a leaflet
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolylines(data = cursos_agua)
cursos_agua

####CUERPOS AGUA####
cuerpos_agua <- readOGR("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/cuerpos-agua/shapefile/cuerpos-agua.shp", encoding = "UTF-8" )

cuerpos_agua <- spTransform(cuerpos_agua, CRS("+proj=longlat +datum=WGS84 +no_defs"))

cuerpos_agua <- leaflet(cuerpos_agua) %>% #Llamamos a leaflet
  addProviderTiles(providers$Esri.WorldImagery)%>% #Cargamos el mapa de fondo
  addPolylines(data = cuerpos_agua)
cuerpos_agua


cuerpos <-  readOGR("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/cuerpos-agua/shapefile/cuerpos-agua.shp", encoding = "UTF-8" )
leaflet() %>% #Llamamos a leaflet
  addProviderTiles(providers$Esri.WorldImagery) %>% #Cargamos el mapa de fondo
  addPolylines(data = cuerpos)
cuerpos

escuelas_csv <- read_csv("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/establecimientos-educativos.csv")
head(escuelas_csv)
escuelasmc <- filter(escuelas_csv, partido== "Mar Chiquita")


#establecimientos educativos
escuelas <- readOGR("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/establecimientos-educativos/shapefile/establecimientos-educativos.shp",  use_iconv=TRUE, encoding = "UTF-8") # use_iconv=TRUE, encoding = "UTF-8"
escuelas <- spTransform(escuelas, CRS("+proj=longlat +datum=WGS84 +no_defs"))
#Para ver qué datos tiene y poder filtrar por Mar Chiquita
head(escuelas)
#Filtro por Mar Chiquita 
mchiq_escuelas_shp <- escuelas %>%
  subset(partido == "Mar Chiquita")

#MAPA ESCUELAS TOTAL 
escuelas_mchiq_mapa <- leaflet (mchiq_escuelas_shp, options = leafletOptions(minZoom = 5, maxZoom = 25)) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%  #addTiles() nos da la capa base del mapa. si lo dejamos vacío agrega tiles default de OpenStreetMap
  #Agregamos marcadores
  addMarkers(label = ~nombre.est) %>%
  addMarkers(lng=-57.582514,  lat=-37.408598, label="Quinta esencia, Escuela de Sustentabilidad",)
  #addCircles(radius=200, color="green")%>% #verde para zona de exclusión
  #addCircles(radius=500, color = "orange") #naranja para zona de amortiguamiento
escuelas_mchiq_mapa

#MAPA ESCUELAS TOTAL RADIO ACTUAL foto satelital
escuelas_mchiq_mapa <- leaflet (mchiq_escuelas_shp, options = leafletOptions(minZoom = 5, maxZoom = 15)) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%  #addTiles() nos da la capa base del mapa. si lo dejamos vacío agrega tiles default de OpenStreetMap
  addMarkers(label = ~nombre.est)%>%
  addCircles(radius=200, color="green")%>% #verde para zona de exclusión
  #Marcamos la escuela de Amartya
  addCircles(radius=500, color = "orange")%>% #naranja para zona de amortiguamiento
  addMarkers(lng=-57.582514,  lat=-37.408598, label="Quinta esencia, Escuela de Sustentabilidad",)%>%
  addCircles(lng=-57.582514,  lat=-37.408598,radius = 200, color = "red")%>%
  addCircles(lng=-57.582514,  lat=-37.408598,radius = 500, color = "blue")#naranja para zona de amortiguamiento
escuelas_mchiq_mapa

#MAPA ESCUELAS TOTAL RADIO DESEADO foto satelital
escuelas_mchiq_mapa <- leaflet (mchiq_escuelas_shp, options = leafletOptions(minZoom = 5, maxZoom = 15)) %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%  #addTiles() nos da la capa base del mapa. si lo dejamos vacío agrega tiles default de OpenStreetMap
  addMarkers(label = ~nombre.est)%>%
  addCircles(radius=500, color="green")%>% #verde para zona de exclusión
  #Marcamos la escuela de Amartya
  addCircles(radius=750, color = "orange")%>% #naranja para zona de amortiguamiento
  addMarkers(lng=-57.582514,  lat=-37.408598, label="Quinta esencia, Escuela de Sustentabilidad",)%>%
  addCircles(lng=-57.582514,  lat=-37.408598,radius = 500, color = "red")%>%
  addCircles(lng=-57.582514,  lat=-37.408598,radius = 750, color = "blue")#naranja para zona de amortiguamiento
escuelas_mchiq_mapa


#reformulo csv solo de escuelas de partido == "Mar Chiquita" - mandado a powa
escuelaspba_csv <- read_csv("/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/establecimientos-educativos.csv")
head(escuelaspba_csv) 
escuelas_mc <- filter (escuelaspba_csv, partido == "Mar Chiquita")
#hago csv: data, path, y .csv al final para q se convierta
write.csv2(escuelas_mc, "/Users/luisavicentin/1er cuatri 2021/LAB/Proyecto Amartya/Escuelas_mc.csv")

