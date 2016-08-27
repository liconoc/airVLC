require("geosphere")
require("randomForest")
require("ggmap")
require("animation")
require("sp")
require("geojsonio")
require("jsonlite")

setwd("C:/Users/Lidia/Dropbox/DSIC/airVLC/airVLC_web_vTFM")

hora_sistema<-format(Sys.time(), "%H")

#Coordenadas estaciones de medición de contaminación
coordenadas_estaciones_contaminacion<-matrix(ncol = 5)
colnames(coordenadas_estaciones_contaminacion) = c("id_estacion", "nombre", "nombre_corto", "latitud", "longitud")
coordenadas_estaciones_contaminacion<-as.data.frame(coordenadas_estaciones_contaminacion)
coordenadas_estaciones_contaminacion[1,]<-c("1","Moli del Sol","Moli","39.4811","-0.4087")
coordenadas_estaciones_contaminacion[2,]<-c("2","Pista de Silla","Pista","39.45806","-0.37664")
coordenadas_estaciones_contaminacion[3,]<-c("3","Avda. Francia","Francia","39.45752","-0.34266")
coordenadas_estaciones_contaminacion[4,]<-c("4","Viveros","Viveros","39.47964","-0.36964")
coordenadas_estaciones_contaminacion[5,]<-c("5","Bulevar Sur","Bulevar","39.45039","-0.39633")
coordenadas_estaciones_contaminacion[6,]<-c("6","Universidad Politécnica","UPV","39.47964","-0.3374")

#Contaminantes #Prueba: Solo los 4 contaminantes iniciales
contaminantes<-c("NO","NO2","O3","SO2")

#Tabla de predicciones de contaminantes por estación
prediccion_contaminantes <- matrix(ncol = dim(coordenadas_estaciones_contaminacion)[1], nrow = length(contaminantes))
colnames(prediccion_contaminantes)=c(coordenadas_estaciones_contaminacion$nombre_corto)
rownames(prediccion_contaminantes)=c(contaminantes)

#################################################################
################### 1. Descarga de datos ########################
#################################################################

#Datos tráfico
#Hay puntos de tráfico duplicados, se eliminan
#X = longitud, Y = latitud
data_trafico<-read.csv("http://mapas.valencia.es/lanzadera/opendata/tra_espiras_p/CSV",sep=";")
data_trafico<-data_trafico[complete.cases(data_trafico),]
data_trafico<-data_trafico[!duplicated(data_trafico$idpm),]

#Datos meteorológicos
#Tiene tres lineas vacías al principio, hay que saltarlas
#La velocidad del viento viene en km/h, necesitamos m/s
#La dirección del viento es un string, necesitamos los grados
data_clima<-readLines("http://www.aemet.es/es/eltiempo/observacion/ultimosdatos_8416Y_datos-horarios.csv?k=val&l=8416Y&datos=det&w=0&f=temperatura&x=h24")
data_clima<-iconv(data_clima, "latin1", "ASCII", sub="")
write(file="tmp.csv",data_clima)
data_clima<-read.csv("tmp.csv", sep=",", skip = 3)
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Calma"] <- NA
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Norte"] <- 0
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Nordeste"] <- 45
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Este"] <- 90
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Sudeste"] <- 135
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Sur"] <- 180
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Sudoeste"] <- 225
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Oeste"] <- 270
levels(data_clima$Direccin.del.viento)[levels(data_clima$Direccin.del.viento)=="Noroeste"] <- 315
data_clima$Direccin.del.viento<-as.numeric(as.character(data_clima$Direccin.del.viento))
data_clima$Velocidad.del.viento..km.h.<-data_clima$Velocidad.del.viento..km.h.*0.277777777778

#Necesito temperatura, humedad, precipitaciones, presión, velocidad del viento y dirección del viento de AHORA
fecha<-format(Sys.time(), "%d/%m/%Y %H:00")
wday<- as.POSIXlt(Sys.time())$wday
dia<-strsplit(strsplit(fecha," ")[[1]][1],"/")[[1]][1]
mes<-strsplit(strsplit(fecha," ")[[1]][1],"/")[[1]][2]
anyo<-strsplit(strsplit(fecha," ")[[1]][1],"/")[[1]][3]
hora<-as.numeric(strsplit(strsplit(fecha," ")[[1]][2],":")[[1]][1])

if(dim(data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),])[1]==0){
  #Si no hay fila, el dato de esa hora aun no se ha publicado, cojo una hora antes
  fecha<-format(Sys.time()-3600, "%d/%m/%Y %H:00")
  wday<- as.POSIXlt(Sys.time()-3600)$wday
}

temperatura<-data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),2]
velocidadViento<-data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),3]
dirViento<-data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),4]
dirViento_original<-dirViento
precipitacion<-data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),7]
presion<-data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),8]
if(is.na(presion)) presion<-1000 #Solo mientras use AEMET
humedad<-data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),10]

#Tabla test para la predicción
load("ficheros/modelos/test.RData")
test<-test[1,]

#Guardamos clima y tráfico
write.csv(data_clima[which(data_clima$Fecha.y.hora.oficial==fecha),], file = paste("ficheros/datos/",hora_sistema,"/clima.csv", sep = ""))
write.csv(data_trafico, file = paste("ficheros/datos/",hora_sistema,"/trafico.csv", sep = ""))

#################################################################
##### 2. Predicción de cada contaminante en las estaciones ######
#################################################################

# Espiras cercanas a cada estación, para el método wdir
espiras_1km<-read.csv("ficheros/espiras_1km.csv")

for(e in 1:dim(coordenadas_estaciones_contaminacion)[1]){
  id<-coordenadas_estaciones_contaminacion[e,"id"]
  nombre_corto<-coordenadas_estaciones_contaminacion[e,"nombre_corto"]
  coordenadas_estacion<-as.numeric(c(coordenadas_estaciones_contaminacion[which(coordenadas_estaciones_contaminacion$id==id),"latitud"],coordenadas_estaciones_contaminacion[which(coordenadas_estaciones_contaminacion$id==id),"longitud"]))

  espiras_estacion<-espiras_1km[which(espiras_1km$estacion==nombre_corto),]
  puntos_cercanos<-as.numeric(espiras_1km$idpm)
  
  #Datos tráfico para la estación para la versión wdir
  data_trafico_estacion<-data_trafico[which(data_trafico$idpm %in% puntos_cercanos),]
  
  #Si existe direccion del viento, la transformo y cojo los puntos dentro de rango
  if(!(is.na(dirViento))){
    if(dirViento>=0&dirViento<=270){
      dirViento<-270-dirViento
    }else{
      dirViento<-360+(270-dirViento)
    }
    
    #Rangos de viento
    if(dirViento>=0&dirViento<=30){
      puntos_dentro<-as.numeric(espiras_1km$punto_medida[which((espiras_1km$direccion>=0&espiras_1km$direccion<=dirViento+30)|(espiras_1km$direccion>=360+(dirViento-30)&espiras_1km$direccion<=359))]) 
    }else if(dirViento>=330&dirViento<=359){
      puntos_dentro<-as.numeric(espiras_1km$punto_medida[which((espiras_1km$direccion>=0&espiras_1km$direccion<=30-(360-dirViento)|(espiras_1km$direccion>=dirViento-30&espiras_1km$direccion<=359)))])
    }else{
      puntos_dentro<-as.numeric(espiras_1km$punto_medida[which(espiras_1km$direccion>=dirViento-30&espiras_1km$direccion<=dirViento+30)])
    }
    
    }else puntos_dentro<-c()
    
  puntos_fuera<-setdiff(puntos_cercanos, puntos_dentro)
  
  #Parametros para los pesos
  N<-length(puntos_cercanos)
  alpha<-1.5
  k<-length(puntos_dentro)
  x<-(N-(alpha*k))/(N-k)
  
  #Pesos y trafico
  peso_dentro<-alpha/N
  peso_fuera<-x/N
  data_trafico_estacion_dentro<-data_trafico[which(data_trafico$idpm %in% puntos_dentro),]
  data_trafico_estacion_fuera<-data_trafico[which(data_trafico$idpm %in% puntos_fuera),]
  trafico<-as.integer(sum(data_trafico_estacion_dentro$ih*peso_dentro,data_trafico_estacion_fuera$ih*peso_fuera))
  

  #Para cada contaminante, cargo modelo y lo aplico con los datos de esa hora
  for (ij in 1:length(contaminantes)){
    objs<-contaminantes
    obj<-objs[ij]
    load(paste("ficheros/modelos/modelo_rf_wdir_",nombre_corto,"_",obj,".RData",sep = ""))
    
    names(test)[10]<-obj
    test[1,]<-c(anyo, mes, dia, hora, velocidadViento, temperatura, humedad, presion, precipitacion, 0, trafico, wday)
    test$Anyo<-as.numeric(test$Anyo)
    test$Mes<-as.numeric(test$Mes)
    test$Dia<-as.numeric(test$Dia)
    test$Hora<-as.numeric(test$Hora)
    test$VelocidadViento<-as.numeric(test$VelocidadViento)
    test$Temperatura<-as.numeric(test$Temperatura)
    test$Humedad<-as.numeric(test$Humedad)
    test$Presion<-as.numeric(test$Presion)
    test$Precipitacion<-as.numeric(test$Precipitacion)
    test$Trafico<-as.numeric(test$Trafico)
    test$wday<-as.numeric(test$wday)
    preds<-predict(m, newdata = test)
    prediccion_contaminantes[obj,coordenadas_estaciones_contaminacion[which(coordenadas_estaciones_contaminacion$id==e),"nombre_corto"]]<-preds
    
  }
}

write.csv(t(prediccion_contaminantes), file = paste("ficheros/datos/",hora_sistema,"/prediccion.csv", sep = ""))

#################################################################
############## 3. Extrapolación a toda la ciudad ################
#################################################################

#Coordenadas de las estaciones
latitud_e<-coordenadas_estaciones_contaminacion[,"latitud"]
longitud_e<-coordenadas_estaciones_contaminacion[,"longitud"]

################################ 2.1 Extrapolación mediante poligonos (para la web) ##################################

#Tamaño y distancia de los polígonos
tam<-0.005
longitud<--0.42
longitud_dest<--0.32-tam
latitud<-39.43
latitud_dest<-39.50-tam
poligonos<-c()
lista<-list()
i=1

while(latitud<=latitud_dest){
  while(longitud<=longitud_dest){
    assign(paste("poly_",i,sep = ""), Polygons(list(Polygon(cbind(c(latitud, latitud, latitud+tam, latitud+tam),c(longitud, longitud+tam, longitud+tam, longitud)))), i))
    longitud<-longitud+tam
    lista<-c(lista,get(paste("poly_",i,sep = "")))
    i<-i+1
  }
  latitud<-latitud+tam
  longitud<--0.42
}

sp_poly <- SpatialPolygons(lista)
json<-geojson_json(sp_poly)
jsonP<-fromJSON(json)

#Aplico Wind Sensitive LIDW
for(contaminante in contaminantes){
  for(p in 1:nrow(jsonP$features)){
    latitud<-jsonP$features[p,]$geometry$coordinates[[1]][1]-0.0025
    longitud<-jsonP$features[p,]$geometry$coordinates[[1]][6]-0.0025
    
    #Matriz de direcciones
    DirDist <- matrix(nrow = 2, ncol = 6, byrow = TRUE, dimnames = list(c("Distancia", "Direccion"), 1:6))
    for(i in 1:6){
      DirDist["Direccion",i]<-bearingRhumb(c(latitud,longitud),c(as.numeric(latitud_e[i]),as.numeric(longitud_e[i])))
      DirDist["Distancia",i]<-distHaversine(c(latitud,longitud),c(as.numeric(latitud_e[i]),as.numeric(longitud_e[i])))
    }
    
    if(!(is.na(dirViento))){
      if(dirViento>=0&dirViento<=270){
        dirViento<-270-dirViento
      }else{
        dirViento<-360+(270-dirViento)
      }}
    
    #Estaciones que caen dentro y fuera del angulo del viento
    if(!(is.na(dirViento))){
      puntos_dentro<-c()
      if(dirViento>=0&dirViento<=30){
        for(i in 1:dim(coordenadas_estaciones_contaminacion)[1]){
          if((DirDist["Direccion",i]>=0&DirDist["Direccion",i]<=dirViento+30)|(DirDist["Direccion",i]>=360+(dirViento-30)&DirDist["Direccion",i]<=359)) puntos_dentro<-union(puntos_dentro,i)
        }
      }else if(dirViento>=330&dirViento<=359){
        for(i in 1:dim(coordenadas_estaciones_contaminacion)[1]){
          if((DirDist["Direccion",i]>=0&DirDist["Direccion",i]<=30-(360-dirViento))|(DirDist["Direccion",i]>=dirViento-30&DirDist["Direccion",i]<=dirViento+30)) puntos_dentro<-union(puntos_dentro,i)
        }
      }else{
        for(i in 1:dim(coordenadas_estaciones_contaminacion)[1]){
          if((DirDist["Direccion",i]>=dirViento-30&DirDist["Direccion",i]<=dirViento+30)) puntos_dentro<-union(puntos_dentro,i)
        }
      }
    }else puntos_dentro<-c()
    
    puntos_fuera<-setdiff(1:6, puntos_dentro)
    SumaDentro<-0
    SumaFuera<-0
    
    #Pesos  
    N<-dim(coordenadas_estaciones_contaminacion)[1]
    alpha<-1.5
    k<-length(puntos_dentro)
    x<-(N-(alpha*k))/(N-k)
    peso_dentro<-(alpha/N)*k
    peso_fuera<-(x/N)*(N-k)
    
    for(d in 1:length(puntos_dentro)){
      SumaDentro<-SumaDentro+DirDist["Distancia",d]
    }
    for(d in 1:length(puntos_fuera)){
      SumaFuera<-SumaFuera+DirDist["Distancia",d]
    }
    
    nuevo_obj<-0
    
    if(length(puntos_dentro)==1){
      #1punto dentro
      nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_dentro[1]]*1*peso_dentro,na.rm = TRUE)
      for(punto in 1:length(puntos_fuera)){
        W<-(SumaFuera-DirDist["Distancia",punto])/(SumaFuera*(length(puntos_fuera)-1))
        nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_fuera[punto]]*W*peso_fuera,na.rm = TRUE)
      }
      
    }else if(length(puntos_fuera)==1){
      #1punto fuera
      nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_fuera[1]]*1*peso_fuera,na.rm = TRUE)
      for(punto in 1:length(puntos_dentro)){
        W<-(SumaDentro-DirDist["Distancia",punto])/(SumaDentro*(length(puntos_dentro)-1))
        nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_dentro[punto]]*W*peso_dentro,na.rm = TRUE)
      }
      
    }else{
      #repartidos o todos en uno
      if(length(puntos_fuera)!=0){
        for(punto in 1:length(puntos_fuera)){
          W<-(SumaFuera-DirDist["Distancia",punto])/(SumaFuera*(length(puntos_fuera)-1))
          nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_fuera[punto]]*W*peso_fuera,na.rm = TRUE)
        }
      }
      if(length(puntos_dentro)!=0){
        for(punto in 1:length(puntos_dentro)){
          W<-(SumaDentro-DirDist["Distancia",punto])/(SumaDentro*(length(puntos_dentro)-1))
          nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_dentro[punto]]*W*peso_dentro,na.rm = TRUE)
          
        }
      }
    }

    jsonP$features$properties$dummy[p]<-nuevo_obj
  }
  geojson_write(toJSON(jsonP), file=paste("ficheros/datos/",hora_sistema,"/grid_",contaminante,".geojson", sep = ""))
}


################################ 2.2 Extrapolación mediante grid de celdas pequeñas (0.001) (para mapa densidad) ###########################

#Mapa Valencia
Valencia <- get_map(location=c(lon=-0.3783341, lat=39.4732093), zoom=14, maptype="terrain", language="es-ES")

#Para cada punto del grid calcular los contaminantes
grid_png<-expand.grid(x=seq(from=39.40, to=39.55, by=0.001), y=seq(from=-0.45, to=-0.30, by=0.001))
colnames(grid_png)<-c("latitud", "longitud")

extrapolacion<-matrix(nrow=nrow(grid_png), ncol = 4)
colnames(extrapolacion)<-contaminantes

#Aplico Wind Sensitive LIDW
for(contaminante in contaminantes){
  
  for(p in 1:nrow(grid_png)){
    latitud<-as.numeric(grid_png[p,1])
    longitud<-as.numeric(grid_png[p,2])
    
    #Matriz de direcciones
    DirDist <- matrix(nrow = 2, ncol = 6, byrow = TRUE, dimnames = list(c("Distancia", "Direccion"), 1:6))
    for(i in 1:6){
      DirDist["Direccion",i]<-bearingRhumb(c(latitud,longitud),c(as.numeric(latitud_e[i]),as.numeric(longitud_e[i])))
      DirDist["Distancia",i]<-distHaversine(c(latitud,longitud),c(as.numeric(latitud_e[i]),as.numeric(longitud_e[i])))
    }
    
    if(!(is.na(dirViento))){
      if(dirViento>=0&dirViento<=270){
        dirViento<-270-dirViento
      }else{
        dirViento<-360+(270-dirViento)
      }}
    

    #Estaciones que caen dentro y fuera del angulo del viento (según el punto de tráfico)
    if(!(is.na(dirViento))){
      puntos_dentro<-c()
      if(dirViento>=0&dirViento<=30){
        for(i in 1:dim(coordenadas_estaciones_contaminacion)[1]){
          if((DirDist["Direccion",i]>=0&DirDist["Direccion",i]<=dirViento+30)|(DirDist["Direccion",i]>=360+(dirViento-30)&DirDist["Direccion",i]<=359)) puntos_dentro<-union(puntos_dentro,i)
        }
      }else if(dirViento>=330&dirViento<=359){
        for(i in 1:dim(coordenadas_estaciones_contaminacion)[1]){
          if((DirDist["Direccion",i]>=0&DirDist["Direccion",i]<=30-(360-dirViento))|(DirDist["Direccion",i]>=dirViento-30&DirDist["Direccion",i]<=dirViento+30)) puntos_dentro<-union(puntos_dentro,i)
        }
      }else{
        for(i in 1:dim(coordenadas_estaciones_contaminacion)[1]){
          if((DirDist["Direccion",i]>=dirViento-30&DirDist["Direccion",i]<=dirViento+30)) puntos_dentro<-union(puntos_dentro,i)
        }
      }
    }else puntos_dentro<-c()
    
    puntos_fuera<-setdiff(1:6, puntos_dentro)
    SumaDentro<-0
    SumaFuera<-0
    
    #Pesos  
    N<-dim(coordenadas_estaciones_contaminacion)[1]
    alpha<-1.5
    k<-length(puntos_dentro)
    x<-(N-(alpha*k))/(N-k)
    peso_dentro<-(alpha/N)*k
    peso_fuera<-(x/N)*(N-k)
    
    for(d in 1:length(puntos_dentro)){
      SumaDentro<-SumaDentro+DirDist["Distancia",d]
    }
    for(d in 1:length(puntos_fuera)){
      SumaFuera<-SumaFuera+DirDist["Distancia",d]
    }
    
    nuevo_obj<-0
    
    if(length(puntos_dentro)==1){
      #1punto dentro
      nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_dentro[1]]*1*peso_dentro,na.rm = TRUE)
      for(punto in 1:length(puntos_fuera)){
        W<-(SumaFuera-DirDist["Distancia",punto])/(SumaFuera*(length(puntos_fuera)-1))
        nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_fuera[punto]]*W*peso_fuera,na.rm = TRUE)
      }
      
    }else if(length(puntos_fuera)==1){
      #1punto fuera
      nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_fuera[1]]*1*peso_fuera,na.rm = TRUE)
      for(punto in 1:length(puntos_dentro)){
        W<-(SumaDentro-DirDist["Distancia",punto])/(SumaDentro*(length(puntos_dentro)-1))
        nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_dentro[punto]]*W*peso_dentro,na.rm = TRUE)
      }
      
    }else{
      #repartidos o todos en uno
      if(length(puntos_fuera)!=0){
        for(punto in 1:length(puntos_fuera)){
          W<-(SumaFuera-DirDist["Distancia",punto])/(SumaFuera*(length(puntos_fuera)-1))
          nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_fuera[punto]]*W*peso_fuera,na.rm = TRUE)
        }
      }
      if(length(puntos_dentro)!=0){
        for(punto in 1:length(puntos_dentro)){
          W<-(SumaDentro-DirDist["Distancia",punto])/(SumaDentro*(length(puntos_dentro)-1))
          nuevo_obj<-sum(nuevo_obj,prediccion_contaminantes[contaminante,puntos_dentro[punto]]*W*peso_dentro,na.rm = TRUE)
          
        }
      }
    }
    extrapolacion[p,contaminante]<-nuevo_obj
  }
  
}

#Creamos en png el mapa de cada contaminante
for(contaminante in contaminantes){
    valores<-matrix(ncol=3)
    valores<-cbind(as.numeric(grid_png$latitud),as.numeric(grid_png$longitud),as.numeric(extrapolacion[,contaminante]))
    
    coordenadas<-matrix(ncol=2)
    for(i in 1:dim(valores)[1]){
      val<-cbind(rnorm(valores[i,3], valores[i,1]+0.0005, 0.001/3),rnorm(valores[i,3], valores[i,2]+0.0005, 0.001/3))
      coordenadas<-rbind(coordenadas, val)
    }
    coordenadas<-as.data.frame(coordenadas)
    
    ggsave(file=paste("ficheros/datos/",hora_sistema,"/mapa_",contaminante,".png", sep = ""), width = 10, height = 10, units = "cm", 
           plot = ggmap(Valencia, extent = "device") + 
             stat_density2d(data = coordenadas,aes(x = V2, y = V1, fill = ..level.., alpha = 10), size = 10, bins = 8, geom = "polygon") + 
             scale_alpha(range = c(0, 0.6), guide = FALSE)+
             scale_fill_distiller(palette = "Spectral", trans = "reverse") +
             theme_nothing(legend = FALSE)) 
}
  

#Creamos un gif con las últimas 24h de los mapas de densidad
for(contaminante in contaminantes){
  hora<-hora_sistema
  horas<-24
  hora<-as.numeric(hora)-1
  if(hora<0) hora<-23
  lista_gif<-c()
  repeat{
    if(hora<10) hora<-paste("0",hora,sep="")
    if(length(Sys.glob(paste("ficheros/datos/",hora,"/mapa_",contaminante,".png",sep = "")))>0){
      lista_gif<-c(lista_gif,paste("ficheros/datos/",hora,"/mapa_",contaminante,".png",sep = ""))
    }
    horas<-horas-1
    hora<-as.numeric(hora)-1
    if(hora<0) hora<-23
    
    if(horas==0){
      im.convert(lista_gif, output = paste("ficheros/datos/mapa_evolucion_",contaminante,".gif",sep = ""))
      break
    }
  }
}


