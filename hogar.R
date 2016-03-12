# Librerias necesarias ----
#install.packages('curl')
#install.packages('xlsx')
library('curl')
library('xlsx')

# Seleccionar google_api.R en su sistema de archivos
source('google_api.R')

hogaresData <- read.xlsx("hogares.xlsx", sheetIndex = 1, endRow = 104, header = TRUE, encoding = 'UTF-8')

destino = "Piazzale Aldo Moro"

# Colocar su API Key 
api_key = "AIzaSyCeqh_fldrB_XU2cEp6qSTrtaZZm0OYC-8"

hogaresData$Dirección <- gsub("\n", " ", hogaresData$Dirección)
hogaresData$Distrito <- gsub("\n", " ", hogaresData$Distrito)

hogaresData["Duracion"] <- 0
for (i in 1:nrow(hogaresData)) {
  origen <- c(hogaresData$Dirección[i],hogaresData$Distrito[i])
  api_url = get_url(origen, destino, api_key)
  datos = get_data(api_url)
  datosParse = parse_data(datos)
  print(datosParse)
  if(length(datosParse$duration$value) > 0){
    hogaresData$Duracion[i] <- datosParse$duration$value
  }
}

hogaresData$Duracion = ceiling(hogaresData$Duracion / 60)

#Se busco en internet respecto a esta direcciones y fueron corregidas para poder obtener un resultado para duracion utilizando el API de Google
#hogaresData$Dirección[33] = "Via San Roberto Bellarmino"
#hogaresData$Distrito[33] = "Ardeatino"
#origen <- c(hogaresData$Dirección[33], hogaresData$Distrito[33])
#api_url = get_url(origen, destino, api_key)
#datos = get_data(api_url)
#datosParse = parse_data(datos)
#hogaresData$Duracion[33] <- datosParse$duration$value[1]

#origen <- c(hogaresData$Distrito[11])
#api_url = get_url(origen, destino, api_key)
#datos = get_data(api_url)
#datosParse = parse_data(datos)
#hogaresData$Duracion[11] <- datosParse$duration$value[1]

#origen <- c(hogaresData$Distrito[61])
#api_url = get_url(origen, destino, api_key)
#datos = get_data(api_url)
#datosParse = parse_data(datos)
#hogaresData$Duracion[61] <- datosParse$duration$value[1]





#Foto y Piso se eliminan por no proporcionar data relevante para el problema y los otros datos preprocesados se pasan a nu
hogaresData$Foto <- NULL
hogaresData$Piso <- NULL

#Tipo de Inmueble
hogaresData$Tipo.de.Inmueble = as.character(hogaresData$Tipo.de.Inmueble)
hogaresData$Tipo.de.Inmueble[grepl("min", hogaresData$Tipo.de.Inmueble, ignore.case = TRUE)] <- 0 
hogaresData$Tipo.de.Inmueble[grepl("monolocale", hogaresData$Tipo.de.Inmueble, ignore.case = TRUE)] <- 1
hogaresData$Tipo.de.Inmueble[grepl("ap", hogaresData$Tipo.de.Inmueble, ignore.case = TRUE)] <- 2


#Notas se categoriza y se cambia el nombre por uno que tenga mas sentido de acuerdo a la data la columna
hogaresData$Notas = as.character(hogaresData$Notas)
hogaresData$Notas[grepl("ragazzi/ragazze | ragazze/ragazzi | ragazze/i | ragazzi/e", hogaresData$Notas)] <- 2 #Ambos
hogaresData$Notas[grepl("ragazze", hogaresData$Notas, ignore.case = TRUE)] <- 1 #Mujeres
hogaresData$Notas[grepl("ragazzi", hogaresData$Notas, ignore.case = TRUE)] <- 0 #Hombre
hogaresData$Notas[39] <- 2 #Como no se especifica se asume que puede ser indiferente el sexo
colnames(hogaresData)[colnames(hogaresData) == "Notas"] <- "Sexo"

#Nueva columna Tutto.Incluso
hogaresData["Todo.Incluido"] <- NA
hogaresData$Todo.Incluido[grepl("spese", hogaresData$Precio.Mensual, ignore.case = TRUE)] <- 0
hogaresData$Todo.Incluido[!grepl("spese", hogaresData$Precio.Mensual, ignore.case = TRUE)] <- 1


# Descripcion
hogaresData["Entradas"] <- 0
hogaresData["Cocinas"] <- 0
hogaresData["Cuartos"] <- 0
hogaresData["Baños"] <- 0
hogaresData["Salas"] <- 0


hogaresData$Entradas[grepl("ing", hogaresData$Descripción, ignore.case = TRUE)] <- 1
hogaresData$Cocinas[grepl("cuc", hogaresData$Descripción, ignore.case = TRUE)] <- 1
hogaresData$Cocinas[grepl("cottura", hogaresData$Descripción, ignore.case = TRUE)] <- 1

hogaresData$Cuartos[grepl("camer", hogaresData$Descripción, ignore.case = TRUE)] <- 1
hogaresData$Cuartos[grepl("2 camer", hogaresData$Descripción, ignore.case = TRUE)] <- 2
hogaresData$Cuartos[grepl("3 camer", hogaresData$Descripción, ignore.case = TRUE)] <- 3
hogaresData$Cuartos[grepl("tre camer", hogaresData$Descripción, ignore.case = TRUE)] <- 3
hogaresData$Cuartos[grepl("4 camer", hogaresData$Descripción, ignore.case = TRUE)] <- 4
hogaresData$Cuartos[grepl("4 stanze", hogaresData$Descripción, ignore.case = TRUE)] <- 4

hogaresData$Baños[grepl("bag", hogaresData$Descripción, ignore.case = TRUE)] <- 1
hogaresData$Baños[grepl("2 bag", hogaresData$Descripción, ignore.case = TRUE)] <- 2
hogaresData$Baños[grepl("2bag", hogaresData$Descripción, ignore.case = TRUE)] <- 2
hogaresData$Baños[grepl("3 bag", hogaresData$Descripción, ignore.case = TRUE)] <- 3
hogaresData$Baños[grepl("4 bag", hogaresData$Descripción, ignore.case = TRUE)] <- 4


hogaresData$Salas[grepl("soggio", hogaresData$Descripción, ignore.case = TRUE)] <- 1
hogaresData$Salas[grepl("sal", hogaresData$Descripción, ignore.case = TRUE)] <- 1

hogaresData$Descripción <- NULL


#Precio mensual se elimina todo y se queda con los montos unicamente

hogaresData$Precio.Mensual = as.character(hogaresData$Precio.Mensual)
for (i in 1:nrow(hogaresData)) {
  hogaresData$Precio.Mensual[i] = gsub(";","",hogaresData$Precio.Mensual[i])
  hogaresData$Precio.Mensual[i] = gsub("[A-Za-z]","",hogaresData$Precio.Mensual[i])
  if (i != 5){
    hogaresData$Precio.Mensual[i] = gsub(",","",hogaresData$Precio.Mensual[i])
  }
  hogaresData$Precio.Mensual[i] = gsub(" ","",hogaresData$Precio.Mensual[i])
  hogaresData$Precio.Mensual[i] = gsub("\\n","",hogaresData$Precio.Mensual[i])
  
  hogaresData$Precio.Mensual[i] = gsub("\\D"," ",hogaresData$Precio.Mensual[i])
  if (i == 5){
    hogaresData$Precio.Mensual[i] = gsub("55 00","",hogaresData$Precio.Mensual[i])
  }
}

#Habitacion disponible y precio

hogaresData$Habitaciones.Disponibles = as.character(hogaresData$Habitaciones.Disponibles)
hogaresData["Tipo.Habitacion"] <- NA
hogaresData["Precio"] <- 0
hogaresData$Tipo.Habitacion[grepl("singol", hogaresData$Habitaciones.Disponibles, ignore.case = TRUE)] <- 0
hogaresData$Tipo.Habitacion[grepl("doppi", hogaresData$Habitaciones.Disponibles, ignore.case = TRUE)] <- 1
hogaresData$Tipo.Habitacion[grepl("posto", hogaresData$Habitaciones.Disponibles, ignore.case = TRUE)] <- 2
hogaresData$Tipo.Habitacion[grepl("app", hogaresData$Habitaciones.Disponibles, ignore.case = TRUE)] <- 3
hogaresData$Tipo.Habitacion[grepl("mon", hogaresData$Habitaciones.Disponibles, ignore.case = TRUE)] <- 3


for (i in 1:nrow(hogaresData)) {
  hab = substr(hogaresData$Habitaciones.Disponibles[i], 1, 4)
  hab2 = substr(hogaresData$Habitaciones.Disponibles[i], 12, 16)
  aux = strsplit(hogaresData$Precio.Mensual[i], " ")
  aux2 = aux[[1]]
  size = length(aux2) - 1
  if(size == 1 || i == 74){
    hogaresData$Precio[i] = aux2[2]
  }else if(size == 2 || i == 18){
    hogaresData$Precio[i] = aux2[2]
    newRow <- hogaresData[i, ]
    if(hab == "1 do"){
      hogaresData$Tipo.Habitacion[i] = 1
      newRow$Tipo.Habitacion = 0
    }else if(hab == "1 si" || hab == "1 Si"){
      hogaresData$Tipo.Habitacion[i] = 0
      if(hab2 == "1 pos"){
        newRow$Tipo.Habitacion = 2
      }else{
        newRow$Tipo.Habitacion = 1
      }
    }
    newRow$Precio = aux2[3]
    hogaresData <- rbind(hogaresData, newRow)
  }else if(size == 3){
    hogaresData$Precio[i] = aux2[2]
    hogaresData$Tipo.Habitacion[i] = 0
    newRow <- hogaresData[i, ]
    newRow$Precio = aux2[3]
    newRow$Tipo.Habitacion = 0
    hogaresData <- rbind(hogaresData, newRow)
    newRow <- hogaresData[i, ]
    if(hab == "2 si"){
      newRow$Tipo.Habitacion = 1
    }else{
      newRow$Tipo.Habitacion = 0
    }
    newRow$Precio = aux2[4]
    hogaresData <- rbind(hogaresData, newRow)
  }else if(size == 4){
    hogaresData$Precio[i] = aux2[2]
    newRow <- hogaresData[i, ]
    newRow$Precio = aux2[3] 
    hogaresData <- rbind(hogaresData, newRow)
    newRow <- hogaresData[i, ]
    newRow$Precio = aux2[4]
    newRow <- hogaresData[i, ]
    newRow$Precio = aux2[5]
    hogaresData <- rbind(hogaresData, newRow)
  }
}

hogaresData$Habitaciones.Disponibles <- NULL
hogaresData$Precio.Mensual <- NULL
hogaresData$Tipo.de.Inmueble <- as.numeric(hogaresData$Tipo.de.Inmueble)
hogaresData$Sexo <- as.numeric(hogaresData$Sexo)
hogaresData$Precio <- as.numeric(hogaresData$Precio)
hogaresData$Tipo.de.Inmueble



dataHombres = hogaresData[hogaresData$Sexo != 1, ]
dataMujeres = hogaresData[hogaresData$Sexo != 0, ]

modelo1=lm(dataHombres$Precio~dataHombres$Tipo.de.Inmueble)
plot(dataHombres$Tipo.de.Inmueble,dataHombres$Precio)
abline(modelo1)
