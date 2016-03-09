# Librerias necesarias ----
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('RWeka')
#install.packages('class')
#install.packages('pROC')
library('rpart')
library('rpart.plot')
library('RWeka')
library('class')
library('pROC')

# Lectura de los datos ----
minable <- read.csv("C:/Users/soluciones/Desktop/Tarea2/minable.csv", header = TRUE)

# Eliminacion de Atributos irrelevantes ----
minable[,"cIdentidad"] <- NULL
minable[,"fNacimiento"] <- NULL
minable[,"tGrado"] <- NULL #Esta cursando Tesis o Pasantia de Grado
minable[,"jReprobadas"] <- NULL #Justificacion de Materias Reprobadas
minable[,"eCivil"] <- NULL
minable[,"dHabitacion"] <- NULL
minable[,"cDireccion"] <- NULL #Ha cambiado de direccion
minable[,"oSolicitudes"] <- NULL #Beneficios solicitados
minable[,"grOdontologicos"] <- NULL
minable[,"aEconomica"] <- NULL #Realiza actividad economica? En dado caso lo importante serian los ingresos
minable[,"rating"] <- NULL #Opinion de Becarios
minable[,"sugerencias"] <- NULL #Sugerencias para mejorar el servicio

#Correccion de la Data----
minable$pReside[2] = 7 #Dado que reside con esposo(a) e hijos (as) Se coloca en la categoria de otros
minable$pReside[26] = 7 #Dado que reside con esposo(a) e hijos (as) Se coloca en la categoria de otros
minable$grOtros[5] = 0 #Se coloca 0 a este outlier y en la linea de abajo se corrige
minable$grOtros[5] = mean(minable$grOtros) # Se utiliza la media de toda la columna para corregir este outlier
minable$pReside = as.integer(minable$pReside) # Si se intenta realizar alguna operacion numerica o logica retorna NA, por lo que se procede a pasarla a formato integer

#Estratificacion de las muestras de train y test-----
aux0 <- minable[minable$mIngreso == 0, ]
#aux1 <- minable[minable$mIngreso == 1, ]
aux2 <- minable[minable$mIngreso == 2, ]
aux3 <- minable[minable$mIngreso == 3, ]

muestra0 <- sample(nrow(aux0), nrow(aux0) * 0.8, replace = FALSE , prob=NULL)
train0 <- aux0[muestra0,]
test0 <- aux0[-muestra0,]
#muestras1 <- sample(nrow(aux1), ceiling(nrow(aux1) * 0.8), replace = FALSE , prob=NULL)
#train1 <- aux1[muestras1,]
#test1 <- aux1[-muestras1,]

muestra2 <- sample(nrow(aux2), nrow(aux2) * 0.8, replace = FALSE , prob=NULL)
train2 <- aux2[muestra2,]
test2 <- aux2[-muestra2,]

muestra3 <- sample(nrow(aux3), nrow(aux3) * 0.8, replace = FALSE , prob=NULL)
train3 <- aux3[muestra3,]
test3 <- aux3[-muestra3,]

finalTraining <- merge(merge(train0, train2, all = TRUE),train3, all = TRUE)
finalTesting <- merge(merge(test0, test2, all = TRUE),test3, all = TRUE)

#####################
#Arbol de decisiones#
#####################
arbolDecision <- rpart(mIngreso ~ ., data = finalTraining , method ="class")
rpart.plot(arbolDecision)
predictArbol <- predict(arbolDecision, finalTesting, type = "class")

matrizConfusionArbol <- table(finalTesting$mIngreso, predictArbol)
matrizConfusionArbol

#Accuracy, 
accuraccyArbol <- (matrizConfusionArbol[1,1] + matrizConfusionArbol[2,2] + matrizConfusionArbol[3,3]) / nrow(finalTesting)
accuraccyArbol

#Error
errorArbol <- 1 - accuraccyArbol
errorArbol

#ROC
rArbol <- roc(finalTesting$mIngreso, as.numeric(predictArbol), levels=c(0,2,3))
plot(rArbol)

#########################
#Reglas de Clasificacion#
#########################

trainClass <- finalTraining
testClass <- finalTesting

trainClass$mIngreso = as.factor(trainClass$mIngreso)
testClass$mIngreso = as.factor(testClass$mIngreso)

predictClass <- predict(OneR(formula = mIngreso ~ ., data = trainClass), testClass, type = "class")
matrizConfusionClass = table(testClass$mIngreso, predictClass)
matrizConfusionClass

#Accuracy, 
accuraccyClass <- (matrizConfusionClass[1,1] + matrizConfusionClass[2,2] + matrizConfusionClass[3,3]) / nrow(finalTesting)
accuraccyClass

#Error
errorClass <- 1 - accuraccyClass
errorClass

#ROC
rClass <- roc(finalTesting$mIngreso, as.numeric(predictClass), levels=c(0,2,3))
plot(rClass)

#######
# KNN #
#######

predictKnn <- knn(train = finalTraining, test = finalTesting, cl = finalTraining$mIngreso, k=14)
matrizConfusionKnn <- table(finalTesting$mIngreso, predictKnn)
matrizConfusionKnn

#Accuracy
accuraccyKnn <- (matrizConfusionKnn[1,1] + matrizConfusionKnn[2,2] + matrizConfusionKnn[3,3]) / nrow(finalTesting)
accuraccyKnn

#Error
errorKnn <- 1 - accuraccyKnn
errorKnn

#ROC
rKnn <- roc(finalTesting$mIngreso, as.numeric(predictKnn), levels=c(0,2,3))
plot(rKnn)
#PARTE I Finalizada

