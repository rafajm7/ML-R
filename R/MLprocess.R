
# Cargamos la base de datos
credit <- read.table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", header=F,
  sep=",", col.names=c('Genero','Edad','Deuda','EstadoCivil','Banco','NivelEducacion','Etnia','AniosTrabajo','IncumplimientoAnterior',
                       'Empleado','PuntajeCredito','LicenciaConductor','Ciudadano','CodigoPostal','Salario', 'Clase'),
  na.strings = "?")

# Visualizamos su estructura 
str(credit)

library(caret)
library(purrr)

# Vamos a mostrar un gráfico para ver cuántos individuos son de cada clase:
library(labeling)
ggplot(data = credit, aes(x = Clase, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")

# El porcetaje aproximado de individuos de cada clase lo vemos con este comando:
prop.table(table(credit$Clase)) %>% round(digits = 2)


# DISTRIBUCIÓN DE CADA VARIABLE NUMÉRICA
# Vemos a continuación la función de densidad de las variables numéricas en función de la clase
# Además observamos en el spineplot cómo están de relacionadas estas variables con la salida

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=credit[,2],y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ Edad,data=credit)

# Podemos ver que la distribución de la Edad está muy "skewed"
dens.edad<-density(credit$Edad,na.rm=T)
hist(credit$Edad, xlab="", main="Edad", ylim=c(0,max(dens.edad$y)*1.1),probability=T)
lines(dens.edad)
rug(jitter(credit$Edad))

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=credit[,3],y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ Deuda,data=credit)

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=credit$AniosTrabajo,y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ AniosTrabajo,data=credit)

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=log(credit$AniosTrabajo),y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ AniosTrabajo,data=credit)

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=credit$PuntajeCredito,y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ PuntajeCredito,data=credit)

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=credit$CodigoPostal,y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ CodigoPostal,data=credit)

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=credit$Salario,y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ Salario,data=credit)

escalas <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=log(credit$Salario + 1),y=credit[,16],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Clase ~ log(Salario+1),data=credit)



# DISTRIBUCIÓN DE LAS VARIABLES DISCRETAS 
# Para cada una de ellas, usando ggplot, vemos cuál es su relación con la clase.

ggplot(data = credit, aes(x = Empleado, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = IncumplimientoAnterior, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = Etnia, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = LicenciaConductor, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = Ciudadano, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = Genero, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = EstadoCivil, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = Banco, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


ggplot(data = credit, aes(x = NivelEducacion, y = ..count.., fill = Clase)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Clase") +
  theme_bw() +
  theme(legend.position = "bottom")


# ANÁLISIS MULTIVARIABLE
library(dplyr)
credit.logMod <- credit
credit.logMod$Salario <- log(credit.logMod$Salario +1)
credit.logMod <-  mutate(credit.logMod,Salario_grupo = case_when(Salario < 1  ~ "bajo",
                                                                 Salario > 1 & Salario <= 5  ~ "medio",
                                                                 Salario > 5 ~ "alto"))
credit.logMod$Salario_grupo <- as.factor(credit.logMod$Salario_grupo)
featurePlot(x=credit.logMod$CodigoPostal,y=credit.logMod[,17],plot="density", scales=escalas, auto.key = list(columns = 2))
spineplot(Salario_grupo ~ CodigoPostal,data=credit.logMod)

# SEPARACIÓN EN TRAIN Y TEST
credit.trainIdx<-readRDS("R/credit.trainIdx.rds")
credit.Datos.Train<-credit[credit.trainIdx,]
credit.Datos.Test<-credit[-credit.trainIdx,]


# PREPROCESADO DE DATOS

# Aplicamos el logaritmo al salario
credit.Datos.Train.logSal <- credit.Datos.Train
credit.Datos.Train.logSal$Salario <- log(credit.Datos.Train.logSal$Salario + 1)

# Eliminamos la variable CodigoPostal
credit.Datos.Train.logSal <-credit.Datos.Train.logSal[setdiff(names(credit.Datos.Train.logSal),"CodigoPostal")]


# VALORES ATÍPICOS
# Los observamos haciendo uso de bwplot, se puede observar que Salario ya no tiene outliers
library(reshape2)
bwplot(~value | variable, data = melt(credit.Datos.Train.logSal, id.vars = 15, measure.vars = c("Edad")))
bwplot(~value | variable, data = melt(credit.Datos.Train.logSal, id.vars = 15, measure.vars = c("Deuda")))
bwplot(~value | variable, data = melt(credit.Datos.Train.logSal, id.vars = 15, measure.vars = c("AniosTrabajo")))
bwplot(~value | variable, data = melt(credit.Datos.Train.logSal, id.vars = 15, measure.vars = c("Salario")))
bwplot(~value | variable, data = melt(credit.Datos.Train.logSal, id.vars = 15, measure.vars = c("PuntajeCredito")))

extremos.edad<-boxplot(credit.Datos.Train.logSal$Edad,boxwex=0.15,ylab="Edad")$out
extremos.deuda<-boxplot(credit.Datos.Train.logSal$Deuda,boxwex=0.15,ylab="Deuda")$out
extremos.aniosTrabajo<-boxplot(credit.Datos.Train.logSal$AniosTrabajo,boxwex=0.15,ylab="aniosTrabajo")$out
extremos.PuntajeCredito<-boxplot(credit.Datos.Train.logSal$PuntajeCredito,boxwex=0.15,ylab="PuntajeCredito")$out

# Calculamos el valor a partir del cual las muestras se consideran outliers para cada una de las cuatro variables
porArriba.edad<-min(extremos.edad[extremos.edad > median(na.omit(credit.Datos.Train.logSal$Edad))])
porArriba.deuda<-min(extremos.deuda[extremos.deuda > median(na.omit(credit.Datos.Train.logSal$Deuda))])
porArriba.aniosTrabajo<-min(extremos.aniosTrabajo[extremos.aniosTrabajo > median(na.omit(credit.Datos.Train.logSal$AniosTrabajo))])
porArriba.puntajeCredito<-min(extremos.PuntajeCredito[extremos.PuntajeCredito > median(na.omit(credit.Datos.Train.logSal$PuntajeCredito))])

# Visualizamos cuáles son las variables correspondientes a los valores
# que consideramos atípicos, basándonos en los gráficos de bwplot anteriores
na.omit(credit.Datos.Train.logSal[credit.Datos.Train.logSal$Edad >= porArriba.edad,]) 
na.omit(credit.Datos.Train.logSal[credit.Datos.Train.logSal$Deuda >= porArriba.deuda,]) 
na.omit(credit.Datos.Train.logSal[credit.Datos.Train.logSal$AniosTrabajo >= porArriba.aniosTrabajo,]) 
na.omit(credit.Datos.Train.logSal[credit.Datos.Train.logSal$PuntajeCredito > porArriba.puntajeCredito,])


# Decidimos eliminar solamente los outliers de la variable Edad
# Se crean dos nuevos conjuntos de datos
credit.Datos.Train.logSal.Out <- credit.Datos.Train.logSal
credit.Datos.Train.logSal.Nout <- credit.Datos.Train.logSal

credit.Datos.Train.logSal.Nout <- credit.Datos.Train.logSal.Nout[-which(credit.Datos.Train.logSal.Nout$Edad %in% extremos.edad),]

# ESTUDIO DE LOS NA's
# Vemos el número de NA's y cuántos corresponden a cada variable
sum(!complete.cases(credit.Datos.Train.logSal.Out))
map_dbl(credit.Datos.Train.logSal.Out, .f = function(x){sum(is.na(x))})

sum(!complete.cases(credit.Datos.Train.logSal.Nout))
map_dbl(credit.Datos.Train.logSal.Nout, .f = function(x){sum(is.na(x))})

# Creamos los nuevos subconjuntos
credit.Datos.Train.logSal.Out.TrNA <- credit.Datos.Train.logSal.Out
credit.Datos.Train.logSal.Nout.TrNA <- credit.Datos.Train.logSal.Nout


# Como la edad tiene una distribución skewed, sustituimos los NA's por el valor de la mediana
credit.Datos.Train.logSal.Out.TrNA[is.na(credit.Datos.Train.logSal.Out.TrNA$Edad),"Edad"] <-          
  median(credit.Datos.Train.logSal.Out.TrNA$Edad,na.rm=T)

credit.Datos.Train.logSal.Nout.TrNA[is.na(credit.Datos.Train.logSal.Nout.TrNA$Edad),"Edad"] <-          
  median(credit.Datos.Train.logSal.Nout.TrNA$Edad,na.rm=T)

sum(!complete.cases(credit.Datos.Train.logSal.Out.TrNA))
map_dbl(credit.Datos.Train.logSal.Out.TrNA, .f = function(x){sum(is.na(x))})

sum(!complete.cases(credit.Datos.Train.logSal.Nout.TrNA))
map_dbl(credit.Datos.Train.logSal.Nout.TrNA, .f = function(x){sum(is.na(x))})


# Como el resto de variables que tienen NA's son factores que no están tan compensadas como Género,
# para los NA's de esas variables vamos a introducir la moda.

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Lo aplicamos al conjunto de datos en el que la variable Edad tiene outliers

credit.Datos.Train.logSal.Out.TrNA[is.na(credit.Datos.Train.logSal.Out.TrNA$Genero),"Genero"] <-          
  Mode(credit.Datos.Train.logSal.Out.TrNA$Genero)

credit.Datos.Train.logSal.Out.TrNA[is.na(credit.Datos.Train.logSal.Out.TrNA$EstadoCivil),"EstadoCivil"] <-          
  Mode(credit.Datos.Train.logSal.Out.TrNA$EstadoCivil)

credit.Datos.Train.logSal.Out.TrNA[is.na(credit.Datos.Train.logSal.Out.TrNA$Banco),"Banco"] <-          
  Mode(credit.Datos.Train.logSal.Out.TrNA$Banco)

credit.Datos.Train.logSal.Out.TrNA[is.na(credit.Datos.Train.logSal.Out.TrNA$NivelEducacion),"NivelEducacion"] <-          
  Mode(credit.Datos.Train.logSal.Out.TrNA$NivelEducacion)

credit.Datos.Train.logSal.Out.TrNA[is.na(credit.Datos.Train.logSal.Out.TrNA$Etnia),"Etnia"] <-          
  Mode(credit.Datos.Train.logSal.Out.TrNA$Etnia)


# Lo aplicamos al conjunto de datos en el que la variable Edad NO tiene outliers

credit.Datos.Train.logSal.Nout.TrNA[is.na(credit.Datos.Train.logSal.Nout.TrNA$Genero),"Genero"] <-          
  Mode(credit.Datos.Train.logSal.Nout.TrNA$Genero)

credit.Datos.Train.logSal.Nout.TrNA[is.na(credit.Datos.Train.logSal.Nout.TrNA$EstadoCivil),"EstadoCivil"] <-          
  Mode(credit.Datos.Train.logSal.Nout.TrNA$EstadoCivil)

credit.Datos.Train.logSal.Nout.TrNA[is.na(credit.Datos.Train.logSal.Nout.TrNA$Banco),"Banco"] <-          
  Mode(credit.Datos.Train.logSal.Nout.TrNA$Banco)

credit.Datos.Train.logSal.Nout.TrNA[is.na(credit.Datos.Train.logSal.Nout.TrNA$NivelEducacion),"NivelEducacion"] <-          
  Mode(credit.Datos.Train.logSal.Nout.TrNA$NivelEducacion)

credit.Datos.Train.logSal.Nout.TrNA[is.na(credit.Datos.Train.logSal.Nout.TrNA$Etnia),"Etnia"] <-          
  Mode(credit.Datos.Train.logSal.Nout.TrNA$Etnia)



# Aplicamos a contrinuación el algoritmo knnImpute y generamos dos conjuntos de datos más
# Primero lo aplicamos a variables numéricas

ppknn.Out <- preProcess(credit.Datos.Train.logSal.Out, method = c("knnImpute"), na.remove = TRUE, 
                        k = 5, knnSummary = mean, outcome = NULL, fudge = .2, numUnique = 3)

credit.Datos.Train.logSal.Out.knn <- predict(ppknn.Out, credit.Datos.Train.logSal.Out)

ppknn.Nout <- preProcess(credit.Datos.Train.logSal.Nout, method = c("knnImpute"), na.remove = TRUE,
                         k = 5, knnSummary = mean, outcome = NULL, fudge = .2, numUnique = 3)

credit.Datos.Train.logSal.Nout.knn <- predict(ppknn.Nout, credit.Datos.Train.logSal.Nout)


# En este caso, para las variables discretas, hacemos lo mismo que antes, rellenamos con la moda

# Lo aplicamos al conjunto de datos en el que la variable Edad tiene outliers

credit.Datos.Train.logSal.Out.knn[is.na(credit.Datos.Train.logSal.Out.knn$Genero),"Genero"] <-          
  Mode(credit.Datos.Train.logSal.Out.knn$Genero)

credit.Datos.Train.logSal.Out.knn[is.na(credit.Datos.Train.logSal.Out.knn$EstadoCivil),"EstadoCivil"] <-          
  Mode(credit.Datos.Train.logSal.Out.knn$EstadoCivil)

credit.Datos.Train.logSal.Out.knn[is.na(credit.Datos.Train.logSal.Out.knn$Banco),"Banco"] <-          
  Mode(credit.Datos.Train.logSal.Out.knn$Banco)

credit.Datos.Train.logSal.Out.knn[is.na(credit.Datos.Train.logSal.Out.knn$NivelEducacion),"NivelEducacion"] <-          
  Mode(credit.Datos.Train.logSal.Out.knn$NivelEducacion)

credit.Datos.Train.logSal.Out.knn[is.na(credit.Datos.Train.logSal.Out.knn$Etnia),"Etnia"] <-          
  Mode(credit.Datos.Train.logSal.Out.knn$Etnia)


# Lo aplicamos al conjunto de datos en el que la variable Edad NO tiene outliers

credit.Datos.Train.logSal.Nout.knn[is.na(credit.Datos.Train.logSal.Nout.knn$Genero),"Genero"] <-          
  Mode(credit.Datos.Train.logSal.Nout.knn$Genero)

credit.Datos.Train.logSal.Nout.knn[is.na(credit.Datos.Train.logSal.Nout.knn$EstadoCivil),"EstadoCivil"] <-          
  Mode(credit.Datos.Train.logSal.Nout.knn$EstadoCivil)

credit.Datos.Train.logSal.Nout.knn[is.na(credit.Datos.Train.logSal.Nout.knn$Banco),"Banco"] <-          
  Mode(credit.Datos.Train.logSal.Nout.knn$Banco)

credit.Datos.Train.logSal.Nout.knn[is.na(credit.Datos.Train.logSal.Nout.knn$NivelEducacion),"NivelEducacion"] <-          
  Mode(credit.Datos.Train.logSal.Nout.knn$NivelEducacion)

credit.Datos.Train.logSal.Nout.knn[is.na(credit.Datos.Train.logSal.Nout.knn$Etnia),"Etnia"] <-          
  Mode(credit.Datos.Train.logSal.Nout.knn$Etnia)


# CORRELACIÓN DE VARIABLES 

corrNum.Out.TrNA <- sapply(credit.Datos.Train.logSal.Out.TrNA,FUN=as.numeric)
symnum(cor(corrNum.Out.TrNA[,1:14],use="complete.obs"))

corrNum.Out.knn <- sapply(credit.Datos.Train.logSal.Out.knn,FUN=as.numeric)
symnum(cor(corrNum.Out.knn[,1:14],use="complete.obs"))

corrNum.Nout.TrNA <- sapply(credit.Datos.Train.logSal.Nout.TrNA,FUN=as.numeric)
symnum(cor(corrNum.Nout.TrNA[,1:14],use="complete.obs"))

corrNum.Nout.knn <- sapply(credit.Datos.Train.logSal.Nout.knn,FUN=as.numeric)
symnum(cor(corrNum.Nout.knn[,1:14],use="complete.obs"))


# Como EstadoCivil y Banco tienen una correlación de 1, eliminamos una de ellas: EstadoCivil 
credit.Datos.Train.logSal.Out.TrNA.Corr<-credit.Datos.Train.logSal.Out.TrNA[setdiff(names(credit.Datos.Train.logSal.Out.TrNA),"EstadoCivil")]
credit.Datos.Train.logSal.Out.knn.Corr<-credit.Datos.Train.logSal.Out.knn[setdiff(names(credit.Datos.Train.logSal.Out.knn),"EstadoCivil")]
credit.Datos.Train.logSal.Nout.TrNA.Corr<-credit.Datos.Train.logSal.Nout.TrNA[setdiff(names(credit.Datos.Train.logSal.Nout.TrNA),"EstadoCivil")]
credit.Datos.Train.logSal.Nout.knn.Corr<-credit.Datos.Train.logSal.Nout.knn[setdiff(names(credit.Datos.Train.logSal.Nout.knn),"EstadoCivil")]


# PCA
# Generamos dos nuevos preprocesos aplicando PCA's a los conjuntos de datos anteriores que no tienen outliers.

ppPCAcont.Nout.TrNA <- preProcess(credit.Datos.Train.logSal.Nout.TrNA.Corr[,1:14],method=c("pca"),thresh = 0.8)
credit.Datos.Train.logSal.Nout.TrNA.Corr.PCA <- predict(ppPCAcont.Nout.TrNA,credit.Datos.Train.logSal.Nout.TrNA.Corr)

ppPCAcont.Nout.knn <- preProcess(credit.Datos.Train.logSal.Nout.knn.Corr[,1:14],method=c("pca"),thresh = 0.8)
credit.Datos.Train.logSal.Nout.knn.Corr.PCA <- predict(ppPCAcont.Nout.knn,credit.Datos.Train.logSal.Nout.knn.Corr)


# NORMALIZACIÓN
# Normalizamos los conjuntos que no tienen knn ni PCA

ppNorm.Out<-preProcess(credit.Datos.Train.logSal.Out.TrNA.Corr[,sapply(credit.Datos.Train.logSal.Out.TrNA.Corr,FUN=is.numeric)],method=c("center","scale"))
credit.Datos.Train.logSal.Out.TrNA.Corr.Norm <- predict(ppNorm.Out,credit.Datos.Train.logSal.Out.TrNA.Corr) 

ppNorm.Nout<-preProcess(credit.Datos.Train.logSal.Nout.TrNA.Corr[,sapply(credit.Datos.Train.logSal.Nout.TrNA.Corr,FUN=is.numeric)],method=c("center","scale"))
credit.Datos.Train.logSal.Nout.TrNA.Corr.Norm <- predict(ppNorm.Nout,credit.Datos.Train.logSal.Nout.TrNA.Corr) 


# RESUMEN PREPROCESOS
# credit.Datos.Train.logSal.Out.TrNA.Corr.Norm
# credit.Datos.Train.logSal.Out.knn.Corr
# credit.Datos.Train.logSal.Nout.TrNA.Corr.Norm
# credit.Datos.Train.logSal.Nout.knn.Corr
# credit.Datos.Train.logSal.Nout.TrNA.Corr.PCA
# credit.Datos.Train.logSal.Nout.knn.Corr.PCA


#MODELOS

#Elección del mejor preproceso

var.Salida <- c("Clase")
train.Vars.Entrada.NoPCA<-setdiff(names(credit.Datos.Train.logSal.Out.TrNA.Corr.Norm),var.Salida)

credit.TrainCtrl.3cv10.resampAll <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  returnResamp = "all" )

set.seed(1234)

credit.Datos.Train.logSal.Out.TrNA.Corr.Norm.3cv10.gbm <- train(credit.Datos.Train.logSal.Out.TrNA.Corr.Norm[train.Vars.Entrada.NoPCA],credit.Datos.Train.logSal.Out.TrNA.Corr.Norm[[var.Salida]],
                                                                method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll)

set.seed(1234)

credit.Datos.Train.logSal.Out.knn.Corr.3cv10.gbm <- train(credit.Datos.Train.logSal.Out.knn.Corr[train.Vars.Entrada.NoPCA],credit.Datos.Train.logSal.Out.knn.Corr[[var.Salida]],
                                                          method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll)

set.seed(1234)

credit.Datos.Train.logSal.Nout.TrNA.Corr.Norm.3cv10.gbm <- train(credit.Datos.Train.logSal.Nout.TrNA.Corr.Norm[train.Vars.Entrada.NoPCA],credit.Datos.Train.logSal.Nout.TrNA.Corr.Norm[[var.Salida]], method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll)

set.seed(1234)

credit.Datos.Train.logSal.Nout.knn.Corr.3cv10.gbm <- train(credit.Datos.Train.logSal.Nout.knn.Corr[train.Vars.Entrada.NoPCA],credit.Datos.Train.logSal.Nout.knn.Corr[[var.Salida]],
                                                           method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll)


# Para los preproces que tienen PCA, las variables de entrada son distintas

train.Vars.Entrada.PCA<-setdiff(names(credit.Datos.Train.logSal.Nout.TrNA.Corr.PCA),var.Salida)

set.seed(1234)

credit.Datos.Train.logSal.Nout.TrNA.Corr.PCA.3cv10.gbm <- train(credit.Datos.Train.logSal.Nout.TrNA.Corr.PCA[train.Vars.Entrada.PCA],credit.Datos.Train.logSal.Nout.TrNA.Corr.PCA[[var.Salida]],
                                                                method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll)

set.seed(1234)

credit.Datos.Train.logSal.Nout.knn.Corr.PCA.3cv10.gbm <- train(credit.Datos.Train.logSal.Nout.knn.Corr.PCA[train.Vars.Entrada.PCA],credit.Datos.Train.logSal.Nout.knn.Corr.PCA[[var.Salida]], 
                                                               method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll)



comparacionMejorPreproceso<-list(
  pp1=credit.Datos.Train.logSal.Out.TrNA.Corr.Norm.3cv10.gbm,
  pp2=credit.Datos.Train.logSal.Out.knn.Corr.3cv10.gbm,
  pp3=credit.Datos.Train.logSal.Nout.TrNA.Corr.Norm.3cv10.gbm,
  pp4=credit.Datos.Train.logSal.Nout.knn.Corr.3cv10.gbm,
  pp5=credit.Datos.Train.logSal.Nout.TrNA.Corr.PCA.3cv10.gbm,
  pp6=credit.Datos.Train.logSal.Nout.knn.Corr.PCA.3cv10.gbm
)

comparacionMejorPreproceso.resam <- resamples(comparacionMejorPreproceso)
summary(comparacionMejorPreproceso.resam)
dotplot(comparacionMejorPreproceso.resam, scales =list(x = list(relation = "free")))


# Elegimos el cuarto: credit.Datos.Train.logSal.Nout.knn.Corr.3cv10.gbm

# ENTRENAMIENTO
# Hacemos el train para ese preproceso usando los modelos: CART, Random Forests, GBM y nnet.

credit.Datos.Train.Final <- credit.Datos.Train.logSal.Nout.knn.Corr
credit.Train.Final.Vars.Entrada.Usadas <- setdiff(names(credit.Datos.Train.Final),var.Salida)

#rpart
set.seed(1234)
credit.Datos.Train.Final.rpart <- train(credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas], 
                                        credit.Datos.Train.Final[[var.Salida]], method = "rpart", trControl = credit.TrainCtrl.3cv10.resampAll)

#rf
set.seed(1234)
credit.Datos.Train.Final.rf <- train(credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas],
                                     credit.Datos.Train.Final[[var.Salida]], method = "rf", trControl = credit.TrainCtrl.3cv10.resampAll, verbose = FALSE)

#gbm
set.seed(1234)
credit.Datos.Train.Final.gbm <- train(credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas],
                                      credit.Datos.Train.Final[[var.Salida]], method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll, verbose = FALSE)

#nnet
set.seed(1234)
credit.Datos.Train.Final.nnet <- train(credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas],
                                       credit.Datos.Train.Final[[var.Salida]], method = "nnet", trControl = credit.TrainCtrl.3cv10.resampAll, verbose = FALSE)



# Puede verse aquí el resumen de los modelos
credit.Datos.Train.Final.rpart
credit.Datos.Train.Final.rf
credit.Datos.Train.Final.gbm
credit.Datos.Train.Final.nnet


# BÚSQUEDA DE HIPERPARÁMETROS
# PRIMER TUNE GRID EN GBM

gbm.grid <- expand.grid(n.trees=c(10, 50, 100, 300, 500, 600, 800, 1000),
                        shrinkage=c(0.005, 0.01, 0.05, 0.1),
                        n.minobsinnode = c(3,5,10,15),
                        interaction.depth=c(1,4,7,10))

set.seed(1234)

credit.Datos.Final.3cv10.grid.gbm <-
  train (credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas], credit.Datos.Train.Final[[var.Salida]],
         method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll, verbose = F, tuneGrid = gbm.grid)

# Visulizamos el resultado del grid
plot(credit.Datos.Final.3cv10.grid.gbm)
credit.Datos.Final.3cv10.grid.gbm



# TUNE GRID GBM REFINADO

gbm.gridAdv <- expand.grid(n.trees=c(500, 550, 600, 650, 700),
                           shrinkage=c(0.001,0.003 ,0.005, 0.007 ,0.01),
                           n.minobsinnode = c(5, 7, 10, 12, 15),
                           interaction.depth=c(2, 3,4, 5, 6))
set.seed(1234)

credit.Datos.Final.3cv10.gridAdv.gbm <-
  train (credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas], credit.Datos.Train.Final[[var.Salida]], 
         method = "gbm", trControl = credit.TrainCtrl.3cv10.resampAll, verbose = F, tuneGrid = gbm.gridAdv)

# Visulizamos el resultado del grid refinado
plot(credit.Datos.Final.3cv10.gridAdv.gbm)
credit.Datos.Final.3cv10.gridAdv.gbm


#comparacion gbm con tuneGrid y sin tuneGrid

comparacionGrid.gbm<-list(
  gbm=credit.Datos.Train.Final.gbm,
  gbmGrid=credit.Datos.Final.3cv10.grid.gbm,
  gbmGridAdv=credit.Datos.Final.3cv10.gridAdv.gbm
)

comparacionGrid.gbm.resam <- resamples(comparacionGrid.gbm)
summary(comparacionGrid.gbm.resam)
dotplot(comparacionGrid.gbm.resam, scales =list(x = list(relation = "free")))


# TUNE GRID CON NNET
nnet.grid <- expand.grid(size = c(1, 2, 3, 4, 5),
                         decay = c(0, 0.01, 0.1, 0.5, 1, 5, 10))

set.seed(1234)

credit.Datos.Final.3cv10.grid.nnet <-
  train (credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas], credit.Datos.Train.Final[[var.Salida]],
         method = "nnet", trControl = credit.TrainCtrl.3cv10.resampAll, verbose = F, tuneGrid = nnet.grid)

# Visulizamos el resultado del grid
plot(credit.Datos.Final.3cv10.grid.nnet)
credit.Datos.Final.3cv10.grid.nnet

# TUNE GRID NNET REFINADO
nnet.gridAdv <- expand.grid(size = c(1, 2, 3),
                            decay = c(0.3, 0.4, 0.45, 0.5, 0.55, 0.6, 0.7))

set.seed(1234)

credit.Datos.Final.3cv10.gridAdv.nnet <-
  train (credit.Datos.Train.Final[credit.Train.Final.Vars.Entrada.Usadas], credit.Datos.Train.Final[[var.Salida]], 
         method = "nnet", trControl = credit.TrainCtrl.3cv10.resampAll, verbose = F, tuneGrid = nnet.gridAdv)

# Visulizamos el resultado del grid refinado
plot(credit.Datos.Final.3cv10.gridAdv.nnet)
credit.Datos.Final.3cv10.gridAdv.nnet

#comparación nnet con tuneGrid y sin tuneGrid

comparacionGrid.nnet<-list(
  nnet=credit.Datos.Train.Final.nnet,
  nnetGrid=credit.Datos.Final.3cv10.grid.nnet,
  nnetGridAdv=credit.Datos.Final.3cv10.gridAdv.nnet
)

comparacionGrid.nnet.resam <- resamples(comparacionGrid.nnet)
summary(comparacionGrid.nnet.resam)
dotplot(comparacionGrid.nnet.resam, scales =list(x = list(relation = "free")))

# COMPARACIÓN DE MODELOS

credit.modelList.3cv10<-list(
  CART=credit.Datos.Train.Final.rpart
  ,RF=credit.Datos.Train.Final.rf
  ,GBM=credit.Datos.Final.3cv10.gridAdv.gbm
  ,NNET=credit.Datos.Final.3cv10.gridAdv.nnet
)

credit.resamps.3cv10 <- resamples(credit.modelList.3cv10)
summary(credit.resamps.3cv10)
dotplot(credit.resamps.3cv10, scales =list(x = list(relation = "free")))

densityplot(credit.resamps.3cv10, scales =list(x = list(relation = "free"), y = list(relation = "free")),
            auto.key = list(columns = 4), pch = "|")

bwplot(credit.resamps.3cv10, metric = "Accuracy")


credit.diffs.3cv10 <- diff(credit.resamps.3cv10)
summary(credit.diffs.3cv10)

dotplot(credit.diffs.3cv10, scales =list(x = list(relation = "free")))


# Con esta información concluimos que GBM, RF y NNET son iguales desde el punto de vista estadístico.

# TEST FINAL


credit.Datos.Test.logSal <- credit.Datos.Test
credit.Datos.Test.logSal$Salario <- log(credit.Datos.Test.logSal$Salario + 1)

credit.Datos.Test.logSal <-credit.Datos.Test.logSal[setdiff(names(credit.Datos.Test.logSal),"CodigoPostal")]

ppCenterScale<-preProcess(credit.Datos.Train.logSal.Nout, method=c("center","scale"))
credit.Datos.Test.logSal.centerScale<-predict(ppCenterScale,credit.Datos.Test.logSal)

credit.Datos.Test.logSal.centerScale <-credit.Datos.Test.logSal.centerScale[setdiff(names(credit.Datos.Test.logSal.centerScale),"EstadoCivil")]


credit.Test.Final.Vars.Entrada.Usadas <- setdiff(names(credit.Datos.Test.logSal.centerScale),var.Salida)

preds <- predict(credit.Datos.Final.3cv10.gridAdv.gbm, newdata = 
                   credit.Datos.Test.logSal.centerScale[credit.Test.Final.Vars.Entrada.Usadas])

caret::confusionMatrix(preds,credit.Datos.Test.logSal.centerScale[[var.Salida]])


