#CARGA DE LIBRERÍAS
install.packages("compare")
library(compare)
install.packages("outliers")
library(outliers)
install.packages("ca")
library(ca)
install.packages("lattice")
library(lattice)
install.packages("MASS")
library(MASS) # Carga la librería que contiene a la función lda()
install.packages("GPArotation")
library(GPArotation)
install.packages("FactoMineR")
library(FactoMineR)
install.packages("psych")
library(psych)
install.packages("gplots")
library("gplots")


#################### 1. CARGA DE LA BASE DE DATOS #######################

datos<-read.table("F:/Users/Alberto/Desktop/MASTER UNIR/TFM/Fraudes.txt", header=TRUE, sep="\t")
dim(datos) #Comprobar la dimensión de la tabla cargada

#Carga de datos con variables con menos del 50% de valores missing (99)
datos<-read.table("F:/Users/Alberto/Desktop/MASTER UNIR/TFM/fraudeLimpio.txt", header=TRUE, sep="\t")
dim(datos) #Comprobar la dimensión de la tabla cargada

#################### 2. SELECCIÓN DE LA MUESTRA INICIAL ####################
# 2.1. Selección de variables


# Muestreo de 20 preguntas de la base de datos original que cuenta con 256 preguntas
# Las 12 restantes preguntas son de control
muestreoPreguntas <- sort(sample(1:256, 20)) 
                                             
muestreoPreguntas
preguntasMuestra <- datos[, c(muestreoPreguntas)] #Creación de la muestra con las 20 preguntas seleccionadas
head(preguntasMuestra)

# 2.2. Selección de registros

# a) Selección aleatoria de registros

#Se añaden las 12 variables de control a la muestra
muestraInicial<-cbind(preguntasMuestra,datos[,257:268]) 
dim(muestraInicial)

#Selección una muestra de 265 registros de la base de datos obtenida anteriormente
#Los registros a seleccionar en la BB.DD estón entre el registro 1 y el 2306
individuosMuestra <-sort(sample(1:2306,245)) 
individuosMuestra
muestraTest<-as.data.frame(muestraInicial[c(individuosMuestra),])
head(muestraTest)
dim(muestraTest)

summary(muestraTest$encuestador)
# b) Inclusión de registros "fraudulentos"

#Identificación de los individuos fraudulentos previamente
individuosFraude<-as.data.frame(muestraInicial[2307:2646,])
head(individuosFraude)
dim(individuosFraude)

#Se unen en una sola muestra final los individuos seleccionados aleatoriamente (265) más los fraudulentos (340)
muestraFinal<-as.data.frame(rbind(muestraTest,individuosFraude))
head(muestraFinal)

dim(muestraFinal)

#################### 3. CÁLCULO DE LOS INDICADORES ####################

# INDICADOR 1: TIEMPO ENTRE ENCUESTAS (0-n SEGUNDOS). 
indicador1<-muestraFinal$TEE
dim(indicador1)
indicador1

# INDICADOR 2: NÚMERO MEDIO DE MATCHES ENTRE RESPUESTAS (0% a 100%). 

dimFil<-dim(muestraFinal)[1] #Dimensión de las filas
dimCol<-dim(muestraFinal)[2] #Dimensión de las columnas

# Se extraen las 32 variables del dataset

variable1<-muestraFinal[,1]
variable2<-muestraFinal[,2]
variable3<-muestraFinal[,3]
variable4<-muestraFinal[,4]
variable5<-muestraFinal[,5]
variable6<-muestraFinal[,6]
variable7<-muestraFinal[,7]
variable8<-muestraFinal[,8]
variable9<-muestraFinal[,9]
variable10<-muestraFinal[,10]
variable11<-muestraFinal[,11]
variable12<-muestraFinal[,12]
variable13<-muestraFinal[,13]
variable14<-muestraFinal[,14]
variable15<-muestraFinal[,15]
variable16<-muestraFinal[,16]
variable17<-muestraFinal[,17]
variable18<-muestraFinal[,18]
variable19<-muestraFinal[,19]
variable20<-muestraFinal[,20]
variable21<-muestraFinal[,21]
variable22<-muestraFinal[,22]
variable23<-muestraFinal[,23]
variable24<-muestraFinal[,24]
variable25<-muestraFinal[,25]
variable26<-muestraFinal[,26]
variable27<-muestraFinal[,27]
variable28<-muestraFinal[,28]
variable29<-muestraFinal[,29]
variable30<-muestraFinal[,30]
variable31<-muestraFinal[,31]
variable32<-muestraFinal[,32]

# Se crea una función que compara los valores en una variable de las observaciones i e i+1
coincidencias<-function (datos){
  matches <- array(0,dimFil)
  for(i in 1:dimFil-1){
    matches[i]<-compare(datos[i],datos[i+1])
    }
    i<i+1
    
  return(as.data.frame(matches))
}
# Se pasa la función a cada una de las 32 variables

coincidencias1<-t(as.data.frame(coincidencias(variable1)))
coincidencias2<-t(as.data.frame(coincidencias(variable2)))
coincidencias3<-t(as.data.frame(coincidencias(variable3)))
coincidencias4<-t(as.data.frame(coincidencias(variable4)))
coincidencias5<-t(as.data.frame(coincidencias(variable5)))
coincidencias6<-t(as.data.frame(coincidencias(variable6)))
coincidencias7<-t(as.data.frame(coincidencias(variable7)))
coincidencias8<-t(as.data.frame(coincidencias(variable8)))
coincidencias9<-t(as.data.frame(coincidencias(variable9)))
coincidencias10<-t(as.data.frame(coincidencias(variable10)))
coincidencias11<-t(as.data.frame(coincidencias(variable11)))
coincidencias12<-t(as.data.frame(coincidencias(variable12)))
coincidencias13<-t(as.data.frame(coincidencias(variable13)))
coincidencias14<-t(as.data.frame(coincidencias(variable14)))
coincidencias15<-t(as.data.frame(coincidencias(variable15)))
coincidencias16<-t(as.data.frame(coincidencias(variable16)))
coincidencias17<-t(as.data.frame(coincidencias(variable17)))
coincidencias18<-t(as.data.frame(coincidencias(variable18)))
coincidencias19<-t(as.data.frame(coincidencias(variable19)))
coincidencias20<-t(as.data.frame(coincidencias(variable20)))
coincidencias21<-t(as.data.frame(coincidencias(variable21)))
coincidencias22<-t(as.data.frame(coincidencias(variable22)))
coincidencias23<-t(as.data.frame(coincidencias(variable23)))
coincidencias24<-t(as.data.frame(coincidencias(variable24)))
coincidencias25<-t(as.data.frame(coincidencias(variable25)))
coincidencias26<-t(as.data.frame(coincidencias(variable26)))
coincidencias27<-t(as.data.frame(coincidencias(variable27)))
coincidencias28<-t(as.data.frame(coincidencias(variable28)))
coincidencias29<-t(as.data.frame(coincidencias(variable29)))
coincidencias30<-t(as.data.frame(coincidencias(variable30)))
coincidencias31<-t(as.data.frame(coincidencias(variable31)))
coincidencias32<-t(as.data.frame(coincidencias(variable32)))

# Se unen las comparaciones de las 31 variables en un solo dataframe
comparacion<-as.data.frame(cbind(coincidencias1,coincidencias2,coincidencias3,coincidencias4,coincidencias5,
              coincidencias6,coincidencias7,coincidencias8,coincidencias9,coincidencias10,
              coincidencias11,coincidencias12,coincidencias13,coincidencias14,coincidencias15,
              coincidencias16,coincidencias17,coincidencias18,coincidencias19,coincidencias20,
              coincidencias21,coincidencias22,coincidencias23,coincidencias24,coincidencias25,
              coincidencias26,coincidencias27,coincidencias28,coincidencias29, coincidencias30,
              coincidencias31, coincidencias32))

dim(comparacion)
head(comparacion)

sumasCoincidencias<-function (datos){
  sumas <- array(0,dimFil)
  for(i in 1:dimFil){
    sumas[i]<-sum(comparacion[i,])
  }
  i<i+1
  
  return(as.data.frame(sumas))
}

indicador2<-sumasCoincidencias(comparacion)
indicador2<-as.data.frame(indicador2)
indicador2[dimFil,]<-indicador2[dimFil-1,]
dim(indicador2)
indicador2

# INDICADOR 3: TASA MEDIA DE RESPUESTAS DE ENCUESTAS (0% a 100%) 

tasaRespuesta<-function(datos){
  tasa<-array(0,dimFil)
  for(i in 1:dimFil){
      tasa[i]<-(sum(which(muestraFinal[i,]=='99', arr.ind=T)[,1])/dimCol)*100
  }
  i<i+1
  return(as.data.frame(tasa))
}

indicador3<-tasaRespuesta(muestraFinal)
dim(indicador3)
indicador3


# INDICADOR 4: EXISTENCIA DE TELÓFONO (SÍ/NO) 
indicador4<-as.data.frame(muestraFinal$TEL)
dim(indicador4) 
indicador4 

# INDICADOR 5: EXPERIENCIA PREVIA DEL ENCUESTADOR (ALTA/MEDIA/BAJA)
indicador5<-as.data.frame(muestraFinal$EXPERIENCIA)
dim(indicador5)
indicador5

# INDICADOR 6: FECHA DE NACIMIENTO EXACTA (SÍ/NO)

indicador6<-as.data.frame(muestraFinal$FNE)
dim(indicador6)
indicador6

#INDICADOR 7: TIEMPO DE ENCUESTA (NUMÉRICA)
indicador7<-as.numeric(muestraFinal$duracion)
dim(indicador7)
indicador7

# INCORPORACIÓN DE LAS VARIABLES DE CONTROL
indicadores<-as.data.frame(cbind(indicador1,indicador2,indicador3,indicador4,indicador5,
                                 indicador6, indicador7, muestraFinal$encuestador, muestraFinal$id, muestraFinal$provincia,
                                  muestraFinal$duracion, muestraFinal$TME, muestraFinal$DIF))
dim(indicadores)
head(indicadores)

names(indicadores)<-c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "ENCUESTADOR", 
                      "ID", "PROVINCIA", "DURACION", "TME", "DIF")

#################### 4. TÉCNICAS ESTADÍSTICAS ####################


# a) DESCRIPTIVOS
# a.1) Detección de outliers o valores atípicos

#Indicador 1

xyplot(as.numeric(indicadores$I1)~indicadores$ENCUESTADOR)
par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(as.numeric(indicadores$I1),  ylab = "Tiempo entre encuestas")
dotchart(as.numeric(indicadores$I1), xlab = "Tiempo entre encuestas",  ylab = "Orden de los datos")

chisq.out.test(indicadores$I1)
outlier1<-which(indicadores$I1=='-3746')
outlier1
indicadores[104,8]

#Indicador 2

xyplot(as.numeric(indicadores$I2)~indicadores$ENCUESTADOR)
par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(as.numeric(indicadores$I2),  ylab = "Matches de respuesta")
dotchart(as.numeric(indicadores$I2), xlab = "Matches de respuesta",  ylab = "Orden de los datos")

chisq.out.test(indicadores$I2)
outlier2<-which(indicadores$I2=='7')
outlier2
indicadores[196,8]

#Indicador 3

xyplot(as.numeric(indicadores$I3)~indicadores$ENCUESTADOR)
par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(as.numeric(indicadores$I3),  ylab = "Tasa media de respuesta")
dotchart(as.numeric(indicadores$I3), xlab = "Tasa media de respuesta",  ylab = "Orden de los datos")

chisq.out.test(indicadores$I3)
outlier3<-which(indicadores$I3=='56.25')
outlier3
indicadores[111,8]
indicadores[183,8]
indicadores[197,8]
indicadores[255,8]
indicadores[262,8]
indicadores[274,8]
indicadores[281,8]
indicadores[298,8]
indicadores[319,8]
indicadores[321,8]

#Indicador 7
xyplot(as.numeric(indicadores$I7)~indicadores$ENCUESTADOR)
par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(as.numeric(indicadores$I7),  ylab = "Tiempo de realización de encuestas")
dotchart(as.numeric(indicadores$I7), xlab = "Tiempo de realización encuestas",  ylab = "Orden de los datos")

chisq.out.test(indicadores$I7)
outlier7<-which(indicadores$I7=='4497')
outlier7
indicadores[105,8]

#a.2.) Distribución de indicadores categóricos

#Indicador 4
ftable(indicadores$I4~indicadores$ENCUESTADOR)
par(mfrow = c(1,1))
plot(indicadores$I4~indicadores$ENCUESTADOR, xlab="Encuestador", ylab="Indicador4")

#Indicador 5
ftable(indicadores$I5~indicadores$ENCUESTADOR)
xyplot(indicadores$I5~indicadores$ENCUESTADOR,
       xlab="Encuestador", ylab="Indicador5")

#Indicador 6
ftable(indicadores$I6~indicadores$ENCUESTADOR)
par(mfrow = c(1,1))
plot(indicadores$I6~indicadores$ENCUESTADOR, xlab="Encuestador", ylab="Indicador6")

# b) INFERENCIA
# ANOVA Y CHI-CUADRADO

boxplot(indicadores$I1~indicadores$ENCUESTADOR,
        col=terrain.colors(22), xlab="Encuestador", ylab="Indicador1")
i1.lm <- lm(indicadores$I1~indicadores$ENCUESTADOR)
anova(i1.lm)
summary(i1.lm)

boxplot(indicadores$I2~indicadores$ENCUESTADOR,
        col=terrain.colors(22), xlab="Encuestador", ylab="Indicador2")
i2.lm <- lm(indicadores$I2~indicadores$ENCUESTADOR)
anova(i2.lm)
summary(i2.lm)

boxplot(indicadores$I3~indicadores$ENCUESTADOR,
        col=terrain.colors(22), xlab="Encuestador", ylab="Indicador3")
i3.lm <- lm(indicadores$I3~indicadores$ENCUESTADOR)
anova(i3.lm)
summary(i3.lm)

boxplot(indicadores$I7~indicadores$ENCUESTADOR,
        col=terrain.colors(22), xlab="Encuestador", ylab="Indicador7")
i7.lm <- lm(indicadores$I7~indicadores$ENCUESTADOR)
anova(i7.lm)
summary(i7.lm)

tablai4<-(ftable(indicadores$i4 ~indicadores$ENCUESTADOR))
resultadosi4<-chisq.test(tablai4)
resultadosi4
resultadosi4$expected
plot(resultadosi4$expected, tablai4, xlab="Observados Indicador4", ylab="Esperados Indicador4")

tablai5<-(ftable(indicadores$I5 ~indicadores$ENCUESTADOR))
resultadosi5<-chisq.test(tablai5)
resultadosi5
resultadosi5$expected
plot(resultadosi5$expected,tablai5)

tablai6<-(ftable(indicadores$I6 ~indicadores$ENCUESTADOR))
resultadosi6<-chisq.test(tablai3)
resultadosi6
resultadosi6$expected
plot(resultadosi6$expected,tablai6)

# ANÁLISIS EXPLORATORIO

# Preparación de datos
# Seleccionamos los 7 indicadores

indicadoresExploratorio<-cbind(indicadores[,1:8])
# Transformamos a valores numéricos los indicadores 4, 5 y 6
# Indicador 4 --> Valor SI pasa a 1 y valor NO a 0
indicadoresExploratorio$I4 <- as.numeric(factor(indicadoresExploratorio$I4, 
                                                labels=c("0","1")))
table(indicadoresExploratorio$I4)
# Indicador 5 --> Valor BAJA pasa a 1, valor MEDIA a 2 y valor ALTA a 3

indicadoresExploratorio$I5 <- as.numeric(factor(indicadoresExploratorio$I5, 
                                                labels=c("1","2","3")))
table(indicadoresExploratorio$I5)
# Indicador 6 --> Valor SI pasa a 1 y valor NO a 0

indicadoresExploratorio$I6 <-as.numeric(factor (indicadoresExploratorio$I6, 
                                                        labels=c("0","1")))
table(indicadoresExploratorio$I6)
#

tabla4<-ftable (indicadores$I4~indicadores$ENCUESTADOR)
dt4 <- as.table(as.matrix(tabla4))
balloonplot(t(dt4), main ="encuestador", xlab ="indicador4", ylab="encuestador", abel = FALSE, show.margins = FALSE)

tabla5<-ftable (indicadores$I5~indicadores$ENCUESTADOR)
dt5 <- as.table(as.matrix(tabla5))
balloonplot(t(dt5), main ="encuestador", xlab ="indicador5", ylab="encuestador", abel = FALSE, show.margins = FALSE)

tabla6<-ftable (indicadores$I6~indicadores$ENCUESTADOR)
dt6 <- as.table(as.matrix(tabla6))
balloonplot(t(dt6), main ="encuestador", xlab ="indicador6", ylab="encuestador", abel = FALSE, show.margins = FALSE)


# ANALISIS FACTORIAL

I1<-round(indicadores$I1)
I2<-round(indicadores$I2)
I3<-round(indicadores$I3)
I4<-indicadores$I4
I5<-indicadores$I5
I6<-indicadores$I6
I7<-indicadores$I7
indicadoresACP<-as.data.frame(cbind(I1,I2,I3,I4,I5,I6,I7))

# ANÁLISIS FACTORIAL

# estandarización de los datos

I1<-scale(I1)
I2<-scale(I2)
I3<-scale(I3)
I4<-I4
I5<-I5
I6<-I6
I7<-scale(I7)

indicadoresACP<-as.data.frame(cbind(I1,I2,I3,I4,I5,I6,I7))
names(indicadoresACP)[1]<-"I1"
names(indicadoresACP)[2]<-"I2"
names(indicadoresACP)[3]<-"I3"
names(indicadoresACP)[4]<-"I4"
names(indicadoresACP)[5]<-"I5"
names(indicadoresACP)[6]<-"I6"
names(indicadoresACP)[7]<-"I7"

# Matriz de correlaciones (Rcor)

Rcor<-cor(indicadoresACP)
det(Rcor)
# Matriz anti-imagen (objeto A)
invRcor <-solve(Rcor)
A<-matrix(1, nrow(invRcor), ncol(invRcor))
for (i in 1:nrow(invRcor)){
	for (j in 1:ncol(invRcor)){
		A[i,j]<-invRcor[i,j]/sqrt(invRcor[i,i]*invRcor[j,j])
		A[j,i]<-A[i,j]
	}
}
colnames(A) <- colnames(indicadoresACP)
rownames(A) <- colnames(indicadoresACP)
print(A)

#Test esfericidad de Bartlett

print(cortest.bartlett(Rcor, n=nrow(indicadoresACP)))

#KMO
kmo.num <- sum(Rcor^2) - sum(diag(Rcor^2))
kmo.denom <- kmo.num + (sum(A^2) - sum(diag(A^2)))
kmo <- kmo.num/kmo.denom
print(kmo)


# ANALISIS FACTORIAL

# Entrada de indicadores y extracción de 3 factores con la rotación varimax
fit <- principal(indicadoresACP, nfactors=3, rotate="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

#Gráfico sedimentación
sedimentación <- princomp(indicadoresACP, scores=TRUE, cor=TRUE)
plot(sedimentación, type="lines")

fit$scores
CPA<-cbind(indicadoresACP, fit$scores)
head(CPA)

# plot factor 1 vs factor 2 
load <- fit$loadings[,1:3] 
plot(load,type="n") # set up plot 
text(load,labels=names(indicadoresACP),cex=2) # se añaden el nombre de los indicadores

# PREPARACIÓN DATOS PARA WEKA


indicadoresWeka<-cbind((indicadores[,1:7]),indicadores$ENCUESTADOR,muestraFinal$FRAUDE)

write.csv(indicadoresWeka, file="G:/indicadoresFraude.csv")

# PARA PRISM

I1<-as.integer(paste(indicadores$I1))
summary(I1) 
Prism1[I1 <= summary(indicadores$I1)[3]] <- "TEE BAJO" 
Prism1[I1 > summary(indicadores$I1)[3]] <- "TEE ALTO"  
table(Prism1) 


I2<-as.integer(paste(indicadores$I2))
summary(I2)
Prism2<-I2
Prism2[I2 <= summary(indicadores$I2)[3]] <- "MATCH BAJO"
Prism2[I2 > summary(indicadores$I2)[3]] <- "MATCH ALTO" 
table(Prism2)

i3<-as.integer(paste(indicadores$I3))
summary(I3)
Prism3<-I3
Prism3[I3 <= summary(indicadores$I3)[3]] <- "TASA R BAJO"
Prism3[I3 > summary(indicadores$I3)[3]] <- "TASA R ALTO" 
table(Prism3)

Prism4<-indicadores$I4
Prism5<-indicadores$I5
Prism6<-indicadores$I6

I7<-as.integer(paste(indicadores$I7))
summary(I7)
Prism7<-I7
Prism7[I7 <= summary(indicadores$I7)[3]] <- "TIEMPO BAJO"
Prism7[I7 > summary(indicadores$I7)[3]] <- "TIEMPO ALTO" 
table(Prism7)

indicadoresPrism<-cbind(Prism1, Prism2, Prism3, Prism4,
                        Prism5, Prism6,Prism7, 
                        indicadores$ENCUESTADOR, muestraFinal$FRAUDE )

write.csv(indicadoresPrism, file="G:/prismFraude.csv")

# Ejemplo de identificación de registros clasificados fraudulentos según
# algoritmo PRISM de salida de Weka
prismFraude1<-which (indicadoresPrism[,5]=="BAJA"&indicadoresPrism[,7]=="TIEMPO BAJO"&
                       indicadoresPrism[,1]=="TEE BAJO"&indicadoresPrism[,2]=="MATCH BAJO"&
                       indicadoresPrism[,3]=="TASA R ALTO"&indicadoresPrism[,4]=="SI"&
                       indicadoresPrism[,6]=="NO")


prismFraude1 <- indicadoresPrism[c(prismFraude1),]
