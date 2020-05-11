#cargo el paquete de sqldf
install.packages("sqldf")
#cargo librería de sql
library(sqldf)
#cargo libreria de graficas
library(ggplot2)
#libreria de rsm
library(rsm)
#libreria para dememorie
library(RVAideMemoire)
#leo la matriz completa
setwd("C:/RWS")
matrizdefinitiva <- read.csv("dana/matriz-definitiva.csv", sep = ",")
#la visualizo
View(matrizdefinitiva)
#cantidad de renglones matrizdefinitva
nrow(matrizdefinitiva)

#tiro rsm de matrizdefinitiva
definitvaRSM <- rsm(aceptabilidad~FO(factor.sal,
                                     factor.fibra,
                                     factor.levadura),
                    data=matrizdefinitiva)
#resumen de rsm
summary(definitvaRSM)
#tiro un anova
definitivaANOVA <- aov(aceptabilidad~FO(factor.sal,
                                        factor.fibra,
                                        factor.levadura), data = matrizdefinitiva)
#resumen de anova
summary(definitivaANOVA)
#anova de consumidor x aceptabilidad
definitivaANOVAXConsumidor <- aov(aceptabilidad~consumidor, data = matrizdefinitiva)
summary(definitivaANOVAXConsumidor)#anova de etapa x aceptabilidad
definitivaANOVAXEtapa <- aov(aceptabilidad~etapa, data = matrizdefinitiva)
summary(definitivaANOVAXEtapa)
#anova de sal x aceptabilidad
definitivaANOVAXSal <- aov(aceptabilidad~factor.sal, data = matrizdefinitiva)
summary(definitivaANOVAXSal)
#anova de fibra x aceptabilidad
definitivaANOVAXFibra <- aov(aceptabilidad~factor.fibra, data = matrizdefinitiva)
summary(definitivaANOVAXFibra)
#anova de levadura x aceptabilidad
definitivaANOVAXLevadura <- aov(aceptabilidad~factor.levadura, data = matrizdefinitiva)
summary(definitivaANOVAXLevadura)
#anova de salFibra x aceptabilidad
definitivaANOVAXSalFibra <- aov(aceptabilidad~FO(factor.sal,factor.fibra), data = matrizdefinitiva)
summary(definitivaANOVAXSalFibra)
#anova de salLevadura x aceptabilidad
definitivaANOVAXSalLevadura <- aov(aceptabilidad~FO(factor.sal,factor.levadura), data = matrizdefinitiva)
summary(definitivaANOVAXSalLevadura)
#anova de EtapaSal x aceptabilidad
definitivaANOVAXEtapaSal <- aov(aceptabilidad~FO(etapa,factor.sal), data = matrizdefinitiva)
summary(definitivaANOVAXEtapaSal)
#anova de FibraLevadura x aceptabilidad
definitivaANOVAXFibraLevadura <- aov(aceptabilidad~FO(factor.fibra,factor.levadura), data = matrizdefinitiva)
summary(definitivaANOVAXFibraLevadura)
#armo un dataframe con los datos de etapa 1
etapa1 <- sqldf("SELECT * FROM matrizdefinitiva WHERE etapa = 1")
matrizdefinitiva2 <- matrizdefinitiva
colnames(matrizdefinitiva2) <- c("consumidor","prototipo",
                                 "aceptabilidad","etapa",
                                 "sal","fibra","levadura")
View(matrizdefinitiva2)
#SAL
AcptabilidadXSal <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE sal = 1")
mean(AcptabilidadXSal$aceptabilidad)
AcptabilidadXSal2 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE sal = 2")
mean(AcptabilidadXSal2$aceptabilidad)
AcptabilidadXSal3 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE sal = 3")
mean(AcptabilidadXSal3$aceptabilidad)
#Fibra
AcptabilidadXFibra <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE fibra = 1")
mean(AcptabilidadXFibra$aceptabilidad)
AcptabilidadXFibra2 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE fibra = 2")
mean(AcptabilidadXFibra2$aceptabilidad)
AcptabilidadXFibra3 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE fibra = 3")
mean(AcptabilidadXFibra3$aceptabilidad)
#Levadura
AcptabilidadXLevadura <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE levadura = 1")
mean(AcptabilidadXLevadura$aceptabilidad)
AcptabilidadXLevadura2 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE levadura = 2")
mean(AcptabilidadXLevadura2$aceptabilidad)
AcptabilidadXLevadura3 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE levadura = 3")
mean(AcptabilidadXLevadura3$aceptabilidad)
#etapa 1 sal
etapa1sal <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE etapa = 1 AND sal = 1")
mean(etapa1sal$aceptabilidad)
etapa1sal2 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE etapa = 1 AND sal = 2")
mean(etapa1sal2$aceptabilidad)
etapa1sal3 <- sqldf("SELECT * FROM matrizdefinitiva2 WHERE etapa = 1 AND sal = 3")
mean(etapa1sal3$aceptabilidad)
#media de aceptabilidad
mean(etapa1$aceptabilidad)
#media de sal
mean(etapa1$factor.sal)
#visualizo los datos de etapa 1
View(etapa1)
#cantidad de renglones en etapa 1
nrow(etapa1)
#armo un dataframe con los datos de etapa 2
etapa2 <- sqldf("SELECT * FROM matrizdefinitiva WHERE etapa = 2")
#media de aceptabilidad
mean(etapa2$aceptabilidad)
#visualizo los datos de etapa 2
View(etapa2)
#cantidad de renglones en etapa 2
nrow(etapa2)
#grafico box plot
ggplot(etapa2,aes(x=factor.sal,y=aceptabilidad))+
  geom_boxplot(aes(x=factor.sal,y=aceptabilidad, group = 1))
#tiro un anova
etapa2Anova <- aov(etapa2$aceptabilidad~etapa2$factor.sal * 
                     etapa2$factor.fibra * etapa2$factor.levadura, data = etapa2)
summary(etapa2Anova)
levels(etapa2$factor.levadura)
