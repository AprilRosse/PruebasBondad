#Prueba de errores
##############################LIBRERIAS REQUERIDAS#########################
#Para las pruebas de bondad
library(car)        #Para la grafica qqplot
library(moments)    #Para calcular la curtosis
library(normtest)   #Pruebas normalidad
library(stats)      #Otras pruebas
library(goftest)    #Otro paquete de pruebas

############################VARIABLES y PARAMETROS REQUERIDOS###########################

ruta <-"E:/xx/xx/MMMM/NNNN/"
archivo <- "archivename.csv"
lect <- paste(ruta, sep = "", archivo)

base_entrada <- read.csv(lect,
                         header = TRUE,
                         sep = ";",
                         dec =  ",")

base_entrada$Datos <- as.numeric(gsub("\\.","",as.character(base_entrada$Datos)))
base_entrada$Pronostico <- as.numeric(gsub("\\.","",as.character(base_entrada$Pronostico)))
base_entrada$Residuales <- as.numeric(gsub("\\.","",as.character(base_entrada$Residuales)))

cuentas <- levels(base_entrada$Series)
Significancia <- 0.01
i = 0


resultados <- data.frame(Variable= character(),
                Prueba = character(),
                P_Valor= numeric(),
                Estadistico = numeric(),
                Valoracion = logical())

Name_Salida <- "Resultados_Pruebas_Bondad.csv"
Salida <- paste(ruta,sep = "", Name_Salida)


##################################PRUEBAS DE BONDAD############################################

for(cuentax in cuentas){
i = i+1
  x <- subset(base_entrada,
              Series==cuentax)

  Errores <- na.omit(x$Residuales)

  TestKurtosis <- kurtosis.norm.test(Errores, nrepl=2000)
  TestKS <- ks.test(Errores,pnorm, mean(Errores),sd(Errores))
  TestShapiro <- shapiro.test(Errores)
  TestCramer <- cvm.test(Errores,pnorm,mean(Errores),sd(Errores))
  TestAD <- ad.test(Errores,pnorm,mean(Errores),sd(Errores))

    #names(TestAD)
    #TestAD$p.value

  result <- data.frame(
  "Variable"= rep(cuentax,5),
  "Prueba" = c(TestKurtosis$method,
               TestKS$method,
               TestShapiro$method,
               TestCramer$method[1],
               TestAD$method[1]),
  "P-Valor"= c(TestKurtosis$p.value,
               TestKS$p.value,
               TestShapiro$p.value,
               TestCramer$p.value,
               TestAD$p.value),
  "Estadistico"= c(TestKurtosis$statistic,
                   TestKS$statistic,
                   TestShapiro$statistic,
                   TestCramer$statistic,
                   TestAD$statistic)
    )

  result$Valoracion <- result$P.Valor > Significancia

  resultados <- rbind(resultados,result)
print(i)
print(cuentax)
}

write.table(resultados,file = Salida,sep = "|",row.names = FALSE)

#----------------Codigos investigados durante el desarrollo pero no empleados-----------------

#__________________________Pruebas gráficas de normalidad_______________________________
#1. Grafico de densidad de los errores Probabilidad de los valores de los errores
#plot(density(x$Residuales, na.rm = TRUE), main = "Densidad Errores Pronostico")

#2. Grafico qqplot, donde se compara con los cuantiles teoricos
#a<-qqnorm(Errores,main = "Distribución de residuos para la variable")
#qqline(Errores, col = 2)

#kurtosis <- kurtosis(Errores) #Este codigo se puede usar para calcular el valor pero con la prueba el estadistico es este valor
#args(write.csv)  #Comando para revisar los argumentos y los valores por defecto de la funcion

#Paquete para Backtesting, aun está en desarrollo en GitHub
# install.packages("devtools")
# require(devtools)
# install_github("braverock/blotter") # dependency
# install_github("braverock/quantstrat")