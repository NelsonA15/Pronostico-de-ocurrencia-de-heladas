#UNIVERSIDAD AUTONOMA CHAPINGO
#DEPARTAMENTO DE IRRIGACON 
#PRONOSTICO DE OCURRENCIA DE HELADAS MEDIANTE MODELOS INFLADOS CON CEROS


#PROYECTO DE TESIS
#CHAPINGO, TEXCOCO ESTADO DE MEXICO, MEXICO

#SE REALIZARA UNA PRUBEA DE HURDLE Y ZERO-INFLATED MODEL PARA PRONOSTICAR 
#HELADAS CON DATOS HISTORICOS


#INCIO DE PROGRAMA


#PRIMERAMENTE SE ACTIVARA LA LIBRERIA QUE SE VA A UTILIZAR

library(pscl)


#AHORA LEEREMOS EL ARCHIVO QUE YA ESTA PREPARADO EN FORMATO CVS DONDE 
#SE ENCUENTRAN TODOS LOS DATOS DE HELADAS HISTORICOS DE LA ZONA DE 
#TEXCOCO ESTADO DE MEXICO

TEXCOCO <- read_xlsx ( "C:\\Users\\nmari\\Desktop\\TESIS HELADAS\\TEXCOCO.xlsx")

#DESPUES DE CARGAR LOS DATOS AL PROGRAMA AHORA PODEMOS EMPEZAR CON EL ANALISIS
#DE DATOS GENERAL

#LOS DATOS TIENEN LA VARIABLE DEPENDIENTE DE NUMERO DE HELADAS POR SEMANA

hist(TEXCOCO$HELADAS, breaks = 0:10 - 0.5)
plot(table(TEXCOCO$HELADAS))
plot(HELADAS ~ HR, data = TEXCOCO)

TexPoi <- glm(HELADAS ~ ., data = TEXCOCO, family = poisson)
summary(TexPoi)
var(TEXCOCO$HELADAS, na.rm = TRUE)
mean(TEXCOCO$HELADAS)
table(TEXCOCO$HELADAS)

plot(x= TEXCOCO$HELADAS, y= TEXCOCO$TMIN)

TexZIP<- zeroinfl(HELADAS ~., data = TEXCOCO, dist = "poisson" )
summary(TexZIP)

pairs(TEXCOCO[,-1])



install.packages("car")
library(car)

scatterplotMatrix(TEXCOCO[,-1])


cor(x=TEXCOCO$HELADAS, y=TEXCOCO$PP)


Texhurdle <- hurdle(HELADAS ~ ., data = TEXCOCO, dist = "poisson")
summary(Texhurdle)


cfac <- function(x, breaks = NULL) {
   if(is.null(breaks)) breaks <- unique(quantile(x, 0:10/10))
   x <- cut(x, breaks, include.lowest = TRUE, right = FALSE)
   levels(x) <- paste(breaks[-length(breaks)], ifelse(diff(breaks) > 1,
                                                        c(paste("-", breaks[-c(1, length(breaks))] - 1, sep = ""), "+"), ""),
                        sep = "")
   return(x)
}

plot(clog(HELADAS) ~ TMIN, data = TEXCOCO)

lm(clog(HELADAS) ~ TMIN, data = TEXCOCO)
 edit(cfac)
 clog <- function(x) log(x + 0.5)
