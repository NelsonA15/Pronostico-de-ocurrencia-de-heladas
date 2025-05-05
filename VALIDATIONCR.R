#VALIDACION CRUZADA
#SE DIVIDIERON LOS DATOS EN DOS
set.seed(1352)
packageVersion("dplyr")
remove.packages("dplyr")
install.packages("lifecycle")
library(lifecycle)
createData
training.samples <- CHAP$HELADAS %>%
  createDataPartition(p=0.8, list = FALSE)

#LECTURA DE DATOS PRINCIPALES
PROVA <- read_xlsx("C:\\Users\\nmari\\Desktop\\TESIS HELADAS\\TODOR.xlsx")

train_data <- CHAP[1:1081,]
test_data <- CHAP [1082:1352,]

#TEST_DATA Y TRAIN_DATA QUE SIRVEN PARA EL SIGUIENTE PASO
#SE CREA EL MODELO

poissonVC <- glm(HELADAS ~ ., data = train_data, family = poisson)
summary(poissonVC)
#YA CON EL MODELO CREADO, HAREMOS LAS PREDICCIONES

predictions <- poissonVC %>% predict(test_data)
summary(predictions)
#CALCULANDO ERRORES



data.frame( R2 = R2(predictions, test_data))
install.packages("ggplot2")
library(ggplot2)

install.packages("caret")


