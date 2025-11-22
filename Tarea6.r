install.packages("randomForest")
library(randomForest)
library(readxl)

data <- read_excel('C:/Users/paula/Downloads/base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx')

datar <- data[, c("VIC_EDAD", "VIC_ESCOLARIDAD", "VIC_TRABAJA", "VIC_OCUP", "AGR_SEXO", "AGR_EDAD", "AGR_TRABAJA", "HEC_TIPAGRE")]
                  
datar$HEC_TIPAGRE <- as.factor(datar$HEC_TIPAGRE)

datar <- na.omit(datar)

set.seed(42)

datar <- datar[sample(1:nrow(datar)),]

index <- 1:round(nrow(datar)*0.7)
train <- datar[index,]
test <- datar[-index,]

bosque <- randomForest(HEC_TIPAGRE ~
        VIC_EDAD + 
        VIC_ESCOLARIDAD + 
        VIC_TRABAJA + 
        VIC_OCUP +
        AGR_SEXO + 
        AGR_EDAD + 
        AGR_TRABAJA,
        data = train,
        ntree = 100, mtry = 4)
        
prueba <- predict(bosque, test)
prueba

matriz <- table(test$HEC_TIPAGRE, prueba)
matriz

pre <- sum(diag(matriz)) / sum(matriz)
pre

persona <- data.frame(
  VIC_EDAD = 30,
  VIC_ESCOLARIDAD = 29,
  VIC_TRABAJA = 1,
  VIC_OCUP = 5153,
  AGR_SEXO = 1,
  AGR_EDAD = 35,
  AGR_TRABAJA = 1
)

result2 <- predict(bosque, persona, type = "prob")
result2

plot(bosque)
