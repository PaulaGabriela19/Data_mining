library(arules)
install.packages("genero")
library(genero)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
library(readxl)

data = read_excel('C:/Users/paula/Downloads/base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx')

data$VIC_TRABAJA[data$VIC_TRABAJA == 9] <- -1
data$VIC_EDAD[data$VIC_EDAD == 99] <- -1
data$TOTAL_HIJOS[data$TOTAL_HIJOS == 99] <- -1
data$VIC_ESCOLARIDAD[data$VIC_ESCOLARIDAD == 99] <- -1
data$VIC_EST_CIV[data$VIC_EST_CIV == 9] <- -1

data_research <- data[, c(
  "VIC_EDAD",
  "VIC_SEXO",
  "VIC_EST_CIV",
  "VIC_ESCOLARIDAD",
  "TOTAL_HIJOS",
  "VIC_TRABAJA"
)]

data_research[is.na(data_research)] <- -1

data_research

data.frame(1:ncol(data_research), colnames(data_research))

arbol <- rpart(
  VIC_TRABAJA ~
    VIC_EDAD +
    VIC_SEXO +
    VIC_EST_CIV +
    VIC_ESCOLARIDAD +
    TOTAL_HIJOS,
  data = data_research,
  method = "class"
)

rpart.plot(
  arbol, type = 2, extra = 0, under = TRUE,
  fallen.leaves = TRUE, box.palette = "BuGn",
  main = "Predicción de si la víctima trabaja",
  cex = 0.55
)

persona <- data.frame(
  VIC_SEXO = c(2),
  VIC_EDAD = c(28),
  VIC_EST_CIV = c(2),     
  VIC_ESCOLARIDAD = c(29),
  TOTAL_HIJOS = c(2)
)


result <- predict(arbol, persona, type = "prob")
result
