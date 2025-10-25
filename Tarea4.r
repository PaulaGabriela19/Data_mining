install.packages("readxl")
install.packages("ggplot2")
install.packages("arules")

install.packages("devtools")
devtools::install_github("hrbrmstr/ggalt")


library(readxl)
library(ggplot2)
library(ggalt)
library(arules)

data <- read_excel("C:\\Users\\paula\\Downloads\\base-de-datos-violencia-intrafamiliar-ano-2024_v3.xlsx")

data_fp <- data[, c("HEC_MES", "HEC_DEPTO", "VIC_EDAD", "VIC_ESCOLARIDAD", "VIC_EST_CIV", "VIC_GRUPET", "VIC_TRABAJA", "VIC_DEDICA", "AGR_EDAD", "AGR_ESCOLARIDAD")]

data_fp[is.na(data_fp)] <- -1

set.seed(42)
cluster <- kmeans(data_fp, centers = 4)

data_fp$cluster <- as.factor(cluster$cluster)

ggplot(data_fp, aes(x = VIC_EDAD, y = AGR_EDAD, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = as.data.frame(cluster$centers), 
             aes(x = VIC_EDAD, y = AGR_EDAD), 
             color = "black", size = 5, shape = 17) +
  geom_encircle(aes(group = cluster, fill = cluster), alpha = 0.2, s_shape = 1, expand = 0.05) +
  labs(title = "Clústeres de Edad del Agresor vs Edad de la Víctima",
       x = "Edad de la víctima",
       y = "Edad del agresor") +
  theme_minimal()
  
aggregate(data_fp[, c("VIC_EDAD", "VIC_ESCOLARIDAD", "AGR_EDAD", "AGR_ESCOLARIDAD")],
          by = list(Cluster = data_fp$cluster), FUN = mean)
          
data_fp_civ <- subset(data_fp, VIC_EST_CIV == 1)
data_fp_civ[is.na(data_fp_civ)] <- -1

cluster2 <- kmeans(data_fp_civ, centers = 3)

ggplot(data_fp_civ, aes(x = VIC_EDAD, y = AGR_EDAD, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = as.data.frame(cluster2$centers), 
             aes(x = VIC_EDAD, y = AGR_EDAD), 
             color = "black", size = 5, shape = 17) +
  geom_encircle(aes(group = cluster, fill = cluster), alpha = 0.2, s_shape = 1, expand = 0.05) +
  labs(title = "Clústeres de Edad del Agresor vs Edad de la Víctima",
       x = "Edad de la víctima",
       y = "Edad del agresor") +
  theme_minimal()


data_fp_civ2 <- subset(data_fp, VIC_EST_CIV != 1)
data_fp_civ2[is.na(data_fp_civ2)] <- -1

cluster3 <- kmeans(data_fp_civ2, centers = 3)

ggplot(data_fp_civ2, aes(x = VIC_EDAD, y = AGR_EDAD, color = cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = as.data.frame(cluster3$centers), 
             aes(x = VIC_EDAD, y = AGR_EDAD), 
             color = "black", size = 5, shape = 17) +
  geom_encircle(aes(group = cluster, fill = cluster), alpha = 0.2, s_shape = 1, expand = 0.05) +
  labs(title = "Clústeres de Edad del Agresor vs Edad de la Víctima",
       x = "Edad de la víctima",
       y = "Edad del agresor") +
  theme_minimal()
