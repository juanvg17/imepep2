# Actividad 12 IME
# Grupo 6: Angel Avendaño, Jorge González y Juan Vásquez

# Importar paquetes.
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

if(!require(ez)){
  install.packages("ez",dependencies = TRUE)
  require(ez)
}
if(!require(nlme)){
  install.packages("nlme",dependencies = TRUE)
  require(nlme)
}
if(!require(emmeans)){
  install.packages("emmeans",dependencies = TRUE)
  require(emmeans)
}

if(!require(ggplot2)){
  install.packages("ggplot2",dependencies = TRUE)
  require(ggplot2)
}

if(!require(WRS2)){
  install.packages("WRS2",dependencies = TRUE)
  require(WRS2)
}

# Generar la tabla de datos
instanciaA <- c(167, 8, 65, 91, 125, 64, 196, 117, 41, 56)
tiempoA <- c(1510394, 251843, 834565, 37449, 48705, 402929, 885722, 8576989, 62764, 783108)

instanciaB <- c(197, 7, 21, 195, 191, 149, 39, 139, 154, 137)
tiempoB <- c(48408, 35974, 5743260, 6684497, 1252837, 6701654, 6568968, 120276, 1174562, 2830464)

tiempo <- c(tiempoA, tiempoB)
algoritmo <- c(rep("A", length(tiempoA)), rep("B", length(tiempoB)))
datos <- data.frame(tiempo, algoritmo)

g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g)

shapiro.test(tiempoA)
shapiro.test(tiempoB)

# Se ordenan los datos
tiempoA <- sort(tiempoA)
tiempoB <- sort(tiempoB)

alfa <- 0.05

gamma <- 0.2
n_a <- length(tiempoA)
n_b <- length(tiempoB)

poda_a <- n_a * gamma
poda_b <- n_b * gamma

a_truncada <- tiempoA[poda_a:(n_a - poda_a)]
b_truncada <- tiempoB[poda_b:(n_b - poda_b)]

tiempo <- c(a_truncada, b_truncada)
algoritmo <- c(rep("A", length(a_truncada)), rep("B", length(b_truncada)))
datos_truncados <- data.frame(tiempo, algoritmo)

g1 <- ggqqplot(datos_truncados, x = "tiempo", facet.by = "algoritmo",
              palette = c("blue", "red"), color = "algoritmo")

print(g1)

shapiro.test(a_truncada)
shapiro.test(b_truncada)

prueba <- yuen(tiempo ~ algoritmo, data = datos, tr = gamma)
print(prueba)

# CAMBIAR PREGUNTA 1 A TRANFORMACION Y PRUEBA PARAMETRICA


# Se falla al rechazar h0 por lo tanto las medias de los algoritmos son , es decir, ninguno de los algoritmos es mas rapido que el otro.

# PREGUNTA 2
# YUEN CON BOOSTRAP CON MEDIANA, SI ES QUE NO CUMPLE LA NORMALIDAD

# PREGUNTA 3
# Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un
# método robusto adecuado.

# SE APLICA METODO ROBUSTO CON MEDIANA

poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")
set.seed(205508111)

muestra2 <- select(poblacion, id.vivienda, edad, ecivil)

casado <- muestra2 %>% filter(ecivil == 'Casado(a)')
casado_m <- sample_n(casado, size = 500, replace = FALSE)
casado_m <- casado_m$edad

separado <- muestra2 %>% filter(ecivil == 'Separado(a)')
separado_m <- sample_n(separado, size = 500, replace = FALSE)
separado_m <- separado_m$edad

soltero <- muestra2 %>% filter(ecivil == 'Soltero(a)')
soltero_m <- sample_n(soltero, size = 500, replace = FALSE)
soltero_m <- soltero_m$edad


edad <- c(casado_m, separado_m, soltero_m)
ecivil <- c(rep("casado_m", length(casado_m)), rep("separado_m", length(separado_m)), rep("soltero_m", length(soltero_m)))

datos <- data.frame(edad, ecivil)

comp_mediana <- med1way(formula = edad ~ ecivil, data = datos, iter = 1000)


print(comp_mediana$p.value)
