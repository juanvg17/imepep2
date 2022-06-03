# Actividad 8 IME
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

# Imprtar datos
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que tardan los usuarios 
# en formular una consulta para un problema de dificultad difícil en las áreas de economía, psicología y arquitectura.

# H0: Las diferencias de las medias en el tiempo que tardan los usuarios en formular una consulta para un problema de dificultad difícil
# en las áreas de economía, psicología y arquitectura son iguales.

# HA: al menos una de las muestras tiene diferencia significativa entre los tiempos promedio que tardan los usuarios en formular una consulta para un problema de dificultad difícil
# en las áreas de economía, psicología y arquitectura.

# Se filtran los datos que son de interés.
muestra <- poblacion %>% filter(dificultad == "Alta", area == "Arquitectura" | area == "Psicología" | area == "Economía")

Arquitectura <- muestra %>% filter(area == "Arquitectura")
Arquitectura <- Arquitectura$tiempo

Psicología <- muestra %>% filter(area == "Psicología")
Psicología <- Psicología$tiempo

Economía <- muestra %>% filter(area == "Economía")
Economía <- Economía$tiempo

datos <- data.frame(Arquitectura, Psicología, Economía)

# Se da un formato pertinente a los datos para realizar una prueba ANOVA.
datos <- datos %>% pivot_longer(c("Arquitectura", "Psicología", "Economía"),
                                names_to = "Area",
                                values_to = "tiempo")

datos[["Area"]] <- factor(datos[["Area"]])
datos[["instancia"]] <- factor(1:nrow(datos))

# Gráfico Q-Q para verificar nromalidad.
g <- ggqqplot(datos,
              x = "tiempo",
              y = "Area",
              color = "Area")

g <- g + facet_wrap(~ Area)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Nivel de significación.
alfa <- 0.05

# Se comprueba si las varianzas son aproximadamente iguales.
desviaciones <- c(var(Arquitectura), var(Psicología), var(Economía))
homVar <- max(desviaciones) / min(desviaciones)
cat("Homogeneidad de las varianzas: ", homVar)

# 2. Se menciona en el enunciado que las muestras son de participantes voluntarios a los cuales se les asignó un área de forma aleatoria,
# por lo cual, es posible decir que las muestras son obtenidas de manera aleatoria e independiente.

# 3. Mediante la gráfica Q-Q es posible afirmar que las muestras sigen el supuesto de normalidad

# 4. El resultado obtenido de la homgenidad de las varianzas da un valor 1.114, el cual es menor a 1.5 se puede
# afirmar que las varianzas son aproximadamente iguales.

#ANOVA
prueba <- aov(tiempo ~ Area,data = datos)
print(summary(prueba))

# HSD de Tukey
post_hoc <- TukeyHSD(prueba,"Area",ordered = TRUE, conf.level = 1 - alfa)
print(post_hoc)

# Conclusión: 
# Con un 95% de confianza podemos decir que existe una diferencia significativa en el tiempo que tardan en formular las consultas
# para los problemas con dificultad alta las areas de economía, psicología y arquitectura, siendo el bloque con mayor diferencia el
# de Arquitectura-Economia.

