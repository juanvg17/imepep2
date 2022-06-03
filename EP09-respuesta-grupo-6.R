# Actividad 9 IME
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
# en formular consultas para problemas con diferente nivel de dificultad en el área de leyes.


# Se filtran los datos que son de interés.
muestra <- poblacion %>% filter(area == "Leyes")

Alta <- muestra %>% filter(dificultad == "Alta")
Alta <- Alta$tiempo

Media <- muestra %>% filter(dificultad == "Media")
Media <- Media$tiempo

Baja <- muestra %>% filter(dificultad == "Baja")
Baja <- Baja$tiempo

datos <- data.frame(Alta, Media, Baja)

# Se da un formato pertinente a los datos para realizar una prueba ANOVA.
datos <- datos %>% pivot_longer(c("Alta", "Media", "Baja"),
                                names_to = "Dificultad",
                                values_to = "tiempo")

datos[["Dificultad"]] <- factor(datos[["Dificultad"]])
datos[["instancia"]] <- factor(1:nrow(datos))

# Gráfico Q-Q para verificar nromalidad.
g <- ggqqplot(datos,
              x = "tiempo",
              y = "Dificultad",
              color = "Dificultad")

g <- g + facet_wrap(~ Dificultad)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Nivel de significación.
alfa <- 0.05

# Se comprueba si las varianzas son aproximadamente iguales.
desviaciones <- c(var(Alta), var(Media), var(Baja))
homVar <- max(desviaciones) / min(desviaciones)
cat("Homogeneidad de las varianzas: ", homVar)

#ANOVA
prueba <- aov(tiempo ~ Dificultad,data = datos)
print(summary(prueba))

# HSD de Tukey
post_hoc <- TukeyHSD(prueba,"Dificultad",ordered = TRUE, conf.level = 1 - alfa)
print(post_hoc)