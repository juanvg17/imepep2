# Actividad 10 IME
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

# Importar datos
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

# Ñami-Ñam, compañía dedicada a la elaboración y comercialización de golosinas, se prepara para lanzar una
# nueva línea de productos al mercado. Para asegurar el éxito comercial, ha solicitado a varias empresas de diseño
# la creación de un empaque para cada uno de los nuevos productos. A fin de decidir qué envase es mejor para
# cada producto y evaluar un contrato permanente con una de las empresas de diseño, Ñami-Ñam ha reclutado a
# 2.000 voluntarios de todo el país, seleccionados aleatoriamente entre los participantes de un concurso
# efectuado por Ñami-Ñam el año anterior. Cada participante debe puntuar las distintas alternativas de envase
# para un producto (seleccionado al azar) mediante una escala Likert de 7 puntos, donde: 1: el envase es muy
# poco atractivo y 7: el envase es muy atractivo. Los datos recolectados contemplan las siguientes variables:
# ▪ Id: identificador único de cada participante.
# ▪ Edad: rango etario del participante. Variable categórica con los niveles Niño, Joven, Adulto.
# ▪ Producto: producto para el cual se evalúan los empaques. Variable categórica con los niveles Alfajor,
# Caramelos, Chocolate, Cuchuflí, Galletas, Queque.
# ▪ Diseno: empresa que diseñó el envase. Variable categórica con los niveles DisenoColor, KoolDesign, LaKajita,
# PackPro.
# ▪ Puntaje: puntuación obtenida por el envase. Entero [1-7].

#   1. ¿Existe diferencia entre la puntuación obtenida por los envases diseñados por DisenoColor y PackPro?
# Las hipótesis a contrarestar son:

# H0 : No hay diferencias significativas entre la puntuación obtenida por los envases diseñados por DisenoColor y PackPro.
# HA : Hay diferencias significativas entre la puntuación obtenida por los envases diseñados por DisenoColor y PackPro.

# H0 : M1 - M2 = 0
# H1 : M1 - M2 != 0

# Filtrar data para los diseños a considerar
disenoColor <- poblacion %>%filter(Diseno == "DisenoColor")
packPro <- poblacion %>% filter(Diseno == "PackPro") 

# Rescatar datos
disenoColor <- disenoColor$Puntaje
packPro <- packPro$Puntaje

# Una vez filtrado los datos, se verifican las condiciones:
# 1. Las observaciones de ambas muestras son independientes, ya que  ¿son distintos diseños.
# 2. La escala de medición empleada es a lo menos ordinal, ya que, se habla de de puntajes obtenidos
# los cual se puede contrarrestar para el problema dado como una "igualdad" 

# Establecer nivel de significación, no se debe ser tan estricto.
alfa <- 0.05

# Hacer la prueba de Mann-Whitney.
prueba1 <- wilcox.test(disenoColor, packPro, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba1)

# Respuesta : Como P < alfa, se rechaza H0 a favor de HA, es decir, con un 95% de confianza se concluye que 
#             Hay diferencias significativas entre la puntuación obtenida por los envases diseñados por 
#             DisenoColor y PackPro 


#   2. ¿Existe diferencias en las puntuaciones obtenidas para el envase de queque diseñado por KoolDesign según 
# la edad de los evaluadores? De ser así, ¿cuál(es) grupo(s) de evaluador(es) se diferencia(n) de los demás?


muestra2 <- poblacion %>%filter(Producto == "Queque")
muestra2.2 <- muestra2 %>% filter(Diseno == "KoolDesign") 

nino <- muestra2.2 %>% filter(Edad == "Nino")
nino <- nino$Puntaje

joven <- muestra2.2 %>% filter(Edad == "Joven")
joven <- joven$Puntaje

adulto <- muestra2.2 %>% filter(Edad == "Adulto")
adulto <- adulto$Puntaje

# Las hipótesis a contrarrestar son:

# H0 : No hay diferencias en las puntuaciones obtenidas para el envase de queque diseñado por KoolDesign según 
# la edad de los evaluadores.
# HA : Al menos una de las puntuaciones obtenidas es diferente para el envase de queque diseñado por KoolDesign según 
# la edad de los evaluadores.

# H0 : M0 - M1 = 0
# H1 : M0 - M1 != 0

# En este caso es apropiado aplicar la prueba de Kruskal Wallis, entonces se deben cumplir las siguientes condiciones.

# 1. La variable independiente debe tener a lo menos dos niveles (aunque, para dos niveles, se suele usar la
# prueba de Wilcoxon-Mann-Whitney).
# 2. La escala de la variable dependiente debe ser, a lo menos, ordinal.
# 3. Las observaciones son independientes entre sí.

# -------------------------------------------------------------------------------------

Puntuaciones <- c(nino, joven, adulto)

Edad <- c(rep("nino", length(nino)),
               rep("joven", length(joven)),
               rep("adulto", length(adulto)))

Edad <- factor(Edad)

datos2 <- data.frame(Puntuaciones, Edad)

print(datos2)

# Establecer nivel de significación
alfa <- 0.01

# Hacer la prueba de Kruskal-Wallis.
prueba2 <- kruskal.test(Puntuaciones ~ Edad, data = datos2)
print(prueba2)

# Efectuar procedimiento post-hoc de Holm si se encuentran diferencias
# significativas.
if(prueba2$p.value < alfa) {
  post_hoc <- pairwise.wilcox.test(datos2$Puntuacion,
                                   datos2$Edad,
                                   p.adjust.method = "holm",
                                   paired = FALSE)
  
  print(post_hoc)
}

# 

