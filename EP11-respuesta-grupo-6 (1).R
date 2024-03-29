# Actividad 11 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez

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

# Importar datos
poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")

# 1. Propongan una pregunta de investigaci�n original, que involucre la comparaci�n de las medias de dos grupos
# independientes (m�s abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una muestra
# aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulaci�n Monte
# Carlo.

# ESTUDIO: En base a los datos recopilados de las personas encuentadas en la Casen 2017, se requiere obtener un 
# estudio de c�mo se ha ido estructurando la educaci�n en base a la orientaci�n sexual de las personas. Es por esto,
# que resulta de suma importancia llevar a cabo un an�lisis de los a�os de escolaridad de cada grupo (muestra) y,
# as� lograr responder a la interrogante: "�La media de a�os de escolaridad de la comunidad lgbt es igual a la media
# de a�os de escolaridad de la comunidad heterosexual?

# Denotando �L como la media de a�os de escolaridad de la comunidad lgbt, y �H como la media de a�os de escolaridad 
# de la comunidad heterosexual, entonces:

# H0 : �H - �L = 0; esto es  �L  = �H
# H1 : �H - �L!= 0; esto es  �L != �H

# Seleccionar varibles para llevar a cabo el estudio
# id viviendo, orientaci�n y escolaridad
muestra <- select(poblacion, id.vivienda, r23, esc)

# Definir semilla 
set.seed(205508111)

# Filtrar datos
heterosexual <- muestra %>% filter(r23 == 'Heterosexual (Atracci�n hacia el sexo opuesto)')
lgbt <- muestra %>% filter(r23 == 'Bisexual (Atracci�n hacia ambos sexos)' | r23 == 'Gay/Lesbiana (Atracci�n hacia el mismo sexo)')

# Obtener la muestra 
muestraHtero <- sample_n(heterosexual, size = 300, replace = FALSE)
muestraLgbt <- sample_n(lgbt, size = 300, replace = FALSE)

# Eliminar valores nulos, es decir, que fueron omitidos por la persona encuestada
a <- muestraHtero$esc
a <- a[!is.na(a)]

b <- muestraLgbt$esc
b <- b[!is.na(b)]

# Conprobar independencia
print(sum(muestraHtero$id.vivienda %in% muestraLgbt$id.vivienda))

# Como no hay id de viviendas que se repita entre las muestras, podemos concluir de que ambos grupos
# resultan ser independientes.

# Establecer cantidad de repeticiones.
R = 5999

# Funci�n para obtener una permutaci�n.
# Argumentos:
# - i: iterador (para llamadas posteriores).
# - muestra_1, muestra_2: muestras.
# Valor:
# - lista con las muestras resultantes tras la permutaci�n.
obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Funci�n para calcular la diferencia de un estad�stico de inter�s entre las
# dos muestras.
# Argumentos:
# - muestras: lista con las muestras.
# - FUN: nombre de la funci�n que calcula el estad�stico de inter�s.
# Valor:
# - diferencia de un estad�stico para dos muestras.
calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1, na.rm = T) - FUN(muestra_2, na.rm = T)
  return(diferencia)
}

# Funci�n para calcular el valor p.
# Argumentos:
# - distribucion: distribuci�n nula del estad�stico de inter�s.
# - valor_observado: valor del estad�stico de inter�s para las muestras
#   originales.
# - repeticiones: cantidad de permutaciones a realizar.
# - alternative: tipo de hip�tesis alternativa. "two.sided" para
#   hip�tesis bilateral, "greater" o "less" para hip�tesis unilaterales.
# Valor:
# - el valorp calculado.
calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative) {
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}

# Funci�n para graficar una distribuci�n.
# Argumentos:
# - distribucion: distribuci�n nula del estad�stico de inter�s.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.
graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estad�stico de inter�s",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una �nica figura con todos los gr�ficos de dispersi�n.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Funci�n para hacer la prueba de permutaciones.
# Argumentos:
# - muestra_1, muestra_2: vectores num�ricos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar.
# - FUN: funci�n del estad�stico E para el que se calcula la diferencia.
# - alternative: tipo de hip�tesis alternativa. "two.sided" para
#   hip�tesis bilateral, "greater" o "less" para hip�tesis unilaterales.
# - plot: si es TRUE, construye el gr�fico de la distribuci�n generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hip�tesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribuci�n.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribuci�n.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

# Prueba de shapiro - wilk para verificar si es cercana a la normal
# H
shapiro.test(a)
# L
shapiro.test(b)

# En base a los test realizados, se ha comprobado que la escolaridad (a�os) de
# ambas comunidades no siguen una distribuci�n normal. Dicho esto, se ha decidido
# usar bootstrapping para la prueba de hip�tesis, en particular la simulaci�n de
# de Monte Carlo

# Como la muestra es grande, se porcede a llevar a cabo la simulaci�n de Monte Carlo.
# Hacer pruebas de permutaciones para la media y la varianza.
contrastar_hipotesis_permutaciones(a, b, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE,
                                   color = "blue", fill = "blue")
# me da Valor p: 0.0001666667 

contrastar_hipotesis_permutaciones(a, b, repeticiones = R, FUN = var,
                                   alternative = "two.sided", plot = FALSE)

# En primera instancia, como se observa que la diferencia para las muestras originales resultan ser
# -2.394949, esto est� sugiriendo que la media de a�os de escolaridad de la comunidad lgbt es mejor a la 
# media de a�os de escolaridad de la comunidad heterosexual.

# Otro punto a mencionar es, tras hacer la prueba, la distribuci�n generada se asemeja bastante a la normal
# Adem�s, en el gr�fico Q-Q se logra visualizar que la diferencia de medias generada mediante permutaciones
# no contiene valores at�picos, por lo que es valido afirmar que el proceso de bootstraping elimin� el sesgo.

########################################################################################################################
# RESPUESTA : Como el valor P = 0.0001666667 es evidencia suficiente para rechazar la hip�tesis nula en favor de la
# hip�tesis alternativa, es decir, la media de a�os de escolaridad de la comunidad lgbt es distinta a la 
# media de a�os de escolaridad de la comunidad heterosexual.

# Por otra parte, La diferencia observada entre las varianzas de la muestra original es ??L?????H=-0.3876305, sugiriendo que
# la variabilidad de de a�os de escolaridad de la comunidad lgbt es mejor a la variablididad de a�os de escolaridad de 
# la comunidad heterosexual, sin embargo, el valor P resultante es 0.8606667, lo cual implica que la variabilidad de de
# a�os de escolaridad de la comunidad lgbt no es distinta  a la variablididad de a�os de escolaridad de la comunidad heterosexual.
#########################################################################################################################


# 2. Propongan una pregunta de investigaci�n original, que involucre la comparaci�n de las medias de m�s de
# dos grupos independientes (m�s abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior,
# seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando
# bootstrapping. Solo por ejercicio acad�mico, aplique un an�lisis post-hoc con bootstrapping aunque este no
# sea necesario
set.seed(223)
muestra2 <- select(poblacion, id.vivienda, edad, ecivil)



casado <- muestra2 %>% filter(ecivil == 'Casado(a)')
casado <- casado$edad

convivienteSin <- muestra2 %>% filter(ecivil == 'Conviviente o pareja sin acuerdo de uni�n civil')
convivienteSin <- convivienteSin$edad

convivienteCon <- muestra2 %>% filter(ecivil == 'Conviviente civil (con acuerdo de uni�n civil)')
convivienteCon <- convivienteCon$edad

anulado <- muestra2 %>% filter(ecivil == 'Anulado(a)')
anulado <- anulado$edad

separado <- muestra2 %>% filter(ecivil == 'Separado(a)')
separado <- separado$edad

divorciado <- muestra2 %>% filter(ecivil == 'Divorciado (a)')
divorciado <- divorciado$edad

viudo <- muestra2 %>% filter(ecivil == 'Viudo(a)')
viudo <- viudo$edad

soltero <- muestra2 %>% filter(ecivil == 'Soltero(a)')
soltero <- soltero$edad

id <- factor(1:500)

g <- ggqqplot(soltero)
print(g)

datos_anchos <- data.frame(id, casado, convivienteSin, convivienteCon, anulado, separado, divorciado, viudo, soltero)

