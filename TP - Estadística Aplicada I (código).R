#Punto 1
#Antes de empezar tomamos una semilla

set.seed(105)

#Definimos el tamaño de la población
p <- 1000

pobln <- rnorm(p, mean = 500, sd = 20) #vector de dimensión 1000 de nuestra población con distribución Normal

poblg <- rgamma(p, shape = 4, scale = 100) #vector de dimensión 1000 de nuestra población con distribución Gamma



#Definimos el tamaño de la muestra

t <- 50

v <- sample(pobln, size = t, replace = FALSE)

w <- sample(poblg, size = t, replace = FALSE)

v <- sort(v)

w <- sort(w)



#Calculamos medidas de tendencia central y dispersión, coeficiente de asimetría y curtósis para la VA 1 (Normal)

library(moments)

xr_n <- mean(v)
med_n <- median(v)
sr_n <- sd(v)
asim_n <- skewness(v)
curt_n <- kurtosis(v)

#Calculamos medidas de tendencia central y dispersion, coeficiente de asimetria y curtosis para la VA 2 (Gamma)

xr_g <- mean(w)
med_g <- median(w)
sr_g <- sd(w)
asim_g <- skewness(w)
curt_g <- kurtosis(w)

#Para el cálculo de las modas nos fijaremos en los histogramas correspondientes 

#Creamos el histograma de la muestra de la población Normal 

hist(v, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 20)

#Superponemos la curva teórica de la Normal con el histograma obtenido

curve(dnorm(x, mean = 500, sd = 20), 
      col = "red", 
      lwd = "2", 
      add = TRUE)

#Creamos el histograma de la muestra de la población Gamma

hist(w, 
     col = "steelblue" , 
     freq = FALSE, 
     breaks = 20)

#Superponemos la curva teórica de la Gamma con el histograma obtenido

curve(dgamma(x, shape = 4, scale = 100), 
      col = "red", 
      lwd = "2", 
      add = TRUE)

#Construimos los boxplots de ambas poblaciones e identificamos los outliers

boxplot(v, horizontal = TRUE, pch = 19, col = "grey", outline = TRUE, range = 1.5)

boxplot(w, horizontal = TRUE, pch = 19, col = "grey", outline = TRUE, range = 1.5)

#Graficamos los Q - Q plot con todos los valores de la muestra

qqnorm(v, pch = 1, frame = FALSE)
qqline(v, col = "steelblue", lwd = 2)

qqnorm(w, pch = 1, frame = FALSE)
qqline(w, col = "steelblue", lwd = 2)

#Debemos ahora eliminar los outliers y obtener los nuevos Q-Q plots

#Para obtener las muestras sin los outliers definimos dos nuevos vectores sin los mismos: l y k
#Para eliminarlos primero debemos encontrar la frecuencia intercuantílica, esta viene dada por la diferencia entre
#los fractiles 0,75 y 0,25. 

q1v <- quantile(v,0.25)
q3v <- quantile(v, 0.75)

q1w <- quantile(w,0.25)
q3w <- quantile(w, 0.75)

fsv <- q3v - q1v
fsw <- q3w- q1w

#Con las frecuencias intercuartílicas idententificamos los límites de los bigotes. Denotaremos a y b al 
#mínimo y al máximo de los valores de la muestra (no outliers) de la población Normal y c y d a los de la Gamma

a <- 1
b <- 50
for (i in 1:t)
{
  if (v[i] < (q1v - 1.5 * fsv))
  {
    a <- i + 1 
  } 
  if (v[i] > (q3v + 1.5 * fsv))
  {
    b <- i - 1
  } 
}

c <- 1
d <- 50
for (i in 1:t)
{
  if (w[i] < (q1w - 1.5 * fsw))
  {
    c <- i + 1 
  } 
  if (w[i] > (q3w + 1.5 * fsw))
  {
    d <- i - 1
  } 
}


l <- c()
k<- c()
for (i in a:b)
{
  l[i - (a - 1)] <- v[i]
}

for (i in c:d)
{
  k[i - (c - 1)] <- w[i]
}

#Ahora sí, finalmente hallamos a los vectores de las muestras sin sus outliers. Con ellos podemos 
#volver a graficar los Q - Q plots. A fines prácticos la diferencia es nula porque solo se sacan dos puntos 
#en la primera muestra y 1 en la segunda. 

qqnorm(l, pch = 1, frame = FALSE)
qqline(l, col = "steelblue", lwd = 2)

qqnorm(k, pch = 1, frame = FALSE)
qqline(k, col = "steelblue", lwd = 2)

#Punto 4
#4.1
#De acuerdo con las expresiones deducidas en el informe, podemos determinar el límite superior y el inferior, 
#que dependerán de la muestra. Primero obtenemos las 100 muestras de tamaño 5 (tc = 5)

#Definimos el tamaño y la cantidad de muestras
n <- 5
reps <- 100

#Creamos las muestras aleatorias 

muestras1 <- replicate(reps, rnorm(n, mean = 500, sd = 20)) #la matriz de muestras de 5 x 100

#Definimos los vectores de los límites superiores e inferiores de los 100 intervalos de confianza

li <- c()
ls <- c()
  
nc <- 0.95

f <- qnorm(1-((1-nc)/2)) #fractil 1-alfa/2 de una normal estándar para un dado nivel de confianza 1 - alfa

sr <- 20

for (i in 1:reps)
{
  xr <- mean(muestras1[,i]) #media de la i - ésima muestra
  li[i] <- xr - (f * sr/sqrt(n)) #límite inferior de cada muestra
  ls[i] <- xr + (f * sr/sqrt(n))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont <- 0

for(i in 1:reps)
{
  if (li[i] <= 500 && 500 <= ls[i])
  {
    cont <- cont + 1
  }
}


#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el niv de confinaza empírico será cont/100

ncemp <- cont/100
ncemp

long <- 0

for (i in 1:reps)
{
  long <- long + ls[i] - li[i]
}

longprom <- long/reps #Longitud promedio de los intervalos
longprom

#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er <- 0

for (i in 1:reps)
{
  xr <- mean(muestras1[,i]) #media de la i - ésima muestra
  er <- er + (f * sr/sqrt(n))/xr
}

erprom <- (er/(reps))*100 #Error relativo porcentual promedio
erprom

#PUNTO 4.2

#Si se desconoce el desvío podemos estimarlo con el estimador estadístico S, mencionado en el informe. Ante este
#escenario nuestro pivote tendrá distribución t de Student y no Normal. 

lit <- c()
lst <- c()

sestm <- c()

ft <- qt(1-((1-nc)/2), df = n -1) #fractil 1-alfa/2 de una normal estándar para un dado nivel de confianza 1 - alfa

for (i in 1:reps)
{
  sestm[i] <- sd(muestras1[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras1[,i]) #media de la i - ésima muestra
  lit[i] <- xr - (ft * sestm[i]/sqrt(n)) #límite inferior de cada muestra
  lst[i] <- xr + (ft * sestm[i]/sqrt(n))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont1 <- 0

for(i in 1:reps)
{
  if (lit[i] <= 500 && 500 <= lst[i])
  {
    cont1 <- cont1 + 1
  }
}

#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el nivel de confianza empírico será cont/100

ncempt <- cont/100
ncempt

#Calculamos la longitud promedio de los intervalos de confianza con desvío desconocido

long <- 0

for (i in 1:reps)
{
  long <- long + lst[i] - lit[i]
}

longprom <- long/reps #Longitud promedio de los intervalos
longprom

#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er <- 0

for (i in 1:reps)
{
  sestm[i] <- sd(muestras1[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras1[,i]) #media de la i - ésima muestra
  er <- er + (ft * sestm[i]/sqrt(n))/xr
}

erprom <- (er/(reps))*100 #Error relativo porcentual promedio
erprom

#Punto 5: Repetimos el mismo ciclo cambiando la cantidad de muestras (100) y su tamaño (30)

#PUNTO 5.1

n30 <- 30
reps2 <- 100

#Creamos las muestras aleatorias 
muestras1g <- replicate(reps2, rnorm(n30, mean = 500, sd = 20)) #la matriz de muestras de 30 x 100

#Definimos los vectores de los límites superiores e inferiores de los 100 intervalos de confianza

li2 <- c()
ls2 <- c()

nc <- 0.95

f <- qnorm(1-((1-nc)/2)) #fractil 1-alfa/2 de una normal estándar para un dado nivel de confianza 1 - alfa

sr <- 20 #Asumimos desvío conocido 

for (i in 1:reps2)
{
  xr <- mean(muestras1g[,i]) #media de la i - ésima muestra
  li2[i] <- xr - (f * sr/sqrt(n30)) #límite inferior de cada muestra
  ls2[i] <- xr + (f * sr/sqrt(n30))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont <- 0

for(i in 1:reps2)
{
  if (li2[i] <= 500 && 500 <= ls2[i])
  {
    cont <- cont + 1
  }
}


#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el niv de confinaza empírico será cont/100

ncemp <- cont/100
ncemp

#Calculamos la longitud promedio de los intervalos de confianza con desvío desconocido

long2 <- 0

for (i in 1:reps2)
{
  long2 <- long2 + ls2[i] - li2[i]
}

longprom2 <- long2/reps2 #Longitud promedio de los intervalos
longprom2

#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er2 <- 0

for (i in 1:reps2)
{
  xr <- mean(muestras1g[,i]) #media de la i - ésima muestra
  er2 <- er2 + (f * sr/sqrt(n30))/xr
}

erprom2 <- (er2/(reps2))*100 #Error relativo porcentual promedio
erprom2

#PUNTO 5.2

#Si se desconoce el desvío podemos estimarlo con el estimador estadístico S, mencionado en el informe. Ante este
#escenario nuestro pivote tendrá distribución t de Student y no Normal. 

lit2 <- c()
lst2 <- c()

sestm <- c()

ft <- qt(1-((1-nc)/2), df = n30 -1) #fractil 1-alfa/2 de una t de Student para un dado nivel de confianza 1 - alfa

for (i in 1:reps2)
{
  sestm[i] <- sd(muestras1g[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras1g[,i]) #media de la i - ésima muestra
  lit2[i] <- xr - (ft * sestm[i]/sqrt(n30)) #límite inferior de cada muestra
  lst2[i] <- xr + (ft * sestm[i]/sqrt(n30))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont1 <- 0

for(i in 1:reps2)
{
  if (lit2[i] <= 500 && 500 <= lst2[i])
  {
    cont1 <- cont1 + 1
  }
}

#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el nivel de confinaza empírico será cont/100

ncempt <- cont/100
ncempt
#Calculamos la longitud promedio de los intervalos de confianza con desvío desconocido

long2 <- 0

for (i in 1:reps2)
{
  long2 <- long2 + lst2[i] - lit2[i]
}

longprom2 <- long2/reps2 #Longitud promedio de los intervalos
longprom2
#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er2 <- 0

for (i in 1:reps2)
{
  sestm[i] <- sd(muestras1g[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras1g[,i]) #media de la i - ésima muestra
  er2 <- er2 + (ft * sestm[i]/sqrt(n30))/xr
}

erprom2 <- (er2/(reps2))*100 #Error relativo porcentual promedio
erprom2

#Punto 7: Repetimos el mismo ciclo cambiando utilizando la variable aleatoria 2 que sigue distribución Gamma  

#Repetimos punto 4 con VA 2
#De acuerdo con las expresiones deducidas en el informe, podemos determinar el límite superior y el inferior, 
#que dependerán de la muestra. Primero obtenemos las 100 muestras de tamaño 5 (tc = 4)

#Definimos el tamaño y la cantidad de muestras
n <- 5
reps <- 100

#Creamos las muestras aleatorias 
muestras_g <- replicate(reps, rgamma(n, shape = 4, scale = 100)) #la matriz de muestras de 5 x 100


#Calculamos la media y varianza de la VA 2:

media_g <- (4 * 100) #Resultado de multiplicar a alpha (parametro de forma) con beta (parametro de escala)
var_g <- (4 * 10000) #Resultado de multiplicar a alpha (parametro de forma) con beta al cuadrado (parametro de escala)

#Definimos los vectores de los límites superiores e inferiores de los 100 intervalos de confianza

li_g <- c()
ls_g <- c()

nc <- 0.95

f <- qnorm(1-((1-nc)/2)) #fractil 1-alfa/2 de una normal estandar (por suma de Gamma indep. TCL ) para un dado nivel de confianza 1 - alfa

sr <- sqrt(var_g)

for (i in 1:reps)
{
  xr <- mean(muestras_g[,i]) #media de la i - ésima muestra
  li_g[i] <- xr - (f * sr/sqrt(n)) #límite inferior de cada muestra
  ls_g[i] <- xr + (f * sr/sqrt(n))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont <- 0

for(i in 1:reps)
{
  if (li_g[i] <= 400 && 400 <= ls_g[i])
  {
    cont <- cont + 1
  }
}


#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el niv de confinaza empírico será cont/100

ncemp_g <- cont/100
ncemp_g

long_g <- 0

for (i in 1:reps)
{
  long_g <- long_g + ls_g[i] - li_g[i]
}

longprom_g <- long_g/reps #Longitud promedio de los intervalos
longprom_g
#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er_g <- 0

for (i in 1:reps)
{
  xr <- mean(muestras_g[,i]) #media de la i - ésima muestra
  er_g <- er_g + (f * sr/sqrt(n))/xr
}

erprom_g <- (er_g/(reps))*100 #Error relativo porcentual promedio
erprom_g

#Si se desconoce el desvío podemos estimarlo con el estimador estadístico S, mencionado en el informe. Ante este
#escenario nuestro pivote tendrá distribución t de Student y no Normal. 

lit_g <- c()
lst_g <- c()

sestm <- c()

ft <- qt(1-((1-nc)/2), df = n -1) #fractil 1-alfa/2 de una t de Student para un dado nivel de confianza 1 - alfa

for (i in 1:reps)
{
  sestm[i] <- sd(muestras_g[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras_g[,i]) #media de la i - ésima muestra
  lit_g[i] <- xr - (ft * sestm[i]/sqrt(n)) #límite inferior de cada muestra
  lst_g[i] <- xr + (ft * sestm[i]/sqrt(n))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont1 <- 0

for(i in 1:reps)
{
  if (lit_g[i] <= 400 && 400 <= lst_g[i])
  {
    cont1 <- cont1 + 1
  }
}

#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el nivel de confianza empírico será cont/100

ncempt_g <- cont1/100
ncempt_g
#Calculamos la longitud promedio de los intervalos de confianza con desvío desconocido

long_g <- 0

for (i in 1:reps)
{
  long_g <- long_g + lst_g[i] - lit_g[i]
}

longprom_g <- long_g/reps #Longitud promedio de los intervalos
longprom_g

#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er_g <- 0

for (i in 1:reps)
{
  sestm[i] <- sd(muestras_g[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras_g[,i]) #media de la i - ésima muestra
  er_g <- er_g + (ft * sestm[i]/sqrt(n))/xr
}

erprom_g <- (er_g/(reps))*100 #Error relativo porcentual promedio
erprom_g

#Repetimos el punto 5 para VA 2 (Gamma): Repetimos el mismo ciclo cambiando la cantidad de muestras (100) y su tamaño (30)

n30 <- 30
reps2 <- 100

#Creamos las muestras aleatorias 
muestras_g2 <- replicate(reps2, rgamma(n30, shape = 4, scale = 100)) #la matriz de muestras de 30 x 100


#Definimos los vectores de los límites superiores e inferiores de los 100 intervalos de confianza

li2_g <- c()
ls2_g <- c()

nc <- 0.95

f <- qnorm(1-((1-nc)/2)) #fractil 1-alfa/2 de una normal estandar para un dado nivel de confianza 1 - alfa

sr <- sqrt(40000) #La varianza de cada VA es alfa . beta^2 = 4 . 100^2 = 40000. Con lo cual, asumiendo desvío conocido, 
                  #este será la raíz cuadrada de 40000

for (i in 1:reps2)

  {
  xr <- mean(muestras_g2[,i]) #media de la i - ésima muestra
  li2_g[i] <- xr - (f * sr/sqrt(n30)) #límite inferior de cada muestra
  ls2_g[i] <- xr + (f * sr/sqrt(n30))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont <- 0

for(i in 1:reps2)
  {
  if (li2_g[i] <= 400 && 400 <= ls2_g[i])
  {
    cont <- cont + 1
  }
}


#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el nivel de confianza empírico será cont/100

ncemp_g2 <- cont/100
ncemp_g2

long2_g <- 0

for (i in 1:reps2)
{
  long2_g <- long2_g + ls2_g[i] - li2_g[i]
}

longprom2_g <- long2_g/reps2 #Longitud promedio de los intervalos
longprom2_g

#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er2_g <- 0

for (i in 1:reps2)
{
  xr <- mean(muestras_g2[,i]) #media de la i - ésima muestra
  er2_g <- er2_g + (f * sr/sqrt(n30))/xr
}

erprom2 <- (er2_g/(reps2))*100 #Error relativo porcentual promedio
erprom2

#Si se desconoce el desvío podemos estimarlo con el estimador estadístico S, mencionado en el informe. Ante este
#escenario nuestro pivote tendrá distribución t de Student y no Normal. 

lit2_g <- c()
lst2_g <- c()

sestm <- c()

ft <- qt(1-((1-nc)/2), df = n30 -1) #fractil 1-alfa/2 de una t de Student para un dado nivel de confianza 1 - alfa

for (i in 1:reps2)
{
  sestm[i] <- sd(muestras_g2[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras_g2[,i]) #media de la i - ésima muestra
  lit2_g[i] <- xr - (ft * sestm[i]/sqrt(n30)) #límite inferior de cada muestra
  lst2_g[i] <- xr + (ft * sestm[i]/sqrt(n30))#límite superior de cada muestra   
}

#Calculamos el intervalo de confianza empíricamente 

cont1 <- 0

for(i in 1:reps2)
{
  if (lit2_g[i] <= 400 && 400 <= lst2_g[i])
  {
    cont1 <- cont1 + 1
  }
}

#Habiendo definido el contador de cuántas veces la media cae dentro del intervalo de confianza calculado, 
#el nivel de confinaza empírico será cont/100

ncempt_g2 <- cont1/100

ncempt_g2
#Calculamos la longitud promedio de los intervalos de confianza con desvío desconocido

long2_g <- 0

for (i in 1:reps2)
{
  long2_g <- long2_g + lst2_g[i] - lit2_g[i]
}

longprom2_g <- long2_g/reps2 #Longitud promedio de los intervalos
longprom2_g
#El error relativo es el error sobre x raya muestral. Luego, el promedio se calcula como

er2_g <- 0

for (i in 1:reps2)
{
  sestm[i] <- sd(muestras_g2[,i]) #desvío estimada de la i - ésima muestra
  xr <- mean(muestras_g2[,i]) #media de la i - ésima muestra
  er2_g <- er2_g + (ft * sestm[i]/sqrt(n30))/xr
}

erprom2 <- (er2_g/(reps2))*100 #Error relativo porcentual promedio
erprom2

#Exportamos los datos de R a Excel

library(writexl)

dfp <- data.frame(pobln, poblg)
dfm <- data.frame(v, w)
df5 <- data.frame(muestras1, muestras_g)
df30 <- data.frame(muestras1g, muestras_g2)
dataset_names <- list('Sheet1' = dfp, 'Sheet2' = dfm, 'Sheet3' = df5, 'Sheet4' = df30)
write_xlsx(dataset_names, "C:\\Users\\ramir\\OneDrive\\Documents\\Estadística Aplicada I\\anexo del tp.xlsx") #Aclaración: Hay que agregar doble barra \\ a todo
