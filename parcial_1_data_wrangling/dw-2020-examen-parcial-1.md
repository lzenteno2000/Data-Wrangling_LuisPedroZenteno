dw-2020-parcial-1
================
Tepi
9/3/2020

# Examen parcial Luis Pedro Zenteno

Indicaciones generales:

  - Usted tiene el período de la clase para resolver el examen parcial.

  - La entrega del parcial, al igual que las tareas, es por medio de su
    cuenta de github, pegando el link en el portal de MiU.

  - Pueden hacer uso del material del curso e internet (stackoverflow,
    etc.). Sin embargo, si encontramos algún indicio de copia, se
    anulará el exámen para los estudiantes involucrados. Por lo tanto,
    aconsejamos no compartir las agregaciones que generen.

## Sección I: Preguntas teóricas.

  - Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
    deberá responder 5. Las 5 a responder estarán determinadas por un
    muestreo aleatorio basado en su número de carné.

  - Ingrese su número de carné en `set.seed()` y corra el chunk de R
    para determinar cuáles preguntas debe responder.

<!-- end list -->

``` r
set.seed(20190516) 
v<- 1:10
preguntas <-sort(sample(v, size = 6, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 3, 4, 5, 7, 8"

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:

<!-- end list -->

  - `str()`

<!-- end list -->

``` 
  glimpse()
```

  - `df[,c("a","b")]`

<!-- end list -->

``` 
   df %>% 
   select("a","b")
```

  - `names(df)[4] <- "new_name"` donde la posición 4 corresponde a la
    variable `old_name`

<!-- end list -->

``` 
   df %>% 
   rename('old_name'='new_name')
```

  - `df[df$variable == "valor",]`

<!-- end list -->

``` 
  df %>% 
  select(variable) %>%
  filter(variabe= "valor")
```

3.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

<!-- end list -->

  - Primero que nada, las funciones de la familia apply permiten tener
    más ordenado y limpio el código. Además, permiten realizar una
    función en especíco a un grupo de elementos al mismo tiempo cuando
    se manda a llamar la función de apply , ya sea una lista o vector
    por lo que simplifica las lineas de código para poder lograrlo.
    Además, se dice que es más rápido a nivel de eficiencia usar las
    funciones de apply, que usar ciclos.

<!-- end list -->

4.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R?

<!-- end list -->

  - Se utiliza `==` cuando se quiere hacer una evaluación lógica,
    mientras que `=`se utiliza para asignar valores a una variable.

<!-- end list -->

5.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`?

<!-- end list -->

  - La forma correcta para cargar el archivo es las siguiente: `textfile
    <- read_delim('archivo.txt', delim=':')`

<!-- end list -->

7.  ¿Qué pasa si quiero agregar una nueva categoría a un factor que no
    se encuentra en los niveles existentes?

<!-- end list -->

  - En factores, una categoria se interpreta como un nivel. Si lo que se
    desea es agregar un nivel, siempre y cuando se agregue la nueva
    categoria al vector que irá en la funcion factor(), no habrá ningún
    problema, *se agregará correctamente* .

<!-- end list -->

8.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?

<!-- end list -->

  - El nuevo elemento de la variable de tipo factor será `<NA>` , y
    arrojará un warning: `invalid factor level, NA generated`. Esto
    porque no hay una categoría existente para poder identificarlo, y
    como no la encuentrá, es por eso que el resultado es `NA`.

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

``` r
library(gtools)
respuesta<-factorial(10) / (factorial(5) * (factorial(5)))
comprobación<-choose(10,5)
cat("Son posibles ",respuesta,"exámenes de 5 peguntas con un banco de 10 preguntas.")
```

    ## Son posibles  252 exámenes de 5 peguntas con un banco de 10 preguntas.

## Sección II Preguntas prácticas.

  - Conteste las siguientes preguntas utilizando sus conocimientos de R.
    Adjunte el código que utilizó para llegar a sus conclusiones en un
    chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas teóricas

## A

Primero que nada, se determinó que no es el mismo precio al que se
vendieron las unidades. Por lo tanto, se evidencia que se trata de
diferentes productos *(en este caso se asumirá que son tomates y su
variedad de tamaños afecta el precio)*, por lo que no se analizáran suma
de unidades de plaza para el análisis de rentabilidad (no es
significativo de ingresos). Además, se asume las unidades y ventas
negativas como pérdidas, lo cual hace sentido asumiendo que los tomates
llegan a ser périddas después de cierto tiempo que no se pueden vender.
Se dejarán en el análisis para hacer la suma de ventas y así brindar una
rentabilidad más efectiva.

``` r
library(dplyr)
library(knitr)
library(highcharter)
parcial_anonimo <- readRDS("~/Desktop/UFM/SEXTO SEMESTRE/Data Wrangling/GitHub/Data-Wrangling/parcial_1_data_wrangling/parcial_anonimo.rds")

parcial_anonimo$precio<-parcial_anonimo$Venta/parcial_anonimo$`Unidades plaza`

ventas_clientes <- parcial_anonimo %>%
  select(Cliente, Pais, Venta) %>% 
  group_by(Cliente) %>%
  summarise(Cliente=unique(Cliente),Paises=n_distinct(Pais),Ventas=sum(Venta)) %>% 
  filter(Paises>1)

#Cliente con más ventas es en teoria el más rentable, ya que esta suma también incluye las perdidas
cliente_max_ventas<- ventas_clientes %>% 
  filter(Ventas==max(Ventas))
kable(cliente_max_ventas,align = "c")
```

| Cliente  | Paises | Ventas  |
| :------: | :----: | :-----: |
| a17a7558 |   2    | 19817.7 |

![Gráfica de Ventas y Clientes](grafica.png) El Cliente con más ventas
netas de tomates en teoria el más rentable, ya que esta suma de ventas
también incluye las perdidas. Por lo
    tanto:

    ## El cliente más rentable es: a17a7558 con ventas de Q. 19817.7 en tomates y actualmente está en 2 paises.

## B

En este análisis también se consideran las cantidades y ventas negativs
como pérdidas. Por lo tanto, el criterio para “pérdidas considerables”
sera aquellos Territorios que este una desviación estándar por arriba de
la media de pérdidas por territorio, que además, no sean territorios
significativos en ventas. Es decir, no estén en el TOP 10 de los
territorios según la tabla de Pareto. Esto porque se hace evidente que
muchos terrotorios al tener más ventas, generarán más perdidas. Pero si
no están en el TOP 10 de Pareto, es probable que no sean tan
significativas sus ventas.

``` r
library(dplyr)
library(formattable)
perdidas_territorio<- parcial_anonimo %>% 
  select(Territorio,Venta) %>% 
  filter(Venta<0) %>% 
  group_by(Territorio) %>% 
  summarise(Territorio=unique(Territorio),Perdidas=sum(Venta))

summary(perdidas_territorio$Perdidas)
```

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -14985.02   -748.59   -232.32   -860.33    -96.66     -5.56

``` r
media<-mean(perdidas_territorio$Perdidas)
desviacion<-sd(perdidas_territorio$Perdidas)
considerable<-media-desviacion

territorios_perdida_considerable<- perdidas_territorio %>% 
  filter(Perdidas<considerable)

kable(territorios_perdida_considerable,caption = "Territorios con perdidas considerables")
```

| Territorio |   Perdidas |
| :--------- | ---------: |
| 1d407777   |  \-3299.56 |
| 2e812869   |  \-3056.10 |
| 69c1b705   |  \-3370.11 |
| 72520ba2   |  \-3760.95 |
| 77192d63   |  \-5640.55 |
| bc8e06ed   |  \-3268.58 |
| f7dfc635   | \-14985.02 |

Territorios con perdidas
considerables

``` r
#pareto solo para saber si algún territorio es más significativo en las ventas
pareto<-parcial_anonimo %>% 
  group_by(Territorio) %>% 
  summarise(Ventas = sum(Venta))%>% 
  arrange(-Ventas)

pareto<-pareto %>%  mutate(Territorios = row_number(),
         porcentaje_territorios = (Territorios/max(Territorios)),
         ventas_ac = cumsum(Ventas),
         porcentaje_ventas = (ventas_ac/max(ventas_ac)))
top_10_territorios<-pareto[1:10,]

kable(top_10_territorios,caption = "TOP 10 Territorios en tabla Pareto")
```

| Territorio |   Ventas | Territorios | porcentaje\_territorios | ventas\_ac | porcentaje\_ventas |
| :--------- | -------: | ----------: | ----------------------: | ---------: | -----------------: |
| f7dfc635   | 916785.7 |           1 |               0.0096154 |   916785.7 |          0.1458403 |
| a0d39798   | 441721.7 |           2 |               0.0192308 |  1358507.4 |          0.2161085 |
| 72520ba2   | 356377.2 |           3 |               0.0288462 |  1714884.6 |          0.2728002 |
| bc8e06ed   | 329852.9 |           4 |               0.0384615 |  2044737.6 |          0.3252725 |
| 77192d63   | 247252.0 |           5 |               0.0480769 |  2291989.5 |          0.3646048 |
| 23e9d55d   | 239481.1 |           6 |               0.0576923 |  2531470.7 |          0.4027010 |
| 1d407777   | 204601.0 |           7 |               0.0673077 |  2736071.6 |          0.4352485 |
| fed6647d   | 194384.1 |           8 |               0.0769231 |  2930455.7 |          0.4661707 |
| c57e6d42   | 179711.4 |           9 |               0.0865385 |  3110167.1 |          0.4947588 |
| 1c81fb6c   | 156006.0 |          10 |               0.0961538 |  3266173.1 |          0.5195759 |

TOP 10 Territorios en tabla
Pareto

``` r
respuesta<-left_join(territorios_perdida_considerable,top_10_territorios)
```

    ## Joining, by = "Territorio"

``` r
respuesta<-respuesta %>% 
  arrange(Ventas) %>% 
  select(Territorio,Perdidas) %>% 
  tail(2)

kable(respuesta,caption = "Territorios con perdidas considerables, sin ventas significativas")
```

| Territorio |  Perdidas |
| :--------- | --------: |
| 2e812869   | \-3056.10 |
| 69c1b705   | \-3370.11 |

Territorios con perdidas considerables, sin ventas
    significativas

    ## Los territorios con pérdidas más considerables son:  2e812869 y 69c1b705

Son aquellos territorios que sus perdidas estan una desviación estándar
por arriba de la media de pérdidas, y tampoco están en el TOP 10
Territorios que representa el 50% de las ventas de la empresa. Si no se
toma en cuenta el factor de importancia en ventas, los territorios a
eliminar son 7 territorios de la tabla
“territorios\_perdida\_considerable”
