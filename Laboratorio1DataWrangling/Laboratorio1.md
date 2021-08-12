Laboratorio\_1
================
Luis Pedro Zenteno
8/11/2021

# Problema 1

## 1\. Unificación de archivos de Excel y exportación

### Cargar archivos de Excel y dejar variables deseadas.

``` r
library(readxl)
ene_2018<-read_excel("Laboratorio_1/01-2018.xlsx")
feb_2018<-read_excel("Laboratorio_1/02-2018.xlsx")
mar_2018<-read_excel("Laboratorio_1/03-2018.xlsx")
abr_2018<-read_excel("Laboratorio_1/04-2018.xlsx")
may_2018<-read_excel("Laboratorio_1/05-2018.xlsx")
jun_2018<-read_excel("Laboratorio_1/06-2018.xlsx")
jul_2018<-read_excel("Laboratorio_1/07-2018.xlsx")
jul_2018<-jul_2018[,c(-9)]
ago_2018<-read_excel("Laboratorio_1/08-2018.xlsx")
```

    ## New names:
    ## * `` -> ...10

``` r
ago_2018<-ago_2018[,c(-9,-10)]
sep_2018<-read_excel("Laboratorio_1/09-2018.xlsx")
sep_2018<-sep_2018[,c(-9)]
oct_2018<-read_excel("Laboratorio_1/10-2018.xlsx")
oct_2018<-oct_2018[,c(-9)]
nov_2018<-read_excel("Laboratorio_1/11-2018.xlsx")
nov_2018<-nov_2018[,c(-9)]
```

### Agrego fechas a cada uno del los datasts

``` r
ene_2018$fecha<-"01-2018"
feb_2018$fecha<-"02-2018"
mar_2018$fecha<-"03-2018"
abr_2018$fecha<-"04-2018"
may_2018$fecha<-"05-2018"
jun_2018$fecha<-"06-2018"
jul_2018$fecha<-"07-2018"
ago_2018$fecha<-"08-2018"
sep_2018$fecha<-"09-2018"
oct_2018$fecha<-"10-2018"
nov_2018$fecha<-"11-2018"
```

### Consolidad los datasets mensuales en un dataset anual

``` r
entregas_2018<-rbind(ene_2018,feb_2018,mar_2018,abr_2018,may_2018,jun_2018,jul_2018,ago_2018,sep_2018,oct_2018,nov_2018)
head(entregas_2018)
```

    ## # A tibble: 6 × 9
    ##   COD_VIAJE CLIENTE      UBICACION CANTIDAD PILOTO        Q CREDITO UNIDAD fecha
    ##       <dbl> <chr>            <dbl>    <dbl> <chr>     <dbl>   <dbl> <chr>  <chr>
    ## 1  10000001 EL PINCHE O…     76002     1200 Fernando… 300        30 Camio… 01-2…
    ## 2  10000002 TAQUERIA EL…     76002     1433 Hector A… 358.       90 Camio… 01-2…
    ## 3  10000003 TIENDA LA B…     76002     1857 Pedro Al… 464.       60 Camio… 01-2…
    ## 4  10000004 TAQUERIA EL…     76002      339 Angel Va…  84.8      30 Panel  01-2…
    ## 5  10000005 CHICHARRONE…     76001     1644 Juan Fra… 411        30 Camio… 01-2…
    ## 6  10000006 UBIQUO LABS…     76001     1827 Luis Jai… 457.       30 Camio… 01-2…

### Exportar ese archivo en formato csv o Excel

``` r
write.csv(entregas_2018,"entregas_2018.csv")
```

# Problema 2

## Utilizando la función lapply, encuentre la moda de cada vector de una lista de por lo menos 3 vectores.

``` r
## Se utilizó la libreria de modeest la cual contiene una función (mfv) que permite calcular la moda. 
## Base R no tiene una función que calcule la moda hasta donde pude investigar. 
library(modeest)
lista<-list(vec1=sample(1:100,size = 300,replace = TRUE),
              vec2=sample(1:100,size = 300,replace = TRUE),
              vec3=sample(1:100,size = 300,replace = TRUE)
)
modas<-lapply(lista,mfv)
modas
```

    ## $vec1
    ## [1] 84
    ## 
    ## $vec2
    ## [1] 20 59
    ## 
    ## $vec3
    ## [1] 68 92

# Problema 3

``` r
library(readr)
parque_vehicular_enero2019<-read_delim("INE_PARQUE_VEHICULAR_080219.txt",delim = "|",show_col_types = FALSE, locale=locale(encoding = "UTF-8"))
```

    ## New names:
    ## * `` -> ...11

    ## Warning: One or more parsing issues, see `problems()` for details

``` r
head(parque_vehicular_enero2019,n = 10)
```

    ## # A tibble: 10 × 11
    ##    ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##        <dbl> <chr> <chr>               <chr>            <chr>          
    ##  1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ##  2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ##  3      2007 05    SAN MARCOS          "OCOS"           2007           
    ##  4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ##  5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ##  6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ##  7      2007 05    QUETZALTENANGO      "QUETZALTENANGO" 2007           
    ##  8      2007 05    SUCHITEPEQUEZ       "CHICACAO"       2007           
    ##  9      2007 05    ESCUINTLA           "ESCUINTLA"      2007           
    ## 10      2007 05    GUATEMALA           "MIXCO"          2007           
    ## # … with 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>
