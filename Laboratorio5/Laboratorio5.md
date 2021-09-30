Laboratorio5
================
Luis Pedro Zenteno
9/29/2021

# Parte 1: Predecir un eclipse solar

``` r
library(lubridate)
eclipse_historico<- mdy_hms("August 21st, 2017,18:26:40")
synodic_month<-days(29)+hours(12)+minutes(44)+seconds(3)
saros<- synodic_month * 223



eclipse_siguiente<-eclipse_historico+saros
eclipse_siguiente
```

    ## [1] "2035-09-02 02:09:49 UTC"

# Parte 2: Agrupaciones y operaciones con fechas

Se decidió eliminar los datos con fechas ambiguas, que no soportó
lubridate debido a que no se pudo convertir las fechas de una manera
eficiente debido al typo encontrado en algunas fechas en excel. Además,
se decidió eliminar aquellas filas donde no se tuviera Cod. En
conclusión, se trabajará con una data más resumida, pero ajustada y
completa para hacer un análisis más integro.

Se entiende como “ocupado” la cantidad de llamadas que se recibieron en
el periodo de tiempo determinado.

``` r
library(readxl)
data<-read_excel("data.xlsx")
data$`Fecha Creación`<-dmy(data$`Fecha Creación`)
data$`Fecha Final`<-dmy(data$`Fecha Final`)
data$`Hora Creación`<- hms(substring(data$`Hora Creación`, 12, 19))
data$`Hora Final` <- hms(substring(data$`Hora Final`, 12, 19))
data$Cod[data$Cod==0]<-NA
data<-na.omit(data)
```

  - ¿En qué meses existe una mayor cantidad de llamadas por código?

<!-- end list -->

``` r
library(dplyr)
library(knitr)
llamadas_meses<- data %>% 
  mutate(mes=month(`Fecha Creación`)) %>% 
  group_by(Cod, mes) %>% 
  tally()

  respuesta<-NA

  maximos <- function(llamadas_meses,unicos){
  llamadas_meses %>% 
    select (Cod,mes,n) %>% 
    filter(Cod==unicos, n==max(n)) 
  
 
}
unicos<-unique(llamadas_meses$Cod)  


respuestas <-  rbind(maximos(llamadas_meses,unicos[1]),
                     maximos(llamadas_meses,unicos[2]),
                     maximos(llamadas_meses,unicos[3]),
                     maximos(llamadas_meses,unicos[4]),
                     maximos(llamadas_meses,unicos[5]),
                     maximos(llamadas_meses,unicos[6]))

respuestas$mes<-month(respuestas$mes, label=TRUE, abbr=FALSE)
kable(respuestas,align = "c",caption = "Maximo de llamadas por codigo")                  
```

|             Cod              |   mes   |  n   |
| :--------------------------: | :-----: | :--: |
| Actualización de Información |   May   | 1045 |
|        Cancelaciones         |  July   | 2515 |
|            Cobros            | January | 425  |
|          Consultas           | October | 6650 |
|         Empresarial          | October | 1926 |
|         Otros/Varios         | January | 694  |

Maximo de llamadas por codigo

  - ¿Qué día de la semana es el más ocupado?

<!-- end list -->

``` r
día_ocupado <- data %>%
  group_by(dia_mas_ocupado=weekdays(ymd(`Fecha Creación`))) %>% 
  tally() %>% 
  filter(n==max(n))
kable(día_ocupado)
```

| dia\_mas\_ocupado |     n |
| :---------------- | ----: |
| Monday            | 21715 |

  - ¿Qué mes es el más ocupado?

<!-- end list -->

``` r
mes_ocupado <- data %>%
  group_by(mes_mas_ocupado=month(ymd(`Fecha Creación`),label = TRUE,abbr = FALSE)) %>% 
  tally() %>% 
  filter(n==max(n))
kable(mes_ocupado)
```

| mes\_mas\_ocupado |     n |
| :---------------- | ----: |
| October           | 12953 |

4.  ¿Existe una concentración o estacionalidad en la cantidad de
    llamadas?

<!-- end list -->

``` r
library(highcharter)
```

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

``` r
mensual <- data %>%
  group_by(mes=month(ymd(`Fecha Creación`),label = TRUE,abbr = FALSE)) %>% 
  tally() %>% 
  hchart("line",hcaes(mes,n)) %>% 
  hc_title(text="Llamadas por mes" ) %>% 
  hc_subtitle(text="Parece fluctuar cada mes, pero no en valores significativos")
mensual
```

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<!--html_preserve-->

<div id="htmlwidget-1d3eda50996ada1094a0" class="highchart html-widget" style="width:100%;height:500px;">

</div>

<script type="application/json" data-for="htmlwidget-1d3eda50996ada1094a0">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Llamadas por mes"},"yAxis":{"title":{"text":"n"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}}},"series":[{"group":"group","data":[{"mes":"January","n":12951,"y":12951,"name":"January"},{"mes":"February","n":10583,"y":10583,"name":"February"},{"mes":"March","n":12946,"y":12946,"name":"March"},{"mes":"April","n":12210,"y":12210,"name":"April"},{"mes":"May","n":12885,"y":12885,"name":"May"},{"mes":"June","n":11983,"y":11983,"name":"June"},{"mes":"July","n":12832,"y":12832,"name":"July"},{"mes":"August","n":12804,"y":12804,"name":"August"},{"mes":"September","n":12315,"y":12315,"name":"September"},{"mes":"October","n":12953,"y":12953,"name":"October"},{"mes":"November","n":12217,"y":12217,"name":"November"},{"mes":"December","n":12825,"y":12825,"name":"December"}],"type":"line"}],"xAxis":{"type":"category","title":{"text":"mes"},"categories":null},"subtitle":{"text":"Parece fluctuar cada mes, pero no en valores significativos"}},"theme":{"chart":{"backgroundColor":"transparent"},"colors":["#7cb5ec","#434348","#90ed7d","#f7a35c","#8085e9","#f15c80","#e4d354","#2b908f","#f45b5b","#91e8e1"]},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->

``` r
enero <- data %>%
  group_by(dia=ymd(`Fecha Creación`)) %>% 
  filter(month(dia)==01) %>% 
  tally() %>% 
  hchart("line",hcaes(dia,n)) %>% 
  hc_title(text="Llamadas de enero" ) %>% 
  hc_subtitle(text="No parece exisir estacionalidad en los días de la semana")
enero
```

<!--html_preserve-->

<div id="htmlwidget-57dde9a902a5a284d1e1" class="highchart html-widget" style="width:100%;height:500px;">

</div>

<script type="application/json" data-for="htmlwidget-57dde9a902a5a284d1e1">{"x":{"hc_opts":{"chart":{"reflow":true},"title":{"text":"Llamadas de enero"},"yAxis":{"title":{"text":"n"},"type":"linear"},"credits":{"enabled":false},"exporting":{"enabled":false},"boost":{"enabled":false},"plotOptions":{"series":{"label":{"enabled":false},"turboThreshold":0,"showInLegend":false},"treemap":{"layoutAlgorithm":"squarified"},"scatter":{"marker":{"symbol":"circle"}}},"series":[{"group":"group","data":[{"dia":"2017-01-13","n":657,"x":1484265600000,"y":657},{"dia":"2017-01-14","n":679,"x":1484352000000,"y":679},{"dia":"2017-01-15","n":686,"x":1484438400000,"y":686},{"dia":"2017-01-16","n":687,"x":1484524800000,"y":687},{"dia":"2017-01-17","n":708,"x":1484611200000,"y":708},{"dia":"2017-01-18","n":649,"x":1484697600000,"y":649},{"dia":"2017-01-19","n":655,"x":1484784000000,"y":655},{"dia":"2017-01-20","n":704,"x":1484870400000,"y":704},{"dia":"2017-01-21","n":726,"x":1484956800000,"y":726},{"dia":"2017-01-22","n":680,"x":1485043200000,"y":680},{"dia":"2017-01-23","n":714,"x":1485129600000,"y":714},{"dia":"2017-01-24","n":696,"x":1485216000000,"y":696},{"dia":"2017-01-25","n":690,"x":1485302400000,"y":690},{"dia":"2017-01-26","n":670,"x":1485388800000,"y":670},{"dia":"2017-01-27","n":653,"x":1485475200000,"y":653},{"dia":"2017-01-28","n":627,"x":1485561600000,"y":627},{"dia":"2017-01-29","n":687,"x":1485648000000,"y":687},{"dia":"2017-01-30","n":669,"x":1485734400000,"y":669},{"dia":"2017-01-31","n":714,"x":1485820800000,"y":714}],"type":"line"}],"xAxis":{"type":"datetime","title":{"text":"dia"},"categories":null},"subtitle":{"text":"No parece exisir estacionalidad en los días de la semana"}},"theme":{"chart":{"backgroundColor":"transparent"},"colors":["#7cb5ec","#434348","#90ed7d","#f7a35c","#8085e9","#f15c80","#e4d354","#2b908f","#f45b5b","#91e8e1"]},"conf_opts":{"global":{"Date":null,"VMLRadialGradientURL":"http =//code.highcharts.com/list(version)/gfx/vml-radial-gradient.png","canvasToolsURL":"http =//code.highcharts.com/list(version)/modules/canvas-tools.js","getTimezoneOffset":null,"timezoneOffset":0,"useUTC":true},"lang":{"contextButtonTitle":"Chart context menu","decimalPoint":".","downloadJPEG":"Download JPEG image","downloadPDF":"Download PDF document","downloadPNG":"Download PNG image","downloadSVG":"Download SVG vector image","drillUpText":"Back to {series.name}","invalidDate":null,"loading":"Loading...","months":["January","February","March","April","May","June","July","August","September","October","November","December"],"noData":"No data to display","numericSymbols":["k","M","G","T","P","E"],"printChart":"Print chart","resetZoom":"Reset zoom","resetZoomTitle":"Reset zoom level 1:1","shortMonths":["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],"thousandsSep":" ","weekdays":["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]}},"type":"chart","fonts":[],"debug":false},"evals":[],"jsHooks":[]}</script>

<!--/html_preserve-->
