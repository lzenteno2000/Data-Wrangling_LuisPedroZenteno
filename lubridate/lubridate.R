
library(lubridate)
library(nycflights13)

#hoy y ahora
today()
now()

#fechas de texto
x<-"1994 October 27th"
ymd(x)

y<-"27.10.1994"
dmy(y)

###
z<-"oct,29th 1994 14:00"
mdy_hm(z)

a<-19942710
ydm(a)

##diferencia de tiempos
#La fecha de aterrizaje y el primer paso en la luna
date_landing <- mdy("July 20, 1969")
moment_step <- mdy_hms("July 20, 1969, 02:56:15", tz = "UTC")

difftime(now(),moment_step,units="days")

##suma de tiempos
mon_1pm<-dmy_hm("27 Sep 2021 13:00")
mon_1pm+weeks(x=1)

## consideracines importantes
#durations: cronometro
##periods: genera, como hablamos

feb<-dmy("28 feb 2020")
feb+ dyears(1)
feb+years(1)

this_jan<-ymd("2021-01-31")+months(1)
this_jan<-ymd("2021-01-31")

add_with_rollback(this_jan, months(1),roll_to_first =FALSE )
add_with_rollback(this_jan, months(1),roll_to_first =TRUE )


#GENERAR SECUENCIAS DE FEHCAS de 1 a 12 periodos de 1 mes
month_seq<-1:12*months(1)
month_seq+this_jan

oct_21<-ymd("2021-10-31")
seq(this_jan,oct_21,"weeks")


#flights
library(dplyr)
View(flights)

flights %>% 
  select(year,month,day,hour,minute,arr_time)

## makedate
make_date(year=1995,month=11,day=21)
flights<-flights %>% 
  mutate(departure=make_date(year,month,day))

flights$departure

## makedatetime
flights<-flights %>% 
  mutate(departure=make_datetime(year,month,day,minute))

flights$departure >=ydm(20120401)



