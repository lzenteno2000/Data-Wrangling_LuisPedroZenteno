library(dplyr)
library(highcharter)
library(tidyverse)
library(readr)

df<-read_delim("spotify.csv",";",escape_double=FALSE,trim_ws=TRUE)

#Ver dataset
View(df)

# base r
str(df)

##dplyr
df<- mutate_if(df,is.character,as.factor)
glimpse(df)

### renombrar columnas con base r 

names(df)[4]<-"top_genre"
names(df)


#renombrar con dplyr
rename(df,top_genre=`top genre`)

#anidando
head(select(df,artist,year))

# pipe operator
df %>% 
  select(artist,year) %>%
  head()

## renombrar columnaas con dplyr

df %>% 
  rename(top_genre=`top genre`)

##todas columnas -1

head(df[-1])

# utilizando dplyr
df %>% 
  select(-X1) %>% 
  head()

df %>% 
  select(artist,title,year) %>% 
  filter(year==2010) %>% 
  head() %>% 
  View()


## cuanto sartistas teenemos por año 
df %>% 
  #select(year,artist) %>% 
  group_by(year) %>% 
  summarise(artistas=n())

#cuantos artistas unicos tenemos por año 
df %>% 
  select(year,artist) %>% 
  group_by(year) %>% 
  summarise(artistas_unicos=n_distinct(artist))

# artistas unicos existen en la base 
df %>% 
  summarise(artistas_unicos=n_distinct(artist))


## canciones unicas en el dataset
df %>% 
  summarise(canciones_unicas=n_distinct(title))

## canciones  repetidas
df %>% 
  summarise(canciones_unicas= n()-n_distinct(title))

# canciones repetidas cuales 
df %>% 
  group_by(title) %>% 
  summarise(canciones_repetidas=n()) %>% 
  filter(canciones_repetidas > 1)

## solución tepi
df %>% 
  group_by(title) %>% 
  summarise(freq=n()) %>% 
  group_by(freq) %>% 
  summarise(n())

#cuantas canciones se llaman igual que soy diferentes artistas
df %>% 
  group_by(artist,title) %>% 
  summarise(canciones=ifelse(n()>1,n(),NA)) %>% 
  na.omit()


##que canciones de que artistas aparecen mas de un año 
df %>% 
  group_by(artist,title) %>% 
  summarise(canciones=ifelse(n_distinct(year)>1,n(),NA)) %>% 
  na.omit()



#cuales artistas han tenido mas de una cancion que fue popular mas de un año

df %>% 
  group_by(artist,title) %>% 
  summarise(canciones=ifelse(n_distinct(year)>1,n(),NA)) %>% 
  na.omit() %>% 
  group_by(artist) %>% 
  summarise(count=n()) %>% 
  filter(count>1)

# hughcharter 
df<-mutate_if(df,is.character, as.factor)
df$title <- iconv(df$title, to = "UTF-8")
df$artist <- iconv(df$artist, to = "UTF-8")


## cuantos artistas distintos tenemos por año

df %>% 
  select(year,artist) %>% 
  group_by(year) %>% 
  summarise(n=n_distinct(artist)) %>% 
  hchart("column",hcaes(year,n)) %>% 
  hc_title(text="Artistas distintos por año" ) %>% 
  hc_subtitle(text="2019 menor variedad de artistas que 2015")


















