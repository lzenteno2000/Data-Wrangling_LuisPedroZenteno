## Instalar librerias
install.packages("dplyr")
install.packages("RMySQL")
install.packages("lubridate")
install.packages("openxlsx")
install.packages("tidyverse")
install.packages("stringr")
install.packages("readr")

library("dplyr")
library("RMySQL")
library("lubridate")
library("openxlsx")
library("tidyverse")
library("stringr")
library("readr")

utils::install.packages()
?install.packages

# folder/location

getwd()
?setwd()

# Data types ans structures in R

### Strings and Character
string <- "this is a string"
class(string)
typeof(string)
length(string)
nchar(string)

### double 
number<-2344
class(number)
length(number)

### Integers
integer<-3L
class(integer)
length(integer)

### Logical
logical<- FALSE
class(logical)
length(logical)

### Vectores
vector<-c("string","this","is")
length(vector)
vector[2]

### factores
vector_2<-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
factor<-factor(vector_2)

### factores ordenados
factor_2<-ordered(factor,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))

### dataframe

df<-data.frame(
  col1<-c("this", "is", "a","vector","of","strings"),
  col2<-1:6,
  col3<-letters[1:6],
  stringsAsFactors = FALSE)

df2<-data.frame(
  col1<-c("this", "is", "a","vector","of","strings"),
  col2<-1:6,
  col3<-letters[1:6])

str(df)
names(df)
names(df)<-c("Columna1","Columna2","Columna3")
names(df)
colnames(df)
head(df,10)
tail(df,5)
nrow(df)
