### base R
text_file<- 'data/quijote.txt'

## ReadLine base R
readLines(text_file,n=10)

readLines(text_file,n=10,encoding = "UTF-8",skipNul = T)

library(readr)
quijote_lines<-read_lines(text_file)
str(quijote_lines)


## Obtener substrings de un vector de caracteres
substr(quijote_lines,1,150)
read_lines(text_file,n_max=20)


####
library(tidytext)
library(tidyverse)

quijote_frame<-data_frame(txt=quijote_lines)


# cuales son las palabras más utilizadas en este archivo de texto 

## generar una tabla de frecuencias

quijote_words<-unnest_tokens(quijote_frame,output=word,input=txt, token="words")
table(quijote_words)

## count 
quijote_count<- count(quijote_words,word,sort=T)
quijote_count


## remover stopwords
library(quanteda)

spanish_stopword<-data_frame(word=quanteda::stopwords(language = "es"))
quijote_words_clean<-anti_join(quijote_words,spanish_stopword)
quijote_count_clean<- count(quijote_words_clean,word,sort=T)


## cargar archivos csv

hour<- read_csv('data/hour.csv')
read_delim("data/hour.csv",delim = ",")


## descargar archivos excel
library(readxl)
bancos_activos<-read_excel("data/bancos.xlsx")
bancos_agencias<-read_excel("data/bancos.xlsx",sheet = "agencias")


is.na #incluir na
df[!is.na(df%col1),]

colSums(df)

df[v>4,]

## apply functions

find_sample<- function(x,s){
  
  for_index<-sample(1:nrow,size = s,replace=FALSE)
  new_df<-x[for_index,]
   return(new df)
  
}


generate_df<- function(x){

   return(
     data.frame(
       a=sample(letters,size = 10,replace = TRUE)
       b=sample(1:10,size = 10,replace = TRUE)
     )
}
generate_df
lista<-lapply(1:400,generate_df)


write.csv


