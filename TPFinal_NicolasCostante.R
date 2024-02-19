##========================================================##
##                                                        ##
##   TALLER DE PROGRAMACION                               ##
##   Trabajo Final: Canserbero                            ##
##   https://drive.google.com/drive/folders/1tKdg8yIdhv5ZX7NvZg7Dy2KvK7D5xYrc?usp=drive_link                   
##                                                        ##
##   Alumno: Nicolas E. Costante                          ##
##   Profesores:Sergio de Raco                            ##
##              Semeshenko Viktoriya                      ##
##                                                        ##
##========================================================##

## Modulo Limpieza workspace, consola y encoding ======================================##
rm(list = ls())
cat("\014")
options(encoding = "utf-8")
########################################################################################################################
## Modulo instalacion paquetes ========================================================##

install.packages('geniusr')
install.packages('showtext')
install.packages('tidyverse')
install.packages('tidytext')
install.packages('pdftools')
install.packages('dplyr')
install.packages('stopwords')
install.packages('tidytext')
install.packages('stringi')
install.packages('stringr')
install.packages('ggplot2')
install.packages('scales')
install.packages('tidyr')
install.packages('widyr')
install.packages('ggraph')
install.packages('igraph')
install.packages('quanteda')
install.packages('topicmodels')
install.packages('cvTools')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages("reshape")
install.packages("stringi")
install.packages("reshape2")
install.packages("aplpack")
install.packages("ggimage")
install.packages('wordcloud2')
########################################################################################################################
## Modulo uso de paquetes =============================================================##

library(tidyverse)
library(tidytext)
library(showtext)
library(geniusr)
library(pdftools)
library(dplyr)
library(stopwords)
library(tidytext)
library(stringi)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(widyr)
library(ggraph)
library(igraph)
library(quanteda)
library(topicmodels)
library(cvTools)
library(wordcloud2)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(reshape)
library(stringi)
library(reshape2)
library(ggplot2)
library(aplpack)
library(ggimage)


########## Modulo API GENIUS #############

# Esta fue mi primer alternativa, trabaje bastante con la API, leí documentación, pero lamentablemente cuando hacia los bucles para traer la información necesaria, nunca terminó de correr el código (incluso corriendo mas de 30hs)
# 
# Creacion de API y Usuario en https://genius.com/api-clients
# 
# Logeo en API con el codigo genius_token y el token que nos aprovisiona la web
# 
# AMvOQAwUnSGmdPn4gu-kNeGEnVovB2ANJwpjjiczVeBEFWs14q_LgPY_2JGktRKb
#  
#  Prueba para ver si nos logeamos correctamente 
# 
# get_song(song_id = 90479)
# 
# # Busco ID del artista Canserbero
# search_artist("Canserbero")
# songs <- get_artist_songs_df(26333)
# ## este no me sirve asi: discografia <-  get_album_df(26333) 
# 
# get_album_tracklist_search(artist_name = "Canserbero",
#                            album_name = "Muerte")
# 
# get_album_tracklist_search(artist_name = "Canserbero",
#                            album_name = "Vida")

########################################################################################################################


## Modulo Disco Muerte =============================================================##

# Comentario: Como no pude correr desde la API, descargue las liricas de los temas de forma manual en diferentes .txt por tema 

## Generación del corpus del disco Muerte con el paquete "tm" =============================================================##

path <- file.path('C:/Users/nicol/Desktop/TallerProg/TpFinalNC/Muerte/txt')
docs   <- Corpus(DirSource(path), readerControl = list(language = 'spa'))

## Limpieza de corpus, números, mayusculas, espacios, stopswords =============================================================##

muerte <- tm_map(docs, removePunctuation, preserve_intra_word_dashes = TRUE)
muerte <- tm_map(muerte, content_transformer(tolower))
muerte <- tm_map(muerte, removeWords, stopwords('es'))
muerte <- tm_map(muerte, content_transformer(stripWhitespace))
muerte <- tm_map(muerte, removeNumbers)

## ALTERNATIVA - se obtuvieron mismos resultados

# muerte <- gsub("\\r", " ", docs)
# muerte <- gsub("\\n", "", muerte)
# muerte <- gsub("\\d\\K\\.(?=\\d)", "", muerte, perl = TRUE)#  Los puntos de separador de mil, lo sustituimos por un espacio
# muerte<-paste(muerte, collapse = '')

# Vectorización =============================================================##

dtm <- DocumentTermMatrix(docs, control =  list(stopwords=FALSE, wordLengths=c(0, Inf)))


# Análisis de Sentimiento =============================================================##

path        <- file.path('C:/Users/nicol/Desktop/TallerProg/TpFinalNC/positive_words_es.txt')
listapositiva <- read.csv(path,header=F,quote="",fileEncoding="Latin1")
listapositiva <- as.matrix(listapositiva)
listapositiva <- as.vector(listapositiva)

ppositivas.tdm<- DocumentTermMatrix(docs, control = list(tokenize    = 'word',
                                                                 dictionary  = listapositiva,
                                                                 stopwords   = FALSE,
                                                                 wordLengths = c(0, Inf)))
ppositivas.tdm

path        <- file.path('C:/Users/nicol/Desktop/TallerProg/TpFinalNC/negative_words_es.txt')
listanegativa <- read.csv(path,header=F,quote="",fileEncoding="Latin1")
listanegativa <- as.matrix(listanegativa)
listanegativa <- as.vector(listanegativa)

pnegativas.tdm<- DocumentTermMatrix(docs, control = list(tokenize    = 'word',
                                                                 dictionary  = listanegativa,
                                                                 stopwords   = FALSE,
                                                                 wordLengths = c(0, Inf)))
pnegativas.tdm

positivas <- apply(ppositivas.tdm,1,sum)
negativas <- apply(pnegativas.tdm,1,sum)
num_total <- apply(dtm,1,sum)

totalmuerte.df  <- data.frame(positive = positivas,
                        negative = negativas,
                        neutral  = num_total - positivas - negativas)
totalmuerte.df


########################################################################################################################

## Modulo Disco Vida =============================================================##
## Generación del corpus del disco Vida con el paquete "tm"

path <- file.path('C:/Users/nicol/Desktop/TallerProg/TpFinalNC/Vida/txt')
docs   <- Corpus(DirSource(path), readerControl = list(language = 'spa'))


## Limpieza de corpus, números, mayusculas, espacios, stopswords

vida <- tm_map(docs, removePunctuation, preserve_intra_word_dashes = TRUE)
vida <- tm_map(vida, content_transformer(tolower))
vida <- tm_map(vida, removeWords, stopwords('es'))
vida <- tm_map(vida, content_transformer(stripWhitespace))
vida <- tm_map(vida, removeNumbers)

# Vectorización

dtm <- DocumentTermMatrix(docs, control =  list(stopwords=FALSE, wordLengths=c(0, Inf)))


# Análisis de Sentimiento

path        <- file.path('C:/Users/nicol/Desktop/TallerProg/TpFinalNC/positive_words_es.txt')
listapositiva <- read.csv(path,header=F,quote="",fileEncoding="Latin1")
listapositiva <- as.matrix(listapositiva)
listapositiva <- as.vector(listapositiva)

ppositivas.tdm<- DocumentTermMatrix(docs, control = list(tokenize    = 'word',
                                                         dictionary  = listapositiva,
                                                         stopwords   = FALSE,
                                                         wordLengths = c(0, Inf)))
ppositivas.tdm

path        <- file.path('C:/Users/nicol/Desktop/TallerProg/TpFinalNC/negative_words_es.txt')
listanegativa <- read.csv(path,header=F,quote="",fileEncoding="Latin1")
listanegativa <- as.matrix(listanegativa)
listanegativa <- as.vector(listanegativa)

pnegativas.tdm<- DocumentTermMatrix(docs, control = list(tokenize    = 'word',
                                                         dictionary  = listanegativa,
                                                         stopwords   = FALSE,
                                                         wordLengths = c(0, Inf)))
pnegativas.tdm

positivas <- apply(ppositivas.tdm,1,sum)
negativas <- apply(pnegativas.tdm,1,sum)
num_total <- apply(dtm,1,sum)

totalvida.df  <- data.frame(positive = positivas,
                        negative = negativas,
                        neutral  = num_total - positivas - negativas)
totalvida.df

########################################################################################################################

# Análisis comparativo: Disco Muerte

#Para poder comparar las canciones entre sí, normalizamos los valores, dividiendo el total de palabras del disco. 
#Además se genera un score, definido como la diferencia entre el porcentaje de palabras positivas y negativas (así, canciones con scores positivos tienen más palabras positivas que negativas y viceversa).


scores <- as.data.frame(apply(totalmuerte.df, 2, function(x)(100 * x / num_total)))
scores$score <- scores$positive - scores$negative
scores$cancion <- substr(rownames(scores),1,nchar(rownames(scores))-4)
scores

ggplot(scores, aes(x = cancion, y = score, fill = cancion)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank())


## Generacion nube de palabras disco muerte

m <- as.matrix(TermDocumentMatrix(muerte))
m <- sort(rowSums(m), decreasing = TRUE)
colorPalette <- brewer.pal(8, "Paired") 
wordcloud(names(m), m, min.freq = 200, colors=colorPalette)


# Análisis comparativo: Disco Vida


scores <- as.data.frame(apply(totalvida.df, 2, function(x)(100 * x / num_total)))
scores$score <- scores$positive - scores$negative
scores$cancion <- substr(rownames(scores),1,nchar(rownames(scores))-4)
scores


ggplot(scores, aes(x = cancion, y = score, fill = cancion)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        axis.title.x = element_blank())


m <- as.matrix(TermDocumentMatrix(vida))
m <- sort(rowSums(m), decreasing = TRUE)
colorPalette <- brewer.pal(8, "Paired") 
wordcloud(names(m), m, min.freq = 200, colors=colorPalette)
