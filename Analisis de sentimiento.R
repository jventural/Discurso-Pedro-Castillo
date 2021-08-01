#Guía del script
#https://programminghistorian.org/es/lecciones/analisis-de-sentimientos-r

##Analisis de sentimiento
library(dplyr)
library(tidytext)
#Cargar la base de datos
txt <- readLines("Discurso.txt",ok = TRUE, warn = FALSE,  encoding="UTF-8") %>% 
  as.data.frame() %>% setNames(., c("variable"))

#Tokenizing la data:
txt_tok <- txt %>%
#word es la nueva columna, variable es la columna previa
  unnest_tokens(word, variable) %>% 
#eliminar las palabras funcionales
  anti_join(tibble(word = tm::stopwords("es")))

library(syuzhet)
texto_palabras <- get_tokens(txt_tok) %>% 
#eliminando la letra "C"
    .[-1]
head(texto_palabras)

#Cargar el diccionario de palabras en español
#información del lexicon: http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
sentimientos_df <- get_nrc_sentiment(texto_palabras, lang="spanish")  

#Resumir las medias aritmeticas 
psych::describe(sentimientos_df, skew = F, ranges = F) %>% select(mean)
library(tidyverse)
library("RColorBrewer")

#Grafico de barras
barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),#los valores en los corchetes cambiar
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Discurso presidencial",
  sub = "Análisis realizado por Dr. José Ventura-León",
  xlab="emociones", ylab = NULL)

#Nube de emociones
nube_emociones_vector <- c(
  paste(texto_palabras[sentimientos_df$negative> 0], collapse = " "),
  paste(texto_palabras[sentimientos_df$positive > 0], collapse = " "))

library(tm)
nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)
colnames(nube_tdm) <- c('negativo', 'positivo')
head(nube_tdm)

pal <- brewer.pal(8,"Dark2")

library("wordcloud")
jpeg("Nube de palabras Negativo y Positivo.jpg", 
     width=6.5, height=6.5, 
     units='in',res=1500)
set.seed(757) # puede ser cualquier número
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = pal,
                 title.size = 1, max.words = 100, scale = c(2.5, 1), rot.per = 0.4)
dev.off()

# palabras_tristeza <- texto_palabras[sentimientos_df$negative> 0]
# palabras_confianza_orden <- sort(table(unlist(palabras_confianza)), decreasing = TRUE)
# head(palabras_confianza_orden, n = 12)

#Analisis de sentimiento con aspectos especificos
#Nube de emociones
nube_emociones_vector <- c(
  paste(texto_palabras[sentimientos_df$sadness> 0], collapse = " "),
  paste(texto_palabras[sentimientos_df$joy > 0], collapse = " "),
  paste(texto_palabras[sentimientos_df$anger > 0], collapse = " "),
  paste(texto_palabras[sentimientos_df$fear > 0], collapse = " "))

nube_emociones_vector <- iconv(nube_emociones_vector, "latin1", "UTF-8")

library(tm)
nube_corpus <- Corpus(VectorSource(nube_emociones_vector))
nube_tdm <- TermDocumentMatrix(nube_corpus)
nube_tdm <- as.matrix(nube_tdm)
head(nube_tdm)
colnames(nube_tdm) <- c('Tristeza', 'Alegría', 'Enojo', 'Miedo')
head(nube_tdm)

pal <- brewer.pal(8,"Dark2")

jpeg("Nube de palabras_Especifica.jpg", 
     width=6.5, height=6.9, 
     units='in',res=1500)
set.seed(757) # puede ser cualquier número
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = pal,
                 title.size = 2, max.words = 100, scale = c(2.5, 1), rot.per = 0.4, 
                 title.colors = pal)
title(adj=0.5, sub = "Elaborado por Dr. José Ventura-León", cex.sub=0.5, line = 1.5) #Adj se mueve entre 0, 0.5 y 1
dev.off()

#evolución del sentimiento
sentimientos_valencia <- (sentimientos_df$negative *-1) + sentimientos_df$positive
simple_plot(sentimientos_valencia)

simple_plot2 = function (raw_values, title = "Syuzhet Plot", legend_pos = "top", 
          lps = 10, window = 0.1) {
  wdw <- round(length(raw_values) * window)
  rolled <- rescale(zoo::rollmean(raw_values, k = wdw, fill = 0))
  half <- round(wdw/2)
  rolled[1:half] <- NA
  end <- length(rolled) - half
  rolled[end:length(rolled)] <- NA
  trans <- get_dct_transform(raw_values, low_pass_size = lps, 
                             x_reverse_len = length(raw_values), scale_range = T)
  x <- 1:length(raw_values)
  y <- raw_values
  raw_lo <- stats::loess(y ~ x, span = 0.5)
  low_line <- rescale(stats::predict(raw_lo))
  graphics::par(mfrow = c(2, 1))
  graphics::plot(low_line, type = "l", ylim = c(-1, 1), main = title, 
                 xlab = "Tiempo narrativo completo", ylab = "Sentimiento escalado", 
                 col = "blue", lty = 2)
  graphics::lines(rolled, col = "grey", lty = 2)
  graphics::lines(trans, col = "red")
  graphics::abline(h = 0, lty = 3)
  graphics::legend(legend_pos, c("Loess Smooth", "Rolling Mean", 
                                 "Syuzhet DCT"), lty = 1, lwd = 1, col = c("blue", "grey", 
                                                                           "red"), bty = "n", cex = 0.75)
  normed_trans <- get_dct_transform(raw_values, scale_range = T, 
                                    low_pass_size = 5)
  graphics::plot(normed_trans, type = "l", ylim = c(-1, 1), 
                 main = "Forma macro simplificada", xlab = "Tiempo narrativo normalizado", 
                 ylab = "Sentimiento escalado", col = "red")
  graphics::par(mfrow = c(1, 1))
}

jpeg("Evolución del senitmiento.jpg", 
     width=6.5, height=6.9, 
     units='in',res=1500)
simple_plot2(sentimientos_valencia, title = "Evolución de sentimientos en el texto",  legend_pos = "bottomright")
dev.off()

#
library(dplyr)
oraciones_vector <- syuzhet::get_sentences(txt_tok$word)
vocabulario_personalizado <- read.csv("archivo.csv")
method <- "custom"
sentimientos_oraciones <- get_sentiment(oraciones_vector, method = method, lexicon = vocabulario_personalizado)

