# Cargar librerias
library(tidyverse)
library(tidytext)
#Cargar la base de datos
txt <- readLines("Discurso.txt",ok = TRUE, warn = FALSE,  encoding="UTF-8") %>% 
  as.data.frame() %>% setNames(., c("variable"))

#Tokenizing la data:
txt_tok <- txt %>%
#word es la nueva columna, variable es la columna previa
unnest_tokens(word, variable) 

#eliminar las palabras funcionales
tidy_txt <- txt_tok %>%
  anti_join(tibble(word = tm::stopwords("es")))

#Contar las palabras 
tidy_txt %>%
  count(word, sort = TRUE) %>% 
  head()

#Grafico de barras
library(ggplot2)
Figura_barras = tidy_txt %>%
#eliminar dos palabras de la columna word
  filter(!word == "hoy", 
         !word == "peruanos") %>% 
  count(word, sort = TRUE) %>%
#Solo quedarse con las 10 palabras más frecuentes.
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = word)) +
  theme(legend.position = "none") +
  geom_col() +
  geom_text(aes(label=n), hjust=-0.2)+ #agregar valores
  xlab(NULL) +
  coord_flip() +
  labs(x = "Palabras",
       y = "Cantidades",
       title = "Palabras más frecuentes del discurso presidencial")

ggsave(filename = "Figura_barras.jpg", plot = Figura_barras,
       height = 6, width = 10, dpi = 600)

#Nuevamente cargas las palabras con contenido
tidy_txt2 <- txt_tok %>%
  anti_join(tibble(word = tm::stopwords("es"))) %>%
  count(word, sort = TRUE) 

#nube de palabras
library(dplyr)
library(wordcloud)
# define a nice color palette
pal <- brewer.pal(8,"Dark2")
# plot the 50 most common words
jpeg("Nube de palabras.jpg", 
     width=6.5, height=6.5, 
     units='in',res=1500)
tidy_txt2 %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors=pal))
#title("Nube de palabras del discurso presidencial", line = -31, outer = TRUE, family="B", font = 2)
dev.off()

#Tabla de frecuencias
tidy_txt2 %>%
  DT::datatable()
view(tidy_txt2)
#
library(ggraph)
library(igraph)
library(tm)
#https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mine-colorado-flood-tweets-science-r/
flood_txt_paired <- txt %>%
  dplyr::select(variable) %>%
  anti_join(tibble(variable = tm::stopwords("es"))) %>% 
  mutate(word = removeWords(variable, stopwords("spanish")),
         variable = removeWords(variable, stopwords("spanish"))) %>% 
  unnest_tokens(paired_words, word, token = "ngrams", n = 2) 

flood_txt_paired %>%
  count(paired_words, sort = TRUE)

flood_txt_separated <- flood_txt_paired %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

flood_word_counts <- flood_txt_separated %>%
  count(word1, word2, sort = TRUE)
flood_word_counts

flood_word_counts %>%
  filter(n >= 5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  # geom_edge_link(aes(edge_alpha = n, edge_width = n)) +
  geom_node_point(color = "darkslategray4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Tweets during the 2013 Colorado Flood Event",
       subtitle = "September 2013 - Text mining twitter data ",
       x = "", y = "") +
  theme_void()

#Analisis de sentimiento
# tidy_txt_sentiment <- tidy_txt2 %>%
#   inner_join(get_sentiments("bing"))%>% 
#   count(word, sentiment) %>%
#   spread(sentiment, n, fill = 0) %>%
#   mutate(sentiment = positive - negative)
# 
# tidy_txt_sentiment %>%
#   ggplot(aes(reorder(word, sentiment), sentiment, fill = sentiment)) +
#   geom_col(show.legend = FALSE) +
#   facet_wrap(~sentiment, ncol = 3, scales = "free") +
#   labs(x = NULL,
#        y = "Sentiment",
#        title = "Discurso de castillo by sentiment",
#        caption = "") +
#   theme_minimal() +
#   coord_flip()
# 
#   ggsave("sentiment.png", width = 10, height = 5.5)
#   
# #
# nrc_sentimiento <- get_sentiments("nrc") %>% 
#     filter(sentiment == "negative" | sentiment == "positive")
#   
# tidy_txt2 %>%
#   select(word) %>% 
#     inner_join(nrc_sentimiento) %>%
#     count(word, sort = TRUE)

##Analisis de sentimiento
#https://programminghistorian.org/es/lecciones/analisis-de-sentimientos-r
library(syuzhet)
texto_palabras <- get_tokens(txt_tok)
head(texto_palabras)

#
sentimientos_df <- get_nrc_sentiment(texto_palabras, lang="spanish")  
summary(sentimientos_df)
psych::describe(sentimientos_df, skew = F, ranges = F) %>% select(mean)

barplot(
  colSums(prop.table(sentimientos_df[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Discurso presidencial",
  sub = "Análisis realizado por Dr. José Ventura-León",
  xlab="emociones", ylab = NULL)

palabras_tristeza <- texto_palabras[sentimientos_df$sadness> 0]

palabras_confianza_orden <- sort(table(unlist(palabras_confianza)), decreasing = TRUE)
head(palabras_confianza_orden, n = 12)

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
colnames(nube_tdm) <- c('tristeza', 'alegría', 'enfado', 'confianza')
head(nube_tdm)

pal <- brewer.pal(8,"Dark2")

jpeg("Nube de palabras2.jpg", 
     width=6.5, height=6.5, 
     units='in',res=1500)
set.seed(757) # puede ser cualquier número
comparison.cloud(nube_tdm, random.order = FALSE,
                 colors = pal,
                 title.size = 1, max.words = 50, scale = c(2.5, 1), rot.per = 0.4)
dev.off()

#
#volviendo a correr sin las palabras
corpus <- VCorpus(VectorSource(tidy_txt))
d3 <- TermDocumentMatrix(corpus)
findAssocs(d3, terms = "gobierno", corlimit = 0.2)

# install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
library(tm)
library(Rgraphviz)
#https://journal.code4lib.org/articles/11626
plot(d3, terms = names(findAssocs(d3,term="gobierno",0.8)[["gobierno"]]), corThreshold = 0.80, cex=7)
plot(d3, terms = names(findAssocs(d3,term="gobierno",0.8)[["gobierno"]]), corThreshold = 0.80, attrs=list(node=list(label="foo", fillcolor="lightgreen", fontsize="16", shape="ellipse"), edge=list(color="cyan"), graph=list(rankdir="LR")))


library(widyr)
library(dplyr)
tay_cors <- tidy_txt %>%
  pairwise_cor(word, word, sort = TRUE)

library(ggraph)
library(igraph)
set.seed(123)
tay_cors %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(show.legend = FALSE, aes(edge_alpha = correlation)) +
  geom_node_point(color = "pink", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5, color = "grey40") +
  theme_void()

  ggsave("taymap.png", width = 15, height = 11)

  
#https://www.tidytextmining.com/sentiment.html
#https://rpubs.com/Joaquin_AR/334526
#https://www.tidytextmining.com/tidytext.html
#https://www.tidytextmining.com/sentiment.html