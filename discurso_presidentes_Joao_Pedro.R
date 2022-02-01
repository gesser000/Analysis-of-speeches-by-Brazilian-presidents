###Carregando Pacotes

library(readr) #ler documentos de texto
library(tidytext) #conjunto de pacotes para manipular dados
library(dplyr) #manipulação de dados e grámatica
library(stringr) #manipulação de strings
library(ggplot2) #fazer gráficos mais complexos
library(gridExtra) #extensão do ggplot
library(tidyr) #Deixar os dados organizados
library(scales) #Visualização de gráficos
library(lexiconPT) #lexico de linguagem em Portugues
library(textdata) #Da acesso a diversos datasets
library(readxl) #ler arquivos de excel
library(wordcloud) #fazer wordclouds
library(reshape2) #Estruturar dados
library(igraph) #Fazer gráficos de redes
library(ggraph) #Extensão do ggplot

###Carregando os disursos de posse dos presidentes
##Os discursos são obtiveis atráves do site http://www.biblioteca.presidencia.gov.br/

dilma <- read_delim("Documentos/Projetos de R/Presidentes/Discursos/dilma", 
                    ";", escape_double = FALSE, col_names = FALSE, 
                    trim_ws = TRUE)

colnames(dilma) <- "texto" #Mudando o nome da coluna para facilitar manipulações futuras

lula <- read_delim("Documentos/Projetos de R/Presidentes/Discursos/lula", 
                   ";", escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)

colnames(lula) <- "texto"

bolsonaro <- read_delim("Documentos/Projetos de R/Presidentes/Discursos/bolsonaro", 
                        ";", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)

colnames(bolsonaro) <- "texto"

##Carregando a lista de "palavras vazias"

stopwords <- read_csv("Documentos/Projetos de R/Presidentes/Discursos/stopwords",
                      col_names = FALSE)

colnames(stopwords) <- "palavras"

###Criando tokens, limpando os textos e fazendo contagem das palavras

#Dilma
dilma$index <- (1:95) #Adcionando uma coluna para marcar a linha do texto

tidy_dilma <- dilma %>%
  unnest_tokens(palavras, texto) %>% #tranformando as linhas que são frases em linhas somente com palavras
  anti_join(stopwords) %>% #removendo as linhas com palavras vazias
  count(palavras, sort = TRUE) #contando e ordenando a quantidade de vezes que cada palavra aparece

#Lula
lula$index <- (1:79)

tidy_lula <- lula %>%
  unnest_tokens(palavras, texto) %>%
  anti_join(stopwords) %>%
  count(palavras, sort = TRUE)

#Bolsonaro
bolsonaro$index <- (1:30)

tidy_bolsonaro <- bolsonaro %>%
  unnest_tokens(palavras, texto) %>%
  anti_join(stopwords) %>%
  count(palavras, sort = TRUE)

##Criando um gráfico de Barras com as palvras mais usadas por cada um

barras_dilma <- tidy_dilma %>%
  top_n(15) %>% #selecionando as 15 palavras mais usadas
  mutate(palavras = reorder(palavras, n)) %>% #ordenando as palavras pela quantidade de vezes que foi usada
  ggplot(aes(palavras, n)) + #colocando as variaveis nos eixos do gráfico
  geom_col() + #selecionando um grafico de colunas
  ggtitle(label = "Top 15 palavras Dilma") + #adicionando titulo
  xlab(NULL) + #removendo a legenda do eixo x
  ylab(NULL) + #removendo a legenda do eixo y
  theme_minimal() + #mudando o tema para um com melhor estética
  coord_flip() #invertendo os eixos do grafico

barras_lula <- tidy_lula %>%
  top_n(15) %>%
  mutate(palavras = reorder(palavras, n)) %>%
  ggplot(aes(palavras, n)) +
  geom_col() +
  ggtitle(label = "Top 15 palavras Lula") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  coord_flip()

barras_bolsonaro <- tidy_bolsonaro %>%
  top_n(15) %>%
  mutate(palavras = reorder(palavras, n)) %>%
  ggplot(aes(palavras, n)) +
  geom_col() +
  ggtitle(label = "Top 15 palavras Lula") +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  coord_flip()

#Juntando os 3 gráficos para comparação

grid.arrange(barras_dilma, barras_lula, barras_bolsonaro, ncol = 3)

###Comparando a frequencia que as palavras são usadas

frequencia <- bind_rows(mutate(tidy_dilma, autor = "Dilma"), #Adiconando ao discurso da dilma linhas para identificar o discurso dela
                        mutate(tidy_lula, autor = "Lula"), #Adiconando ao discurso do lula linhas para identificar o discurso dele
                        mutate(tidy_bolsonaro, autor = "Bolsonaro")) %>% #Adiconando ao discurso do bolsonaro linhas para identificar o discurso dele
  mutate(palavras = str_extract(palavras, "[a-z']+")) %>% #remover alguma palavra que tenha problema de encoding
  count(autor, palavras) %>% #contando as palavras por autor
  group_by(autor) %>% #juntando as palvras por autor
  mutate(proporção = n / sum(n)) %>% #adicionando uma colula da proporção que a palvra é usada
  select(-n) %>% #removendo a coluna n
  spread(autor, proporção) %>% #ajustando as colunas
  gather(autor, proporção, `Bolsonaro`:`Dilma`) #criando a proporção em relação ao lula

ggplot(frequencia, aes(x = proporção, y = `Lula`, color = abs(`Lula` - proporção))) + #adiconado os eixos do gráfico
  geom_abline(color = "gray40", lty = 2) + #adicionando a linha tracejada
  geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3) + #adicionando as bolinhas
  geom_text(aes(label = palavras), check_overlap = TRUE, vjust = 1.0) + #adicionando as palavras as bolinhas
  scale_x_log10(labels = percent_format()) + #deixando o eixo x em escala logaritima
  scale_y_log10(labels = percent_format()) + #deixando o eixo y em escala logaritima
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") + #adicionando cores as bolinhas
  facet_wrap(~autor, ncol = 2) + #deixando os dois gráficos lado alado
  theme(legend.position="none") + #removendo leganda do tema
  labs(y = "Lula", x = NULL) #adicionado a legenda ao eixo y

#Neste gráfico as palavras que estão acima da linha são usadas com mais frequencia por lula em seu discurso de posse,
#já o que está baixo é mais usada por dilma ou bolsonaro, e o  que está proximo a linha é usada de forma parecida por ambos

##Correlação de Pearson
#E também podemos calucular a correlção do discursos

cor.test(data = frequencia[frequencia$autor == "Bolsonaro",],
         ~ proporção + `Lula`) #Correlação do discurso de lula com o de bolsonaro = 0,6119

cor.test(data = frequencia[frequencia$autor == "Dilma",],
         ~ proporção + `Lula`) #Correlação do discurso de lula com o da dilma = 0,5485

###Ánalise dos sentimentos dentro dos discurso

bing <- read_excel("Documentos/Projetos de R/Presidentes/bing.xlsx") #dataset de sentimentos traduzido
colnames(bing) <- c("word","sentiments","palavras","sentimentos") #Mudando o nome das colunas para facilitar na manipulação

##Dilma
sentimentos_dilma <- tidy_dilma %>%
  inner_join(bing) %>% #adicionando os sentimentos ao dataframe
  count(palavras, sentimentos, sort = T) %>% #contando as palavras
  ungroup()

sentimentos_dilma

sentimentos_dilma %>%
  group_by(sentimentos) %>% #agrupando palavras pelo sentimento
  top_n(10) %>% #selecionando o top 10
  ungroup() %>%
  mutate(palavras = reorder(palavras, n)) %>% #organizando em ordem crescente
  ggplot(aes(palavras, n, fill = sentimentos)) + # adicionado os eixos ao gráfico
  geom_col(show.legend = FALSE) + #selecionado o gráfico de colunas
  facet_wrap(~sentimentos, scales = "free_y") + #mudando as escalas do gráfico
  labs(y = "Contribuição para o sentimento", #adiconando legenda aos eixos
       x = NULL) + 
  coord_flip() #invertendo eixos do gráfico

##lula
sentimentos_lula <- tidy_lula %>%
  inner_join(bing) %>% 
  count(palavras, sentimentos, sort = T) %>% 
  ungroup()

sentimentos_lula

sentimentos_lula %>%
  group_by(sentimentos) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(palavras = reorder(palavras, n)) %>%
  ggplot(aes(palavras, n, fill = sentimentos)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentimentos, scales = "free_y") +
  labs(y = "Contribuição para o sentimento",
       x = NULL) +
  coord_flip()

##bolsonaro
sentimentos_bolsonaro <- tidy_bolsonaro %>%
  inner_join(bing) %>% 
  count(palavras, sentimentos, sort = T) %>% 
  ungroup()

sentimentos_bolsonaro

sentimentos_bolsonaro %>%
  group_by(sentimentos) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(palavras = reorder(palavras, n)) %>%
  ggplot(aes(palavras, n, fill = sentimentos)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentimentos, scales = "free_y") +
  labs(y = "Contribuição para o sentimento",
       x = NULL) +
  coord_flip()

###Wordcloud

##Dilma
tidy_dilma %>%
  anti_join(stopwords) %>% #removendo palavras-vazias
  count(palavras) %>% #contando palavras
  with(wordcloud(palavras, n, max.words = 40)) #criando uma wordcloud

#worcloud dividido pelos setimentos
tidy_dilma %>%
  inner_join(bing) %>% #adicionando os sentimentos
  count(palavras, sentimentos, sort = TRUE) %>% #contando palavras
  acast(palavras ~ sentimentos , value.var = "n", fill = 0) %>% #Use acast ou dcast, dependendo se você deseja saída de vetor / matriz / matriz ou saída de quadro de dados. Os quadros de dados podem ter no máximo duas dimensões.
  comparison.cloud(colors = c("#e03b3b", "#3be0e0"), #criando uma nuvem de comparaçao e definindo cores
                   max.words = 40) #delimitando o maximo de 40 palavras

##Lula

tidy_lula %>%
  anti_join(stopwords) %>%
  count(palavras) %>%
  with(wordcloud(palavras, n, max.words = 40))

#wordcloud de sentimentos
tidy_lula %>%
  inner_join(bing) %>%
  count(palavras, sentimentos, sort =TRUE) %>%
  acast(palavras ~ sentimentos , value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#e03b3b", "#3be0e0"),
                   max.words = 40)

##bolsonaro
tidy_bolsonaro%>%
  anti_join(stopwords) %>%
  count(palavras) %>%
  with(wordcloud(palavras, n, max.words = 40))

#wordcloud de sentimentos
tidy_bolsonaro%>%
  inner_join(bing) %>%
  count(palavras, sentimentos, sort =TRUE) %>%
  acast(palavras ~ sentimentos , value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#e03b3b", "#3be0e0"), random.order = FALSE,
                   max.words = 40)

#Bi-grams

dilma_bi <- dilma %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2) #agora veremos qual são as duplas de palavras mais usadas e não palavras unicas

dilma_separado <- dilma_bi %>%
  separate(bigram, c("palavra1","palavra2"), sep = " ") #separando as duplas de palavras em duas colunas

dilma_filtrado <- dilma_separado %>%
  filter(!palavra1 %in% stopwords$palavras) %>% #removendo stopwords da primeira coluna
  filter(!palavra2 %in% stopwords$palavras) #removendo stopwords da segunda coluna

dilma_bi_conta <- dilma_filtrado %>%
  count(palavra1, palavra2, sort = TRUE) #contando as duplas de palavras

dilma_unida <- dilma_bi_conta %>%
  unite(bigram, palavra1, palavra2, sep = " ") #unindo novamente as duplas de palavras

dilma_bi_graf <- dilma_bi_conta %>%
  filter(n > 1) %>% #filtrando o nº de vezes que a dupla aparece
  graph_from_data_frame() #criando o gráfico

set.seed(2020) #usando a seed garantimos que o gráfico sai sempre igual

a <- grid::arrow(type = "closed", length = unit(.15, "inches")) #definindo como vai ser a flecha usada no gráfico

ggraph(dilma_bi_graf, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#lula
lula_bi <- lula %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2)

lula_separado <- lula_bi %>%
  separate(bigram, c("palavra1","palavra2"), sep = " ")

lula_filtrado <- lula_separado %>%
  filter(!palavra1 %in% stopwords$palavras) %>%
  filter(!palavra2 %in% stopwords$palavras)

lula_bi_conta <- lula_filtrado %>%
  count(palavra1, palavra2, sort = TRUE)

lula_unida <- lula_bi_conta %>%
  unite(bigram, palavra1, palavra2, sep = " ")

lula_bi_graf <- lula_bi_conta %>%
  filter(n > 1) %>%
  graph_from_data_frame()

ggraph(lula_bi_graf, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#bolsonaro

bolsonaro_bi <- bolsonaro %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2)

bolsonaro_separado <- bolsonaro_bi %>%
  separate(bigram, c("palavra1","palavra2"), sep = " ")

bolsonaro_filtrado <- bolsonaro_separado %>%
  filter(!palavra1 %in% stopwords$palavras) %>%
  filter(!palavra2 %in% stopwords$palavras)

bolsonaro_bi_conta <- bolsonaro_filtrado %>%
  count(palavra1, palavra2, sort = TRUE)

bolsonaro_unida <- bolsonaro_bi_conta %>%
  unite(bigram, palavra1, palavra2, sep = " ")

bolsonaro_bi_graf <- bolsonaro_bi_conta %>%
  filter(n > 1) %>%
  graph_from_data_frame()

ggraph(bolsonaro_bi_graf, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
