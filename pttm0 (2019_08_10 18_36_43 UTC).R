library(magrittr)
library(tidyverse)
library(plyr)
library(tm)
library(lattice) 
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(lexiconPT)
library(abjutils) 

############ objetos ############
sws <- c("tem", "o", "a", "mais", "mas", "melhor","muito", "onde", "com", "uma", "que", "nao", "serie", "temporada", "episodio")
lexsent <- lexiconPT::oplexicon_v3.0
names(lexsent) <- c("term", "class", "pol", "ann") 
xtabs(~class + pol, data = lexsent)


### GAME OF THRONES ###

# Removendo acentos, deixando em caixa baixa, removendo pontuações, etc..

s1 <- nam[1]
txt1 <- res_formatado[[1]]
p1p <-     abjutils::rm_accent(txt1) %>% 
  tolower() %>% 
  removePunctuation() %>% 
  stripWhitespace() %>% 
  removeNumbers() %>% 
  removeWords(words = sws) %>%
  stemDocument(language = "portuguese")

# Conferindo
head(txt1)
head(p1p)

#criando corpus 
corp1 <- VCorpus(VectorSource(x = p1p), 
                 readerControl = list(language = "pt", 
                                      load = TRUE)) 

sapply(corp1, content) 

##

meta(corp1, tag = "ts", type = "indexed") <- p1p
dtm1 <- DocumentTermMatrix(corp1) # Documentos nas linhas. 
tdm1 <- TermDocumentMatrix(corp1) # Termos nas linhas. 


# Número de documentos e termos (vocabulário). 
dim(dtm1) 
dim(tdm1) 


# Algumas linhas e colunas da matriz. 
inspect(dtm1) 

# Esparsidade (proporção de cédulas com 0 na matriz). 

us1 <- VCorpus(VectorSource(p1p)) 
ds1 <- DocumentTermMatrix(us1) 
inspect(ds1) 

non_zero1 <- sum(ds1 > 0) 
cells1 <- prod(dim(ds1)) 
100 * (1 - (non_zero1/cells1))

# Converte para matriz ordinária.
msnt1 <- as.matrix(tdm1) 

# Reordenar matriz por frequencia dos termos. 
msnt1 <- msnt1[order(apply(msnt1, MARGIN = 1, sum), decreasing = TRUE), ] 
msnt1 <- t(msnt1) 

# Termos com frequencia superior a um valor de corte. 
mft1 <- findFreqTerms(tdm1, lowfreq = 10) 
mft1 
# Frequência dos termos. 
trm1 <- as.matrix(tdm1) 
colSums(trm1 > 0) # Frequência.
# Termos associados por um valor acima de um limite. 

asssn1 <- findAssocs(tdm1, terms = mft1, corlimit = 0.5) 

# Correlação de Pearson. 
cor(trm1) 
us1 <- sort(slam::col_sums(dtm1 > 0), decreasing = TRUE) 

# Esparsidade da matrix. 
1 - sum(us1)/prod(dim(dtm1))
# Espasidade de cada termo. 
tsp1 <- 1 - us1/nDocs(dtm1) 
sum(tsp1 < 0.95) # Quantos tem menos que o corte. 
# Esparsidade da DTM com acúmulo dos termos. 
sps1 <- 1 - cumsum(us1)/(nDocs(dtm1) * seq_along(us1))
# Remove os termos mais esparsos. 
rst1 <- removeSparseTerms(dtm1, sparse = 0.95) 
# nTerms(rst) 
Terms(rst1)

#nuvem de palavras

ds1 <- data.frame(word = names(us1), 
                  freq = us1) 
knitr::kable(ds1)

wc1 <- wordcloud2(ds1)


########## polarização - ANÁLISE DE SENTIMENTOS ###################

#**Cálculo da polaridade de cada avaliação
# A intersecção entre os termos do corpus e do dicionário. 
inter1 <- intersect(x = Terms(dtm1), 
                    y = lexsent$term[lexsent$pol != 0]) 


# Obter o vetor de polaridades associada aos termos na matriz. 
lex1 <- merge(x = data.frame(term = inter1, 
                             stringsAsFactors = FALSE), 
              y = lexsent, 
              sort = FALSE) 
# str(lex1) 

# Remover os termos na DTM que não tem polaridade atribuida. 
ms1 <- as.matrix(dtm1) 
ms1 <- ms1[, lex1$terms] 

# Verifica dimensões e disposição. 
all.equal(colnames(ms1), lex1$terms) 

# Média aritmética das polaridades por termo em cada documento. 
pol1 <- (ms1 %*% (lex1$pol))/rowSums(ms1) 

fras1 <- sapply(lapply(p1p, strwrap, width = 60), "[[", 1) 
knitr::kable(data.frame(Polaridade = pol1, Fragmento = fras1))


###### modelo de linguagem n gramas ###################
# Tokenizador básico (unigram, quebra nos espaços)


#dtm com n-gramas
# Cria uma função para fazer bigramas. 
BigramTokenizer1 <- NGramTokenizer(p1p, RWeka::Weka_control(min = 2, max = 2)) 


tdm1 <- DocumentTermMatrix(us1, control = list(tokenize = BigramTokenizer)) 
                           
#n-gramas mais frequentes
                           
frqsnt1 <- sort(slam::colapply_simple_triplet_matrix(dtm1, FUN = sum)) 
bc1 <- barchart(tail(frqsnt1, n = 30), xlim = c(0, NA)) 
                           
### BREAKING BAD ###
# Removendo acentos, deixando em caixa baixa, removendo pontuações, etc..
                           
s2 <- nam[2]
txt2 <- res_formatado[[2]]
                           
p2p <-     abjutils::rm_accent(txt2) %>% 
                    tolower() %>% 
                    removePunctuation() %>% 
                    stripWhitespace() %>% 
                    removeNumbers() %>% 
                    removeWords(words = sws) %>%
                    stemDocument(language = "portuguese")
                           
# Conferindo
head(txt2)
head(p2p)
                           
                           
corp2 <- VCorpus(VectorSource(x = p2p), 
                                            readerControl = list(language = "pt", 
                                                                 load = TRUE)) 
                           
corp2 <- tm_map(corp2,~{
                   removeWords(words = stopwords("portuguese")),
                   stemDocument(language = "portuguese")})
sapply(corp2, content) 
                           
##
                           
meta(corp2, tag = "ts", type = "indexed") <- p2p 
head(meta(corp2))
dtm2 <- DocumentTermMatrix(corp2) # Documentos nas linhas. 
tdm2 <- TermDocumentMatrix(corp2) # Termos nas linhas. 
                           
                           
# Número de documentos e termos (vocabulário). 
dim(dtm2) 
dim(tdm2) 
                           
# Algumas linhas e colunas da matriz. 
inspect(dtm2) 
                           
# Esparsidade (proporção de cédulas com 0 na matriz). 
                           
us2 <- VCorpus(VectorSource(p2p)) 
ds2 <- DocumentTermMatrix(us2) 
inspect(ds2) 
                           
non_zero2 <- sum(ds2 > 0) 
cells2 <- prod(dim(ds2)) 
100 * (1 - (non_zero2/cells2))
                           
# Converte para matriz ordinária.
msnt2 <- as.matrix(dtm2) 
                           
# Reordenar matriz por frequencia dos termos. 
msnt2 <- msnt2[order(apply(msnt2, MARGIN = 1, sum), decreasing = TRUE), ] 
msnt2 <- t(msnt2) 
                           
# Termos com frequencia superior a um valor de corte. 
mft2 <- findFreqTerms(tdm2, lowfreq = 10) 
mft2 
# Frequência dos termos. 
trm2 <- as.matrix(dtm2) 
colSums(trm2 > 0) # Frequência.
# Termos associados por um valor acima de um limite. 
                           
asssn2 <- findAssocs(dtm2, terms = mft2, corlimit = 0.5) 
                           
# Correlação de Pearson. 
cor(trm2) 
us2 <- sort(slam::col_sums(dtm2 > 0), decreasing = TRUE) 
                           
# Esparsidade da matrix. 
1 - sum(us2)/prod(dim(dtm2))
# Espasidade de cada termo. 
tsp2 <- 1 - us2/nDocs(dtm2) 
sum(tsp2 < 0.95) # Quantos tem menos que o corte. 
# Esparsidade da DTM com acúmulo dos termos. 
sps2 <- 1 - cumsum(us2)/(nDocs(dtm2) * seq_along(us1))
# Remove os termos mais esparsos. 
rst2 <- removeSparseTerms(dtm2, sparse = 0.95) 
# nTerms(rst) 
Terms(rst2)

#nuvem de palavras
                           
ds2 <- data.frame(word = names(us2), 
                                             freq = us2) 
knitr::kable(ds2)
                           
wc2 <- wordcloud2(ds2)
                           
                           
########## polarização - ANÁLISE DE SENTIMENTOS ###################
#**Cálculo da polaridade de cada avaliação
# A intersecção entre os termos do corpus e do dicionário. 
inter2 <- intersect(x = Terms(dtm2), 
                  y = sentsn2$term[lexsent$pol != 0]) 
# length(inter2) 
                           
# Obter o vetor de polaridades associada aos termos na matriz. 
lex2 <- merge(x = data.frame(term = inter2, 
                    stringsAsFactors = FALSE), 
                     y = lexsent, 
                     sort = FALSE) 
                           
# Remover os termos na DTM que não tem polaridade atribuida. 
ms2 <- as.matrix(dtm2) 
ms2 <- ms2[, lex2$terms] 
                           
# Verifica dimensões e disposição. 
all.equal(colnames(ms2), lex2$terms) 
                           
# Média aritmética das polaridades por termo em cada documento. 
pol2 <- (ms2 %*% (lex2$pol))/rowSums(ms2) 
                           
fras2 <- sapply(lapply(p2p, strwrap, width = 60), "[[", 1) 
knitr::kable(data.frame(Polaridade = pol2, Fragmento = fras2))
                           
                           
###### modelo de linguagem n gramas ###################
                           
#dtm com n-gramas
BigramTokenizer2 <- NGramTokenizer(p2p, RWeka::Weka_control(min = 2, max = 2))  
                           
tdm2 <- DocumentTermMatrix(us2, control = list(tokenize = BigramTokenizer2))
                                                      
# nTerms(dtm) 
# nDocs(dtm) 
# head(Terms(dtm2))
                                                      
findFreqTerms(dtm2, lowfreq = 12) 
                                                      
#n-gramas mais frequentes
                                                      
                                                      
frqsnt2 <- sort(slam::colapply_simple_triplet_matrix(dtm2, FUN = sum)) 
bc2 <- barchart(tail(frqsnt2, n = 30), xlim = c(0, NA)) 
                                                      
                                                      
                                                      
### SHERLOCK ###
# Removendo acentos, deixando em caixa baixa, removendo pontuações, etc..
                                                      
s3 <- nam[3]
txt3 <- res_formatado[[3]]
                                                      
p3p <-     abjutils::rm_accent(txt3) %>% 
                    tolower() %>% 
                    removePunctuation() %>% 
                    stripWhitespace() %>% 
                    removeNumbers() %>% 
                    removeWords(words = sws) %>%
                    stemDocument(language = "portuguese")
                                                      
# Conferindo
head(txt3)
head(p3p)
                                                      
                                                      
corp3 <- VCorpus(VectorSource(x = p3p), 
                  readerControl = list(language = "pt", 
                  load = TRUE)) 
corp3 <- tm_map(corp3,~{
                    removeWords(words = stopwords("portuguese")),
                    stemDocument(language = "portuguese")})
                    sapply(corp3, content) 
                                                      
##
                                                      
meta(corp3, tag = "ts", type = "indexed") <- p3p 
dtm3 <- DocumentTermMatrix(corp3) # Documentos nas linhas. 
tdm3 <- TermDocumentMatrix(corp3) # Termos nas linhas. 
                                                      
                                                      
# Número de documentos e termos (vocabulário). 
dim(dtm3) 
dim(tdm3) 

                                                      
# Algumas linhas e colunas da matriz. 
inspect(dtm3) 
                                                      
# Esparsidade (proporção de cédulas com 0 na matriz). 
                                                      
us3 <- VCorpus(VectorSource(p3p)) 
ds3 <- DocumentTermMatrix(us3) 
inspect(ds3) 
                                                      
non_zero3 <- sum(ds3 > 0) 
cells3 <- prod(dim(ds3)) 
100 * (1 - (non_zero3/cells3))
                                                      
# Converte para matriz ordinária.
msnt3 <- as.matrix(dtm3) 
                                                      

# Reordenar matriz por frequencia dos termos. 
msnt3 <- msnt3[order(apply(msnt3, MARGIN = 1, sum), decreasing = TRUE), ] 
                                                      
                                                      
# Termos com frequencia superior a um valor de corte. 
mft3 <- findFreqTerms(tdm3, lowfreq = 10) 
mft3 
# Frequência dos termos. 
trm3 <- as.matrix(dtm3) 
colSums(trm3 > 0) # Frequência.
# Termos associados por um valor acima de um limite. 
asssn3 <- findAssocs(dtm3, terms = mft3, corlimit = 0.5) 
                                                      
# Correlação de Pearson. 
cor(trm3) 
us3 <- sort(slam::col_sums(dtm3 > 0), decreasing = TRUE) 

# Esparsidade da matrix. 
1 - sum(us3)/prod(dim(dtm3))
# Espasidade de cada termo. 
tsp3 <- 1 - us3/nDocs(dtm3) 
sum(tsp3 < 0.95) # Quantos tem menos que o corte. 
# Esparsidade da DTM com acúmulo dos termos. 
sps3 <- 1 - cumsum(us3)/(nDocs(dtm3) * seq_along(us3))
# Remove os termos mais esparsos. 
rst3 <- removeSparseTerms(dtm3, sparse = 0.95) 
# nTerms(rst) 
Terms(rst3)
                                                      
#nuvem de palavras
                                                      
ds3 <- data.frame(word = names(us3), 
                      freq = us3) 
knitr::kable(ds3)
                                                      
wc3 <- wordcloud2(ds3)
                                                      
                                                      
########## polarização - ANÁLISE DE SENTIMENTOS ###################
                                                      
#**Cálculo da polaridade de cada avaliação
# A intersecção entre os termos do corpus e do dicionário. 
inter3 <- intersect(x = Terms(dtm3), 
                    y = lexsent$term[lexsent$pol != 0]) 
                                                      
                                                      
# Obter o vetor de polaridades associada aos termos na matriz. 
lex3 <- merge(x = data.frame(term = inter3, 
                                 stringsAsFactors = FALSE), 
                                 y = lexsent, 
                                 sort = FALSE) 
# str(lex3) 
                                                      
# Remover os termos na DTM que não tem polaridade atribuida. 
ms3 <- as.matrix(dtm3) 
ms3 <- ms3[, lex3$terms] 
                                                      
# Verifica dimensões e disposição. 
all.equal(colnames(ms3), lex3$terms) 
                                                      
# Média aritmética das polaridades por termo em cada documento. 
pol3 <- (ms3 %*% (lex3$pol))/rowSums(ms3) 
                                                      
fras3 <- sapply(lapply(p3p, strwrap, width = 60), "[[", 1) 
knitr::kable(data.frame(Polaridade = pol3, Fragmento = fras3))
                                                      
                                                      
###### modelo de linguagem n gramas ###################
                                                      
#dtm com n-gramas
BigramTokenizer3 <- NGramTokenizer(p3p, RWeka::Weka_control(min = 2, max = 2)) 
                                                      
tdm3 <- DocumentTermMatrix(us3, control = list(tokenize = BigramTokenizer3))
                                                                                 
# nTerms(dtm) 
# nDocs(dtm) 
# head(Terms(dtm2))
                                                                                 
                                                                                 
#n-gramas mais frequentes
                                                                                 
                                                                                 
frqsnt3 <- sort(slam::colapply_simple_triplet_matrix(dtm3, FUN = sum)) 
                                                                                 
bc3 <- barchart(tail(frqsnt3, n = 30), xlim = c(0, NA)) 
                                                                                 
                                                                                 
### SUITS ###
# Removendo acentos, deixando em caixa baixa, removendo pontuações, etc..
                                                                                 
s4 <- nam[4]
txt4 <- res_formatado[[4]]
                                                                                 
p4p <-     abjutils::rm_accent(txt4) %>% 
                    tolower() %>% 
                    removePunctuation() %>% 
                    stripWhitespace() %>% 
                    removeNumbers() %>% 
                    removeWords(words = sws) %>%
                    stemDocument(language = "portuguese")
                                                                                 
# Conferindo
head(txt4)
head(p4p)
                                                                                 
                                                                                 
corp4 <- VCorpus(VectorSource(x = p4p), 
                                                                                                  readerControl = list(language = "pt", 
                                                                                                                       load = TRUE)) 
corp4 <- tm_map(corp4,~{
                removeWords(words = stopwords("portuguese")),
                stemDocument(language = "portuguese")})
sapply(corp4, content) 
                                                                                 
##
                                                                                 
meta(corp4, tag = "ts", type = "indexed") <- p4p 
dtm4 <- DocumentTermMatrix(corp4) # Documentos nas linhas. 
tdm4 <- TermDocumentMatrix(corp4) # Termos nas linhas. 
                                                                                 
                                                                                 
# Número de documentos e termos (vocabulário). 
dim(dtm4) 
dim(tdm4) 
                                                                                 
                                                                                 
# Algumas linhas e colunas da matriz. 
inspect(dtm4) 
                                                                                 
# Esparsidade (proporção de cédulas com 0 na matriz). 
                                                                                 
us4 <- VCorpus(VectorSource(p4p)) 
ds4 <- DocumentTermMatrix(us4) 
inspect(ds4) 
                                                                                 
non_zero4 <- sum(ds4 > 0) 
cells4 <- prod(dim(ds4)) 
100 * (1 - (non_zero4/cells4))
                                                                                 
# Converte para matriz ordinária.
msnt4 <- as.matrix(dtm4) 
                                                                                 
# Reordenar matriz por frequencia dos termos. 
msnt4 <- msnt4[order(apply(msnt4, MARGIN = 1, sum), decreasing = TRUE), ] 
msnt4 <- t(msnt4) 
                                                                                 
# Termos com frequencia superior a um valor de corte. 
mft4 <- findFreqTerms(tdm4, lowfreq = 10) 
mft4 
# Frequência dos termos. 
trm4 <- as.matrix(dtm4) 
colSums(trm4 > 0) # Frequência.
# Termos associados por um valor acima de um limite. 

asssn4 <- findAssocs(dtm4, terms = mft4, corlimit = 0.5) 
                                                                                 
# Correlação de Pearson. 
cor(trm4) 
us4 <- sort(slam::col_sums(dtm4 > 0), decreasing = TRUE) 
                                                                                 
# Esparsidade da matrix. 
1 - sum(us4)/prod(dim(dtm4))
# Espasidade de cada termo. 
tsp4 <- 1 - us4/nDocs(dtm4) 
sum(tsp4 < 0.95) # Quantos tem menos que o corte. 
# Esparsidade da DTM com acúmulo dos termos. 
sps4 <- 1 - cumsum(us4)/(nDocs(dtm4) * seq_along(us4))
# Remove os termos mais esparsos. 
rst4 <- removeSparseTerms(dtm4, sparse = 0.95) 
# nTerms(rst) 
Terms(rst4)
                                                                                 
#nuvem de palavras
                                                                                 
ds4 <- data.frame(word = names(us4), freq = us4)
knitr::kable(ds4)
                                                                                 
wc4 <- wordcloud2(ds4)
                                                                                 
                                                                                 
########## polarização - ANÁLISE DE SENTIMENTOS ###################
#**Cálculo da polaridade de cada avaliação
# A intersecção entre os termos do corpus e do dicionário. 
inter4 <- intersect(x = Terms(dtm4), 
                    y = lexsent$term[lexsent$pol != 0]) 
# length(inter4) 
                                                                                 
# Obter o vetor de polaridades associada aos termos na matriz. 
lex4 <- merge(x = data.frame(term = inter4, 
                  stringsAsFactors = FALSE), 
                  y = lexsent, 
                  sort = FALSE) 
# str(lex4) 
                                                                                 
# Remover os termos na DTM que não tem polaridade atribuida. 
ms4 <- as.matrix(dtm4) 
ms4 <- ms4[, lex4$terms] 
                                                                                 
# Verifica dimensões e disposição. 
all.equal(colnames(ms4), lex4$terms) 
                                                                                 
# Média aritmética das polaridades por termo em cada documento. 
pol4 <- (ms4 %*% (lex4$pol))/rowSums(ms4) 
                                                                                 
fras4 <- sapply(lapply(rev, strwrap, width = 60), "[[", 1) 
knitr::kable(data.frame(Polaridade = pol4, Fragmento = fras4))
                                                                                 
                                                                                 
###### modelo de linguagem n gramas ###################
#dtm com n-gramas
BigramTokenizer4 <- NGramTokenizer(p4p, RWeka::Weka_control(min = 2, max = 2)) 
                                                                                 

tdm4 <- DocumentTermMatrix(us4, control = list(tokenize = BigramTokenizer))
                                                                                 
# nTerms(dtm) 
# nDocs(dtm) 
# head(Terms(dtm2))
                                                                                 
                                                                                 
#n-gramas mais frequentes
                                                                                 
                                                                                 
frqsnt4 <- sort(slam::colapply_simple_triplet_matrix(dtm4, FUN = sum)) 
bc4 <- barchart(tail(frqsnt4, n = 30), xlim = c(0, NA)) 
                                                                                 
                                                                                 
### SUPERNATURAL ###
# Removendo acentos, deixando em caixa baixa, removendo pontuações, etc..
                                                                                 
s5 <- nam[5]
txt5 <- res_formatado[[5]]
                                                                                 
p5p <-     abjutils::rm_accent(txt5) %>% 
                    tolower() %>% 
                    removePunctuation() %>% 
                    stripWhitespace() %>% 
                    removeNumbers() %>% 
                    removeWords(words = sws) %>%
                    stemDocument(language = "portuguese")
                                                                                 
# Conferindo
head(txt5)
head(p5p)
                                                                                 
                                                                                 
corp5 <- VCorpus(VectorSource(x = p5p), 
                readerControl = list(language = "pt", 
                load = TRUE)) 
corp5 <- tm_map(corp5,~{
                        removeWords(words = stopwords("portuguese")), 
                        stemDocument(language = "portuguese")})
sapply(corp5, content) 
                                                                                 
##
                                                                                 
meta(corp5, tag = "ts", type = "indexed") <- p5p 
dtm5 <- DocumentTermMatrix(corp5) # Documentos nas linhas. 
tdm5 <- TermDocumentMatrix(corp5) # Termos nas linhas. 
                                                                                 
                                                                                 
# Número de documentos e termos (vocabulário). 
dim(dtm5) 
dim(tdm5) 
                                                                                 
# Algumas linhas e colunas da matriz. 
inspect(dtm5) 
                                                                                 
# Esparsidade (proporção de cédulas com 0 na matriz). 
                                                                                 
us5 <- VCorpus(VectorSource(p5p)) 
ds5 <- DocumentTermMatrix(us5) 
inspect(ds5) 
                                                                                 
non_zero5 <- sum(ds5 > 0) 
cells5 <- prod(dim(ds5)) 
100 * (1 - (non_zero5/cells5))
                                                                                 
# Converte para matriz ordinária.
msnt5 <- as.matrix(dtm5) 
                                                                                 
# Reordenar matriz por frequencia dos termos. 
msnt5 <- msnt5[order(apply(msnt5, MARGIN = 1, sum), decreasing = TRUE), ] 
                                                                                 
                                                                                 
# Termos com frequencia superior a um valor de corte. 
mft5 <- findFreqTerms(tdm5, lowfreq = 10) 
mft5 
# Frequência dos termos. 
trm5 <- as.matrix(dtm5) 
colSums(trm5 > 0) # Frequência.


# Termos associados por um valor acima de um limite. 

asssn5 <- findAssocs(dtm5, terms = mft5, corlimit = 0.5) 
                                                                                 
# Correlação de Pearson. 
                                                                                 
cor(trm5) 
                                                                                 
us5 <- sort(slam::col_sums(dtm5 > 0), decreasing = TRUE) 
                                                                                 
# Esparsidade da matrix. 
1 - sum(us5)/prod(dim(dtm5))
# Espasidade de cada termo. 
tsp5 <- 1 - us5/nDocs(dtm5) 
sum(tsp5 < 0.95) # Quantos tem menos que o corte. 
# Esparsidade da DTM com acúmulo dos termos. 
sps5 <- 1 - cumsum(us5)/(nDocs(dtm5) * seq_along(us5))
# Remove os termos mais esparsos. 
rst5 <- removeSparseTerms(dtm5, sparse = 0.95) 
# nTerms(rst) 
Terms(rst5)
                                                                                 
#nuvem de palavras
                                                                                 
                                                                                 
ds5 <- data.frame(word = names(us5), freq = us5) 
knitr::kable(ds5)
                                                                                 
wc5 <- wordcloud2(ds5)
                                                                                 
########## polarização - ANÁLISE DE SENTIMENTOS ###################
#**Cálculo da polaridade de cada avaliação
# A intersecção entre os termos do corpus e do dicionário. 
inter5 <- intersect(x = Terms(dtm5), 
                      y = lexsent$term[lexsent$pol != 0]) 
# length(inter5) 
                                                                                 
# Obter o vetor de polaridades associada aos termos na matriz. 
lex5 <- merge(x = data.frame(term = inter5, 
                      stringsAsFactors = FALSE), 
                      y = lexsent, 
                      sort = FALSE) 
# str(lex5) 
                                                                                 
# Remover os termos na DTM que não tem polaridade atribuida. 
ms5 <- as.matrix(dtm5) 
ms5 <- ms5[, lex5$terms] 
                                                                                 
# Verifica dimensões e disposição. 
all.equal(colnames(ms5), lex5$terms) 
                                                                                 
# Média aritmética das polaridades por termo em cada documento. 
pol5 <- (ms5 %*% (lex5$pol))/rowSums(ms5) 
                                                                                 
fras5 <- sapply(lapply(p5p, strwrap, width = 60), "[[", 1) 
knitr::kable(data.frame(Polaridade = pol5, Fragmento = fras5))
                                                                                 
                                                                                 
###### modelo de linguagem n gramas ###################
#dtm com n-gramas
BigramTokenizer5 <- NGramTokenizer(p5p, RWeka::Weka_control(min = 2, max = 2)) 
                                                                                 
                                                                                 
tdm5 <- DocumentTermMatrix(us5, control = list(tokenize = BigramTokenizer)) 
                                                                                 
# nTerms(dtm) 
# nDocs(dtm) 
# head(Terms(dtm2))
                                                                                 
findFreqTerms(dtm5, lowfreq = 12) 
                                                                                 
#n-gramas mais frequentes
                                                                                 
                                                                                 
frqsnt5 <- sort(slam::colapply_simple_triplet_matrix(dtm5, FUN = sum)) 
bc5 <- barchart(tail(frqsnt5, n = 30), xlim = c(0, NA)) 
                                                                                 
                                                                                 