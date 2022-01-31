
library(tidyverse)
library(RCurl)
library(xml2)
library(XML)
library(rvest)
library(htmltools)

# As melhores series por nota
url2 <- "www.adorocinema.com/series-tv/melhores"

handle <- getCurlHandle(useragent = "R-web-scrap",
                        followlocation = TRUE,
                        cookiefile = "")
H <- htmlParse(getURL(url2, curl = handle), asText = TRUE)


# ids das 5 primeiras series
sufis <- c("//*[@id='col_content']/div/div[1]/div/div[2]/div/h2/a", 
           "//*[@id='col_content']/div/div[3]/div/div[2]/div/h2/a", 
           "//*[@id='col_content']/div/div[5]/div/div[2]/div/h2/a", 
           "//*[@id='col_content']/div/div[7]/div/div[2]/div/h2/a", 
           "//*[@id='col_content']/div/div[9]/div/div[2]/div/h2/a")

# pegando os nos 
punt <- getNodeSet(H, path = sufis)

# Nomes
nam <- xmlSApply(punt, FUN = xmlValue, trim = TRUE)

# Codigos
sufist <- xmlSApply(punt, FUN = xmlGetAttr, name = 'href')

# montando url para webscraping
url <- "www.adorocinema.com"
fim <- "criticas"
urls <- paste0(url, sufist, fim)

num <- "//nav[@class ='pagination cf']"

nums <- urls %>% 
  map(
    ~{
      read_html(getURL(.x
                       , curl = handle)) %>% 
        xml_find_all(xpath = num) %>%  
        xml_text(trim = TRUE) %>% 
        as.character() %>% 
        str_split(pattern = "\\...")
      })
  
  
nums <- nums %>% map(~{.x[[1]][2]}) %>% c() %>% as.numeric()
nums[3:4] <- 6


# criando os links para as 5 primeiras paginas de criticas
urls_ext <- paste0(
  c(rep(urls[1], nums[1]), 
    rep(urls[2], nums[2]),
    rep(urls[3], nums[3]),
    rep(urls[4], nums[4]),
    rep(urls[5], nums[5])),
  paste0("/?page=",
         c(1:nums[1], 1:nums[2], 1:nums[3],
           1:nums[4], 1:nums[5])))

# capturando os textos de cada uma das 5 urls 
# Criando uma funÃ§ao que extrai os textos 
pega_texto <- function(res_webscraping){
  crits <- ".//div[@class = 'content-txt review-card-content']"
  
  texto <- res_webscraping %>% 
    xml_find_all(xpath = crits) %>% 
    xml_text(trim = TRUE)
  }

# Pegando os comentários, demora pois são várias páginas
textos <- purrr::map(
  urls_ext, 
  ~{ 
    read_html(getURL(.x, curl = handle))  %>% 
      pega_texto() 
  }) 

# checando se as dimensoes estao corretas
length(textos)
textos %>% map(length)

# arrumando o resultado

res_formatado <- list(
  serie1 = textos[1:nums[1]] %>% unlist(),
  serie2 = textos[(nums[1]+1):(sum(nums[1:2]))] %>% unlist(),
  serie3 = textos[(sum(nums[1:2])+1):(sum(nums[1:3]))] %>% unlist(),
  serie4 = textos[(sum(nums[1:3])+1):(sum(nums[1:4]))] %>% unlist(),
  serie5 = textos[(sum(nums[1:4])+1):(sum(nums[1:5]))] %>% unlist()
)

#