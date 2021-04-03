setwd("C:\\Users\\ferna\\Dropbox\\Projeto_Data_Science")

library(tidyverse) #manipulate and plot data
library(sf) #work with "simple features" maps
library(geobr) #download up to date shape files for Brazil and it's regions
library(readxl)
library(lubridate)
library(gganimate)
library(transformr) #pro gganimate com mapa
library(gifski) #pro gganimate
library(png) #pro gganimate
library(magick) #pro gganimate


####DATA####

memory.limit (9999999999)

filenames<-c(paste0(2020,str_pad(seq(1,12),2, pad = "0")),
             paste0(2021, str_pad(seq(1,2),2, pad="0")))


# data<- list.files(path = ".\\Dados_CAGED",
#                   pattern = "*.txt",
#                   full.names = T) %>%
#   map_df(~ read.csv(.,sep=";",dec=",",header=T, fileEncoding = 'UTF-8'))
# 
# 
# data %>%
#   saveRDS(paste0(".\\Dados_CAGED\\CAGED_",first(filenames),"_",last(filenames),".rds"))

#Dados

# data<- readRDS(".\\Dados_CAGED\\CAGED_202001_202102.rds")
# 
# #Dicion�rios
# 
# dic_regiao<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimenta��o.xlsx",
#                        sheet="regi�o")
# 
# dic_uf<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimenta��o.xlsx",
#                        sheet="uf")
# 
# dic_municipio<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimenta��o.xlsx",
#                        sheet="munic�pio")
# 
# dic_secao<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimenta��o.xlsx",
#                           sheet="se��o")
# 
# dic_subclasse<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimenta��o.xlsx",
#                           sheet="subclasse")
# 
# dic_ocupacao<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimenta��o.xlsx",
#                           sheet="cbo2002ocupa��o")
# 
# dic_tamanhoestab<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimenta��o.xlsx",
#                           sheet="tamestabjan")
# 
# #Uni�o
# 
# data <- data %>%
#   left_join(dic_regiao, 
#             by = c("regi�o" = "C�digo")) %>% 
#   mutate(cod_regiao = regi�o,
#          regi�o = Descri��o) %>%
#   select(- Descri��o) %>%
#   left_join(dic_uf, 
#             by = c("uf" = "C�digo")) %>% 
#   mutate(cod_uf = uf,
#          uf = Descri��o) %>%
#   select(- Descri��o) %>%
#   left_join(dic_municipio, 
#             by = c("munic�pio" = "C�digo")) %>% 
#   mutate(cod_municipio = munic�pio,
#          munic�pio = Descri��o) %>%
#   select(- Descri��o) %>%
#   left_join(dic_secao, 
#             by = c("se��o" = "C�digo")) %>% 
#   mutate(cod_secao = se��o,
#          se��o = Descri��o) %>%
#   select(- Descri��o) %>%
#   left_join(dic_subclasse, 
#             by = c("subclasse" = "C�digo")) %>% 
#   mutate(cod_subclasse = subclasse,
#          subclasse = Descri��o) %>%
#   select(- Descri��o) %>%
#   separate(col = munic�pio,
#            into = c("uf2", "munic�pio"),
#            sep="-",
#            extra = "merge") %>%
#   mutate(compet�ncia = ymd(paste0(compet�ncia,"01")))
# 
# data %>%
#   saveRDS(paste0(".\\Dados_CAGED\\CAGED_",first(filenames),"_",last(filenames),"_COM_DICIONARIO",".rds"))


data<- readRDS(".\\Dados_CAGED\\CAGED_202001_202102_COM_DICIONARIO.rds")

data_estado <- data %>%
  group_by(compet�ncia, uf, se��o) %>%
  summarise(saldomovimenta��o = sum(saldomovimenta��o))

data_municipio <- data %>%
  group_by(compet�ncia, uf, munic�pio, cod_municipio) %>%
  summarise(saldomovimenta��o = sum(saldomovimenta��o)) %>%
  filter(uf == "Rio Grande do Sul")


data_brasil <- data %>%
  group_by(compet�ncia, uf) %>%
  summarise(saldomovimenta��o = sum(saldomovimenta��o))

#An�lise


rs_map <- read_municipality(code_muni = "RS", year= 2020)

rs <- rs_map %>%
  mutate(code_muni = str_sub(code_muni, start = 1, end = 6)) %>%
  left_join(data_municipio %>%
              #filter(compet�ncia == "2020-01-01") %>%
              mutate(cor = case_when(
                saldomovimenta��o > 0 ~ "darkblue",
                saldomovimenta��o <= 0 ~ "darkred"),
                cod_municipio = as.character(cod_municipio)),
            by = c("code_muni" = "cod_municipio"))
  

anim_map <- ggplot() +
  geom_sf(data=rs, aes(fill= cor), color= "white", size=0.15, show.legend = FALSE) +
  labs(subtitle="Rio Grande do Sul", size=8) +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  transition_time(compet�ncia) +
  ease_aes('linear') +
  labs(title = 'Data: {compet�ncia}')

animate(anim_map, 
        #fps = 1,
        renderer = magick_renderer())