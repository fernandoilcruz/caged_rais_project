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
# #Dicionários
# 
# dic_regiao<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimentação.xlsx",
#                        sheet="região")
# 
# dic_uf<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimentação.xlsx",
#                        sheet="uf")
# 
# dic_municipio<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimentação.xlsx",
#                        sheet="município")
# 
# dic_secao<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimentação.xlsx",
#                           sheet="seção")
# 
# dic_subclasse<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimentação.xlsx",
#                           sheet="subclasse")
# 
# dic_ocupacao<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimentação.xlsx",
#                           sheet="cbo2002ocupação")
# 
# dic_tamanhoestab<-read_excel(".\\Dados_CAGED\\Layout Novo Caged Movimentação.xlsx",
#                           sheet="tamestabjan")
# 
# #União
# 
# data <- data %>%
#   left_join(dic_regiao, 
#             by = c("região" = "Código")) %>% 
#   mutate(cod_regiao = região,
#          região = Descrição) %>%
#   select(- Descrição) %>%
#   left_join(dic_uf, 
#             by = c("uf" = "Código")) %>% 
#   mutate(cod_uf = uf,
#          uf = Descrição) %>%
#   select(- Descrição) %>%
#   left_join(dic_municipio, 
#             by = c("município" = "Código")) %>% 
#   mutate(cod_municipio = município,
#          município = Descrição) %>%
#   select(- Descrição) %>%
#   left_join(dic_secao, 
#             by = c("seção" = "Código")) %>% 
#   mutate(cod_secao = seção,
#          seção = Descrição) %>%
#   select(- Descrição) %>%
#   left_join(dic_subclasse, 
#             by = c("subclasse" = "Código")) %>% 
#   mutate(cod_subclasse = subclasse,
#          subclasse = Descrição) %>%
#   select(- Descrição) %>%
#   separate(col = município,
#            into = c("uf2", "município"),
#            sep="-",
#            extra = "merge") %>%
#   mutate(competência = ymd(paste0(competência,"01")))
# 
# data %>%
#   saveRDS(paste0(".\\Dados_CAGED\\CAGED_",first(filenames),"_",last(filenames),"_COM_DICIONARIO",".rds"))


data<- readRDS(".\\Dados_CAGED\\CAGED_202001_202102_COM_DICIONARIO.rds")

data_estado <- data %>%
  group_by(competência, uf, seção) %>%
  summarise(saldomovimentação = sum(saldomovimentação))

data_municipio <- data %>%
  group_by(competência, uf, município, cod_municipio) %>%
  summarise(saldomovimentação = sum(saldomovimentação)) %>%
  filter(uf == "Rio Grande do Sul")


data_brasil <- data %>%
  group_by(competência, uf) %>%
  summarise(saldomovimentação = sum(saldomovimentação))

#Análise


rs_map <- read_municipality(code_muni = "RS", year= 2020)

rs <- rs_map %>%
  mutate(code_muni = str_sub(code_muni, start = 1, end = 6)) %>%
  left_join(data_municipio %>%
              #filter(competência == "2020-01-01") %>%
              mutate(cor = case_when(
                saldomovimentação > 0 ~ "darkblue",
                saldomovimentação <= 0 ~ "darkred"),
                cod_municipio = as.character(cod_municipio)),
            by = c("code_muni" = "cod_municipio"))
  

anim_map <- ggplot() +
  geom_sf(data=rs, aes(fill= cor), color= "white", size=0.15, show.legend = FALSE) +
  labs(subtitle="Rio Grande do Sul", size=8) +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  transition_time(competência) +
  ease_aes('linear') +
  labs(title = 'Data: {competência}')

animate(anim_map, 
        #fps = 1,
        renderer = magick_renderer())