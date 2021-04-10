####LIBRARIES####
library(tidyverse) #manipulate and plot data
library(sf) #work with "simple features" maps
library(geobr) #download up to date shape files for Brazil and it's regions
library(readxl)
library(lubridate)
library(gganimate)
library(transformr) #for gganimate with a map
library(gifski) #for gganimate
library(png) #for gganimate
library(magick) #for gganimate

#Inspiration: https://rpubs.com/rdwinkelman/covid19_us_spread_gif
#https://www.javaer101.com/en/article/32254326.html

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
  group_by(competência, uf) %>%
  summarise(saldomovimentação = sum(saldomovimentação))

data_municipio <- data %>%
  group_by(competência, uf, município, cod_municipio) %>%
  summarise(saldomovimentação = sum(saldomovimentação)) %>%
  filter(uf == "Rio Grande do Sul")


data_brasil <- data %>%
  group_by(competência, uf) %>%
  summarise(saldomovimentação = sum(saldomovimentação))

data_estado2 <- data %>%
  filter(uf == "Rio Grande do Sul") %>%
  mutate(cod_subclasse = str_pad(cod_subclasse, 7, pad = "0"),
         cod_subclasse2 = str_sub(cod_subclasse, start=1, end=2)) %>%
  filter(cod_subclasse2 %in% c("55","56")) %>%
  group_by(competência, uf, cod_subclasse2) %>%
  summarise(saldomovimentação = sum(saldomovimentação))

data_estado3 <- data %>%
  filter(uf == "Rio Grande do Sul") %>%
  mutate(cod_subclasse = str_pad(cod_subclasse, 7, pad = "0"),
         cod_subclasse2 = str_sub(cod_subclasse, start=1, end=5)) %>%
  filter(cod_subclasse2 %in% c("55108","55906","56112","56121","56201",
                               "90019","90027","90035", "92003","93212","93298",
                               "49507","50220","51111","51129","50912","50998",
                               "77110","79112","79121","79902","49221")) %>%
  group_by(competência, uf, cod_subclasse2) %>%
  summarise(saldomovimentação = sum(saldomovimentação))

data_estado4 <- data %>%
  filter(uf == "Rio Grande do Sul") %>%
  mutate(cod_subclasse = str_pad(cod_subclasse, 7, pad = "0"),
         cod_subclasse2 = str_sub(cod_subclasse, start=1, end=5)) %>%
  filter(cod_subclasse2 %in% c(
    "90019","90027","90035", "92003","93212","93298",
    "49507","50220","51111","51129","50912","50998",
    "77110","79112","79121","79902","49221")) %>%
  group_by(competência, uf, cod_subclasse2) %>%
  summarise(saldomovimentação = sum(saldomovimentação))

#### ANIMATED MAP

#Get shape files and complementary maps data from geobr

rs_map <- geobr::read_municipality(code_muni = "RS", year= 2020)

#Join CAGED data and map data

rs <- rs_map %>%
  mutate(code_muni = str_sub(code_muni, start = 1, end = 6)) %>%
  left_join(data_municipio %>%
              #filter(competência < "2020-07-01") %>% #comentar na versão final
              mutate(situação = case_when(
                saldomovimentação > 0 ~ "Positivo",
                saldomovimentação <= 0 ~ "Negativo"),
                cod_municipio = as.character(cod_municipio)),
            by = c("code_muni" = "cod_municipio")) %>%
  ungroup()


#Create plot

map_plot <- rs %>%
  #filter(competência == "2020-03-01") %>%
  ggplot() +
  geom_sf( 
    aes(fill= as.factor(situação), 
        group = competência), 
    color= "white", 
    size=0.15, 
    show.legend = TRUE) +
  scale_fill_manual(name = "Saldo de Emprego",
                    values = c("darkred","lightblue")) +
  labs(title = 'Data: {frame_time}',
       subtitle="Rio Grande do Sul", 
       size=8) +
  theme_minimal() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  transition_time(competência) +
  ease_aes('linear')

#run animation

anim_map <- animate(map_plot,
                    renderer = magick_renderer())


#### Diffusion Index ####

#Creating the index

dif_index <- data_municipio %>%
  group_by(competência) %>%
  summarise(negativos = if_else(saldomovimentação < 0, 1, 0) %>%
              sum(),
            `nº de municípios` = n(),
            `índice de difusão` = round(100*(negativos/`nº de municípios`),1))

#Plotting the index

dif_index_plot <- dif_index %>%
  ggplot(aes(x = competência, y = `índice de difusão`)) +
  geom_line(colour = "darkred", size = 1) +
  geom_segment(aes(xend=max(competência), 
                   yend = `índice de difusão`), 
               linetype=2, 
               colour='blue') +
  geom_point(size = 3) +
  geom_text(aes(x = max(competência)+.1, 
                label = `índice de difusão`), 
            hjust=0) +
  transition_reveal(competência) + 
  view_follow(fixed_y = TRUE) +
  coord_cartesian(clip = 'off') + 
  labs(title = "Índice de difusão da perda de empregos \n nos municípios do Rio Grande do Sul", 
       y = "Índice de difusão de perda de empregos") +
  theme_minimal()

#run animation
anim_difusion <- animate(dif_index_plot,
                         renderer = magick_renderer())


#### PLOTING THE PANEL WITH TWO FIGURES

#https://github.com/thomasp85/gganimate/wiki/Animation-Composition

a_mgif <- image_read(anim_map)
b_mgif <- image_read(anim_difusion)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif
