setwd("C:\\Users\\ferna\\Dropbox\\Projeto_Data_Science")

####Libraries####
library(tidyverse)
library(RCurl)
#devtools::install_github("jimhester/archive")
#https://stackoverflow.com/questions/16096192/how-to-programmatically-extract-unzip-a-7z-7-zip-file-with-r
library(archive)

#site: ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/Movimenta%C3%A7%C3%B5es/


#Mês do Arquivo
mes<-"Fevereiro"

"CAGEDMOV202102.7z"
