setwd("C:\\Users\\ferna\\Dropbox\\Projeto_Data_Science")

####Libraries####
library(tidyverse)
library(RCurl)
#devtools::install_github("jimhester/archive")
#https://stackoverflow.com/questions/16096192/how-to-programmatically-extract-unzip-a-7z-7-zip-file-with-r
library(archive)


#Site: ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/

#source('http://cemin.wikidot.com/local--files/raisr/rais.r')


#ano<-c(1994:2019)
ano<-c(2008:2019)

uf<-c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA",
      "PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")

regiao<-c("Norte","Nordeste","Norte","Norte","Nordeste","Nordeste","Distrito Federal",
          "Sudeste","Centro-Oeste","Nordeste","Centro-Oeste","Centro-Oeste",
          "Sudeste","Norte","Nordeste","Sul","Nordeste","Nordeste","Sudeste",
          "Nordeste","Sul","Norte","Norte","Sul","Sudeste","Nordeste","Norte")


# ano1<-c(1994,1995)
# uf1<-c("RS","SP")

memory.limit(9999999999)

for(i in ano){
  for(j in uf){
    
    my_file<-paste0("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/",i,"/",j,i,".7z")
    tf <- tempfile() 
    td <- tempdir()
    file.path <- my_file
    download.file(file.path , tf , mode = "wb" )
    #archive(tf)
    archive_read(tf) %>% 
      read.table(sep=";",header=T) %>%
      as_tibble() %>%
      saveRDS(file=paste0("C:\\Users\\ferna\\Dropbox\\Projeto_Data_Science\\Dados_RAIS\\",j,i,".rds"))
    unlink(tf)
  }
}

x1<-readRDS("RS1994.rds")

saveRDS(bind_cols(uf,regiao),".\\Dados_RAIS\\uf.rds")

my_file<-paste0("ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/",2000,"/AM2000.7z")

# myfile <- getURL(file, ssl.verifyhost=FALSE, ssl.verifypeer=FALSE) %>%
#   read.csv(textConnection())


dest<-"E:\\Dados_RAIS\\AM2000.zip" #cria o arquivo e a pasta de destino. (Vou usar assim no lugar de criar um arquivo temporário (tempfile()) porque tenho a intenção de manter esses arquivos zipados no meu computador.)
download.file(file,dest, mode="wb")

data <- dest %>% zip::unzip("E:\\Dados_RAIS\\AM2000.zip",
                            files="AM2000.txt",
                            exdir="E:\\Dados_RAIS")
  unzip(unzip=getOption("unzip")) %>% read.table(header=T)
  unzip(files="AM2000.txt", exdir="E:\\Dados_RAIS") %>%
  read.table(header=T)
#unlink(temp) #for deleting files


temp <- tempfile(tmpdir="E:\\Dados_RAIS")
download.file("https://perso.telecom-paristech.fr/eagan/class/igr204/data/BabyData.zip",temp)
carsData <- read.table(unz(temp, "Sleep_data_11_29_2012.csv"))
unlink(temp)


x<-read.table("E:\\Dados_RAIS\\AM2000.txt",sep=";", header=T)

dest %>% unz(filename="AM2000.txt") %>% 
  read.table(sep=";", header=T)



temp <- tempfile(tmpdir="E:\\Dados_RAIS")
download.file(file,temp)
carsData <- read.table(unz(temp, "AM2000.txt\\"))
unlink(temp)


read.table(unz("E:\\Dados_RAIS","AM2000.zip"),sep=";",header=T)




tf <- tempfile() 
td <- tempdir()
file.path <- my_file
download.file( file.path , tf , mode = "wb" )
#archive(tf)
read.table(archive_read(tf), sep=";",header=T)
unlink(tf)