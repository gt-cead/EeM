#https://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true
#Reinstalling rgdal after installing rgeos and maptools helped for me!

library(readxl)
library(tidyverse)

library(rgeos)
library(maptools)
library(rgdal)
library(ggplot2)
library(RColorBrewer)


setwd("C:/Users/tiago.pereira/OneDrive - Secretaria do Tesouro Nacional/GT-CEAD/projs")

##### preparando a base #####

painelRec <- read.csv2("EeM/painel_rec.csv")

# lista vari�veis
names(painelRec)

# gerar a base
baseRec <- painelRec %>%
  mutate(recTrs =`TransfCorrentesGov.imput`
         +`TransfCorrentesOutras.imput`
         +`TransfCorrentesExterior.imput`
         +`TransfCapitalGov.imput`
         +`TransfCapitalOutras.imput`
         +`TransfCapitalExterior.imput`,
         recTot =`RecTotal.imput`,
         indDep = ifelse(recTrs>recTot, 1, ifelse(recTrs == 0, NA, recTrs / recTot)) #pq tem uns gracinhas com
  ) %>%                                     # indDep > 1! a� forcei.
  select(IBGE,Ano,Municipio,recTrs,recTot,indDep)  # tb tinha uma galera com recTrs = 0,
# atribu� NA a eles para expurg�-los.
# converter IBGE para factor

baseRec$IBGE <- as.factor(baseRec$IBGE)

## 2018-03-22 incorporando dados da base do IBGE
## munic�pios, popula��o, classe de popula��o etc.
## dispon�vel em https://downloads.ibge.gov.br/downloads_estatisticas.htm
## Perfil_Munic�pios

shtsPrfMun <- excel_sheets("EeM/Base_MUNIC_2015.xls")

dadosMunIBGE <- read_excel("EeM/Base_MUNIC_2015.xls", sheet="Vari�veis externas")

# melhorando os nomes das colunas
names(dadosMunIBGE) <- c("IBGEcomDig","Regi�o","UF_cod","UF","Nome_Municipio","FaixaPop","Pop")

# ajustando o que � factor
dadosMunIBGE$UF <- as.factor(dadosMunIBGE$UF)

# o velhor problema do tamanho do c�digo do munic�pio
dadosMunIBGE$IBGE <- as.factor(substr(dadosMunIBGE$IBGEcomDig,1,6))

# incorporando esses dados � baseRec
baseRec <- baseRec %>%
  inner_join(dadosMunIBGE, by = "IBGE")

length(levels(baseRec$IBGE))

baseRec$Ano <- as.factor(baseRec$Ano)

# pegar um ano espec�fico

baseRec2016 <- baseRec %>%
  filter(Ano == 2016)

##### preparando o mapa #####

# abrindo o arquivo SDP do IBGE
# https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais.html
mapaBR <- readOGR(dsn="maps", layer="BRMUE250GC_SIR")

mapaBR$CD_GEOCODM = as.factor(substr(mapaBR$CD_GEOCMU,1,6))
#pq nao � com @data?

## Juntar ao mapa os dados a serem plotados
# o exemplo:
# rj <- merge(rj, base.media, by.x='CD_GEOCODM', by.y='ibge')  

# no nosso caso, j� criei uma vari�vel nova no mapaBR chamada IBGE, 
# com o mesmo nome da vari�vel na base de dados. 
# ent�o n�o preciso do by.x e by.y, s� o by.

mapaBR <- merge(mapaBR, baseRec2016, by.x='CD_GEOCODM', by.y='IBGE')

# [coment�rio do original:]
# http://rstudio-pubs-static.s3.amazonaws.com/24563_3b7b0a6414824e3b91769a95309380f1.html
# Para usarmos o ggplot2 para plotar os dados no gr�fico, 
# ser� necess�rio extrair da base geoferenciada um data frame 
# com as informa��es a serem plotadas. 
# Para fazer esta extra��o utiliza-se a fun��o fortify() do pacote ggplot2.

# Extrai um data frame com coordenadas - vari�veis: long  lat  order  hole  piece  group  id
# gpclibPermitStatus(TRUE)

mapaBR.df <- fortify(mapaBR, region = "CD_GEOCODM") 

# tava dando um erro no fortify do ggplot, a� reinstalei os pacotes, come�ando pelo rgeos.

#install.packages('rgeos', type='source')
#install.packages('maptools')
#install.packages('rgdal', type='source')

mapaBR.df <- merge(mapaBR.df, mapaBR@data, by.x = "id", by.y = "CD_GEOCODM")

mapaBR.df$indDepCat <- cut(mapaBR.df$indDep, breaks = c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                         labels=c('0-25%', '25-50%', '50-60%', '60-70%', '70-80%', '80-90%', '90-100%'), include.lowest=TRUE)

plotMapaInd <- ggplot(mapaBR.df, aes(long, lat, group = group, fill = indDepCat)) +  
  geom_polygon(colour=NA)  + 
  coord_equal() +
  ggtitle('�ndice de depend�ncia') +
  #theme(plot.title=element_text(size=rel(1), lineheight=.9, face="bold", colour="blue")) +
  labs(x = "", y = "", fill = "%") +
  scale_fill_manual(values=brewer.pal(9, 'Greens')[3:9])

plotMapaFaixa <- ggplot(mapaBR.df, aes(long, lat, group = group, fill = FaixaPop)) +  
  geom_polygon(colour=NA)  + 
  coord_equal() +
  ggtitle('Faixa Popula��o') +
  #theme(plot.title=element_text(size=rel(1), lineheight=.9, face="bold", colour="blue")) +
  labs(x = "", y = "", fill = "%") +
  scale_fill_manual(values=brewer.pal(9, 'Greens')[9:3])

plotMapaInd + facet_wrap(~Regi�o) + theme_GTCEAD()
plotMapaFaixa + theme_GTCEAD()