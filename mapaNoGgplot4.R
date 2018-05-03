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

# lista variáveis
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
  ) %>%                                     # indDep > 1! aí forcei.
  select(IBGE,Ano,Municipio,recTrs,recTot,indDep)  # tb tinha uma galera com recTrs = 0,
# atribuí NA a eles para expurgá-los.
# converter IBGE para factor

baseRec$IBGE <- as.factor(baseRec$IBGE)

## 2018-03-22 incorporando dados da base do IBGE
## municípios, população, classe de população etc.
## disponível em https://downloads.ibge.gov.br/downloads_estatisticas.htm
## Perfil_Municípios

shtsPrfMun <- excel_sheets("EeM/Base_MUNIC_2015.xls")

dadosMunIBGE <- read_excel("EeM/Base_MUNIC_2015.xls", sheet="Variáveis externas")

# melhorando os nomes das colunas
names(dadosMunIBGE) <- c("IBGEcomDig","Região","UF_cod","UF","Nome_Municipio","FaixaPop","Pop")

# ajustando o que é factor
dadosMunIBGE$UF <- as.factor(dadosMunIBGE$UF)

# o velhor problema do tamanho do código do município
dadosMunIBGE$IBGE <- as.factor(substr(dadosMunIBGE$IBGEcomDig,1,6))

# incorporando esses dados à baseRec
baseRec <- baseRec %>%
  inner_join(dadosMunIBGE, by = "IBGE")

length(levels(baseRec$IBGE))

baseRec$Ano <- as.factor(baseRec$Ano)

# pegar um ano específico

baseRec2016 <- baseRec %>%
  filter(Ano == 2016)

##### preparando o mapa #####

# abrindo o arquivo SDP do IBGE
# https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais.html
mapaBR <- readOGR(dsn="maps", layer="BRMUE250GC_SIR")

mapaBR$CD_GEOCODM = as.factor(substr(mapaBR$CD_GEOCMU,1,6))
#pq nao é com @data?

## Juntar ao mapa os dados a serem plotados
# o exemplo:
# rj <- merge(rj, base.media, by.x='CD_GEOCODM', by.y='ibge')  

# no nosso caso, já criei uma variável nova no mapaBR chamada IBGE, 
# com o mesmo nome da variável na base de dados. 
# então não preciso do by.x e by.y, só o by.

mapaBR <- merge(mapaBR, baseRec2016, by.x='CD_GEOCODM', by.y='IBGE')

# [comentário do original:]
# http://rstudio-pubs-static.s3.amazonaws.com/24563_3b7b0a6414824e3b91769a95309380f1.html
# Para usarmos o ggplot2 para plotar os dados no gráfico, 
# será necessário extrair da base geoferenciada um data frame 
# com as informações a serem plotadas. 
# Para fazer esta extração utiliza-se a função fortify() do pacote ggplot2.

# Extrai um data frame com coordenadas - variáveis: long  lat  order  hole  piece  group  id
# gpclibPermitStatus(TRUE)

mapaBR.df <- fortify(mapaBR, region = "CD_GEOCODM") 

# tava dando um erro no fortify do ggplot, aí reinstalei os pacotes, começando pelo rgeos.

#install.packages('rgeos', type='source')
#install.packages('maptools')
#install.packages('rgdal', type='source')

mapaBR.df <- merge(mapaBR.df, mapaBR@data, by.x = "id", by.y = "CD_GEOCODM")

mapaBR.df$indDepCat <- cut(mapaBR.df$indDep, breaks = c(0, 0.25, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                         labels=c('0-25%', '25-50%', '50-60%', '60-70%', '70-80%', '80-90%', '90-100%'), include.lowest=TRUE)

plotMapaInd <- ggplot(mapaBR.df, aes(long, lat, group = group, fill = indDepCat)) +  
  geom_polygon(colour=NA)  + 
  coord_equal() +
  ggtitle('Índice de dependência') +
  #theme(plot.title=element_text(size=rel(1), lineheight=.9, face="bold", colour="blue")) +
  labs(x = "", y = "", fill = "%") +
  scale_fill_manual(values=brewer.pal(9, 'Greens')[3:9])

plotMapaFaixa <- ggplot(mapaBR.df, aes(long, lat, group = group, fill = FaixaPop)) +  
  geom_polygon(colour=NA)  + 
  coord_equal() +
  ggtitle('Faixa População') +
  #theme(plot.title=element_text(size=rel(1), lineheight=.9, face="bold", colour="blue")) +
  labs(x = "", y = "", fill = "%") +
  scale_fill_manual(values=brewer.pal(9, 'Greens')[9:3])

plotMapaInd + facet_wrap(~Região) + theme_GTCEAD()
plotMapaFaixa + theme_GTCEAD()