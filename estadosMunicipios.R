library(tidyverse)
library(readxl)

# diretório de trabalho
setwd("C:/Users/tiago.pereira/OneDrive - Secretaria do Tesouro Nacional/GT-CEAD/projs")

#Local do arquivo
arq <- "EeM/Receitas.xlsx"
listaNomes <- excel_sheets(arq)

### pule até parar

# #tentativa de arrumar um jeito de atribuir os 
# # 4 últimos caracteres do nome de cada planilha como
# # nomes dos dataframes que vão receber os dados de cada planilha
# 
# vetorNomes <- listaNomes %>%
#   map(~ substr(.,nchar(.)-3,nchar(.)))
# 
# arq %>% 
#   set_names(~ vetorNomes) %>% 
#   map(~ read_excel)
# 
# for (nome in listaNomes) {
#   if (nome != "Municípios") {
#     qdeCaract = nchar(nome)
#     ano = substr(nome,qdeCaract-3,qdeCaract)
#     df_generico <- read_excel(nome, sheet = )
#   }
# }

### pare de pular. retomemos.

### lusitan way

for (i in 1:length(listaNomes)) {
  nome = listaNomes[i]
  if (nome != "Municípios") {
    qdeCaract = nchar(nome)
    ano = substr(nome,qdeCaract-3,qdeCaract)
    df_generico <- read_excel(arq, sheet = i, range = cell_rows(c(4,NA))) #para começar na linha 4 
    df_generico$ano = as.numeric(ano)
    assign(paste("df_", ano, sep=""), df_generico)
  }
}

# com isso tenho um dataset para cada planilha/ano.
# agora vamos tentar juntá-las

df <- rbind(df_2013,df_2014,df_2015,df_2016) #certeza q tem jeito melhor. move on.

#"espalhando" a planilha, transformando as contas da variável "Conta" em variáveis.

tidy_df <- df %>%
  spread(key=Conta,value=Valor,fill=0)

# não precisa mais, por causa do argumento "fill = 0" no spread
# # substituindo os NAs por zeros...
# tidy_df[is.na(tidy_df)] <- 0

# vetor com os nomes das variáveis
names(tidy_df)

# calculando as novas variáveis de interesse, receita propria e receita de transferência
# calculando o índice de dependência
# reduzindo a tabela apenas para as variáveis calculadas e para as informações do Mun
# sumarizando para somar Receitas com Deduções
#
# atenção para usar `...` nos nomes das variáveis, e não '...' ou "..." 
# :/

baseMun <- tidy_df %>%
  mutate(recPropria=`1.1.0.0.00.00.00 - Receita Tributária`
                   -`1.1.1.2.04.31.00 - Retido nas Fontes - Trabalho`
                   -`1.1.1.2.04.34.00 - Retido nas Fontes - Outros Rendimentos`
                   +`1.2.0.0.00.00.00 - Receitas de Contribuições`
                   +`1.3.0.0.00.00.00 - Receita Patrimonial`
                   -`1.3.2.2.00.00.00 - Dividendos`
                   +`1.4.0.0.00.00.00 - Receita Agropecuária`
                   +`1.5.0.0.00.00.00 - Receita Industrial`
                   +`1.6.0.0.00.00.00 - Receita de Serviços`
                   -`1.6.0.0.02.00.00 - Serviços Financeiros`
                   +`1.9.0.0.00.00.00 - Outras Receitas Correntes`
                   +`2.3.0.0.00.00.00 - Amortização de Empréstimos`
                   +`2.5.0.0.00.00.00 - Outras Receitas de Capital`,
         recTransf =`1.7.0.0.00.00.00 - Transferências Correntes`
                   +`2.4.0.0.00.00.00 - Transferências de Capital`
                   +`1.1.1.2.04.31.00 - Retido nas Fontes - Trabalho`
                   +`1.1.1.2.04.34.00 - Retido nas Fontes - Outros Rendimentos`,
         recTotal = recTransf + recPropria
         #indDepen = recTransf / recTotal
         ) %>%
  select(Instituição:ano,recPropria:recTotal) %>%
  group_by(Instituição, Cod.IBGE, UF, População, ano) %>%
  summarize(recPropria = sum(recPropria), recTransf = sum(recTransf), recTotal = sum(recTotal), indDepen = recTransf / recTotal)

## tentando agora fazer o gráfico com o mapa do brasil
# https://leobastos.wordpress.com/2015/04/10/mapas-do-ibge-no-r/
# ^ ajudou bastante.

# abrindo o mapa

# carregando a biblioteca
library(rgdal)

# abrindo o arquivo SDP do IBGE
# https://mapas.ibge.gov.br/bases-e-referenciais/bases-cartograficas/malhas-digitais.html
mapaBR <- readOGR(dsn="EeM", layer="BRMUE250GC_SIR")

# essa variável que carreguei com o arquivo SHP tem um dataframe, acessível via @data, 
# onde constam as cidades com seus códigos IBGE. o nome da coluna é CD_GEOCMU.

# vou tentar fazer um join nesse mapaBR@data, trazendo os dados de baseMun para ele. 

# primeiro vou gerar uma baseMun para um único ano

baseMun2016 <- baseMun %>%
  filter(ano==2016)

# como em baseMun o código ibge está na variável "Cod.IBGE" e na mapaBR@data está na
# variável "CD_GEOCMU", vou usar a opção "by = c("CD_GEOCMU" = "Cod.IBGE")" (pg185 do R4DS)

# não deu muito certo, vou renomear o "Cod.IBGE" do baseMun2016 e mudar para factor.

colnames(baseMun2016)[2] <- "CD_GEOCMU"
baseMun2016$CD_GEOCMU <- factor(baseMun2016$CD_GEOCMU)

# agora sim, o left join

tsa <- mapaBR@data %>%
  left_join(baseMun2016, by = "CD_GEOCMU")

# agora tenho um df na mesma ordem do mapaBR@data, e agora vou 
# acrescentar, no mapaBR@data, as colunas que quero plotar. 
# no caso o índice de Dependência.

mapaBR@data$indDepen = tsa$indDepen

# plotando... (demora pacas)

spplot(mapaBR, "indDepen")

# aparentemente, com "col = "transparent"" as bordas não aparecem... vamos ver
# https://stackoverflow.com/questions/12682212/how-to-get-spplot-lattice-to-not-draw-borders-around-polygons

spplot(mapaBR, "indDepen", col="transparent")

            
## scatterplot dessa amostra

# carregando tabela de regioes

regioes <- read.csv2("EeM/regioes.csv")

# fazendo o join

## tô fazendo uma cópia antes pq aparentemente ficou um monte de lixo no baseMun2016
baseMun2016v <- baseMun2016[,c(1:9)]

# converter "UF" para factors, para poder fazer o join e trazer a variável de região.

baseMun2016v$UF <- factor(baseMun2016v$UF)

# join propriamente dito

baseMun2016v <- baseMun2016v %>%
  inner_join(regioes, by = "UF")

# mudando a ordem das colunas... por frescura mesmo, 
# e voltando os dados para a variável baseMun2016

baseMun2016 <- baseMun2016v[,c(1,2,3,10,4:9)]

# cálcula máximos para padronizar o gráfico
recMin = min(baseMun2016$recTransf,baseMun2016$recPropria)
recMax = max(baseMun2016$recTransf,baseMun2016$recPropria)

plot2016 <- ggplot(baseMun2016, aes(x=recPropria,y=recTransf,size=População, color=Região))+
  geom_point()+
  scale_x_log10(limits=c(recMin,recMax))+
  scale_y_log10(limits=c(recMin,recMax))+
  geom_abline(intercept = 0, slope = 1)

# com facet

plot2016faceted <- ggplot(baseMun2016, aes(x=recPropria,y=recTransf,size=População, color=Região))+
  geom_point()+
  scale_x_log10(limits=c(recMin,recMax))+
  scale_y_log10(limits=c(recMin,recMax))+
  geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~Região)

######################## 2013 ################

## gerar base para 2013 e refazer o gráfico scatter.
baseMun2013 <- baseMun %>%
  filter(ano==2013)

## tô fazendo uma cópia antes pq aparentemente ficou um monte de lixo no baseMun2016
baseMun2013v <- baseMun2013[,c(1:9)]

# converter "UF" para factors, para poder fazer o join e trazer a variável de região.

baseMun2013v$UF <- factor(baseMun2013v$UF)

# join propriamente dito

baseMun2013v <- baseMun2013v %>%
  inner_join(regioes, by = "UF")

# mudando a ordem das colunas... por frescura mesmo, 
# e voltando os dados para a variável baseMun2016

baseMun2013 <- baseMun2013v[,c(1,2,3,10,4:9)]

# cálcula máximos para padronizar o gráfico
recMin = min(baseMun2013$recTransf,baseMun2013$recPropria)
recMax = max(baseMun2013$recTransf,baseMun2013$recPropria)

# na verdade, o melhor era pegar os mesmos mínimos e máximos do plot anterior!, 
# pros gráficos ficarem comparáveis
# ixe, vou ter que ajustar os preços de 2013... deixar todos a preços de 2016.

# inflação
atuMon = (1.0641)*(1.1067)*(1.0629)

# atualizando
baseMun2013$recPropria <- baseMun2013$recPropria * atuMon
baseMun2013$recTransf <- baseMun2013$recTransf * atuMon
baseMun2013$recTotal <- baseMun2013$recTotal * atuMon

# mínimos e máximos gerais

## tem um município com recTransf == 0 :/
baseMun2013$Instituição[which.min(baseMun2013$recTransf)]
#ou
baseMun2013$Instituição[which(baseMun2013$recTransf == 0)]

# vou fazer um arrange e ver quem é a segunda menor rectransf
baseMun2013 %>%
  arrange(recTransf)

# vou atribuir o mínimo na marra. depois penso num jeito melhor.
recMin = 2617043
recMax = max(baseMun2013$recTransf,baseMun2013$recPropria,baseMun2016$recTransf,baseMun2016$recPropria)

plot2013 <- ggplot(baseMun2013, aes(x=recPropria,y=recTransf,size=População, color=Região))+
  geom_point()+
  scale_x_log10(limits=c(recMin,recMax))+
  scale_y_log10(limits=c(recMin,recMax))+
  geom_abline(intercept = 0, slope = 1)

plot2013faceted <- ggplot(baseMun2013, aes(x=recPropria,y=recTransf,size=População, color=Região))+
  geom_point()+
  scale_x_log10(limits=c(recMin,recMax))+
  scale_y_log10(limits=c(recMin,recMax))+
  geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~Região)

plot2016 <- ggplot(baseMun2016, aes(x=recPropria,y=recTransf,size=População, color=Região, label=Instituição))+
  geom_point()+
  scale_x_log10(limits=c(recMin,recMax))+
  scale_y_log10(limits=c(recMin,recMax))+
  ylab("Receitas de Transferências")+
  xlab("Receitas Próprias")+
  geom_abline(intercept = 0, slope = 1)

plot2016faceted <- ggplot(baseMun2016, aes(x=recPropria,y=recTransf,size=População, color=Região, label=Instituição))+
  geom_point()+
  scale_x_log10(limits=c(recMin,recMax))+
  scale_y_log10(limits=c(recMin,recMax))+
  geom_abline(intercept = 0, slope = 1)+
  ylab("Receitas de Transferências")+
  xlab("Receitas Próprias")+
  facet_wrap(~Região)


################ fim 2013 ##############

#plotly!

library(plotly)
ggplotly(plot2016faceted)
ggplotly(plot2013faceted)

################################# painel Rec ###### 

#base Cesef com quase todos os municípios

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

# fazer para os outros... (deve dar para automatizar isso.)

baseRec2015 <- baseRec %>%
  filter(Ano == 2015)

baseRec2014 <- baseRec %>%
  filter(Ano == 2014)

baseRec2013 <- baseRec %>%
  filter(Ano == 2013)

baseRec2012 <- baseRec %>%
  filter(Ano == 2012)

baseRec2011 <- baseRec %>%
  filter(Ano == 2011)

baseRec2010 <- baseRec %>%
  filter(Ano == 2010)

###### gráficos #####

ggplot(baseRec, aes(y=indDep,x=Ano)) +
  geom_boxplot()+
  facet_wrap(~Região)

ggplot(baseRec2016, aes(x=indDep))+
  geom_histogram(binwidth = 0.005)+
  xlab("Índice de dependência (Receitas de Transferências / Receitas Totais) - Municípios - 2016")

ggplot(baseRec2016, aes(x=FaixaPop,y=indDep)) +
  geom_boxplot()+
  ylab("Índice de Dependência")+
  xlab("Tamanho município (IBGE)")

ggplot(baseRec2016, aes(x=Região,y=indDep)) +
  geom_boxplot()+
  ylab("Índice de Dependência")+
  xlab("Região")

ggplot(baseRec, aes(x=Ano,y=indDep)) +
  geom_boxplot()+
  ylab("Índice de Dependência")+
  xlab("Ano")+
  facet_wrap(~Região)

ggplot(baseRec, aes(x=Ano,y=indDep)) +
  geom_boxplot()+
  ylab("Índice de Dependência")+
  xlab("Ano")+
  facet_wrap(~FaixaPop)

#calculando a "receita própria" pela diferença...
baseRec2016$recProp <- baseRec2016$recTot - baseRec2016$recTrs

baseRec2016 %>%
  arrange(recProp)

##### PRODUÇÃO - gráfico geral scatter todos municípios

library(extrafont)
font_import()
loadfonts(device = "win")

# para mostrar os rótulos numa notação mais bonitinha
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l) # original era: l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}

scatter2016_regiao <- ggplot(baseRec2016, aes(y=recTrs,x=recProp,color=Região,size=Pop,label=Nome_Municipio))+
  geom_point()+
  scale_x_log10(limits=c(1e+04,3.315440e+10), labels = fancy_scientific)+ #1.100340e+04
  scale_y_log10(limits=c(1e+04,3.315440e+10), labels = fancy_scientific)+ #acrescentar: labels = scales::comma, para não ficar em formato científico
  scale_size(breaks = c(10000,100000,1000000), labels = c("10 mil", "100 mil", "1 milhão"))+
  labs(
    size = "População",
    x = "Receitas Próprias",
    y = "Receitas de Transferências",
    title = "Receitas de Transferências x Receitas Próprias dos Municípios Brasileiros em 2016",
    subtitle = "Grau de dependênca das receitas dos municípios com relação às transferências",
    caption = "Fonte: Siconfi"
  )+
  geom_abline(intercept = 0, slope = 1)+
  annotate(geom="text", x=1e+06, y=2e+09, label="Região em que as Receitas de Transferências \n superam as Receitas Próprias \n (mais dependência de transferências)", color = "grey50", size = 4, family = "Cambria", fontface = "italic")+
  annotate(geom="text", x=5e+08, y=1e+06, label="Região em que as Receitas Próprias \n superam as Receitas de Transferências \n (menos dependência de transferências)", color = "grey50", size = 4, family = "Cambria", fontface = "italic")

## definindo o nosso tema
theme_GTCEAD <- function(){
  theme_minimal() +
  theme(
    text = element_text(family = "Cambria"), #Bookman?
    title = element_text(colour = "gray25", face = "bold"),
    plot.subtitle = element_text(face = "plain"),
    plot.caption = element_text(colour = "gray30", face = "plain"),
    strip.background = element_rect(fill = "gray60", color = "gray95")
  )
}

# indDep x pop
scatter2016_pop <- ggplot(baseRec2016, aes(y=indDep,x=Pop,color=Região,size=recTot,label=Nome_Municipio))+
  geom_point(alpha=0.5)+
  scale_x_log10(labels = fancy_scientific)+ #1.100340e+04
  #scale_y_log10(labels = fancy_scientific)+ #acrescentar: labels = scales::comma, para não ficar em formato científico
  scale_size(labels = fancy_scientific)+
  labs(
    size = "Receita Total",
    #alpha = "Faixa População",
    x = "População",
    y = "Índice de Dependência",
    title = "Índice de Dependência x População do município",
    subtitle = "Grau de dependênca das receitas dos municípios com relação às transferências",
    caption = "Fonte: Siconfi"
  )#+
  #geom_abline(intercept = 0.5, slope = 0)#+
  #annotate(geom="text", x=1e+06, y=2e+09, label="Região em que as Receitas de Transferências \n superam as Receitas Próprias \n (mais dependência de transferências)", color = "grey50", size = 4, family = "Cambria", fontface = "italic")+
  #annotate(geom="text", x=5e+08, y=1e+06, label="Região em que as Receitas Próprias \n superam as Receitas de Transferências \n (menos dependência de transferências)", color = "grey50", size = 4, family = "Cambria", fontface = "italic")


# PRODUÇÃO - plot com o tema customizado
scatter2016_regiao +
  theme_GTCEAD()

scatter2016_pop +
  theme_GTCEAD()

library(plotly)
ggplotly(scatter2016_regiao +
           theme_GTCEAD())

#### boxplot massa agora

bpRegiao <- ggplot(baseRec2016, aes(x = Região, y = indDep, fill=Região))+
  geom_boxplot()+#outlier.size=0)+
  #geom_jitter(aes(x = Região, y = indDep),
  #            position = position_jitter(width=0.2, height = 0),
  #            alpha = 0.6,
  #            size= 3,
  #            show_guide=FALSE)+
  labs(
    x = "Região", # ou "Tamanho do município"
    y = "Índice de dependência",
    title = "Índice de Dependência das transferências - Municípios Brasileiros em 2016",
    subtitle = "Distribuição por Região dentro de cada faixa de tamanho de Município", # ou "Distribuição por tamanho dos municípios dentro de cada região",
    caption = "Fonte: Siconfi"
  ) +
  theme_minimal() +
  theme_GTCEAD() +
  theme(axis.text.x = element_text(angle=-30, size = 7))+
  facet_wrap(~FaixaPop)

library(ggplot2)

bpFaixaPop <- ggplot(baseRec2016, aes(x = FaixaPop, y = indDep, fill=Região))+
  geom_boxplot()+#outlier.size=0)+
  #geom_jitter(aes(x = Região, y = indDep),
  #            position = position_jitter(width=0.2, height = 0),
  #            alpha = 0.6,
  #            size= 3,
  #            show_guide=FALSE)+
  labs(
    x = "Região", # ou "Tamanho do município"
    y = "Índice de dependência",
    title = "Índice de Dependência das transferências - Municípios Brasileiros em 2016",
    subtitle = "Distribuição por Região dentro de cada faixa de tamanho de Município", # ou "Distribuição por tamanho dos municípios dentro de cada região",
    caption = "Fonte: Siconfi"
  ) +
  theme_minimal() +
  theme_GTCEAD() +
  theme(axis.text.x = element_text(angle=-30, size = 7))+
  facet_wrap(~Região)

bpRegiao
bpFaixaPop

#para ver os parâmetros que o ggplot usou
ggplot_build(bpFaixaPop)$data

#paleta de cores padrão do ggplot
library(scales)
show_col(hue_pal()(5))
cores_bp <- c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")

#### dotplot ####

baseRec2010e2016 <- baseRec %>%
  filter(Ano == 2010 | Ano == 2016) %>%
  group_by(UF, Ano) %>%
  summarize(mediaDepUF = mean(indDep, na.rm="true")) %>%
  #summarize(mediaDepUF = weighted.mean(indDep,Pop, na.rm="true")) %>%
  ungroup()
  
baseRec10e16 <- baseRec2010e2016 %>%
  arrange(Ano) %>%
  mutate(UFnova = fct_reorder(UF,mediaDepUF,first))

ggplot(baseRec10e16) +
  geom_path(aes(x = mediaDepUF, y = UFnova), size = 1,
            arrow = arrow(length = unit(2, "mm"), type = "closed"), color="darkred") +
  # Specify the hjust aesthetic with a conditional value
  geom_text(
    aes(x = mediaDepUF,
        y = UFnova,
        label = round(mediaDepUF, 2),
        hjust = ifelse(Ano == "2010", -0.5, 1.5),            
        family = "Cambria"
    )
  ) +
  labs(
    x = "Média Ponderada do índice de dependência",
    y = "",
    title = "Média Ponderada do índice de dependência dos municípios de cada estado",
    caption = "Fonte: Siconfi"
  ) +
  theme_GTCEAD()+
  coord_cartesian(xlim = c(0.5, 1))

####

#### Sumário por uf ####

baseSumario <- baseRec %>%
  group_by(UF, Ano) %>%
  summarize(mediaInd = mean(indDep, na.rm="true"),
            medianaInd = median(indDep, na.rm="true"),
            primQuartInd=quantile(indDep, probs=0.25, na.rm="true"),
            tercQuartInd=quantile(indDep, probs=0.75, na.rm="true"),
            minInd = min(indDep, na.rm="true"),
            maxInd = max(indDep, na.rm="true"))

baseSumario_sp <- baseSumario %>%
  filter(UF == "SP")

# primeiro plot. agora falta formatar.

ggplot(baseSumario_sp, aes(x = Ano, y = mediaInd, group=1)) +
  geom_line()+
  geom_point()+
  geom_ribbon(aes(ymin = primQuartInd, ymax = tercQuartInd), fill = "Green", alpha = 0.5)+
  geom_line(aes(y = medianaInd, group = 1), linetype = "dashed")+
  geom_line(aes(y = minInd, group = 1)) + 
  geom_line(aes(y = maxInd, group = 1)) +
  annotate(geom="text", x="2010", y=baseSumario_sp$mediaInd[which(baseSumario_sp$Ano=="2010")]+0.02, label="Média", color = "grey50", size = 4, family = "Cambria", fontface = "italic")+
  theme_GTCEAD()
  
  
  

####


scatter2016_regiao_facet <- ggplot(baseRec2016, aes(y=recTrs,x=recProp,color=FaixaPop,size=Pop,label=Nome_Municipio))+
  geom_point()+
  scale_x_log10(limits=c(1.100340e+04,3.315440e+10))+
  scale_y_log10(limits=c(1.100340e+04,3.315440e+10))+
  ylab("Receitas de Transferências")+
  xlab("Receitas Próprias")+
  geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~Região)

scatter2016_regiao_facet +
  theme(
    strip.background = element_rect(fill = "gray60", color = "gray95")
  )

scatter2016_FaixaPop <- ggplot(baseRec2016, aes(y=recTrs,x=recProp,color=FaixaPop,size=Pop,label=Nome_Municipio))+
  geom_point()+
  scale_x_log10(limits=c(1.100340e+04,3.315440e+10))+ #1.100340e+04
  scale_y_log10(limits=c(1.100340e+04,3.315440e+10))+
  ylab("Receitas de Transferências")+
  xlab("Receitas Próprias")+
  geom_abline(intercept = 0, slope = 1)

scatter2016_FaixaPop_indice <- ggplot(baseRec2016, aes(y=recTrs,x=recProp,color=FaixaPop,size=1/indDep,label=Nome_Municipio))+
  geom_point()+
  scale_x_log10(limits=c(1.100340e+04,3.315440e+10))+
  scale_y_log10(limits=c(1.100340e+04,3.315440e+10))+
  ylab("Receitas de Transferências")+
  xlab("Receitas Próprias")+
  geom_abline(intercept = 0, slope = 1) 

### por região

baseRec2016NO <- baseRec2016 %>%
  filter(Região == "1 - Norte")

baseRec2016NE <- baseRec2016 %>%
  filter(Região == "2 - Nordeste")

baseRec2016SE <- baseRec2016 %>%
  filter(Região == "3 - Sudeste")

baseRec2016SU <- baseRec2016 %>%
  filter(Região == "4 - Sul")

baseRec2016CO <- baseRec2016 %>%
  filter(Região == "5 - Centro-Oeste")

ggplot(baseRec2016SU, aes(y=recTrs,x=recProp,color=FaixaPop,size=Pop,label=Nome_Municipio))+
  geom_point()+
  scale_x_log10(limits=c(1.100340e+04,3.315440e+10))+
  scale_y_log10(limits=c(1.100340e+04,3.315440e+10))+
  ylab("Receitas de Transferências")+
  xlab("Receitas Próprias")+
  geom_abline(intercept = 0, slope = 1)+
  facet_wrap(~UF)

library(plotly)
ggplotly(scatter2016_regiao)
ggplotly(scatter2016_FaixaPop)
ggplotly(scatter2016_FaixaPop_indice)


###### geomap

# join no mapaBR@data, left

# descobri que o código do IBGE no arquivo da Cesef, painelRec, só tem 6 dígitos:

#levels(mapaBR@data$CD_GEOCMU)
#levels(baseRec2016$IBGE)

# tb descobri que o último/sétimo dígito é só um dígito verificador

#vamos testar a correspondência... no painelRec, 110001 é Alta Floresta.
#No mapaBR@data (do IBGE), o código 1100015 é...

#mapaBR@data$NM_MUNICIP[which(mapaBR@data$CD_GEOCMU=="1100015")]

# ALTA FLORESTA! :)

# beleza, então vamos fazer um substring aqui... vamos pensar
# acho q vou fazer o seguinte, vou gerar uma nova variável no mapaBR@data, 
# sendo um substring do código IBGE que está nesse df ($CD_GEOCMU). Aí faço o join
# por essa variável. Posso até chamá-la de "IBGE" para faciliar o join.

mapaBR@data$IBGE = as.factor(substr(mapaBR@data$CD_GEOCMU,1,6))

# agora sim, as duas tabelas tem uma variável IBGE. vamos fazer o join

mapa_temp <- mapaBR@data %>%
  left_join(baseRec2016, by = "IBGE")

# beleza! agora vamos levar a coluna "indDep" do mapa_temp para o mapaBR@data

mapaBR@data$indDep = mapa_temp$indDep

cores <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c")
# e agora só resta plotar...

spplot(mapaBR, "indDep", col="transparent", col.regions = cores)

#pouca_dep <- subset(baseRec2016,indDep<0.3)



ggplot(sumario, aes(fill=`Categoria População`, y=Valor, x=Variavel)) +
  geom_bar( stat="identity", position="fill") +
  scale_x_discrete(labels=c("Qde" = "Quantidade de municípios", "Pop" = "População",
                            "pib" = "Produto Interno Bruto"))


tab_aux <- unique(read_excel("Indicadores Municipais - Parte 2.xlsx", sheet="Base I-C-Receitas Orçamentarias", range = cell_limits(c(5, 1), c(NA, 2))))
