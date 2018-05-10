# devtools::install_github("tidyverse/ggplot2")
# # devtools::install_github("slowkow/ggrepel")
# # esse pacote ggrepel faz com que os rótulos não se superponham!
# library(ggplot2)
# library(ggrepel)

setwd("C:/Users/tiago.pereira/OneDrive - Secretaria do Tesouro Nacional/GT-CEAD/projs/EeM")

painel <- read.csv2('dados/painel.csv')

# pos <- position_jitter(width = 0.1, height = 0, seed = 1)

capitais <- painel %>% filter(eh_capital)
summary(capitais$GrauDep)


bp_teste <- function(regiao) {
  ggplot(capitais %>% filter(Regiao %in% regiao))+#, 
                   #aes(
                    # x = Regiao, 
                    # y = GrauDep, 
                    # fill=Regiao
                   #)
                  
  #coord_flip() +
  ylim(0.25,1) +
  xlim(0,3)+
  #geom_boxplot(outlier.colour = "darkgray", outlier.alpha = 0.2)+#outlier.size=0)+
  geom_jitter(aes(x = 0.2, y = GrauDepCapit, size = pop, color = Regiao, label=Municipio),
              position = position_jitter(width = 0.1, height = 0),
              alpha = 0.6,
              show_guide=FALSE,
              na.rm = TRUE)+
  # geom_text_repel(
  #   aes(x = 0.3, y = GrauDepCapit, label = ifelse(eh_capital, as.character(MunicipioSemUF), '')), 
  #   size = 2, 
  #   #position = pos,
  #   segment.color = 'grey50')+
  labs(
    x = NULL,
    y = "Grau de dependência",
    title = "Grau de dependência das capitais de cada Região",
    subtitle = NULL, 
    caption = "Fonte: Siconfi") +
  theme_GTCEAD()+
  theme(axis.text.x=element_blank())
}

# uma maneira de circular

i <- 1
l <- list()
reg <- NULL
while(i <= 5){
  reg <- c(reg, levels(capitais$Regiao)[i])
  l[[i]] <- bp_teste(reg)
  i <- i+1
}



l[[5]]

summary(capitais$GrauDep)

ggplot(capitais)+ 
  ylim(0.25,1) +
  xlim(-1e+7,1e+8) +
  #scale_x_log10()+
  #geom_boxplot(outlier.colour = "darkgray", outlier.alpha = 0.2)+#outlier.size=0)+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.25, ymax = quantile(capitais$GrauDep, probs=0.25), alpha = 0.1, fill = "blue")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = quantile(capitais$GrauDep, probs=0.25), ymax = quantile(capitais$GrauDep, probs=0.75), alpha = 0.1, fill = "yellow")+
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = quantile(capitais$GrauDep, probs=0.75), ymax = Inf, alpha = 0.1, fill = "red")+
  geom_point(aes(x = pop, 
                 y = GrauDepCapit, 
                 size = pop, 
                 color = Regiao), 
             alpha = 0.6)+
  geom_text_repel(
     aes(x = pop, y = GrauDepCapit, label = MunicipioSemUF, color = Regiao, segment.color = Regiao), 
     size = 3)+#, 
     #segment.color = 'grey50')+
  geom_boxplot(aes(y=GrauDepCapit, x=8e+7))+
  geom_hline(yintercept = median(capitais$GrauDep), linetype = 2)+
  geom_hline(yintercept = quantile(capitais$GrauDep, probs=0.25), linetype = 2)+
  geom_hline(yintercept = quantile(capitais$GrauDep, probs=0.75), linetype = 2)+
  labs(
    x = NULL,
    y = "Grau de dependência",
    title = "Grau de dependência das capitais",
    subtitle = NULL, 
    size = NULL,
    caption = "Fonte: Siconfi") +
  theme_GTCEAD()+
  theme(axis.text.x=element_blank())

ggplot(capitais)+ 
  ylim(0.25,1) +
  xlim(0,1)+
  #scale_x_log10()+
  #geom_boxplot(outlier.colour = "darkgray", outlier.alpha = 0.2)+#outlier.size=0)+
  geom_boxplot(aes(y=GrauDepCapit, x=0.1), width =0.1)+
  geom_jitter(aes(x = 0.1, y = GrauDepCapit, size = pop, color = Regiao, label=Municipio),
              position = position_jitter(width = 0.05, height = 0),
              alpha = 0.6,
              show_guide=FALSE,
              na.rm = TRUE)+
  labs(
    x = NULL,
    y = "Grau de dependência",
    title = "Grau de dependência das capitais",
    subtitle = NULL, 
    size = NULL,
    caption = "Fonte: Siconfi") +
  theme_GTCEAD()+
  theme(axis.text.x=element_blank())

ggplot(capitais)+ 
  ylim(0.25,1) +
  xlim(0,1)+
  labs(
    x = NULL,
    y = "Grau de dependência",
    title = "Grau de dependência das capitais",
    subtitle = NULL, 
    size = NULL,
    caption = "Fonte: Siconfi") +
  theme_GTCEAD()+
  theme(axis.text.x=element_blank())
