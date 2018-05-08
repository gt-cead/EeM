ggplot(sumario, aes(fill=CatPop, y=Valor, x=Variavel, 
                                   label = format(ifelse(Variavel!="Qde",
                                                         ifelse(Variavel == "Pop" | Variavel == "Pib", 
                                                                round(Valor/1000000,0),
                                                                round(Valor/1000000000,0)),
                                                         Valor),
                                                  big.mark = "."))) +
  coord_flip() +
  geom_bar( stat="identity", position ="fill",width=0.6, color = "white", size = 1) +
  geom_text(size = 2, position = position_fill(vjust = 0.5), color = "white")+ #position = "fill"
  labs(
    x = NULL,
    y = NULL,
    fill = "Categoria População",
    title = "Municípios Brasileiros em 2015, conforme a categoria de tamanho",
    subtitle = "As linhas pontilhadas indicam a distribuição da população nas categorias"
  ) +
  scale_x_discrete(labels=c("Qde" = "Quantidade de municípios", 
                            "Pop" = "População",
                            "Pib" = "Produto Interno Bruto",
                            "recFed" = "Arrecadação Federal (exceto Previdência)",
                            "recIPTU" = "Arrecadação de IPTU",
                            "despEduc" = "Despesas municipais com Educação",
                            "despSaud" = "Despesas municipais com Saúde")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = tresCores)+#c("#D5845E","#F3987D","#FECE60")) +
  geom_hline(yintercept = (sumario$Valor[which(sumario$CatPop == "menos de 50 mil" & sumario$Variavel=="Pop")])/total_pop, color = "white", linetype="dotted", size = 1) +
  geom_hline(yintercept = 1 - (sumario$Valor[which(sumario$CatPop == "mais de 500 mil" & sumario$Variavel=="Pop")])/total_pop, color = "white", linetype="dotted", size = 1) +
  theme_GTCEAD() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8))


#plot percents

ggplot(sumario_percent, aes(fill=CatPop, y=Valor, x=Variavel, label=Percent)) +
  coord_flip() +
  geom_bar( stat="identity", position ="fill",width=0.6, color = "white", size = 1) +
  geom_text(size = 3, position = position_fill(vjust = 0), hjust = -0.1, vjust = -4, family = "Roboto Condensed Light")+ #position = "fill" // 
  labs(
    x = NULL,
    y = NULL,
    fill = "Categoria População",
    title = "Municípios Brasileiros em 2015, conforme a categoria de tamanho",
    subtitle = "As linhas pontilhadas indicam a distribuição da população nas categorias"
  ) +
  scale_x_discrete(labels=c("Qde" = "Quantidade de municípios", 
                            "Pop" = "População",
                            "Pib" = "Produto Interno Bruto",
                            "recFed" = "Arrecadação Federal (exceto Previdência)",
                            "recIPTU" = "Arrecadação de IPTU",
                            "despEduc" = "Despesas municipais com Educação",
                            "despSaud" = "Despesas municipais com Saúde")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = tresCores)+#c("#D5845E","#F3987D","#FECE60")) +
  geom_hline(yintercept = (sumario$Valor[which(sumario$CatPop == "menos de 50 mil" & sumario$Variavel=="Pop")])/total_pop, color = "white", linetype="dotted", size = 1) +
  geom_hline(yintercept = 1 - (sumario$Valor[which(sumario$CatPop == "mais de 500 mil" & sumario$Variavel=="Pop")])/total_pop, color = "white", linetype="dotted", size = 1) +
  theme_GTCEAD() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8))


# calculando percentuais

sumario_percent <- sumario
sumario_percent$Percent <- 0
for (i in 1:nrow(sumario_percent)) {
  sumario_percent$Percent[i] <- scales::percent(round(sumario_percent$Valor[i]/totais_v[as.character(sumario_percent$Variavel[i])],2))
  }