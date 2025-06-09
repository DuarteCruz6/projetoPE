# INFO
## Contém dados desde 1997 até 2024
## Os dados incluem:
##          - quantidade de vinho em armazém antes da colheita (Opening Stock em 10^3 hL)
##          - quantidade de vinho produzido durante a colheita (Production em 10^3 hL)
##          - quantidade de vinho em armazém após a colheira   (Availability em 10^3 hL)

# EXERCÍCIO
## Eliminar todas as observações que não incluem a variável Category
## Eliminar todas as observações que a variável Product Group == Non-Vinified
## Pacote ggplot2
## Produzir gráfico de barras que permita comparar a distribuição da variável Availability
## Apenas em 2016
## Entre: France, Italy, Spain, Portugal e Others (agrupamento)

# SUBMISSÃO 
## PDF com uma página A4 que inclua o código e o gráfico produzido

#pacote ggplot2 e readxl
library(ggplot2)
library(readxl)

#ler o ficheiro
dados <- read_excel("pergunta2/wine_prod_EU.xlsx")

#remover observações com Category em falta ou Product Group == "Non-Vinified"
dados_limpos <- dados[!is.na(dados$Category) & dados$`Product Group` != "Non-Vinified", ]

#filtrar apenas o ano de 2016
dados_2016 <- dados_limpos[dados_limpos$Year == 2016, ]

#criar nova coluna para agrupar países
dados_2016$CountryGroup <- ifelse(
  dados_2016$`Member State` %in% c("France", "Italy", "Spain", "Portugal"),
  dados_2016$`Member State`,
  "Others"
)

#agrupar e somar Availability por CountryGroup
resumo <- aggregate(
  Availability ~ CountryGroup,
  data = dados_2016,
  sum,
  na.rm = TRUE
)

#criar gráfico de barras
ggplot(resumo, aes(x = CountryGroup, y = Availability, fill = CountryGroup)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Wine Availability in 2016 by Category and Country Group",
    x = "Country Group",
    y = "Availability (10^3 hL)",
  ) +
  theme_minimal()

ggsave("grafico_vinho.png", width = 8, height = 5)