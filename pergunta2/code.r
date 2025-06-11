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
df_wine_cleaned <- subset(dados, !is.na(Category))
df_wine_cleaned <- subset(df_wine_cleaned, `Product Group` != "Non-Vinified")

#filtrar apenas o ano de 2016
df_2016 <- subset(df_wine_cleaned, Year == 2016)

#criar nova coluna para agrupar países
df_2016$Country_Group <- ifelse(df_2016$`Member State` == "France", "France",
                                 ifelse(df_2016$`Member State` == "Italy", "Italy",
                                        ifelse(df_2016$`Member State` == "Spain", "Spain",
                                               ifelse(df_2016$`Member State` == "Portugal", "Portugal", "Others"))))

#agrupar e somar Availability por CountryGroup
df_plot_data <- aggregate(Availability ~ Category + Country_Group, data = df_2016, FUN = sum, na.rm = TRUE)

df_plot_data$Country_Group <- factor(df_plot_data$Country_Group,
                                     levels = c("France", "Italy", "Spain", "Portugal", "Others"))

#criar gráfico de barras
wine_plot <- ggplot(df_plot_data, aes(x = Category, y = Availability, fill = Country_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Wine Availability by Category and Country Group in 2016",
    x = "Wine Category",
    y = "Availability (10^3 hL)",
    fill = "Country Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Roda as legendas do eixo X para melhor leitura

ggsave("grafico_vinho.png", width = 8, height = 5)