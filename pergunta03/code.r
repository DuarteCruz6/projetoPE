# INFO
## Dados de 2010 a 2014 num determinado local
## Dados incluem:
##          - poluição
##          - orvalho
##          - temperatura
##          - velocidade do vento
##          - neve
##          - chuva

## EXERCÍCIO
## Apenas dados de Setembro de 2014
## Pacote ggplot2
## Gráfico que ilustre a variação horária da variável Poluição ao longo do mês
## Representar a variação da mediana diária dessa variável

# SUBMISSÃO 
## PDF com uma página A4 que inclua o código e o gráfico produzido

#pacotes
library(ggplot2)

#ler o ficheiro
dados <- read.csv("pergunta03/clima.csv", header = TRUE, sep = ",")

#converter a coluna Data para o formato datetime
dados$Data <- as.POSIXct(dados$Data, format = "%Y-%m-%d %H:%M:%S")

#filtrar só setembro de 2014
dados_set2014 <- dados[
  format(dados$Data, "%Y") == "2014" & format(dados$Data, "%m") == "09",
]

#criar coluna com dia e sem horas
dados_set2014$dia <- as.Date(dados_set2014$Data)
dados_set2014 <- dados_set2014[!is.na(dados_set2014$Poluição), ] #para tirar os com valores NA

#calcular mediana diária de Poluição
mediana_diaria <- aggregate(
  Poluição ~ dia,
  data = dados_set2014,
  FUN = function(x) median(x, na.rm = TRUE)
)

#converter dia para POSIXct
mediana_diaria$dia <- as.POSIXct(mediana_diaria$dia)

#criar o gráfico
Sys.setlocale("LC_TIME", "pt_PT.UTF-8") #para meter em português
ggplot() +
  geom_line(data = dados_set2014, aes(x = Data, y = Poluição), color = "blue", alpha = 0.5) +
  geom_line(data = mediana_diaria, aes(x = dia, y = Poluição), color = "red", linewidth = 1) +
  scale_x_datetime(date_labels = "%b %d") +
  labs(
    title = "Variação horária da Poluição em Setembro de 2014",
    x = "Data e hora",
    y = "Poluição",
    caption = "Linha azul: variação horária | Linha vermelha: mediana diária"
  ) +
  theme_minimal()
ggsave("pergunta03/grafico.png", width = 8, height = 5)