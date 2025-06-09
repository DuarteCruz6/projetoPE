# INFO
## 4898 amostras de vinho verde branco
## variáveis 1 a 11 -> caraterísticas físico-químicas dos vinhos
## variáveis 12     -> avaliação feita por especialistas (1-má a 5-excelente) 

# EXERCÍCIO
## pacote ggplot2
## produzir gráfico com box plots paralelos 
## como é que a raiz quadrada da variável citric.acid (variável 3) se relociona com quality (variável 12)
## realçar os outliers e tentar contrariar a sua sobreposição

# SUBMISSÃO 
## PDF com uma página A4 que inclua o código e o gráfico produzido


#pacote ggplot2
library(ggplot2)

#ler o ficheiro
dados <- read.csv("pergunta1/winequality-white-q5.csv", header = TRUE, sep = ",")

#adicionar uma nova coluna com a raiz quadrada do ácido cítrico
dados$sqrt_citric_acid <- sqrt(dados$`citric.acid`)

#criar o boxplot com pontos sobrepostos deslocados (jitter)
ggplot(dados, aes(x = factor(quality), y = sqrt_citric_acid)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  #boxplot + outliers a vermelho
  labs(
    title = "Relation between the square root of citric.acid and the wine quality",
    x = "Wine's quality",
    y = "Square root of citric.acid"
  ) +
  theme_minimal()
ggsave("boxplot_vinho.png", width = 8, height = 5)