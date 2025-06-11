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
wine_data <- read.csv("pergunta01/winequality-white-q5.csv")

#adicionar uma nova coluna com a raiz quadrada do ácido cítrico
get_boxplot_outliers <- function(data, x_var, y_var) {
  outliers_df <- data.frame()
  unique_x_values <- unique(data[[x_var]])
  for (val in unique_x_values) {
    subset_data <- data[data[[x_var]] == val, ]
    y_values <- subset_data[[y_var]]
    
    q1 <- quantile(y_values, 0.25, na.rm = TRUE)
    q3 <- quantile(y_values, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    current_outliers <- subset_data[(y_values < lower_bound | y_values > upper_bound), ]
    if (nrow(current_outliers) > 0) {
      outliers_df <- rbind(outliers_df, current_outliers)
    }
  }
  return(outliers_df)
}

wine_data$sqrt_citric_acid <- sqrt(wine_data$citric.acid)
outliers_data <- get_boxplot_outliers(wine_data, "quality", "sqrt_citric_acid")


ggplot(wine_data, aes(x = factor(quality), y = sqrt(citric.acid))) +
  geom_boxplot(outlier.shape = NA) + 
  geom_jitter(data = outliers_data,
              aes(x = factor(quality), y = sqrt_citric_acid, color = "red"),
              width = 0.2, height = 0, alpha = 0.7, size = 2) + #tirar sobreposição
  labs(
    title = "Relationship between Square Root of Citric Acid and Wine Quality",
    x = "Wine Quality (1 = poor, 5 = excellent)",
    y = "Square Root of Citric Acid",
  ) +
  theme_minimal()
ggsave("pergunta01/boxplot_vinho.png", width = 8, height = 5)