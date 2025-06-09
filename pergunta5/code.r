#fixar semente
set.seed(1084)

#dimensão da amostra
n <- 39000

#simular n jogadas: cada jogada é a soma de 3 dados (uniformes de 1 a 6)
dados <- sample(1:6, 3 * n, replace = TRUE)
somas <- rowSums(matrix(dados, nrow = n, ncol = 3))

#calcular frequências relativas
freq_9 <- sum(somas == 9) / n
freq_10 <- sum(somas == 10) / n

#diferença entre frequências relativas
diff_freq <- freq_10 - freq_9

#arredondar a 4 casas decimais
round(diff_freq, 4)