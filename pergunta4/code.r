#parâmetros
lambda <- 13 #escala
k <- 3       #forma

#pergunta 1
expected_value <- lambda * gamma(1 + 1/k)
cat("Resposta pergunta 1:", expected_value, "\n")


#pergunta 2

#fixar a semente
set.seed(621)

#dimensão da amostra
n <- 8500

#gerar amostra da distribuição Weibull
sample <- rweibull(n, shape = k, scale = lambda)

#calcular o valor aproximado do esperado pela média da amostra (Monte Carlo)
mean_sample <- mean(sample)

#pergunta 2
cat("Resposta pergunta 2:", mean_sample, "\n")

#calcular a diferença absoluta e arredondar a 4 casas decimais
difference <- abs(expected_value - mean_sample)
difference_rounded <- round(difference, 4)

#resposta final
cat("Resposta final:", difference_rounded, "\n")
