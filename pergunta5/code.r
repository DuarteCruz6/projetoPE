#fixar semente
set.seed(1084)

#dimensão da amostra
n <- 39000

somas_9 <- 0
somas_10 <- 0

#simulação
for (i in 1:n) {
  #lançar 3 dados
  dados <- sample(1:6, 3, replace = TRUE)
  
  #soma
  soma_atual <- sum(dados)
  
  #contar somas
  if (soma_atual == 9) {
    somas_9 <- somas_9 + 1
  } else if (soma_atual == 10) {
    somas_10 <- somas_10 + 1
  }
}

#calcular frequências relativas
freq_rel_9 <- somas_9 / n
freq_rel_10 <- somas_10 / n

#diferença entre frequências relativas
diferenca_freq <- freq_rel_10 - freq_rel_9

#arredondar a 4 casas decimais
print(round(diferenca_freq, 4))