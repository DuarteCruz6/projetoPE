#amostra original
dados_originais <- c(
  2.1, 3.4, 1.3, 0.9, 4.8, 2.9, 3.6, 4.6, 4.4, 2.8, 3.8, 3.6, 5.2, 5.8, 3.5,
  4.7, 4.3, 2.7, 5.8, 5.3, 5.5, 6, 2, 2.5, 2.6, 0.7, 4.3, 5.3, 3.2, 1.7,
  3.6, 4.8, 2, 6.7, 3.9, 1.5, 2.8, 2.9, 2.7, 7.5, 1.9, 5.1, 2.5, 7.7, 4.5,
  0.6, 4.6, 3.4, 5.5, 3.6, 4.9, 6.1, 1.6, 0.6, 2.2, 0.8, 0.7, 2, 10.6,
  4.7, 1.9, 3.8, 5.8, 4.2, 3.3, 3.8, 5.7, 3.1, 3.9, 7.5, 1, 2.7, 2.3,
  4.5, 6, 4.4, 7.6, 3.4, 3.5, 8, 4.1, 3.7, 6.2, 1.2, 6, 8.4, 2.9, 0.6,
  5.6, 4.4, 4.2, 3.9, 1.9, 3.2, 3.8, 1.9, 9.2, 3.3, 4.3, 6.4, 0.7, 3.2,
  6.5, 5, 2.6, 4.4, 3.6, 4.7, 1.4, 3.8, 8.4, 0.7, 7, 2.5, 4.3, 0.6, 2.4,
  3, 4.5, 0.9, 2.2, 2.3, 5, 4.7, 3, 4.7, 8.4, 3.2, 2.6, 4.8, 4.5, 3.1,
  0.7, 0.5, 3.5, 6, 6.1, 6.6, 0.9, 3.4, 3.7, 3.9, 4.2, 6.3, 4.6, 3.1,
  5.3, 1.2, 5.6, 1.6, 4.1, 4.1, 2.2, 2.2, 3.2, 8.4, 3.1, 2.6, 2.7, 5.8,
  8.7, 2.3, 9, 1.5, 2.9, 3, 5.5, 0.7, 5.1, 3.5, 3.2, 7, 3, 3.7, 5.5,
  10.8, 5.6, 5.9, 4.2, 7.9, 2.7, 5.6, 4.2, 6.1, 1.8, 2.2, 3.6, 1.7, 1.4,
  7.3, 3.6, 5.2, 8.2, 4.2, 0.8, 2.9, 4.7, 2.9, 3.2, 3.3, 5.5, 3.5, 7.7,
  6.7, 4.1, 4.5, 1.7, 6.8, 1.4, 8.4, 1, 1.1, 4.4, 4.6, 1.3, 3.1, 0.6,
  3.4, 2.6, 1.1, 3.5, 2.6, 0.4, 7.2, 4.7, 4.7, 6.8, 1.7, 4.1, 3.8, 1.4,
  1.6, 6.7, 1, 2.5, 3.7, 4.2, 2.2, 1.9, 1.2, 1.9, 4.1, 2.2, 3.8, 7.3,
  3.3, 5.3, 3.1, 2.1, 1.1, 2.5, 0.4, 5.6, 7, 11, 4.2, 6.1, 6.3, 4.8,
  5.7, 3.7, 2.6, 7.5, 2, 6.2, 7.1, 3, 3, 6.6, 2.2
)

#parametros
set.seed(5114)
n_subamostra <- 242
sigma_H0 <- 2.7
k <- 5

#calcular subamostra
subamostra <- sample(dados_originais, size = n_subamostra, replace = FALSE)

#vetor para guardar os limites das classes
limites_classes <- numeric(k - 1)

#calcular os limites das classes
for (j in 1:(k - 1)) {
  limites_classes[j] <- sqrt(-2 * sigma_H0^2 * log(1 - j/k))
}

#criar vetor completo com os limites das classes, incluindo 0 e Inf
limites_completos <- c(0, limites_classes, Inf)

#agrupar as observações da subamostra
frequencias_observadas <- table(cut(subamostra, breaks = limites_completos, right = TRUE, include.lowest = TRUE))

#calcular as frequências esperadas
frequencias_esperadas <- rep(n_subamostra / k, k)

#calcular a estatística do teste chi-squared
chi2_stat <- sum((frequencias_observadas - frequencias_esperadas)^2 / frequencias_esperadas)
cat("Estatística Qui-Quadrado (X^2):", chi2_stat, "\n")

#definir graus de liberdade para o teste chi-squared
graus_liberdade <- k - 1
cat("Graus de Liberdade:", graus_liberdade, "\n")

#calcular valor p
valor_p <- pchisq(chi2_stat, df = graus_liberdade, lower.tail = FALSE)
cat("Valor-p:", valor_p, "\n")

# Para decidir qual opção escolher, precisamos comparar o valor-p com níveis de significância comuns.
# Se o valor-p for muito pequeno, rejeitamos H0. Se for grande, não rejeitamos H0.

if (valor_p < 0.01) {
    #rejeita-se 1%, 5% e 10%
    opcao_escolhida <- "opcao d"

} else if (valor_p < 0.05) {
    #rejeita-se 5% e 10%
    opcao_escolhida <- "opcao c"

} else if (valor_p < 0.10) {
    #rejeita-se a 10%
    opcao_escolhida <- "opcao e"

} else {
    #nao se reijeita nenhuma
    opcao_escolhida <- "opcao b"
}

cat("decisao final:", opcao_escolhida, "\n")
