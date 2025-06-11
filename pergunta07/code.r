#parametros
n <- 11
sum_x <- 67.78
sum_log_x <- 19.96

#media do x
mean_x <- sum_x / n

#função para encontrar alpha_hat
g_alpha <- function(alpha) {
  log(alpha) - digamma(alpha) - log(mean_x) + (sum_log_x / n)
}

#uniroot para encontrar alpha_hat
#intervalo de pesquisa é [0.001, 130.9]
alpha_hat_solution <- uniroot(f = g_alpha, interval = c(0.001, 130.9))

#estimativa de alpha é o 'root' da solução
alpha_hat <- alpha_hat_solution$root

#alpha_hat
cat("A estimativa de máxima verosimilhança de alpha (alpha_hat) é:", alpha_hat, "\n")

#lambda_hat
lambda_hat <- alpha_hat / mean_x

#lambda_hat
cat("A estimativa de máxima verosimilhança de lambda (lambda_hat) é:", lambda_hat, "\n")

#estimativa do comprimento modal (alpha - 1) / lambda
modal_length_estimate <- (alpha_hat - 1) / lambda_hat

#arredondar para 2 casas decimais
rounded_modal_length <- round(modal_length_estimate, 2)

#resposta
cat("A estimativa do comprimento modal (alpha - 1) / lambda é:", rounded_modal_length, "\n")