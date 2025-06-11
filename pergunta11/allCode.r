### ------------------- PERGUNTA 4 ------------------- ###




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




### ------------------- PERGUNTA 5 ------------------- ###




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




### ------------------- PERGUNTA 6 ------------------- ###




# --- 1. Cálculo do valor exato de p_n (Distribuição de Irwin-Hall) ---

#parametros
n <- 7
x <- 3.25

#calcular os termos da soma da fórmula de Irwin-Hall
#soma vai de k=0 até floor(x)
termo0 <- (-1)^0 * choose(n, 0) * (x - 0)^n
termo1 <- (-1)^1 * choose(n, 1) * (x - 1)^n
termo2 <- (-1)^2 * choose(n, 2) * (x - 2)^n
termo3 <- (-1)^3 * choose(n, 3) * (x - 3)^n #floor(3.25) é 3, então k vai até 3

#soma total dos termos
soma_termos <- termo0 + termo1 + termo2 + termo3

#fatorial de n
fatorial_n <- factorial(n)

#valor exato de p_n
p_n_exato <- soma_termos / fatorial_n

#valor 1
cat("1. Valor exato de p_n (Irwin-Hall):", p_n_exato, "\n")


# --- 2a. Cálculo de p_n_TLC (Teorema do Limite Central) ---

#parâmetros da distribuição uniforme(0,1)
#média E[X] = 0.5
#variância Var[X] = 1/12

#Média e Variância da soma S_n
mu_Sn <- n * 0.5
sigma2_Sn <- n * (1/12)
sigma_Sn <- sqrt(sigma2_Sn)

#Z-score para x = 3.25
z_score <- (x - mu_Sn) / sigma_Sn

#valor aproximado de p_n_TLC usando a CDF da normal
p_n_TLC <- pnorm(z_score)

#valor 2a
cat("2a. Valor aproximado de p_n_TLC (TLC):", p_n_TLC, "\n")


# --- 2b. Cálculo de p_n_sim (Simulação) ---

set.seed(5916) #fixar a semente
m <- 140 #num de amostras

#gerar 'm' amostras, cada uma com 'n' observações da uniforme(0,1) e calcular a soma
simulated_Sn <- replicate(m, sum(runif(n)))

#obter a proporção de valores simulados de S_n que são menores ou iguais a 'x'
p_n_sim <- sum(simulated_Sn <= x) / m

#valor 2b
cat("2b. Valor aproximado de p_n_sim (Simulação):", p_n_sim, "\n")

# --- 3. Desvio absoluto entre p_n e p_n_TLC ---
desvio_TLC <- abs(p_n_exato - p_n_TLC)

#valor 3
cat("3. Desvio absoluto |p_n - p_n_TLC|:", desvio_TLC, "\n")

# --- 4. Desvio absoluto entre p_n e p_n_sim ---
desvio_sim <- abs(p_n_exato - p_n_sim)

#valor 4
cat("4. Desvio absoluto |p_n - p_n_sim|:", desvio_sim, "\n")

# --- 5. Quociente entre os desvios ---
quociente_desvios <- desvio_TLC / desvio_sim

# Arredondar o quociente para 4 casas decimais
quociente_arredondado <- round(quociente_desvios, 4)


#resposta final
cat("5. Quociente dos desvios (arredondado a 4 casas decimais):", quociente_arredondado, "\n")




### ------------------- PERGUNTA 7 ------------------- ###




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




### ------------------- PERGUNTA 8 ------------------- ###




# --- 1. Geração de Amostras e Determinação de Intervalos de Confiança ---

#fixar a semente
set.seed(1257)

#parametros
m <- 1800    # Número de amostras a gerar
n <- 18      # Dimensão de cada amostra
mu_verdadeiro <- 0.3 # Valor esperado verdadeiro da população (para verificar a cobertura)
sigma <- 0.5 # Desvio padrão populacional (conhecido)
gamma <- 0.97 # Nível de confiança desejado

#calcular alpha (nível de significância)
alpha <- 1 - gamma

#calcular o valor crítico Z_(alpha/2)
# qnorm(p) retorna o quantil para a probabilidade p da distribuição normal padrão
z_alpha_2 <- qnorm(1 - alpha/2)

#vetor lógico para armazenar se cada intervalo de confiança contém o mu_verdadeiro
contem_mu <- logical(m)

#loop para gerar 'm' amostras e calcular os ICs para cada uma
for (i in 1:m) {
  #gerar uma amostra de 'n' observações da distribuição normal especificada
  amostra <- rnorm(n, mean = mu_verdadeiro, sd = sigma)

  #calcular a média amostral para a amostra atual
  media_amostral <- mean(amostra)

  #calcular o erro padrão da média
  erro_padrao <- sigma / sqrt(n)

  #calcular o limite inferior do intervalo de confiança
  limite_inferior <- media_amostral - z_alpha_2 * erro_padrao

  #calcular o limite superior do intervalo de confiança
  limite_superior <- media_amostral + z_alpha_2 * erro_padrao

  #verificar se o intervalo de confiança atual contém o valor verdadeiro de mu
  #o valor deve ser maior ou igual ao limite inferior E menor ou igual ao limite superior
  contem_mu[i] <- (mu_verdadeiro >= limite_inferior && mu_verdadeiro <= limite_superior)
}

#1
cat("1. (Cálculos internos): Intervalos de confiança gerados para", m, "amostras.\n")

# --- 2. Obtenção da Proporção de Intervalos que Contêm o Valor Esperado ---

#soma do vetor lógico 'contem_mu' dá o número de TRUEs (intervalos que contêm mu)
proporcao_contem_mu <- sum(contem_mu) / m

#valor 2
cat("2. Proporção de intervalos de confiança que contêm o valor esperado (mu = 0.3):", proporcao_contem_mu, "\n")

# --- 3. Cálculo do Quociente entre a Proporção e o Nível de Confiança ---

#calcular o quociente
quociente <- proporcao_contem_mu / gamma

#arredondar o quociente para 4 casas decimais
quociente_arredondado <- round(quociente, 4)


#resposta final
cat("3. Quociente entre a proporção obtida e o nível de confiança (arredondado a 4 casas decimais):", quociente_arredondado, "\n")




### ------------------- PERGUNTA 9 ------------------- ###




#parametros
set.seed(3615) #fixar a semente
m <- 600       #número de amostras
n <- 20        #dimensão de cada amostra
mu0 <- 3       #valor de mu sob H0
mu1 <- 3.2     #valor de mu sob H1 (para geração das amostras)
alpha <- 0.09  #nível de significância

#--- 1. Estimativa de beta (beta_hat) via Simulação ---

#graus de liberdade para a distribuição qui-quadrado
df <- 2 * n

#calcular o valor crítico para o teste de hipóteses (sob H0)
#região de rejeição: T0 > qchisq(1 - alpha, df)
valor_critico_chi2_H0 <- qchisq(1 - alpha, df = df)

#contador para o número de vezes que H0 não é rejeitada (erro tipo II)
nao_rejeita_H0_count <- 0

#loop para gerar 'm' amostras e aplicar o teste
for (i in 1:m) {
  #gerar uma amostra de dimensão 'n' da distribuição exponencial com média mu1
  #distribuição exponencial em R (rexp) usa taxa (rate), que é 1/media
  amostra <- rexp(n, rate = 1 / mu1)

  #calcular a média amostral
  media_amostral <- mean(amostra)

  #calcular a estatística de teste T0 sob H0
  T0 <- (2 * n * media_amostral) / mu0

  #verificar se H0 é rejeitada ou não
  #não rejeita H0 se T0 <= valor_critico_chi2_H0
  if (T0 <= valor_critico_chi2_H0) {
    nao_rejeita_H0_count <- nao_rejeita_H0_count + 1
  }
}

#estimar beta (beta_hat) como a proporção de vezes que H0 não foi rejeitada
beta_hat <- nao_rejeita_H0_count / m

#valor 1
cat("Estimativa de beta (beta_hat) pela simulação:", beta_hat, "\n")

#--- 2. Cálculo da Probabilidade Teórica de beta (beta_teorico) ---

#primeiro, o valor crítico do teste para T0 é F_inv_chi2(1-alpha)
#sob H1, a variável T = 2n*X_bar / mu1 ~ chi2(2n)
#queremos P(T0 <= valor_critico_H0 | mu = mu1)
#o que é P( (2n*X_bar)/mu0 <= valor_critico_H0 | mu = mu1)
#equivalente a P( 2n*X_bar/mu1 <= (mu0/mu1) * valor_critico_H0 | mu = mu1)

#calcular o valor para a CDF da chi-quadrado sob H1
limite_para_pchisq <- (mu0 / mu1) * valor_critico_chi2_H0

#calcular beta teórico
beta_teorico <- pchisq(limite_para_pchisq, df = df)

#valor 2
cat("Probabilidade teórica de beta (beta_teorico):", beta_teorico, "\n")

#--- 3. Quociente de beta_hat e beta_teorico ---
quociente <- beta_hat / beta_teorico

#arredondar o resultado a 4 casas decimais
quociente_arredondado <- round(quociente, 4)

#resposta final
cat("Quociente (beta_hat / beta_teorico) arredondado a 4 casas decimais:", quociente_arredondado, "\n")




### ------------------- PERGUNTA 10 ------------------- ###




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