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