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