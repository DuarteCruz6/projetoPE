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