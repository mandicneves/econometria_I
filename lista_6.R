require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

# CASO NÃO TENHA INSTALADO ESSAS BIBLIOTECAS É PRECISO FAZER A INSTALAÇÃO
install.packages("wooldridge") # APENAS SE NÃO TIVER INSTALADO
install.packages("tidyverse") # APENAS SE NÃO TIVER INSTALADO

#C1.i)

view(vote1) # Visualizar dados do 'vote1'

# Beta1 mostra a variação em pontos percentuais em 'voteA' dado a variação em pontos percentuais em 'expendA', considerando todos os outros valores fixados

#C1.ii)

# H0: B1 = -B2 ou H0: B1 + B2 = 0

#C1.iii)

regressao_c1_iii <- lm(voteA ~ log(expendA) + log(expendB) + prtystrA, data = vote1)
summary(regressao_c1_iii)

# voteA = 45,078 + 6,083log(expendA) - 6,615log(expendB) + 0,151prtystrA
#         (3,926)   (0,382)             (0,378)             (0,062)
# n = 173    R² = 0,7926

# Tanto 'expendaA' quanto 'expendB' afetam fortemente 'voteA'
# Para testar a hipóte do item (ii) é preciso do EP(B1 + B2)

#C1.iv)

# Para calcular EP(B1 + B2), definimos θ = B1 + B2, então B1 = θ - B2
# Substituindo em voteA
# voteA = B0 + (θ - B2)*log(expendaA) + B2*log(expendB) + B3*prtystrA
# Abrindo a expressão
# voteA = B0 + θ*log(expendA) - B2*log(expendA) + B2*log(expendB) + B3*prtystrA
# Colocando B2 em evidência
# voteA = B0 + θ*log(expendA) + B2*(log(expendB) - log(expendA)) + B3*prtystrA

regressao_c1_iv <- lm(voteA ~ log(expendA) + I(log(expendB) - log(expendA)) + prtystrA, data = vote1)
summary(regressao_c1_iv)

# A estatística t encontrada = -0,998 é muito baixa. Portanto não rejeitamos a hipóte de que 'expendA' e 'expendB' se anulam

# ==========================================================================

#C8.i)

view(k401ksubs) # Visualizar dados do '401ksubs'

k401ksubs %>% count(fsize == 1)

# Existem 2017 residências com apenas uma pessoa

#C8.ii)

dados_filtrados <- k401ksubs[which(k401ksubs$fsize == 1),] # filtra dados cuja coluna fsize == 1

regressao_c8_ii <- lm(nettfa ~ inc + age, data = dados_filtrados)
summary(regressao_c2_ii)

# nettfa = -43,039 + 0,799inc + 0,842age
#           (4,080)   (0,059)    (0,092)
# n = 2017  R² = 0,1193

#C8.iii)

# O valor do intercepto é negativo, ou seja, as famílias de uma só pessoa, nascem com ativos financeiros líquidos negativos

#C8.iv)

# H0: B2 = 1
# H1: B2 < 1
# alpha = 1%

# Para encontrar o p-valor desse teste precisamos do valor t de B2

t_B2 <- (0.842 - 1)/0.092
t_B2

p_valor <- pnorm(t_B2)
p_valor

p_valor < 0.02 # pnorm calcula a densidade de probabilidade de um valor 't'

# Não rejeitamos a hipótese nula, dado que o p_valor encontrado é maior que o nível de significância

#C8.v)

regressao_c8_v <- lm(nettfa ~ inc, data = dados_filtrados)
summary(regressao_c8_v)

# O coeficiente estimado nas duas regressões são parecidos, dado que a variável explicativa renda anual não possui multicolinearidade com as outras variáveis
