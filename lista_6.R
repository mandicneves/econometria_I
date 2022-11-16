require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

#C1.i)

view(vote1)

# Beta1 mostra a variação em pontos percentuais em 'voteA' dado a variação em pontos percentuais em 'expendA', considerando todos os outros valores fixados

#C1.ii)

# H0: B1 = -B2 ou H0: B1 + B2 = 0

#C1.iii)

regressao_c1_iii <- lm(voteA ~ log(expendA) + log(expendB) + prtystrA, data = vote1)
summary(regressao_c1_iii)

# voteA = 45,078 + 6,083log(expendA) - 6,615log(expendB) + 0,151prtystrA
#         (3,926)   (0,382)             (0,378)             (0,062)
# n = 173    R² = 0,792

# Tanto 'expendaA' quanto 'expendB' afetam fortemente 'voteA'
# Para testar a hipóte do item (ii) é preciso do EP(B1 + B2)

#C1.iv)

# Para calcular EP(B1 + B2), definimos θ = B1 + B2, então B1 = θ - B2
# Substituindo em voteA
# voteA = B0 + (θ - B2)*log(expendaA) + B2*log(expendB) + B3*prtystrA
# Abrindo a expressão
# voteA = B0 + θ*log(expendA) - B2*log(expendA) + B2*log(expendB) + B3*prtystrA
# Coloando B2 em evidência
# voteA = B0 + θ*log(expendA) + B2*(log(expendB) - log(expendA)) + B3*prtystrA

regressao_c1_iv <- lm(voteA ~ log(expendA) + I(log(expendB) - log(expendA)) + prtystrA, data = vote1)
summary(regressao_c1_iv)

# A estatística t encontrada = -0,998 é muito baixa. Portanto não rejeitamos a hipóte de que 'expendA' e 'expendB' se anulam
