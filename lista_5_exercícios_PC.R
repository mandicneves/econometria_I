require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

# CASO NÃO TENHA INSTALADO ESSAS BIBLIOTECAS É PRECISO FAZER A INSTALAÇÃO
install.packages("wooldridge") # APENAS SE NÃO TIVER INSTALADO
install.packages("tidyverse") # APENAS SE NÃO TIVER INSTALADO

#===============================================================================

#C5)

view(wage1)

regressao_c5_i <- lm(educ ~ exper + tenure, data = wage1)
summary(regressao_c5_i)


r1 <- residuals(regressao_c5_i)
r1

regressao_c5_ii <- lm(lwage ~ r1, data = wage1)
summary(regressao_c5_ii)

# lwage = 1,6232 + 0,092(r1)
#         (0,0206)  (0,0078)

regressao_c5_iii <- lm(lwage ~ educ + exper + tenure, data = wage1)
summary(regressao_c5_iii)

# lwage = 0,2843 + 0,092(educ)
#         (0,1041)   (0,0073)

# Os coeficientes de educ na regressao iii e de r1 na regressao ii são idênticos