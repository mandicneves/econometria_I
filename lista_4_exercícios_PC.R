require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

# CASO NÃO TENHA INSTALADO ESSAS BIBLIOTECAS É PRECISO FAZER A INSTALAÇÃO
install.packages("wooldridge") # APENAS SE NÃO TIVER INSTALADO
install.packages("tidyverse") # APENAS SE NÃO TIVER INSTALADO

#C2.i)

view(ceosal2)

salario_medio <- mean(ceosal2$salary)
salario_medio

# salário médio $ 865,86

permanencia_media <- mean(ceosal2$ceoten)
permanencia_media

# permanência média 7,95 anos

#C2.ii)

ceos_no_primeiro_ano <- ceosal2 %>% count(ceoten == 0)

# 5 CEO no primeiro ano

permanencia_mais_longa <- max(ceosal2$ceoten)

# 37 anos é a permanência mais longa

#C2.iii)

regressao_c2_iii <- lm(log(salary) ~ ceoten, data = ceosal2)
summary(regressao_c3_iii)

# log(salary) = 6,5054 + 0,0097(ceoten)
# n = 177  R² = 0,0131

#=============================================================================


#C3.i)

view(sleep75)

regressao_c3_i <- lm(sleep ~ totwrk, data = sleep75)
summary(regressao_c3_i)

# sleep = 3586,3769 - 0,1507(totwrk)
# n = 706  R² = 0,1033

# O intercepto mostra quantos minutos um indivíduo da amostra dorme sem ter trabalhado nenhum minuto

#C3.ii)

B0 <- 3586.37695
B1 <- 0.15075

duas_horas_totwrk <- B0 - (B1*120)
duas_horas_totwrk

variacao_sleep <- 100*(B0 - duas_horas_totwrk)/B0
variacao_sleep

# A variação será pequena, apenas de 0,5%. Portanto o aumento de totwrk em duas horas não afeta sleep significativamente
