require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

# CASO NÃO TENHA INSTALADO ESSAS BIBLIOTECAS É PRECISO FAZER A INSTALAÇÃO
install.packages("wooldridge") # APENAS SE NÃO TIVER INSTALADO
install.packages("tidyverse") # APENAS SE NÃO TIVER INSTALADO

#C4)

view(wage1)

ggplot(data = wage1) + geom_point(mapping = aes(x = educ, y = lwage))

#C5)

regressao_c5 <- lm(lwage ~ educ, data = wage1)
summary(regressao_c5)

# lwage = 0,5837 + 0,0827(educ)
# n = 525 R² = 0,1858