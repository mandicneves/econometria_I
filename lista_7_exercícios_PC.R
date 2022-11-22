require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

# CASO NÃO TENHA INSTALADO ESSAS BIBLIOTECAS É PRECISO FAZER A INSTALAÇÃO
install.packages("wooldridge") # APENAS SE NÃO TIVER INSTALADO
install.packages("tidyverse") # APENAS SE NÃO TIVER INSTALADO

#===============================================================================

#C1.i)

view(kielmc)

dados_filtrados = kielmc[which(kielmc$year == 1981), ] # filtra dados cuja coluna year == 1981
view(dados_filtrados)

# Dado que a presença do incinerador deprecia o preço das casas, espera-se que B1 > 0, ou seja, quanto maior B1, maior a distância entre a casa e o incinerador e portanto menor a interferência do incinerador no preço da casa

regressao_c1_i <- lm(log(price) ~ log(dist), data = dados_filtrados)
summary(regressao_c1_i)

# log(price) = 8,0472 + 0,3649*log(dist)
#              (0,6462)  (0,0658)
# n = 142  R² = 0,1803

# C1.ii)

regressao_c1_ii <- lm(log(price) ~ log(dist) + log(intst) + log(area) + 
                        log(land) + rooms + baths + age, data = dados_filtrados)
summary(regressao_c1_ii)

# log(price) = 7,5923 + 0,05539*log(dist)
#              (0,6417)  (0,05762)
# n = 142  R² = 0,7475

# Nesta nova regressão o valor t de B1 é não significativo, dado que outras variáveis que determinam a qualidade de uma casa foram acrescentados. Contudo, o significado de B1 não se alterou, ou seja, a proximidade ao incinerador não é desejada

# C1.iii)

regressao_c1_iii <- lm(log(price) ~ log(dist) + log(intst) + log(area) + 
                         log(land) + rooms + baths + age + (log(intst))^2, 
                       data = dados_filtrados)
summary(regressao_c1_iii)


# C1.iv)

regressao_c1_iv <- lm(log(price) ~ log(dist) + log(intst) + log(area) + 
                        log(land) + rooms + baths + age + 
                        (log(intst))^2 + (log(dist))^2, 
                      data = dados_filtrados)
summary(regressao_c1_iv)

#===============================================================================

# C2.i)

view(wage1)

regressao_c2_i <- lm(log(wage) ~ educ + exper + expersq, data = wage1)
summary(regressao_c2_i)

# log(wage) = 0,1280 + 0,0904(educ) + 0,0410(exper) - 0,0007(exper²)
#             (0,1060)  (0,0075)      (0,0052)        (0,0001)
# n = 526  R² = 0,3003

# C2.ii)

# A estatística t de exper² é -6,164 e o p-valor é basicamente zero, então exper² é estatísticamente significante ao nível de 1%

# C2.iii)

B2 <- 0.0410089
B3 <- -0.0007136
exp <- 4
Δexp <- 1
Δwage <- 100*(B2 + 2*B3*exp)*Δexp
Δwage

# Retorno para o quinto ano de experiência
# %Δ(wage) = 3,53%

exp <- 19
Δwage <- 100*(B2 + 2*B3*exp)*Δexp
Δwage

# Retorno para o vigésimo ano de experiência
# %Δ(wage) = 1.39%

# C2.iv)

exp <- B2/(2*B3)
exp

# Com aproximadamente 28,7 anos de experiência

wage1 %>% count(exper > exp * -1)

# Nessa amostra, 121 pessoas possuem mais do que 28,7 anos de experiência