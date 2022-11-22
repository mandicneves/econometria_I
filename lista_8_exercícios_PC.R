require(wooldridge) # Importando biblioteca com dados do livro
require(tidyverse) # Importando biblitoeca para visualização dos dados

# CASO NÃO TENHA INSTALADO ESSAS BIBLIOTECAS É PRECISO FAZER A INSTALAÇÃO
install.packages("wooldridge") # APENAS SE NÃO TIVER INSTALADO
install.packages("tidyverse") # APENAS SE NÃO TIVER INSTALADO

#===============================================================================

# C3-capitulo-5

view(bwght)

# Filtrando base de dados para eliminar os valores 'NA'
primeiro_filtro <- bwght[which(bwght$motheduc != 'NA'), ]
dados_filtrados <- primeiro_filtro[which(primeiro_filtro$fatheduc != 'NA'), ]

view(dados_filtrados)

regressao_c3_i <- lm(bwght ~ cigs + parity + faminc, data = dados_filtrados)
summary(regressao_c3_i)

# Obtendo resíduos da primeira regressão
residuos <- residuals(regressao_c3_i)

regressao_c3_ii <- lm(residuos ~ cigs + parity + faminc + motheduc + fatheduc,
                      data = dados_filtrados)
summary(regressao_c3_ii)

R2 <- 0.00242
n <- 1191
X2 <- R2 * n
X2

#===============================================================================

# C2-capitulo-7

view(wage2)

#i)

regressao_c2_i <- lm(log(wage) ~ educ + exper + tenure + married + black
                     + south + urban, data = wage2)
summary(regressao_c2_i)

# log(wage) = 5,3955 + 0,0654educ + 0,014exper + 0,0117tenure + 0,1994married - 0,1883black - 0,0909south + 0,1839urban
#             (0.1132) (0.0062)    (0.0031)     (0.0024)         (0.0390)       (0.0376)       (0.0262)     (0.0269)
# n = 935  R² = 0,2526

# Negros recebem 18,83% menos que não negros. Essa diferença é bastante significativa, dado t = -5.00

#ii)

df <- wage2
df$expersq <- wage2$exper ^ 2
df$tenuresq <- wage2$tenure ^ 2

view(df)

regressao_c2_ii <- lm(log(wage) ~ educ + exper + tenure + married + black
                      + south + urban + expersq + tenuresq, data = df)
summary(regressao_c2_ii)

k <- regressao_c2_i$df.residual
q <- 2
r_ur <- 0.2526
r_r <- 0.255

f <- (k/q)*((r_ur - r_r)/(1 - r_ur))
f

a <- pt(f, df = k)
b <- pt(f, df = q)
p_valor <- a + b

# Dado que o p valor é maior que 0,20, exper² e tenure² são estatísticamente insignificantes

#iii)

regressao_c2_iii <- lm(log(wage) ~ educ + exper + tenure + married + black
                     + south + urban + I(educ * black), data = wage2)
summary(regressao_c2_iii)

# B(educ*black) = - 0.022624
# t_B(educ*black)  = - 1.121
# O retorno de outro ano de educação é aproximadamente 2,3% menor para homens negros. Contudo a estatística t é - 1.121, valor baixo para uma significância de 10%

#iv)

df <- wage2
df$marriednonblack <- wage2$married & !wage2$black
df$marriedblack <- wage2$married & wage2$black
df$singleblack <- !wage2$married & wage2$black
df$singlenonblack <- !wage2$married & !wage2$black



regressao_c2_iv <- lm(log(wage) ~ educ + exper + tenure+ south + urban +
                        marriednonblack + singleblack + marriedblack +
                        singlenonblack, 
                      data = df)
summary(regressao_c2_iv)

# log(wage) = 5.4037 + 0.0654educ + 0.0141exper + 0.0116tenure - 0.0919south + 0.1843urban + 0.1889(casado-não-negro) - 0.2408(solteiro-negro) + 0.0094(casado-negro)
#            (0.1141)  (0.0062)     (0.0031)      (0.0024)       (0.0263)      (0.0269)      (0.0428)                   (0.0960)                 (0.0560)
# n = 935  R² = 0.2528

marriedblack <- 0.009448422
marriednonblack <- 0.188914701

diferencial <- marriedblack - marriednonblack
diferencial

# Casados negros ganham 18% menos que casados não negros
