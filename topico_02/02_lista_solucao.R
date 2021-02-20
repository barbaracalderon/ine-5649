# Exercício 1
#
# Os gráficos a seguir mostram a relação entre tempo de estudo (min/dia) vs.
# notas e número de faltas vs. notas. Com base nos gráficos e nos seus
# respectivos valores, pede-se:
#
# a) Coeficientes de correlação linear. Classifique a intensidade da
# correlação entre os pares de variáveis.
tempo_nota <- (0.80)^(1/2)
# tempo_nota = 0.894
falta_nota <- (0.91)^(1/2)
# falta_nota = 0.953
#
# b) Faça a interpretação do coeficiente angular dos modelos apresentados
# no gráfico.
#
# Em notas vs. tempo, o coeficiente angular (+1.30) é positivo, ou seja,
# crescente. Para cada unidade de tempo de estudo, aumenta-se 1,30 unidade
# de nota.
#
# Em falta vs. notas, o coeficiente angular (-3.39) é negativo, 
# decrescente, o que se verifica no gráfico. Para cada unidade de falta,
# diminui-se 3.39 unidade de nota.
# 
# c) Qual seria a nota esperada de um aluno que estudou 1h por dia?
#
Y <- -24.38+1.30*60
# Espera-se que ele tire a nota de Y = 53,62
#
# d) Qual seria a nota esperada de um aluno que faltou 7 vezes?
#
F <- 99.11-3.39*7
# A nota esperada é F = 75,38
#
# e) Qual das predições (c ou d) é mais eficiente? Justifique.
#
# Devido ao coeficiente de correlação maior, a predição d é mais eficiente.
#
#
# Exercício 2
# Os dados a seguir referem-se à massa muscular (Y) em relação à
# idade de uma pessoa (X).
Xi <- 1108
Yi <- 1530
XiYi <- 91964
Xi2 <- 70362
Yi2 <-133300
# a) Calcule a reta de mínimos quadrados. (Y = AX + B) ou (Y = B0+B1X)
B1 <- (XiYi-(18*(Xi/18)*(Yi/18)))/(Xi2-18*(Xi/18)^2)
B0 <- (Yi/18)-B1*(Xi/18)
# Reta de mínimos quadrados: Y = 148,19 - 1,02 * X
#
# b) Qual o valor do intercepto?
# O valor é 148,19
#
# c) Qual o valor do coeficiente angular? Interprete-o.
# O valor é -1,02, o que significa que é decrescente. Para cada unidade
# de X que cresce, Y diminui -1,02 unidades.
# 
# d) A relação entre X e Y é positiva ou negativa? Justifique.
# Negativa. Ver letra c.
#
# e) Uma nova pessoa de 65 anos tem sua massa muscular avaliada. Se
# considerarmos o modelo ajustado no item (a), quanto de massa muscular
# esperamos que essa pessoa tenha?
R <- 148.19 - 1.02*65
# Esperamos que tenha 81,89 de massa muscular.
#
# f) Qual a percentagem de variabilidade de massa muscular que é explicada
# pela variabilidade da idade das pessoas, por meio do modelo ajustado
# no item (a)?
# Coeficiente é R^2 = 0,70
#
#
# Exercício 3
# Um corretor de imóveis gostaria de estimar o valor (xR$10.000,00) de
# alguns apartamentos em função do tamanho da área privativa de cada
# imóvel (m^2) e da idade dos mesmos (anos). Para isto, ele ajustou dois
# modelos de regressão linear simples como apresentado a seguir. Pede-se:
#
# a) Determine os coeficientes de correlação linear entre "área privativa"
# vs. "valor do imóvel" e "idade do imóvel" vs. "valor do imóvel". Discuta
# sobre o tipo de relação encontrado por meio do coeficiente de correlação.
area_valor = 0.82^(1/2) # 0.9055
idade_valor = 0.05^(1/2) # 0.2236
# A área do imóvel incide mais fortemente sobre o preço do mesmo, do que
# a idade do imóvel sobre o preço.
#
# b) Faça a interpretação do coeficiente angular dos modelos apresentados
# no gráfico.
# No gráfico área-valor, o coeficiente angular é positivo: quanto maior
# a área do imóvel, maior o seu preço.
# No gráfico idade-valor, o coeficiente é negativo: quanto mais idade
# o imóvel tem, menor o seu preço.
#
# c) Qual seria o valor esperado de um imóvel com área privativa de
# 50m^2?
imovel_50 <- -31.65 + 1.27 * 50
imovel_50 * 10000
# O valor esperado é de R$318.500,00
#
# d) Qual seria o valor esperado de um imóvel construído há 6 meses?
imovel_6m <- 116.78 - 1.86 * 0.5
imovel_6m * 10000
# O valor esperado é de R$1.158.500,00
#
# e) Qual das predições (c ou d) é mais eficiente? Justifique.
# A predição por área (c) porque o coeficiente de correlação é maior.
# 0.9055 vs. 0.2236
#
#
# Exercício 4
# Um estudo foi desenvolvido para verificar o quanto o comprimento [...].
#
# a) Construa o diagrama de dispersão
comp_cabo <- c(8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14, 15)
taxa_falha <- c(0.79, 0.74, 1.10, 1.06, 1.41, 1.50, 1.82, 1.77, 2.28, 2.16, 2.53, 2.57, 2.96, 2.86, 3.34)
dados <- data.frame(comp_cabo, taxa_falha)
library(ggplot2)
ggplot() + 
  geom_point(data = dados,
             aes(x = comp_cabo, y = taxa_falha)) +
  labs(x = 'Comprimento do Cabo (m)', y = 'Taxa de Falha') +
  theme_minimal()
#
# b) Encontre a reta de mínimos quadrados, esboçando-a no gráfico de
# dispersão obtido no item (a).
modelo <- lm(taxa_falha ~ comp_cabo)
coef(modelo)
# B0 = -2.193 e B1 = 0.365
# A reta de mínimos é Y = -2.193 + 0.365 * X
ggplot() + 
  geom_point(data = dados,
             aes(x = comp_cabo, y = taxa_falha)) +
  geom_smooth(data = dados, 
              aes(x = comp_cabo, y = taxa_falha),
              method = 'lm', se=F) +
  labs(x = 'Comprimento do Cabo (m)', y = 'Taxa de Falha') +
  theme_minimal()
#
# c) Interprete o valor encontrado para o coeficiente angular.
# Para cada uma unidade de cabo, a taxa de falha aumenta em 0.365 unidades
#
# d) Encontre as estimativas de variância e desvio-padrão dos erros.
# S^2 e S
summary(modelo) # Encontrar "Residual Standard Error" aqui e elevar ao 
# quadrado para ter S^2
S2 <- 0.04799^2 # S2 = 0.002303
S <- S2^(1/2) # S = 0.04799
# R^2 = 0.9969
#
# e) Considerando a reta de mínimos quadrados calculada, estime qual será a
# taxa de falha média para um cabo de 8,5m.
falha_8m <- -2.193 + 0.365 * 8.5
# A taxa de falha média é de 0.9095 
#
# f) Qual o coeficiente de determinação? Interprete o resultado. (R^2)
# Só ver dentro do summary(modelo) penúltima linha "Multiple R-squadred"
# que é de R^2 = 0.9969. Ou seja, 99,69% da variabilidade da taxa de falha
# pode ser explicada pela variabilidade do comprimento do cabo.
#
#
# Exercício 5
# Um estudo foi desenvolvido para verificar o quanto o tamanho do imóvel [...].
# 
# a) Escreva a reta de mínimos quadrados estimado para o problema.
# Y = 226.19 + 8.6471 * X
#
# b) Interprete o valor encontrado para o coeficiente angular.
# A cada aumento na unidade de tamanho, o aluguel aumenta em 8.6471 vezes.
#
# c) Qual o valor esperado de um imóvel de 100m^2?
valor_100 <- 226.19 + 8.6471 * 100
# O valor esperado é  de 1090.9 ou $1090.9
#
# d) Qual o percentual de variabilidade do valor do aluguel do imóvel que é 
# explicado pela variabilidade do tamanho do imóvel, por meio do modelo de
# regressão linear ajustado?
# Em média 22.62% da variabilidade pode ser explicada pela variabilidade
# do tamanho do imóvel por meio do modelo de regressão linear.
#
# e) Baseado no percentual obtido em d, quais outras variáveis você acredita
# que poderia influenciar o valor do aluguel de um imóvel.
# -