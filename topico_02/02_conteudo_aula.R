library(ggplot2) # carregar biblioteca

# leitura da variável x
idade <- c(56, 42, 72, 36, 47, 55, 49, 38, 42, 68, 60, 63)
# leitura da variável y
pressao <- c(147, 125, 160, 118, 128, 150, 145, 115, 140, 152, 155, 149)
# construção do data frame
dados <- data.frame(idade, pressao)

# Construção do diagrama de dispersão
ggplot() +  #função principal do ggplot2
  geom_point(data = dados,  #geom_point: gráfico de dispersão
             aes(x = idade, y = pressao)) + #indicação das variáveis em estudo
  labs(x = 'Idade', y = 'Pressão') + #nomeação dos eixos
  theme_minimal() #mudando tema

# modelo de regressão linear simples
modelo <- lm(pressao ~ idade) # var. dependente ~ var. independente

coef(modelo) # coeficientes do modelo

ggplot() + 
  geom_point(data = dados,
             aes(x = idade, y = pressao))+
  geom_smooth(data = dados, # Esboço da reta de mínimos quadrados
              aes(x = idade, y = pressao),
              method = 'lm', se=F) +
  labs(x = 'Idade', y = 'Pressão') + 
  theme_minimal()

g1 <- ggplot(data = dados, # definindo data e variáveis na função principal
             aes(x = idade, y = pressao)) + 
  geom_point() + 
  labs(x = 'Idade', y = 'Pressão') + 
  theme_minimal()
g1

g1 + geom_smooth(method = 'lm', se = F) 


#====================
library(ggplot2)
massa <- c(82,91,100,68,87,73,78,80,65,84)
idade <- c(71,64,43,67,56,73,68,56,76,65)
dados <- data.frame(massa, idade)

g1 <- ggplot(data=dados, aes(x=idade,y=massa))+
  geom_point(); g1

modelo <- lm(massa ~ idade)
coef(modelo)

g1 + geom_smooth(method = 'lm', se = F)

summary(modelo) # resumo do modelo
# verificar desvio padrão do erro: "Residual standard error"
# verificar R^2: "Multiple R-squared"
