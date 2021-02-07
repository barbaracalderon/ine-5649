# leitura da variável x
idade <- c(56, 42, 72, 36, 47, 55, 49, 38, 42, 68, 60, 63)
# leitura da variável y
pressao <- c(147, 125, 160, 118, 128, 150, 145, 115, 140, 152, 155, 149)
plot(idade, pressao) # gráfico de dispersão padrão

install.packages('ggplot2') # instalação do pacote ggplot2
library(ggplot2) # carregar pacote ggplot2
dados <- data.frame(idade, pressao) # construção de um data frame
# gráfico de dispersão pelo ggplot
ggplot(data = dados) + 
  geom_point(aes(x = idade, y = pressao)) 

cor(idade, pressao) # cálculo do Coeficiente de correlação linear de Pearson

#===== Intervalo de confiança e teste de hipótese para \rho
# leitura da variável x
idade <- c(56, 42, 72, 36, 47, 55, 49, 38, 42, 68, 60, 63)
# leitura da variável y
pressao <- c(147, 125, 160, 118, 128, 150, 145, 115, 140, 152, 155, 149)

# Cálculo passo a passo
r <- cor(idade, pressao) # coeficiente de correlação
n <- length(idade) # número de observações
alpha <- 0.05 # nível de significância

est_teste <- r*sqrt(n-2)/sqrt(1-r^2); est_teste # estatística do teste
val_critico <- qt(alpha/2, n-2, lower.tail = FALSE) # valor crítico

# cálculo do p-valor (teste bilateral)
p_valor <- 2 * pt(est_teste, n-2, lower.tail = FALSE)

# Coeficiente de correlação e inferência
cor.test(idade, pressao)
