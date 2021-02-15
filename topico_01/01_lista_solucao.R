# Exercício 03
# Uma rede de loja de vendas por atacado quer avaliar o desempenho de suas
# filiais e, de quebra, verificar a viabilidade de implantar uma nova  loja
# em Joinville, SC. Produziu a tabela a seguir, relacionando o número de
# clientes com as vendas em milhares de reais em um determinado mês.
#
# Construa um diagrama de dispersão e calcule o coeficiente de correlação
# linear de Pearson.
# 
# leitura da variável x
clientes <- c(423, 898, 1095, 1001, 597, 1200, 862, 1300, 845, 440, 922, 620, 876, 745, 1345, 865, 1170, 692, 955, 913, 845, 1004, 1003, 1200, 712)
# leitura da variável y
vendas <- c(88, 192, 196, 191, 100, 240, 169, 240, 157, 120, 160, 135, 155, 141, 250, 172, 203, 138, 182, 177, 164, 189, 208, 201, 118)
plot(clientes, vendas) # gráfico de dispersão padrão
cor(clientes, vendas)

# Exercício 06
# Considere as notas da prova do vestibular de matemática e na disciplina
# de Cálculo. Estas variáveis foram observadas em 20 alunos, ao final do
# primeiro período letivo de um curso de engenharia. Os dados são apresentados.
#
# Construa um diagrama de dispersão
vestibular <- c(39, 57, 34, 40, 43, 35, 80, 64, 75, 30, 47, 52, 70, 21, 28, 32, 65, 47, 28, 67)
calculo <- c(65, 92, 56, 70, 78, 50, 90, 82, 98, 50, 89, 75, 50, 52, 73, 58, 88, 71, 52, 88)
plot(vestibular, calculo)
# Calcule a correlação  entre nota do vestibular e cálculo
cor(vestibular, calculo)
#
# Coeficiente de correlação e inferência
cor.test(vestibular, calculo)
# Teste
x <- c(0, 2, 4, 6, 8, 10)
y <- c(30, 26, 20, 16, 12, 10)
cor(x, y)
