#Gerar um modelo linear aleatorio para aula de Regressão Linear

#Gera variável independente, 100 observações
var.ind <- rnorm(100)

#Gera erros, 100 observações (mude o denominador para alterar a intensidade da variabilidade)
erro <- rnorm(100)/1

#Gera constante
const. <- rnorm(1)

#Gera coeficiente
coef. <- rnorm(1)

#Calcula variável resposta
var.resp <- coef. + const. * var.ind + erro

#Une as variáveis em um df
dado <- data.frame(var.resp, var.ind)

#Gera o modelo e resume
modelo <- lm(var.resp ~ var.ind, dado)
summary(modelo)

#Plota o modelo
ggplot(dado, aes(var.ind, var.resp)) + geom_point() + geom_smooth(method = lm)


