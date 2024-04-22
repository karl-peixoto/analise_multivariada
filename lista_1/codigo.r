library(tidyverse)
setwd('C:/Users/kmenezes/Desktop/OneDrive - unb.br/7 semestre/analise_multivariada/lista_1')

## QUESTÃO 3
#Definindo função para calcular volume do hipercubo
cal_cubo <- function(a, r) {
  return((2 * a)**r)
}

#Definindo função para calcular volume da hiperesfera
calc_esfera <- function(a,r){
  return(((a**r)*(pi**(r/2)))/(gamma((r/2)+1)))
}

#Definindo função para calcular proporção de volume não ocupado
proporcao <- function(v_cubo, v_esfera){
  return((v_cubo - v_esfera)/v_cubo)
}


simulacao <- function(n_dim){
  #Construindo vetor de resultados
  resultados <- c()
  dimensao <- c()

  #Fazendo simulação para diferentes valores de raio e dimensão
  for(r in seq(1,n_dim)){
    a <- 1
    v_cubo <- cal_cubo(a,r)
    v_esfera <- calc_esfera(a, r)
    prop <- proporcao(v_cubo, v_esfera)
    resultados <- c(resultados, prop)
    dimensao <- c(dimensao, r)
  }

  #Organizando resultados no data Frame
  dados <- data.frame(dimensoes = dimensao, prop = resultados)
  return(dados)
}

dados <- simulacao(15)
#Plotando resultados
ggplot(dados, aes(x = dimensoes, y = prop)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:max(dados$dimensoes), labels = 1:max(dados$dimensoes)) +
  labs(x = "Dimensão r",
       y = "Proporção de volume")


view(dados)  
ggsave('relacao_volume.pdf', width = 15, height = 9, units = 'cm')


#Questão 4
x_1 <- c(3, 4, 2, 6, 8, 2, 5)
x_2 <- c(5, 5.5, 4, 7, 10, 5, 7.5)
mean(x_1)
mean(x_2)
var(x_1)
var(x_2)
cov(x_1, x_2)


#Questão 5
x_1 <- c(1, 2, 3, 3, 4, 5, 6, 8, 9, 11)
x_2 <- c(18.95, 19 , 17.95, 15.54, 14, 12.95, 8.94, 7.49, 6, 3.99)
ggplot() +
  geom_point(aes(x = x_1, y = x_2)) +
  scale_x_continuous(breaks = 1:12, labels = 1:12) +
  scale_y_continuous(breaks = seq(0, 20, 2), labels = seq(0, 20, 2)) +
  labs(x = "Valor X_1",
       y = "Valor X_2")

ggsave('questao_5.pdf', width = 15, height = 9, units = 'cm')

mean(x_1)
mean(x_2)
var(x_1)
var(x_2)
cov(x_1, x_2)
cor(x_1, x_2)


## QUESTÃO 6
dados <- read.csv('dados/questao_1_6.csv')
View(dados)


#