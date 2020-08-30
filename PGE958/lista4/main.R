# Title     : main.R
# Objective : Script principal da lista 4 - Agrupamento
# Created by: Elmon, Izabella, Jordan, Lucas
# Created on: 8/5/20

# Remove objetos carregados no ambiente
rm(list = objects())

# Carrega arquivo com biblioteca das funções a serem utilizadas
source('lib.R')

# ###
# programa k-means
# ###

n <- 40   # tamanho das amostras
p <- 2    # dimensão das matrizes de covariância

# ###
# cenário 1:  Vetores de médias mu1 = (1, 1) e mu2 = (0, 0) e matrizes positivas definidas
#             aleatórias, amostras com tamanho padrão de 40 observações
# ###

# gera médias, covariâncias e normais multivariadas pseudo-aleatórias
c(mu1, mu2, sig1, sig2, x1, x2) %<-% gera.params.iniciais.2dim(dim = 2, seed = 5785, mean1 = c(1, 1), mean2 = c(0, 0))

# gráfico das amostras sem grupos
plot(rbind(x1, x2), main = "Amostras sem grupos")

# geração dos grupos
grupos <- gera.agrupamentos(x1, x2)

# gráfico dos grupos antes do k-means
plot(grupos, col = grupos[, 3] + 1, lwd = 2, main = "Grupos originais")

# redefinindo os grupos
km.grupos <- k.means1(grupos[, 1:2])

# gráfico dos conjuntos reagrupados
plot(grupos[, 1:2], col = km.grupos$cluster + 1, lwd = 2, main = "Grupos com k-means")

# gera análise de taxa de erros
gera.taxa.erros(grupos[, 3], km.grupos$clusters)

# ###
# cenário 2:  Vetores de médias e matrizes positivas definidas aleatórias,
#             amostras com tamanho padrão de 40 observações
# ###

# gera médias, covariâncias e normais multivariadas pseudo-aleatórias
c(mu1, mu2, sig1, sig2, x1, x2) %<-% gera.params.iniciais.2dim(dim = 2, seed = 5785)

# gráfico das amostras sem grupos
plot(rbind(x1, x2), main = "Amostras sem grupos")

# geração dos grupos
grupos <- gera.agrupamentos(x1, x2)

# gráfico dos grupos antes do k-means
plot(grupos, col = grupos[, 3] + 1, lwd = 2, main = "Grupos originais")

# redefinindo os grupos
km.grupos <- k.means1(grupos[, 1:2])

# gráfico dos conjuntos reagrupados
plot(grupos[, 1:2], col = km.grupos$cluster + 1, lwd = 2, main = "Grupos com k-means")

# gera análise de taxa de erros
gera.taxa.erros(grupos[, 3], km.grupos$clusters)

# ###
# cenário 3:  setup do cenário 2, exceto pela semente de pseudo-aleatorização
# ###

# gera médias, covariâncias e normais multivariadas pseudo-aleatórias
c(mu1, mu2, sig1, sig2, x1, x2) %<-% gera.params.iniciais.2dim(dim = 2, seed = 999)

# gráfico das amostras sem grupos
plot(rbind(x1, x2), main = "Amostras sem grupos")

# geração dos grupos
grupos <- gera.agrupamentos(x1, x2)

# gráfico dos grupos antes do k-means
plot(grupos, col = grupos[, 3] + 1, lwd = 2, main = "Grupos originais")

# redefinindo os grupos
km.grupos <- k.means1(grupos[, 1:2])

# gráfico dos conjuntos reagrupados
plot(grupos[, 1:2], col = km.grupos$cluster + 1, lwd = 2, main = "Grupos com k-means")

# gera análise de taxa de erros
gera.taxa.erros(grupos[, 3], km.grupos$clusters)
