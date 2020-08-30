# Title     : lib.R
# Objective : Biblioteca de funções para a lista 4 - Agrupamento
# Created by: Elmon, Izabella, Jordan, Lucas
# Created on: 8/5/20

# verifica se pacotes necessários estão instalados
packages <- c('sparsebnUtils', 'mvtnorm', 'zeallot', 'caret')
lapply(packages,
       function(x) {
         if (!require(x, character.only = TRUE)) {
           install.packages(x, dependencies = TRUE, repos = 'http://cran.us.r-project.org')
           library(x, character.only = TRUE)
         }
       }
)

#' ###
#' @title Função para calcular a distância euclidiana
#'
#' @description `euc.dist()`
#'
#' @details Recebe dois vetores e retorna os escalares referentes à distância euclideana entre seus elementos
#'
#' @param `x1, x2` Vetores a terem distância euclidiana calculada
#' ###
euc.dist <- function(x1, x2) sum((x1 - x2)^2)

#' ###
#' @title Função k-means
#'
#' @description `kMeans1()`
#'
#' @details Algoritmo de agrupamento pelo método k-means
#'
#' @param dados Observações a serem agrupadas
#' @param k Número de grupos
#' ###
k.means1 <- function(dados, k = 2) {
  # rótulos
  rotulo <- 1:k
  rownames(dados)[nrow(dados)] <- 1

  # centróides aleatórios
  for (i in seq_len(nrow(dados))) {
    rownames(dados)[i] <- sample(rotulo, 1)
  }

  centroids <- colMeans(dados[rownames(dados) == 1,])

  for (j in 2:k) {
    centroids <- rbind(centroids, colMeans(dados[rownames(dados) == j,]))
  }

  rownames(centroids) <- 1:k #  identifica o centróide de cada grupo

  for (i in seq_len(nrow(dados))) {
    distancias <- NULL
    for (j in 1:k) {
      distancias[j] <- euc.dist(dados[i,], centroids[j,])
    }
    names(distancias) <- 1:k
    rownames(dados)[i] <- as.numeric(names(distancias[distancias == min(distancias)]))

    # recalcula as médias
    centroids <- colMeans(dados[rownames(dados) == 1,])

    for (z in 2:k) {
      centroids <- rbind(centroids, colMeans(dados[rownames(dados) == z,]))
    }
  }

  #  centróides
  return(list(centroides = centroids,
              grupo1 = dados[rownames(dados) == 1,],
              grupo2 = dados[rownames(dados) == 2,],
              grupo3 = dados[rownames(dados) == 3,],
              clusters = as.numeric(rownames(dados))))
}

#' ###
#' @title Gera parâmetros (médias, covariâncias, observações) iniciais
#'
#' @description `gera.params.iniciais.2dim()`
#'
#' @details Função para gerar parâmetros iniciais para exercício
#'
#' @param dim Dimensão das matrizes de covariância aleatórias (quadradas)
#' @param obs Número de observações
#' @param seed Semente de pseudo-aleateriedade
#' ###
gera.params.iniciais.2dim <- function(dim = 2, obs = 40, seed = 1234, mean1, mean2, covar1, covar2) {
  # pseudo-aleatoriedade
  set.seed(seed)

  # médias aleatórias
  mu1 <- if (missing(mean1)) runif(dim) else mean1
  mu2 <- if (missing(mean2)) runif(dim) else mean2

  # covariâncias aleatórias
  Sigma1 <- if (missing(covar1)) random.spd(dim) else covar1
  Sigma2 <- if (missing(covar2)) random.spd(dim) else covar2

  # normais aleatórias
  mvnorm1 <- rmvnorm(obs, mean = mu1, sigma = Sigma1)
  mvnorm2 <- rmvnorm(obs, mean = mu2, sigma = Sigma2)

  return(list(mu1, mu2, Sigma1, Sigma2, mvnorm1, mvnorm2))
}

#' ###
#' @title Gera agrupamentos
#'
#' @description `gera.agrupamentos()`
#'
#' @details Função para gerar agrupamentos para duas amostras fornecidas
#'
#' @param `sample1, sample2` Amostras a serem atribuídas a grupos
#' ###
gera.agrupamentos <- function(sample1, sample2) {
  n <- if (length(sample1) != length(sample2)) stop('Amostras com tamanhos diferentes') else nrow(sample1)

  amostra <- rbind(sample1, sample2)

  grupo1 <- array(1, dim = c(n, 1))
  grupo2 <- array(2, dim = c(n, 1))
  grupo <- rbind(grupo1, grupo2)

  amostra2 <- cbind(amostra, grupo)
  amostra3 <- amostra2[sample(1:(2 * n), 2 * n),]

  return(amostra3)
}

#' ###
#' @title Gera matriz de confusão
#'
#' @description `gera.taxa.erros()`
#'
#' @details Função para gerar matriz de confusão para análise de taxa de erros na classificação do k-means
#'
#' @param `amostra.original, amostra.validacao` Amostra original e amostra de validação
#' ###
gera.taxa.erros <- function(amostra.original, amostra.validacao) {
  if (missing(amostra.original) || missing(amostra.validacao))
    stop('Amostras vazias não são permitidas')

  return(confusionMatrix(as.factor(amostra.original), as.factor(amostra.validacao)))
}
