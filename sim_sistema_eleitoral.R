## simulação eleição

## 1:n eleitoes
## i:k candidatos num distrito
## eleitor adquire info sobre candidato até ficar próximo enough
## adquirir info tem custo 1 por cada unidade de info
## info é sujeita a ruído
## eleitor vota no mais próximo, sujeito a não gastar mais que c para adquirir info
## eleitor tem priori sobre os candidatos
## dois tipos de eleitores (informado sobre alguns candidatos, não-informado sobre todos)

## eleitor do tipo informado (k de knowlageble, e n de non-know..)
# candidato j faz uma unidade propaganda, 
# e com prob p_j eleitor informado vê a propagand do cand j
# onde candidato se localiza
# eleitor atualiza opinião (mas tem ruído)
# espaço unidimensional (quão próximo está do eleitor)


## fun gera eleitores
gera_eleitores < function(n=5000) {
  library(MASS)
  # Simulate bivariate normal data
  mu <- c(0,0)                         # Mean
  Sigma <- matrix(c(.8, .3, .3, .8), 2)  # Covariance matrix
  eleitores <- mvrnorm(n, mu = mu, Sigma = Sigma )  # from Mass package
  eleitores <- as.data.frame(eleitores)
}

## fun gera candidatos
gera_candidatos <- function(k=100) {
  library(MASS)
  # Simulate bivariate normal data
  mu <- c(0,0)
  Sigma_cand <- matrix(c(1, .6, .6, 1), 2)
  candidatos <- mvrnorm(k, mu = mu, Sigma = Sigma_cand )  # from Mass package
  candidatos <- as.data.frame(candidatos)
}

# plataforma do honesto
plat_honesto <- function() {
  candidato
}

gera_plataformas <- function(k=100, prop.tipos = rep(.25, 4)) {
  plat <- matrix(nrow=100, ncol=2)
  stopifnot(sum(prop.tipos) <= 1 & all(prop.tipos) >= 0)
  size <- cumsum(floor(prop.tipos*k))
  if(max(size) > k) size[4] <- 100
  for ( i in 1:k) {
    if(i <= size[1]) {
      plat[i,] <- candidatos[i,] ## honesto
    } else {
      if ( i <= size[2]) {
        
      }
    }
    

  }
  
}


## coleta info eleitor
pesq_eleitor <- function(s=10, n=5000, eleitores) {
  # s é o tamanho da amostra e reflete dinheiro arrecadado
  pesq <- matrix(nrow=k, ncol=2)
  pesq[j,] <- colMeans(eleitores[sample(1:n, s),])
}


# plataforma do mentiroso
plat_mentiroso <- function(k, n, s=10, eleitores) {
  pesq <- pesq_eleitor(s, n)
  plat <- matrix(nrow=100, ncol=2)
  stopifnot(alpha <= 1 & alpha >= 0)
  size <- floor(alpha*k)
  for ( j in 1:size) {
    plat[j,] <- candidatos[j,]
  }
}

# candidato observa média amostral (pequena amostra) das duas dimensões,
# várias regras (tipos de candidatos)
# honesto: anuncia a verdade
# mentiroso: sempre anuncia a média observada
# volúvel: muda plataforma onde está mais distante
# resistente: muda plataforma onde está mais perto


# honesto anuncia x


# mentiroso, anuncia media_eleitor


# 1. coleta info s/ eleitor: 
# s = f($), tamanho da amostra ou info sobre o eleitor
s <- 10
pesq <- matrix(nrow=k, ncol=2)
pesq[j,] <- colMeans(eleitores[sample(1:n, s),])

## mentiroso, anuncia resultado de pesq
plat[j,] <- pesq[j,]

Volúvel e resistente
# 2. computa diferença 
dif[j] <- candidatos[j,] - pesq[j,]
# 3. anuncia plataforma se resistente
keep.indx <- which.min(candidatos[j,] - pesq[j,])
keep <- pesq[j,keep.indx]

plat[j,] <- candidatos[j,]
if (keep.indx == 1) {
  plat[j,2] <- keep
} else {
  plat[j,1] <- keep
}

# 3. anuncia plataforma se volúvel
keep.indx <- which.max(candidatos[j,] - pesq[j,])
keep <- pesq[j,keep.indx]

plat[j,] <- candidatos[j,]
if (keep.indx == 1) {
  plat[j,2] <- keep
} else {
  plat[j,1] <- keep
}

## eleitor
## por enquanto, ouve aleatoriamente um candidato
c <- 5 # quantos candidatos procura
cand <- sample(1:k, c)
Sigma <- matrix(c(.01, .005, .005, .01), 2)

# del mede quão alienado é o eleitor
# quanto menor del, menos ele é alienado

del <- 1


# cria vetor para guardar escolha do voto
voto <- matrix(c(1:n, rep(NA, n)), nrow=n, ncol=2)

# cria vetor para guardar cálculo de função de perda
loss_vec <- numeric()

for ( j in 1:c) {
  mu <- plat[cand[j],]
  plat_eleitor <- mvrnorm(1, mu = mu, Sigma = Sigma )  # from Mass package
loss <- sqrt(sum((plat_eleitor - eleitores[i,])^2))
    if ( loss <= del) {
    # vota no candidato
      voto[i,2] <- cand[j]
    break
  } else {
    # procura outro candidato
    loss_vec[j] <- loss
  }
}

if (is.na(voto[i,2])) {
  voto[i,2] <- cand[which.min(loss_vec)] 
}


