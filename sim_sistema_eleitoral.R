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



# candidato observa média amostral (pequena amostra) das duas dimensões,
# várias regras (tipos de candidatos)
# honesto: anuncia a verdade
# mentiroso: sempre anuncia a média observada
# volúvel: muda plataforma onde está mais distante
# resistente: muda plataforma onde está mais perto


# honesto anuncia x
# mentiroso, anuncia media_eleitor

## fun gera eleitores
gera_eleitores <- function(n) {
  library(MASS)
  # Simulate bivariate normal data
  mu <- c(0,0)                         # Mean
  Sigma <- matrix(c(.8, .3, .3, .8), 2)  # Covariance matrix
  eleitores <- mvrnorm(n, mu = mu, Sigma = Sigma )  # from Mass package
  eleitores <- as.data.frame(eleitores)
  colnames(eleitores) <- c("dim1", "dim2")
  return(eleitores)
}

## fun gera candidatos
gera_candidatos <- function(k) {
  library(MASS)
  # Simulate bivariate normal data
  mu <- c(0,0)
  Sigma_cand <- matrix(c(1, .6, .6, 1), 2)
  candidatos <- mvrnorm(k, mu = mu, Sigma = Sigma_cand )  # from Mass package
  candidatos <- as.data.frame(candidatos)
  names(candidatos) <- c("pos_dim1", "pos_dim2")
  return(candidatos)
}

## coleta info eleitor
pesq_eleitor <- function(s=10, n=5000, eleitores) {
  # s é o tamanho da amostra e reflete dinheiro arrecadado
  pesq <- colMeans(eleitores[sample(1:n, s),])
}

## candidatos definem plataforma
gera_plataformas <- function(k, n,
                             prop.tipos = rep(.25, 4),
                             candidatos_true, poll) {
  dif <- abs(candidatos - poll)
  
  ## gerar plataforma
  plat <- matrix(nrow=k, ncol=2)
  size <- cumsum(floor(prop.tipos*k))
  if(max(size) > k)  { size[4] <- 100 } ## corrige arredondamento, pode dar 101, por exemplo.
  
  ## honesto
  for ( i in 1:size[1]) {
      plat[i,] <- unlist(candidatos[i,])
    } 
  
  ## mentiroso
  for ( i in (size[1]+1):size[2]) {
    plat[i,] <- unlist(poll) ## nesta versão não tem diferença entre candidatos
  } 
    
  ## muda pouco (muda na dim dele mais próxima do mediano )

  for ( i in (size[2]+1):size[3]) {
      plat[i,] <- unlist(candidatos[i,])
      aux <- which.min(dif[i,])
      plat[i,aux] <- unlist(poll[aux])
    }

  
  ## muda-muito (muda na dim dele mais distanto do mediano )
  for ( i in (size[3]+1):size[4]) {
    plat[i,] <- unlist(candidatos[i,])
    aux <- which.max(dif[i,])
    plat[i,aux] <- unlist(poll[aux])
  } 
  return(plat)
}

gera_voto <- function(n, c, k, vec_prob, init_candidatos, eleitores) {
  voto <- numeric()
  for (i in 1:n) {
    x <- eleitores[i,]
    index_match <- which(rbinom(k, c, vec_prob)==1)
    size_vec <- length(index_match)
    while (size_vec == 0) {
      index_match <- which(rbinom(k, c, vec_prob)==1)
      size_vec <- length(index_match)
    }
    aux <- matrix(rep(unlist(x),size_vec), nrow=size_vec, ncol=2, byrow=T)
    aux2 <- init_candidatos[index_match,]
    dif_elec <- aux - aux2
    loss <- numeric()
    #print(size_vec)
    for ( j in 1:size_vec) {
      #print(paste(paste(i, "este foi i; j é"), j))
      loss[j] <- sqrt(sum(dif_elec[j,]^2))
    }
    voto[i] <- index_match[which.min(loss)]
  }
  return(voto)
}

computa_eleitos <- function(df_voto, n_cadeiras) {
  resultado <- as.data.frame(table(df_voto))
  resultado <- resultado[order(-resultado$Freq),]
  eleitos <- head(resultado, n_cadeiras)
  return(eleitos)
}

realiza_eleicoes <- function (n, k, prop.tipos, vec_prob, c ) {
  voters <- gera_eleitores(n=n)

  candidatos_true <- gera_candidatos(k=k)

  mediano_amostral <- pesq_eleitor(s=10, eleitores = voters)
  plat <- gera_plataformas(k=k_cand, candidatos_true, poll=mediano_amostral)

  voto <- gera_voto(n=n, k=k, c=c, init_candidatos=plat,
                    vec_prob = vec_prob, eleitores = voters)
  
  votacao <- computa_eleitos(df_voto = voto, n_cadeiras = num_cadeiras)
  eleitos <- as.numeric(as.character(unlist(votacao[, 1])))
  return(eleitos)
}


## Sim eleição inicial

## inicializa variáveis
n_elect <- 10000 # num eleitores
k_cand <- 100 # num candidatos
vec_prob_init <- (1:k)/k # prob do candidato contatar eleitor
num_cadeiras <- 10
n.sim <- 100 ## número de eleições
delta <- .01

# primeira eleição
eleitos <- realiza_eleicoes(n=n_elect, k=k_cand, prop.tipos=rep(.25, 4), 
                            vec_prob=vec_prob_init, c=num_cadeiras)

history_elections <- matrix(nrow=n.sim, ncol=num_cadeiras)
history_elections[1, ] <- eleitos
# segunda em diante
cand_aux <- 1:k
for ( i in 2:n.sim) {

  vec_prob_init[cand_aux[eleitos]] <- vec_prob_init[cand_aux[eleitos]]+ delta
  # normaliza
  vec_prob_init <- vec_prob_init/sum(vec_prob_init)
  history_elections[i, ] <- realiza_eleicoes(n=n_elect, k=k_cand, prop.tipos=rep(.25, 4), 
                              vec_prob=vec_prob_init, c=num_cadeiras)
  print(i)
}
head(history_elections)
history_elections1 <- t(apply(t(history_elections), 2, sort))
head(history_elections1)
View(history_elections1)
