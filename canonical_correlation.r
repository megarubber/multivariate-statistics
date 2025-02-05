install.packages("CCA")
library(CCA)

my_data <- read.csv2("./dados_correlcanonicas.csv")

x <- data.frame(
  as.double(my_data$vendas),
  as.double(my_data$lucros),
  as.double(my_data$novos_clientes)
)

y <- data.frame(
  as.double(my_data$escrita),
  as.double(my_data$logica),
  as.double(my_data$social),
  as.double(my_data$matematica)
)

xy <- cbind(as.matrix(x), as.matrix(y)) 
m_corr_x <- as.matrix(cor(x))
m_corr_y <- as.matrix(cor(y))

m_cova_x <- cov(x)
m_cova_y <- cov(y)

create.CCM <-
  function(X.test, X.train = NULL, method = "pearson", use = "everything", verbose = 1) {
    if (is.null(X.train)) {
      K = cor(X.test, method = method, use = use)
      diag(K) = NA
      attr(K, "class") <- "CCM"
      return(K)
    }
    
    if (is.null(rownames(X.train)) | is.null(rownames(X.test))) {
      if (nrow(X.train) != nrow(X.test)) {
        cat("error: X.test and X.train must have the same number of rows or matching rownames must exist\n")
        return(NULL)
      }
      m = 1:nrow(X.train)
    }
    m = match(rownames(X.train), rownames(X.test))
    if (method == "spearman") {
      if (verbose) cat("calculating ranks...\n")
      X.test = apply(X.test,2,rank)
      X.train = apply(X.train,2,rank)
    }
    if (verbose) cat("calculating cors...\n")
    K = matrix(NA, nrow = ncol(X.test[m,]), ncol = ncol(X.train))
    for (i in 1:ncol(X.test)) {
      K[i,] = apply(X.train,2,cor,X.test[m,i])
    }
    attr(K, "class") <- "CCM"
    return(K)
  }

c_xy <- create.CCM(xy)
c_xy <- c_xy[-4:-7, -1:-3]

s_x <- c_xy %*% solve(m_corr_y) %*% t(c_xy)
s_y <- t(c_xy) %*% solve(m_corr_x) %*% c_xy

a1 <- 0.9946009 
a2 <- 0.8674983
a3 <- 0.3983291

calcular_autovetores_adaptado <- function(A, B) {
  # Verifica se A e B são quadradas e da mesma dimensão
  if (nrow(A) != ncol(A) || nrow(B) != ncol(B) || nrow(A) != nrow(B)) {
    stop("As matrizes A e B devem ser quadradas e de mesma ordem!")
  }
  
  # Calculando os autovalores e autovetores de (A - lambda*B)
  autovetores_e_autovalores <- eigen(solve(B) %*% A)  # Usamos a inversa de B para adaptar o cálculo
  
  # Extrai autovalores e autovetores
  autovalores <- autovetores_e_autovalores$values
  autovetores <- autovetores_e_autovalores$vectors
  
  return(list(autovalores = autovalores, autovetores = autovetores))
}

testex <- calcular_autovetores_adaptado(s_x, m_corr_x)
testey <- calcular_autovetores_adaptado(s_y, m_corr_y)

cc1 <- cancor(x, y)
cc2 <- cc(x, y)
