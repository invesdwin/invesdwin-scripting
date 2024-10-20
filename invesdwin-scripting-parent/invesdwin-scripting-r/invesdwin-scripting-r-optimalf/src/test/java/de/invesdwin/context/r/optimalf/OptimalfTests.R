#sudo apt-get -y build-dep libcurl4-gnutls-dev
#sudo apt-get -y install libcurl4-gnutls-dev
if (!require("LSPM")) {
  if (!require("devtools")) {
    install.packages("devtools")
    library("devtools")
  }
  install_github("joshuaulrich/lspm")
  library("LSPM")
}

if (!require("DEoptim")) {
  install.packages("DEoptim")
  library("DEoptim")
}

if (!require("matrixStats")) {
  install.packages("matrixStats")
  library("matrixStats")
}

if (!require("snow")) {
  install.packages("snow")
  library("snow")
}

#clust <- makeSOCKcluster(2)
clust <- NULL

trades <- cbind(c(.5,-.3, .4,-.2), c(0.1,-.15, .4,-.1))
probs <- cbind(c(0.25, 0.25, 0.25, 0.25), c(0.25, 0.25, 0.25, 0.25))
#trades <- c(2, -3, 10, -5)
#probs <- c(0.25,0.25,0.25,0.25)
lspobj <- lsp(trades, probs)

DEctrl <- list(NP = 30, itermax = 200)
result <- optimalf(lspobj, snow = clust, control = DEctrl)
#result <- optimalf(lspobj, probDrawdown, 0.1, DD=0.2, horizon=4, snow=clust, control=DEctrl)
#result <- optimalf(lspobj, probRuin, 0.1, DD=0.2, horizon=4, snow=clust, control=DEctrl)
profit <- result$G
optimalf <- result$f

#colProds(trades)
profit
optimalf
format(round(optimalf, 6), nsmall = 6)