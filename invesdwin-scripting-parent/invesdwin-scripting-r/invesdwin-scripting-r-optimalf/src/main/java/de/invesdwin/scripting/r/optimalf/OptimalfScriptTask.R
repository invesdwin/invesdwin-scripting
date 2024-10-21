#sudo apt-get -y build-dep libcurl4-gnutls-dev libgit2-dev
#sudo apt-get -y install libcurl4-gnutls-dev libgit2-dev
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org" 
       options(repos=r)
})
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

#if (!require("snow")) {
#  install.packages("snow")
#  library("snow")
#}

#clust <- makeSOCKcluster(2)
clust <- NULL

#trades <- cbind(c(.5, -.3, .4, -.2),c(0.1, -.15, .4, -.1))
#trades <- c(2, -3, 10, -5)
#probabilities <- cbind(c(0.25,0.25,0.25,0.25), c(0.25,0.25,0.25,0.25))

if (length(trades) == 0) {
  stop("No trades!")
}

portfolio <- lsp(trades, probabilities)

DEctrl <- list(NP = 30, itermax = 200)
result <- optimalf(portfolio, snow = clust, control = DEctrl)
#result <- optimalf(portfolio, probDrawdown, 0.1, DD=0.2, horizon=4, snow=clust, control=DEctrl)
#result <- optimalf(portfolio, probRuin, 0.1, DD=0.2, horizon=4, snow=clust, control=DEctrl)
profit <- result$G - 1
if (profit == -Inf) {
  profit <- -999
} else if (profit == Inf) {
  profit <- 999
}

optimalf <- result$f
