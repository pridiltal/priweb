data1 <- read.csv(file.choose())
library(mvtsplot)

mvtsplot(data1,levels=8,gcol=2,norm="global")
f <- extract_tsfeatures(data1)
dim(f)
library(lattice)
splom(~f[, 1:7])


nobs = 1500
nts = 1000
tsframe1 <- ts(matrix(ncol=nts, nrow=nobs))
i=1

for(i in 1:nts){
  #tsframe1[,i] <- exp(0.002*(1:nobs)) + rnorm(nobs,0,2) # adding noise
  tsframe1[,i] <- 10 + runif(nobs,0,2) # adding noise
}

f <- extract_tsfeatures(tsframe1)
pc <- get_pc_space(f)


#devtools::install_github("pridiltal/oddstream")
oddstream :: plotpc(pc$pcnorm, colour = "red", pc_boundary =8)

