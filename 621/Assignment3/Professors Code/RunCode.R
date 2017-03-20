setwd("C:/Data/RRouah")
library(pracma)

source("HestonProb.R")
source("HestonPrice.R")
source("HestonPriceExample.R")

a=HestonPriceExample()


source("Rouah_HestonExplicitPDE.R")
source("Rouah_Run_HestonExplicitPDE.R")


U = Rouah_Run_HestonExplicitPDE()
S0 = 101.52
V0 = 0.05412
(UniformPrice = interp2(V, S, U, V0,S0))

ii=read.table("MyFile.txt")
