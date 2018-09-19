#Facktor analysis
getwd()
library("psych")
stock<-read.csv("/home/jzsyuan/python/Survey sampling /STOCK.csv",header=T)
R<-cor(stock) #corrleation Matrix
fit1<-principal(stock,nfactors=1,residuals = TRUE,rotate = "none")
fit1$loadings
fit2<-principal(stock,nfactors=2,residuals = TRUE,rotate = "none")
fit2$loadings
fit3<-principal(stock,nfactors=2,residuals = TRUE,rotate = "varimax")
fit3$loadings

