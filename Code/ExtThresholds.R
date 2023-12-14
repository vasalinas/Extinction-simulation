#Script for Extinctions with Threshold and Sorted in Ascending and Descending Order
setwd("~/Documentos/Extinciones Vane Salinas")

library(readxl)
library(EcoNetwork)
library(igraph)

#####Extinctions by Degree Desc and Asc#####

red<- read_excel("Matricesxpasos_CREC.xlsx", sheet=1,col_names=F)
names(red)<- paste(letters, seq(1:91))
row.names(red)<- paste(letters, seq(1:91)) #ascending order
red<-as.data.frame(red)
orden<-names(red)

red<- read_excel("Matricesxpasos_DEC.xlsx", sheet=1,col_names=F)
names(red)<- paste(letters, seq(1:91))
row.names(red)<- paste(letters, seq(1:91)) #descending order
red<-as.data.frame(red)
orden<-names(red)

#We need to identify the producers in order not to eliminate them.
S <- length(red)
productores <- vector("character", S)
for(i in 1:S){
  if(sum(red[,i] == 0)) productores[i] <- colnames(red)[i]
  if(sum(red[,i] != 0)) productores[i] <- NA
}
productores <- productores[!is.na(productores)]
N <-length(productores)
Net<-list()

#threshold vector
for (i in 1:9){
umbral<- i/10
NumPresas <-rep(0,S)
vectorumbral <- as.data.frame(NumPresas,row.names=names(red))

for (b in 1:S) {
vectorumbral[b,1] <- sum(red[,b])}
vectorumbral <- (vectorumbral*umbral)
vectorumbral <- as.vector(vectorumbral[,1])

net<-list()

A <- rbind(red,vectorumbral)

#Primary extinction

for (x in 1:S){#In the case of NT ascending, I'll have to stop it earlier because everything collapses, so I changed the loop to 30
if(length(A)>9 & sum(A)!=0){ #Greater than 9 because we had set a cutoff of 10%.
B <- A[-1,]
A <- B[,-1]

#We need to find the secondary extinctions.

repeat {
  if (length(A) > 1){
    a <- vector("integer",0)
    for (l in 1:length(A)) {
      z <- vector("logical",0)
      for (k in 1:N) {z <- c(z, names(A)[l] == productores[k])}
      if ((sum((A[-(length(A)+1),])[,l]) <= A[(length(A)+1),l]) & !any(z)){
        a <- c(a,l)}}
    if (length(a)>0){
      A <- A[-a,]
      A <- A[,-a]
    } else {
      break}}}
m<-A[-(length(A)+1),]
net[[x]] <-m
}} 
Net[[i]] <- net

}

#I run it twice and save the list of extinctions with threshold for each order.
#NetCre<-Net
#NetDec<-Net

library(EcoNetwork)
library(igraph)

#### QSS ascending ####
#1

NetCre1 <-lapply(NetCre[[1]], as.matrix)
NetCre1 <- lapply(NetCre1, graph_from_adjacency_matrix)

NetCre1.QSS<- data.frame()

for (i in 1:length(NetCre1)){
  qss<-print(calc_QSS(NetCre1[[i]]))
  NetCre1.QSS <- rbind(NetCre1.QSS, qss)
}

#2

NetCre2 <-lapply(NetCre[[2]], as.matrix)
NetCre2 <- lapply(NetCre2, graph_from_adjacency_matrix)

NetCre2.QSS<- data.frame()

for (i in 1:length(NetCre2)){
  qss<-print(calc_QSS(NetCre2[[i]]))
  NetCre2.QSS <- rbind(NetCre2.QSS, qss)
}

#3

NetCre3 <-lapply(NetCre[[3]], as.matrix)
NetCre3 <- lapply(NetCre3, graph_from_adjacency_matrix)

NetCre3.QSS<- data.frame()

for (i in 1:length(NetCre3)){
  qss<-print(calc_QSS(NetCre3[[i]]))
  NetCre3.QSS <- rbind(NetCre3.QSS, qss)
}

#4

NetCre4 <-lapply(NetCre[[4]], as.matrix)
NetCre4 <- lapply(NetCre4, graph_from_adjacency_matrix)

NetCre4.QSS<- data.frame()

for (i in 1:length(NetCre4)){
  qss<-print(calc_QSS(NetCre4[[i]]))
  NetCre4.QSS <- rbind(NetCre4.QSS, qss)
}

#5

NetCre5 <-lapply(NetCre[[5]], as.matrix)
NetCre5 <- lapply(NetCre5, graph_from_adjacency_matrix)

NetCre5.QSS<- data.frame()

for (i in 1:length(NetCre5)){
  qss<-print(calc_QSS(NetCre5[[i]]))
  NetCre5.QSS <- rbind(NetCre5.QSS, qss)
}

#6

NetCre6 <-lapply(NetCre[[6]], as.matrix)
NetCre6 <- lapply(NetCre6, graph_from_adjacency_matrix)

NetCre6.QSS<- data.frame()

for (i in 1:length(NetCre6)){
  qss<-print(calc_QSS(NetCre6[[i]]))
  NetCre6.QSS <- rbind(NetCre6.QSS, qss)
}

#7

NetCre7 <-lapply(NetCre[[7]], as.matrix)
NetCre7 <- lapply(NetCre7, graph_from_adjacency_matrix)

NetCre7.QSS<- data.frame()

for (i in 1:length(NetCre7)){
  qss<-print(calc_QSS(NetCre7[[i]]))
  NetCre7.QSS <- rbind(NetCre7.QSS, qss)
}

#8

NetCre8 <-lapply(NetCre[[8]], as.matrix)
NetCre8 <- lapply(NetCre8, graph_from_adjacency_matrix)

NetCre8.QSS<- data.frame()

for (i in 1:length(NetCre8)){
  qss<-print(calc_QSS(NetCre8[[i]]))
  NetCre8.QSS <- rbind(NetCre8.QSS, qss)
}

#9

NetCre9 <-lapply(NetCre[[9]], as.matrix)
NetCre9 <- lapply(NetCre9, graph_from_adjacency_matrix)

NetCre9.QSS<- data.frame()

for (i in 1:length(NetCre9)){
  qss<-print(calc_QSS(NetCre9[[i]]))
  NetCre9.QSS <- rbind(NetCre9.QSS, qss)
}


dev.new()
par(mfrow=c(2,2))
plot(seq(1:74), NetCre2.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,75))
plot(seq(1:66), NetCre4.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,75))
plot(seq(1:61), NetCre6.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,75))
plot(seq(1:52), NetCre8.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,75))
dev.off()

dev.new()
par(mfrow=c(2,2))
plot(seq(1:74), NetCre2.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,75))
plot(seq(1:66), NetCre4.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,75))
plot(seq(1:61), NetCre6.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,75))
plot(seq(1:52), NetCre8.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,75))
dev.off()


#### QSS descending ####

#1

NetDec1 <-lapply(NetDec[[1]], as.matrix)
NetDec1 <- lapply(NetDec1, graph_from_adjacency_matrix)

NetDec1.QSS<- data.frame()

for (i in 1:length(NetDec1)){
  qss<-print(calc_QSS(NetDec1[[i]]))
  NetDec1.QSS <- rbind(NetDec1.QSS, qss)
}

#2

NetDec2 <-lapply(NetDec[[2]], as.matrix)
NetDec2 <- lapply(NetDec2, graph_from_adjacency_matrix)

NetDec2.QSS<- data.frame()

for (i in 1:length(NetDec2)){
  qss<-print(calc_QSS(NetDec2[[i]]))
  NetDec2.QSS <- rbind(NetDec2.QSS, qss)
}

#3

NetDec3 <-lapply(NetDec[[3]], as.matrix)
NetDec3 <- lapply(NetDec3, graph_from_adjacency_matrix)

NetDec3.QSS<- data.frame()

for (i in 1:length(NetDec3)){
  qss<-print(calc_QSS(NetDec3[[i]]))
  NetDec3.QSS <- rbind(NetDec3.QSS, qss)
}

#4

NetDec4 <-lapply(NetDec[[4]], as.matrix)
NetDec4 <- lapply(NetDec4, graph_from_adjacency_matrix)

NetDec4.QSS<- data.frame()

for (i in 1:length(NetDec4)){
  qss<-print(calc_QSS(NetDec4[[i]]))
  NetDec4.QSS <- rbind(NetDec4.QSS, qss)
}

#5

NetDec5 <-lapply(NetDec[[5]], as.matrix)
NetDec5 <- lapply(NetDec5, graph_from_adjacency_matrix)

NetDec5.QSS<- data.frame()

for (i in 1:length(NetDec5)){
  qss<-print(calc_QSS(NetDec5[[i]]))
  NetDec5.QSS <- rbind(NetDec5.QSS, qss)
}

#6

NetDec6 <-lapply(NetDec[[6]], as.matrix)
NetDec6 <- lapply(NetDec6, graph_from_adjacency_matrix)

NetDec6.QSS<- data.frame()

for (i in 1:length(NetDec6)){
  qss<-print(calc_QSS(NetDec6[[i]]))
  NetDec6.QSS <- rbind(NetDec6.QSS, qss)
}

#7

NetDec7 <-lapply(NetDec[[7]], as.matrix)
NetDec7 <- lapply(NetDec7, graph_from_adjacency_matrix)

NetDec7.QSS<- data.frame()

for (i in 1:length(NetDec7)){
  qss<-print(calc_QSS(NetDec7[[i]]))
  NetDec7.QSS <- rbind(NetDec7.QSS, qss)
}

#8

NetDec8 <-lapply(NetDec[[8]], as.matrix)
NetDec8 <- lapply(NetDec8, graph_from_adjacency_matrix)

NetDec8.QSS<- data.frame()

for (i in 1:length(NetDec8)){
  qss<-print(calc_QSS(NetDec8[[i]]))
  NetDec8.QSS <- rbind(NetDec8.QSS, qss)
}

#9

NetDec9 <-lapply(NetDec[[9]], as.matrix)
NetDec9 <- lapply(NetDec9, graph_from_adjacency_matrix)

NetDec9.QSS<- data.frame()

for (i in 1:length(NetDec9)){
  qss<-print(calc_QSS(NetDec9[[i]]))
  NetDec9.QSS <- rbind(NetDec9.QSS, qss)
}

dev.new()
par(mfrow=c(2,2))
plot(seq(1:24), NetDec2.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,30))
plot(seq(1:18), NetDec4.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,30))
plot(seq(1:15), NetDec6.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,30))
plot(seq(1:13), NetDec8.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,30))
dev.off()

dev.new()
par(mfrow=c(2,2))
plot(seq(1:24), NetDec2.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,30))
plot(seq(1:18), NetDec4.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,30))
plot(seq(1:15), NetDec6.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,30))
plot(seq(1:13), NetDec8.QSS$MEing, xlab="Extinctions", ylab= "MEing", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,30))
dev.off()

#Topological Indices
#In Ascending Order

NetCre9.Top<- data.frame()

for (i in 1:length(NetCre9)){
  Top<-print(calc_topological_indices(NetCre9[[i]]))
  NetCre9.Top <- rbind(NetCre9.Top, Top)
}

TopCREb<- rbind(NetCre1.Top, NetCre2.Top, NetCre3.Top, NetCre4.Top, NetCre5.Top, NetCre6.Top, NetCre7.Top, NetCre8.Top, NetCre9.Top)
TopCRE<-data.frame(TopCREb, umbrales, Extinciones)

#In Descending Order

NetDec9.Top<- data.frame()

for (i in 1:length(NetDec9)){
  Top<-print(calc_topological_indices(NetDec9[[i]]))
  NetDec9.Top <- rbind(NetDec9.Top, Top)
}

TopDECb<- rbind(NetDec1.Top, NetDec2.Top, NetDec3.Top, NetDec4.Top, NetDec5.Top, NetDec6.Top, NetDec7.Top, NetDec8.Top, NetDec9.Top)
TopDEC<-data.frame(TopDECb, umbralesD, ExtincionesD)

#Modularity for Ascending Degree and Descending Degree

#To calculate modularity, we need to remove the vertices with a degree of 0. For that, the igraph function 'delete.vertex' can be used.

ModNet<-list()
for (i in 1:9){
x<-c("NDec")
x2<- c("NetDec")
c<-c("Iso")
f <-c("NDec")
E<-list()
for(j in 1:length(get(paste(x2,i,sep="")))){
a<-(paste(x,i, sep=""))
b<- (paste(x2,i,sep=""))
a <- b
d<-(paste(c,i,sep=""))
e<-(paste(f,i,sep=""))
d<- which(degree(get(a)[[j]])==0)
e<-delete.vertices(get(a)[[j]],d)
E[[j]]<-e
}
ModNet[[i]]<-E
}


print(calc_modularity(ModNet[[1]][[1]]))

Isolated = which(degree(NetDec1[[1]])==0)
G2 = delete.vertices(NetDec1[[1]], Isolated)
print(calc_modularity(G2))

Mod1<-data.frame()
for (i in 1:length(NetDec1)){
 mod1<- print(calc_modularity(ModNet[[1]][[i]]))
 Mod1<-rbind(Mod1,mod1)
}

Mod2<-data.frame()
for (i in 1:length(NetDec2)){
  mod2<- print(calc_modularity(ModNet[[2]][[i]]))
  Mod2<-rbind(Mod2,mod2)
}

Mod3<-data.frame()
for (i in 1:length(NetDec3)){
  mod3<- print(calc_modularity(ModNet[[3]][[i]]))
  Mod3<-rbind(Mod3,mod3)
}

Mod4<-data.frame()
for (i in 1:length(NetDec4)){
  mod4<- print(calc_modularity(ModNet[[4]][[i]]))
  Mod4<-rbind(Mod4,mod4)
}

Mod5<-data.frame()
for (i in 1:length(NetDec5)){
  mod5<- print(calc_modularity(ModNet[[5]][[i]]))
  Mod5<-rbind(Mod5,mod5)
}

Mod6<-data.frame()
for (i in 1:length(NetDec6)){
  mod6<- print(calc_modularity(ModNet[[6]][[i]]))
  Mod6<-rbind(Mod6,mod6)
}

Mod7<-data.frame()
for (i in 1:length(NetDec7)){
  mod7<- print(calc_modularity(ModNet[[7]][[i]]))
  Mod7<-rbind(Mod7,mod7)
}

Mod8<-data.frame()
for (i in 1:length(NetDec8)){
  mod8<- print(calc_modularity(ModNet[[8]][[i]]))
  Mod8<-rbind(Mod8,mod8)
}

Mod9<-data.frame()
for (i in 1:length(NetDec9)){
  mod9<- print(calc_modularity(ModNet[[9]][[i]]))
  Mod9<-rbind(Mod9,mod9)
}

#We have to do the same for the ascending order

ModNetCre<-list()
for (i in 1:9){
  x<-c("NCre")
  x2<- c("NetCre")
  c<-c("Iso")
  f <-c("NCre")
  E<-list()
  for(j in 1:length(get(paste(x2,i,sep="")))){
    a<-(paste(x,i, sep=""))
    b<- (paste(x2,i,sep=""))
    a <- b
    d<-(paste(c,i,sep=""))
    e<-(paste(f,i,sep=""))
    d<- which(degree(get(a)[[j]])==0)
    e<-delete.vertices(get(a)[[j]],d)
    E[[j]]<-e
  }
  ModNetCre[[i]]<-E
}

Mod1Cre<-data.frame()
for (i in 1:length(NetCre1)){
  mod1Cre<- print(calc_modularity(ModNetCre[[1]][[i]]))
  Mod1Cre<-rbind(Mod1Cre,mod1Cre)
}

Mod2Cre<-data.frame()
for (i in 1:length(NetCre2)){
  mod2Cre<- print(calc_modularity(ModNetCre[[2]][[i]]))
  Mod2Cre<-rbind(Mod2Cre,mod2Cre)
}

Mod3Cre<-data.frame()
for (i in 1:length(NetCre3)){
  mod3Cre<- print(calc_modularity(ModNetCre[[3]][[i]]))
  Mod3Cre<-rbind(Mod3Cre,mod3Cre)
}

Mod4Cre<-data.frame()
for (i in 1:length(NetCre4)){
  mod4Cre<- print(calc_modularity(ModNetCre[[4]][[i]]))
  Mod4Cre<-rbind(Mod4Cre,mod4Cre)
}

Mod5Cre<-data.frame()
for (i in 1:length(NetCre5)){
  mod5Cre<- print(calc_modularity(ModNetCre[[5]][[i]]))
  Mod5Cre<-rbind(Mod5Cre,mod5Cre)
}

Mod6Cre<-data.frame()
for (i in 1:length(NetCre6)){
  mod6Cre<- print(calc_modularity(ModNetCre[[6]][[i]]))
  Mod6Cre<-rbind(Mod6Cre,mod6Cre)
}

Mod7Cre<-data.frame()
for (i in 1:length(NetCre7)){
  mod7Cre<- print(calc_modularity(ModNetCre[[7]][[i]]))
  Mod7Cre<-rbind(Mod7Cre,mod7Cre)
}

Mod8Cre<-data.frame()
for (i in 1:length(NetCre8)){
  mod8Cre<- print(calc_modularity(ModNetCre[[8]][[i]]))
  Mod8Cre<-rbind(Mod8Cre,mod8Cre)
}

Mod9Cre<-data.frame()
for (i in 1:length(NetCre9)){
  mod9Cre<- print(calc_modularity(ModNetCre[[9]][[i]]))
  Mod9Cre<-rbind(Mod9Cre,mod9Cre)
}

dev.new()
par(mfrow=c(2,2))
plot(seq(1:18), Mod2$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,18))
plot(seq(1:11), Mod4$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.4", xlim=c(1,11))
plot(seq(1:10), Mod6$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.6", xlim=c(1,10))
plot(seq(1:7), Mod8$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.8", xlim=c(1,7))
dev.off()

dev.new()
par(mfrow=c(2,2))
plot(seq(1:74), Mod2Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,74))
plot(seq(1:66), Mod4Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.4", xlim=c(1,66))
plot(seq(1:61), Mod6Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.6", xlim=c(1,61))
plot(seq(1:52), Mod8Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.8", xlim=c(1,52))
dev.off()


#Trophic Level Extinctions

redNTdec <- read.csv("NTdec.csv", header=T, row.names=1)
redNTcre <- read.csv("NTcre.csv", header=T, row.names=1)

red<-redNTdec #To run the script above 
red<-redNTcre #I will run the scripts above again but with this new network

NetNTdec<-Net
NetNTcre<-Net

#####Ascending####
#1
NetCre1NT <-lapply(NetNTcre[[1]], as.matrix)
NetCre1NT <- lapply(NetCre1NT, graph_from_adjacency_matrix)

NetCre1NT.QSS<- data.frame()

for (i in 1:length(NetCre1NT)){
  qss<-print(calc_QSS(NetCre1NT[[i]]))
  NetCre1NT.QSS <- rbind(NetCre1NT.QSS, qss)
}

#2

NetCre2NT <-lapply(NetNTcre[[2]], as.matrix)
NetCre2NT <- lapply(NetCre2NT, graph_from_adjacency_matrix)

NetCre2NT.QSS<- data.frame()

for (i in 1:length(NetCre2NT)){
  qss<-print(calc_QSS(NetCre2NT[[i]]))
  NetCre2NT.QSS <- rbind(NetCre2NT.QSS, qss)
}

#3

NetCre3NT <-lapply(NetNTcre[[3]], as.matrix)
NetCre3NT <- lapply(NetCre3NT, graph_from_adjacency_matrix)

NetCre3NT.QSS<- data.frame()

for (i in 1:length(NetCre3NT)){
  qss<-print(calc_QSS(NetCre3NT[[i]]))
  NetCre3NT.QSS <- rbind(NetCre3NT.QSS, qss)
}

#4

NetCre4NT <-lapply(NetNTcre[[4]], as.matrix)
NetCre4NT <- lapply(NetCre4NT, graph_from_adjacency_matrix)

NetCre4NT.QSS<- data.frame()

for (i in 1:length(NetCre4NT)){
  qss<-print(calc_QSS(NetCre4NT[[i]]))
  NetCre4NT.QSS <- rbind(NetCre4NT.QSS, qss)
}

#5

NetCre5NT <-lapply(NetNTcre[[5]], as.matrix)
NetCre5NT <- lapply(NetCre5NT, graph_from_adjacency_matrix)

NetCre5NT.QSS<- data.frame()

for (i in 1:length(NetCre5NT)){
  qss<-print(calc_QSS(NetCre5NT[[i]]))
  NetCre5NT.QSS <- rbind(NetCre5NT.QSS, qss)
}

#6

NetCre6NT <-lapply(NetNTcre[[6]], as.matrix)
NetCre6NT <- lapply(NetCre6NT, graph_from_adjacency_matrix)

NetCre6NT.QSS<- data.frame()

for (i in 1:length(NetCre6NT)){
  qss<-print(calc_QSS(NetCre6NT[[i]]))
  NetCre6NT.QSS <- rbind(NetCre6NT.QSS, qss)
}

#7

NetCre7NT <-lapply(NetNTcre[[7]], as.matrix)
NetCre7NT <- lapply(NetCre7NT, graph_from_adjacency_matrix)

NetCre7NT.QSS<- data.frame()

for (i in 1:length(NetCre7NT)){
  qss<-print(calc_QSS(NetCre7NT[[i]]))
  NetCre7NT.QSS <- rbind(NetCre7NT.QSS, qss)
}

#8

NetCre8NT <-lapply(NetNTcre[[8]], as.matrix)
NetCre8NT <- lapply(NetCre8NT, graph_from_adjacency_matrix)

NetCre8NT.QSS<- data.frame()

for (i in 1:length(NetCre8NT)){
  qss<-print(calc_QSS(NetCre8NT[[i]]))
  NetCre8NT.QSS <- rbind(NetCre8NT.QSS, qss)
}

#9

NetCre9NT <-lapply(NetNTcre[[9]], as.matrix)
NetCre9NT <- lapply(NetCre9NT, graph_from_adjacency_matrix)

NetCre9NT.QSS<- data.frame()

for (i in 1:length(NetCre9NT)){
  qss<-print(calc_QSS(NetCre9NT[[i]]))
  NetCre9NT.QSS <- rbind(NetCre9NT.QSS, qss)
}

dev.new()
par(mfrow=c(2,2))
plot(seq(1:30), NetCre2NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,30))
plot(seq(1:30), NetCre4NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,30))
plot(seq(1:30), NetCre6NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,30))
plot(seq(1:30), NetCre8NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,30))
dev.off()

#####descending####

#1
NetDec1NT <-lapply(NetNTdec[[1]], as.matrix)
NetDec1NT <- lapply(NetDec1NT, graph_from_adjacency_matrix)

NetDec1NT.QSS<- data.frame()

for (i in 1:length(NetDec1NT)){
  qss<-print(calc_QSS(NetDec1NT[[i]]))
  NetDec1NT.QSS <- rbind(NetDec1NT.QSS, qss)
}

#2

NetDec2NT <-lapply(NetNTdec[[2]], as.matrix)
NetDec2NT <- lapply(NetDec2NT, graph_from_adjacency_matrix)

NetDec2NT.QSS<- data.frame()

for (i in 1:length(NetDec2NT)){
  qss<-print(calc_QSS(NetDec2NT[[i]]))
  NetDec2NT.QSS <- rbind(NetDec2NT.QSS, qss)
}

#3

NetDec3NT <-lapply(NetNTdec[[3]], as.matrix)
NetDec3NT <- lapply(NetDec3NT, graph_from_adjacency_matrix)

NetDec3NT.QSS<- data.frame()

for (i in 1:length(NetDec3NT)){
  qss<-print(calc_QSS(NetDec3NT[[i]]))
  NetDec3NT.QSS <- rbind(NetDec3NT.QSS, qss)
}

#4

NetDec4NT <-lapply(NetNTdec[[4]], as.matrix)
NetDec4NT <- lapply(NetDec4NT, graph_from_adjacency_matrix)

NetDec4NT.QSS<- data.frame()

for (i in 1:length(NetDec4NT)){
  qss<-print(calc_QSS(NetDec4NT[[i]]))
  NetDec4NT.QSS <- rbind(NetDec4NT.QSS, qss)
}

#5

NetDec5NT <-lapply(NetNTdec[[5]], as.matrix)
NetDec5NT <- lapply(NetDec5NT, graph_from_adjacency_matrix)

NetDec5NT.QSS<- data.frame()

for (i in 1:length(NetDec5NT)){
  qss<-print(calc_QSS(NetDec5NT[[i]]))
  NetDec5NT.QSS <- rbind(NetDec5NT.QSS, qss)
}

#6

NetDec6NT <-lapply(NetNTdec[[6]], as.matrix)
NetDec6NT <- lapply(NetDec6NT, graph_from_adjacency_matrix)

NetDec6NT.QSS<- data.frame()

for (i in 1:length(NetDec6NT)){
  qss<-print(calc_QSS(NetDec6NT[[i]]))
  NetDec6NT.QSS <- rbind(NetDec6NT.QSS, qss)
}

#7

NetDec7NT <-lapply(NetNTdec[[7]], as.matrix)
NetDec7NT <- lapply(NetDec7NT, graph_from_adjacency_matrix)

NetDec7NT.QSS<- data.frame()

for (i in 1:length(NetDec7NT)){
  qss<-print(calc_QSS(NetDec7NT[[i]]))
  NetDec7NT.QSS <- rbind(NetDec7NT.QSS, qss)
}

#8

NetDec8NT <-lapply(NetNTdec[[8]], as.matrix)
NetDec8NT <- lapply(NetDec8NT, graph_from_adjacency_matrix)

NetDec8NT.QSS<- data.frame()

for (i in 1:length(NetDec8NT)){
  qss<-print(calc_QSS(NetDec8NT[[i]]))
  NetDec8NT.QSS <- rbind(NetDec8NT.QSS, qss)
}

#9

NetDec9NT <-lapply(NetNTdec[[9]], as.matrix)
NetDec9NT <- lapply(NetDec9NT, graph_from_adjacency_matrix)

NetDec9NT.QSS<- data.frame()

for (i in 1:length(NetDec9NT)){
  qss<-print(calc_QSS(NetDec9NT[[i]]))
  NetDec9NT.QSS <- rbind(NetDec9NT.QSS, qss)
}

dev.new()
par(mfrow=c(2,2))
plot(seq(1:60), NetDec2NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,60))
plot(seq(1:60), NetDec4NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,60))
plot(seq(1:60), NetDec6NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,60))
plot(seq(1:58), NetDec8NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,60))
dev.off()

#Topological metrics for extinctions with ascending NT.

NetCre1NT_top <-lapply(NetNTcre[[1]], as.matrix) 
NetCre1NT_top <- lapply(NetCre1NT_top, graph_from_adjacency_matrix)

NetCre1NT.TOP<- data.frame()

for (i in 1:length(NetCre1NT_top)){
  top<-print(calc_topological_indices(NetCre1NT_top[[i]]))
  NetCre1NT.TOP <- rbind(NetCre1NT.TOP, top)
}

#Topological metrics for extinctions with descending NT.

NetDec1NT_top <-lapply(NetNTdec[[1]], as.matrix) 
NetDec1NT_top <- lapply(NetDec1NT_top, graph_from_adjacency_matrix)

NetDec1NT.TOP<- data.frame()

for (i in 1:length(NetDec1NT_top)){
  top<-print(calc_topological_indices(NetDec1NT_top[[i]]))
  NetDec1NT.TOP <- rbind(NetDec1NT.TOP, top)
}

#####Mod by NT#####

#NT Descending#

ModNetNT_dec<-list()
for (i in 1:9){
  x<-c("NDec")
  x2<- c("NetDec")
  x3<-c("NT")
  c<-c("Iso")
  f <-c("NDec_NT")
  E<-list()
  for(j in 1:length(get(paste(x2,i,x3,sep="")))){
    a<-(paste(x,i, sep=""))
    b<- (paste(x2,i,x3, sep=""))
    a <- b
    d<-(paste(c,i,sep=""))
    e<-(paste(f,i,sep=""))
    d<- which(degree(get(a)[[j]])==0)
    e<-delete.vertices(get(a)[[j]],d)
    E[[j]]<-e
  }
  ModNetNT_dec[[i]]<-E
}

Mod2NT_dec<-data.frame()
for (i in 1:length(NetDec2NT)){
  mod2<- print(calc_modularity(ModNetNT_dec[[2]][[i]]))
  Mod2NT_dec<-rbind(Mod2NT_dec,mod2)
}

Mod4NT_dec<-data.frame()
for (i in 1:length(NetDec4NT)){
  mod4<- print(calc_modularity(ModNetNT_dec[[4]][[i]]))
  Mod4NT_dec<-rbind(Mod4NT_dec,mod4)
}

Mod6NT_dec<-data.frame()
for (i in 1:length(NetDec6NT)){
  mod6<- print(calc_modularity(ModNetNT_dec[[6]][[i]]))
  Mod6NT_dec<-rbind(Mod6NT_dec,mod6)
}

Mod8NT_dec<-data.frame()
for (i in 1:length(NetDec8NT)){
  mod8<- print(calc_modularity(ModNetNT_dec[[8]][[i]]))
  Mod8NT_dec<-rbind(Mod8NT_dec,mod8)
}

#NT Ascending#

ModNetNT_cre<-list()
for (i in 1:9){
  x<-c("NCre")
  x2<- c("NetCre")
  x3<-c("NT")
  c<-c("Iso")
  f <-c("NCre_NT")
  E<-list()
  for(j in 1:length(get(paste(x2,i,x3,sep="")))){
    a<-(paste(x,i, sep=""))
    b<- (paste(x2,i,x3, sep=""))
    a <- b
    d<-(paste(c,i,sep=""))
    e<-(paste(f,i,sep=""))
    d<- which(degree(get(a)[[j]])==0)
    e<-delete.vertices(get(a)[[j]],d)
    E[[j]]<-e
  }
  ModNetNT_cre[[i]]<-E
}

Mod2NT_cre<-data.frame()
for (i in 1:length(NetCre2NT)){
  mod2<- print(calc_modularity(ModNetNT_cre[[2]][[i]]))
  Mod2NT_cre<-rbind(Mod2NT_cre,mod2)
}

Mod4NT_cre<-data.frame()
for (i in 1:length(NetCre4NT)){
  mod4<- print(calc_modularity(ModNetNT_cre[[4]][[i]]))
  Mod4NT_cre<-rbind(Mod4NT_cre,mod4)
}

Mod6NT_cre<-data.frame()
for (i in 1:length(NetCre6NT)){
  mod6<- print(calc_modularity(ModNetNT_cre[[6]][[i]]))
  Mod6NT_cre<-rbind(Mod6NT_cre,mod6)
}

Mod8NT_cre<-data.frame()
for (i in 1:length(NetCre8NT)){
  mod8<- print(calc_modularity(ModNetNT_cre[[8]][[i]]))
  Mod8NT_cre<-rbind(Mod8NT_cre,mod8)
}

dev.new()
par(mfrow=c(2,2))
plot(seq(1:40), Mod2NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,40))
plot(seq(1:40), Mod4NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.4", xlim=c(1,40))
plot(seq(1:40), Mod6NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.6", xlim=c(1,40))
plot(seq(1:38), Mod8NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.8", xlim=c(1,38))
dev.off()


dev.new()
svg(filename="ModComb_DEC.svg", width=10)
par(mfrow=c(2,2))
plot(seq(1:40), Mod2NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,40), col="red", ylim=c(0,1))
points(seq(1:18), Mod2$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.2", col="blue")
plot(seq(1:40), Mod4NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.4", xlim=c(1,40), col="red", ylim=c(0,1))
points(seq(1:11), Mod4$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.4", col="blue")
plot(seq(1:40), Mod6NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.6", xlim=c(1,40), col="red", ylim=c(0,1))
points(seq(1:10), Mod6$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.6", col="blue")
plot(seq(1:38), Mod8NT_dec$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.8", xlim=c(1,40), col="red", ylim=c(0,1))
points(seq(1:7), Mod8$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.8", col="blue")
dev.off()

dev.new()
svg(filename="ModComb_CREC.svg", width=10)
par(mfrow=c(2,2))
plot(seq(1:30), Mod2NT_cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,74), ylim=c(0,1), col="red")
points(seq(1:74), Mod2Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.2", col="blue")
plot(seq(1:30), Mod4NT_cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.4",  xlim=c(1,74), ylim=c(0,1), col="red")
points(seq(1:66), Mod4Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.4", col="blue")
plot(seq(1:30), Mod6NT_cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.6",  xlim=c(1,74), ylim=c(0,1), col="red")
points(seq(1:61), Mod6Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.6", col="blue")
plot(seq(1:30), Mod8NT_cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.8",  xlim=c(1,74), ylim=c(0,1), col="red")
points(seq(1:52), Mod8Cre$Modularity, xlab="Extinctions", ylab= "Mod", pch = 19, cex = 1,  main= "Umbral 0.8", col="blue")
dev.off()

#QSS

dev.new()
svg(filename="QSSUmbrales_DEC_comb.svg", width=10)
par(mfrow=c(2,2))
plot(seq(1:60), NetDec2NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,60), col="red", ylim=c(0,1))
points(seq(1:24), NetDec2.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", col="blue")
plot(seq(1:60), NetDec4NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,60), col="red", ylim=c(0,1))
points(seq(1:18), NetDec4.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", col="blue")
plot(seq(1:60), NetDec6NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,60), col="red", ylim=c(0,1))
points(seq(1:15), NetDec6.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", col="blue")
plot(seq(1:58), NetDec8NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,60), col="red", ylim=c(0,1))
points(seq(1:13), NetDec8.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", col="blue")
dev.off()

dev.new()
svg(filename="QSSUmbrales_CREC_Comb.svg", width=10)
par(mfrow=c(2,2))
plot(seq(1:30), NetCre2NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", xlim=c(1,75), col="red", ylim=c(0,1))
points(seq(1:74), NetCre2.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main= "Umbral 0.2", col="blue")
plot(seq(1:30), NetCre4NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", xlim=c(1,75), col="red", ylim=c(0,1))
points(seq(1:66), NetCre4.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.4", col="blue")
plot(seq(1:30), NetCre6NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", xlim=c(1,75), col="red", ylim=c(0,1))
points(seq(1:61), NetCre6.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1, main="Umbral 0.6", col="blue")
plot(seq(1:30), NetCre8NT.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", xlim=c(1,75), col="red", ylim=c(0,1))
points(seq(1:52), NetCre8.QSS$QSS, xlab="Extinctions", ylab= "Quasi Sign-Stability", pch = 19, cex = 1,  main="Umbral 0.8", col="blue")
dev.off()
