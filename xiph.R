# evolution of female preferences and male traits

require(ape)

tree<-read.nexus("/Users/lukereding/Desktop/xiph")
plot(tree[[2]])

data<-read.csv("/Users/lukereding/Desktop/xiph.csv",colClasses=c("character","numeric","factor","numeric"))
head(data)
data[26,1]<-"Priapella"
rownames(data)<-data$species
name.check(tree[[1]],data)

# this drop tips from the tree if the species doesn't have preference data
# also deletes those species from the dataset
for(i in nrow(data):1){
  if(is.na(data$pref[i])==TRUE){
    for(j in 1:length(tree)){
      tree[[j]]<-drop.tip(tree[[j]],rownames(data)[i])
    }
  data<-data[-i,]
  }
}
name.check(tree[[1]],data)




write.nexus(tree, file="/Users/lukereding/Desktop/xiph_trees_cleaned.nex")


require(caper)
fish<-comparative.data(tree[[1]],data,species)
model<-pgls(pref~sword,fish)
summary(model)
logLik(model)[1]



runModels<-function(tree,data,l=1.0,k=1.0,d=1.0){
  require(caper)
  likelihood<-p<-slopes<-numeric(length(tree))
  for(i in 1:length(tree)){
    print(i)
    fish<-comparative.data(tree[[i]],data,species,force.root=TRUE)
    model<-pgls(pref~sword,fish,lambda=l,kappa=k,delta=d)
    likelihood[i]<-model$model$log.lik[[1]]
    p[i]<-summary(model)$coefficients[[2,4]]
    slopes[i]<-summary(model)$coefficients[[2,1]]
  }
  return(data.frame(likelihood,slopes,p))
}

brown<-runModels(tree,data)
hist(brown$p);mean(brown$p)
lambda<-runModels(tree,data,l="ML");hist(lambda$p);mean(lambda$p)
kappa<-runModels(tree,data,k="ML");hist(kappa$p);mean(kappa$p)
delta<-runModels(tree,data,d="ML")




plot(tree[[1]])
ot <- ape2ouch(tree[[1]])
otd <- as(ot,"data.frame")
data$labels <- rownames(data)
otd <- merge(otd,data,by="labels",all=TRUE)
rownames(otd) <- otd$nodes
print(otd)
ot <- with(otd,ouchtree(nodes=nodes,ancestors=ancestors,times=times,labels=labels))
b1 <- brown(tree=ot,data=otd[c("pref","sword")])
summary(b1)

otd$regimes <- as.factor("global")
h1 <- hansen(
  tree=ot,
  data=otd[c("pref","sword")],
  regimes=otd["regimes"],
  sqrt.alpha=c(1,0,1),
  sigma=c(1,0,1),
  maxit=10000
)
summary(h1)









