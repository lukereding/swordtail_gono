# evolution of female preferences and male traits

require(ape)

tree<-read.nexus("~/Desktop/swordtail_gono/xiph")
plot(tree[[2]])
tree <- tree[[1]]
tree$tip.label <- gsub("_.*", "", tree$tip.label)
# resolve polytomies / make binary
tree <- multi2di(tree, random = TRUE)
# add a tiny branch length
tree$edge.length  <- ifelse(tree$edge.length == 0, tree$edge.length + 0.001, tree$edge.length) 

data<- read_csv("~/Desktop/swordtail_gono/df_wide.csv")
head(data)
data$species<-gsub(" ", "", data$species)
data$species<-gsub("\\.", "", data$species)
data <- as.data.frame(data)
rownames(data) <- data$species

# rownames(data)<-data$species




## clean up corresponse between tree and data
require(geiger)
name.check(tree,data)

# delete tips from tree
no_data <- name.check(tree,data)$tree_not_data
tree <- drop.tip(tree, no_data)

no_tree <- name.check(tree,data)$data_not_tree
data <- data[!rownames(data) %in% no_tree,]


name.check(tree,data)

### need to use DISCRETE?
## see http://www.amjbot.org/content/103/7/1223.full.pdf+html for a recent example
## and / or see here: http://www.zoology.ubc.ca/prog/diversitree/examples/unreplicated/
## first, to visualize, follow the code from here using diverstree : https://github.com/mwpennell/fuse/blob/master/analysis/R/tree-figs.R
# 
# trait.plot(ladderize(phy), dat=dat, cols=col.tree,
#            +            lab=c("Genotype", "Fused"), str=list(c("XY", "ZW"), c("No", "Yes")),
#            +            class=genus.fish, quiet=TRUE, w=1/10, margin=1/1.4, cex.legend = 1)



cols <- palette_brr(4)
plotTree(tree,type="fan",fsize=0.8,ftype="i")
tiplabels(pie=to.matrix(factor(data$"Grave spot."),sort(unique(data$"Grave spot."))),piecol=cols,cex=0.8)

fitER <- ace(factor(data$"Grave spot."),tree,model="ER",type="discrete")
plotTree(tree,type="fan",fsize=0.8,ftype="i")
nodelabels(node=1:tree$Nnode+Ntip(tree),
           pie=fitER$lik.anc,piecol=cols,cex=0.5)
tiplabels(pie=to.matrix(data$"Grave spot.",sort(unique(data$"Grave spot."))),piecol=cols,cex=0.7)
add.simmap.legend(colors=cols,prompt=FALSE,x=0.9*par()$usr[1],
                  y=-max(nodeHeights(tree)),fsize=0.8)



# this drop tips from the tree if the species doesn't have preference data
# also deletes those species from the dataset
# for(i in nrow(data):1){
#   if(is.na(data$pref[i])==TRUE){
#     for(j in 1:length(tree)){
#       tree[[j]]<-drop.tip(tree[[j]],rownames(data)[i])
#     }
#   data<-data[-i,]
#   }
# }
# name.check(tree[[1]],data)




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









