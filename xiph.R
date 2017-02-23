# evolution of female preferences and male traits
require(tidyverse)
source("~/Documents/random_scripts/plotting_functions.R")
require(ape)
require(geiger)
require(phytools)
require(diversitree)

tree<-read.nexus("~/Desktop/swordtail_gono/xiph")
plot(tree[[2]])
tree <- tree[[1]]
tree$tip.label <- gsub("_.*", "", tree$tip.label)
# resolve polytomies / make binary
tree <- multi2di(tree, random = TRUE)
# add a tiny branch length
tree$edge.length  <- ifelse(tree$edge.length == 0, tree$edge.length + 0.001, tree$edge.length) 

data<- read_csv("~/Desktop/swordtail_gono/df_wide_with_jones_columns.csv")
head(data)
data$species<-gsub(" ", "", data$species)
data$species<-gsub("\\.", "", data$species)
data <- as.data.frame(data)
rownames(data) <- data$species

# rownames(data)<-data$species


## subset the dataframe to only the variables of interest
# , sword, `Vertical bars`, `Claw presence vs absence`
df<- data %>%
  select(species, `Well-developed precopulatory behavior`, `Claw presence vs absence`)


## clean up corresponse between tree and data
require(geiger)
name.check(tree,df)

# delete tips from tree
no_data <- name.check(tree,df)$tree_not_data
tree_reduced <- drop.tip(tree, no_data)

no_tree <- name.check(tree,df)$data_not_tree
data_reduced <- df[!rownames(df) %in% no_tree,]


name.check(tree_reduced, data_reduced)

names(data_reduced)[2] <- "precop"
names(data_reduced)[3] <- "claw"
col.tree <- list(precop=c("#65C6BB","#16A085"), claw=c("#FD9567","#CD4071", "#451077"))
trait.plot(ladderize(tree_reduced), w = 0.1,dat = data_reduced, type = "p", cols = col.tree, str=list(c("no", "yes"), c("no", "yes")), cex.lab = 1.5, cex.legend = 1)



##########

# messing around here

#http://www.zoology.ubc.ca/prog/diversitree/examples/unreplicated/
# phy <- chronopl(tree_reduced, lambda = 0)
# lik1 <- make.mk2(phy, data.frame(data_reduced$claw, row.names = data_reduced$species))
# fit1 <- find.mle(lik1, rep(.1, 2))
# zapsmall(coef(fit1))

#############





library(diversitree)
### need to use DISCRETE?
## see http://www.amjbot.org/content/103/7/1223.full.pdf+html for a recent example
## and / or see here: http://www.zoology.ubc.ca/prog/diversitree/examples/unreplicated/
## first, to visualize, follow the code from here using diverstree : https://github.com/mwpennell/fuse/blob/master/analysis/R/tree-figs.R
# 
# trait.plot(ladderize(phy), dat=dat, cols=col.tree,
#            +            lab=c("Genotype", "Fused"), str=list(c("XY", "ZW"), c("No", "Yes")),
#            +            class=genus.fish, quiet=TRUE, w=1/10, margin=1/1.4, cex.legend = 1)


library(ggtree)
library(phytools)
cols <- palette_blues(4)[c(1,3,4)]
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









