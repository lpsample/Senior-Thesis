library(dplyr)
library(ggplot2)
library(animation)
library(igraph)

source('neat.R')
source('neatCharting.R')
source('neatFormula.R')

N.moves <-100
N.bots <- 10
N.gen <- 100
plotState <- F
plot.num <- 1

set.seed(12604)

do.static.run<- function(){
  static.run<- run(1)

static.data.ann<- static.run$gen.history%>%
  group_by(generation)%>%
  summarise(minFitness = min(fitness), maxFitness = max(fitness), meanFitness=mean(fitness), medianFitness = median(fitness))

write.csv(static.data.ann,"finalists/ann_static.csv", row.names = T)
return(static.run)
}

static.ann<- do.static.run()

plotState <- T
runBot(static.ann$best.agent.weights,1)


#=====================================
#dynamic.run<- run(2)

do.dynamic.run<- function(){
  dynamic.run<- run(2)
  
 
  dynamic.data.ann<- dynamic.run$gen.history%>%
    group_by(generation)%>%
    summarise(minFitness = min(fitness), maxFitness = max(fitness), meanFitness=mean(fitness), medianFitness = median(fitness))
  
  write.csv(dynamic.data.ann,"finalists/ann_dynamic.csv", row.names = T)
  
  return(dynamic.run)
}


dynamic.ann<- do.dynamic.run()
dynamic.ann$best.agent.weights
plotState <- T
runBot(dynamic.ann$best.agent.weights,2)

#dynamic.run
x<-0
while (x<63){
  dev.off()
  x<-x+1
}
