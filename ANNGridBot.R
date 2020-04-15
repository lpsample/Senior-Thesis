library(dplyr)

N.moves <-100
N.bots <- 10
N.gen <- 100
plotState <- F
plot.num <- 1

set.seed(12604)

do.static.run<- function(){
  static.run<- run(1)
  
  plotState <- T
  runBot(static.run$best.agent.weights,1)

static.data.ann<- static.run$gen.history%>%
  group_by(generation)%>%
  summarise(minFitness = min(fitness), maxFitness = max(fitness), meanFitness=mean(fitness), medianFitness = median(fitness))

write.csv(static.data.ann,"finalists/ann_static.csv", row.names = T)
return(static.run)
}

do.static.run()


#dynamic.run<- run(2)

static.run

#dynamic.run
x<-0
while (x<63){
  dev.off()
  x<-x+1
}
