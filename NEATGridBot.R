#Create Grid and GridBot
library(ggplot2)
library(animation)

source('neat.R')
source('neatCharting.R')
source('neatFormula.R')

#Parameters to control the simulation
simulation.timestep = 0.005
simulation.numoftimesteps = 100
N.moves <-100
N.bots <- 10
N.gen <- 100

# Initial State (1 or 2 is static or dynamic, going to start with static): Initial genomes
# STATE IN THIS VERSION IS WEIGHTS!!! EACH GEN CHANGES
gridBot.InitialState <- function(){
  grids<- makeGrids(1)
  obsGrid <- grids[[1]]
  lightGrid <- grids[[2]]
  state <- list(
    cardinal = c(-1,0),
    obsGrid,
    lightGrid
  )
  #weights <- makeRandWeights()
  return(state)
}


#Convert to NN Inputs
gridBot.ConvertStateToNeuralNetInputs <- function(currentState){
  leftIR<- getLeftIR(currentState)
  rightIR<- getRightIR(currentState)
  leftLight<- getLeftLight(currentState)
  rightLight<- getRightLight(currentState)
  bump<- getBump(currentState)
  
  neuralNetInputs<- c(leftLight,leftIR, bump, rightIR, rightLight) 
  
  
  return (neuralNetInputs)
}

i <- 1
plotState <- F
#Update State: Takes data of gen's run in Grid, Gives next Genome
gridBot.UpdateState <- function(currentState,neuralNetOutputs){
  if(plotState){
    p <- gridBot.PlotState(currentState)
    ggsave(paste0('plots/',i,'.png'), p, device = png())
    i <<- i + 1
  }
  nn.vector <- unlist(neuralNetOutputs)
  sum.output<- sum(nn.vector)
  if(sum.output != 0){
    adj.output <- nn.vector/sum.output
  } else {
    adj.output <- rep(0.25,4)
  }
  
  
  action <- sample(c(moveForward,moveBackward, turnClock, turnCounter),1, prob = adj.output)
  #need to add prob = adj.output back in, it was having errors
  new.state <- action[[1]](currentState)
  
  return (new.state)
}

#Update Fitness
gridBot.UpdateFitness <- function(oldState,updatedState,oldFitness){
  #should this be oldState or updated
  total.light <- oldFitness + getLeftLight(oldState) + getRightLight(oldState)
  return (total.light)
}

#Termination (True or False)
gridBot.CheckForTermination <- function(frameNum,oldState,updatedState,oldFitness,newFitness){
  if(frameNum< N.moves){
  return(FALSE) 
  } else {
      return(TRUE)
    }
}
  
  #Plot
  gridBot.PlotState <-function(updatedState){
    gridVisPlot<-updatedState[[2]]
    df<- as.data.frame(which(gridVisPlot == 1, arr.ind = T))
    df$type <- 1
    df2<- as.data.frame(which(gridVisPlot == 0, arr.ind = T))
    df2$type <- 0
    df.all<- rbind(df, df2)
    df.all$type <- factor(df.all$type)
    p <- ggplot(df.all, aes(x=col, y=row, shape = type))+
      scale_shape_manual(values = c(21,22), guide=F)+
      geom_point(size=5)+
      scale_y_reverse()+
      theme_minimal()+
      theme(axis.text = element_blank(),
            panel.grid=element_blank(),
            axis.title= element_blank())
    return(p)
    #google r shapes
    }
  
 #Run
  set.seed(12604)
  config <- newConfigNEAT(5,4,50,10)
  gridBot <- newNEATSimulation(config, gridBot.InitialState,
                               gridBot.UpdateState,
                               gridBot.ConvertStateToNeuralNetInputs,
                               gridBot.UpdateFitness,
                               gridBot.CheckForTermination,
                               gridBot.PlotState)
  
  nMax <- 10 #Number of generations to run
  for(i in seq(1,nMax)){
    gridBot <- NEATSimulation.RunSingleGeneration(gridBot)
    #poleSimulation <- NEATSimulation.RunSingleGeneration(poleSimulation,T,"videos",
    #                                            "poleBalance",1/simulation.timestep)
  }
  
  

#drawGenotypeNEAT(gridBot, config, 100,100)
  
library(igraph)
  
# drawGenotypeNEAT.genome(gridBot$Pool$currentGenome,config,topLeftX=0,topLeftY=0)
# drawPhenotypeNEAT.genome(gridBot$Pool$currentGenome,config,topLeftX,topLeftY) #what are topLeft X and Y
# createGraph(gridBot$Pool$currentGenome,config)
# drawNEAT.genome(gridBot$Pool$currentGenome,config)
# 
# drawGenotypeNEAT.genome(gridBot$Pool$species[[1]]$genomes[[1]], config, topLeftX = 0, topLeftY = 0)

drawNEAT(gridBot$Pool$species[[1]]$genomes[[1]], config)

<<<<<<< HEAD
gridBot$Pool

connectivity <- matrix(0, nrow=54, ncol=54)
library(purrr)
connections <- gridBot$Pool$species[[1]]$genomes[[1]]$ConnectionGenes

map(connections, function(x){
  print(x)
  from <- x$InNode
  to <- x$OutNode
  w <- x$Weight
  print(from)
  print(to)
  print(w)
  connectivity[from,to] <<- w
})

which(connectivity != 0)

g <- igraph::graph_from_adjacency_matrix(connectivity)

?modularity_matrix()
modularity(g)
=======
gridBot$PerformanceTracker


### try to run one agent

plotState <- T
simulationRunner(gridBot, 1, 1, F, 100, 1)
<<<<<<< HEAD
>>>>>>> 6fb7b3b1ae50b754bda8fff7c1d6f68692552b8d
=======
>>>>>>> 6fb7b3b1ae50b754bda8fff7c1d6f68692552b8d
