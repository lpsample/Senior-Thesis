#Create Grid and GridBot

#Parameters to control the simulation
simulation.timestep = 0.005
simulation.numoftimesteps = 2000
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

#Update State: Takes data of gen's run in Grid, Gives next Genome
gridBot.UpdateState <- function(currentState,neuralNetOutputs){
  sum.output<- sum(unlist(neuralNetOutputs))
  adj.output <- unlist(neuralNetOutputs)/sum.output
  
  action <- sample(c(moveForward,moveBackward, turnClock, turnCounter),1,  prob=adj.output)
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
    updatedState[[2]]
    }
  
 #Run
  config <- newConfigNEAT(5,4,50,10)
  gridBot <- newNEATSimulation(config, gridBot.InitialState,
                               gridBot.UpdateState,
                               gridBot.ConvertStateToNeuralNetInputs,
                               gridBot.UpdateFitness,
                               gridBot.CheckForTermination,
                               gridBot.PlotState)
  
  nMax <- 2 #Number of generations to run
  for(i in seq(1,nMax)){
    gridBotSimulation <- NEATSimulation.RunSingleGeneration(gridBot)
    #poleSimulation <- NEATSimulation.RunSingleGeneration(poleSimulation,T,"videos",
    #                                            "poleBalance",1/simulation.timestep)
  }
  
  
