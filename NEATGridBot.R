#Create Grid and GridBot

#Parameters to control the simulation
simulation.timestep = 0.005
simulation.numoftimesteps = 2000
N.moves <-100
N.bots <- 10
N.gen <- 100


run<- function(x){
  #x<-1
  genomes<- createGeneration(N.bots)
  gen<- runGeneration(genomes, x)
  
  for (a in 2:N.gen) {
   # print(a)
    genomes<- next.genomes(gen, genomes)
    for (i in 1:length(genomes)) {
      genomes[[i]]<- mutate(genomes[[i]])
    }
    gen <- runGeneration(genomes, x)
  }
  top.scorer <- as.numeric(which.max(gen$fitness))
  final.weights<-genomes[[top.scorer]]
  return(final.weights)
}
static.run<- run(1)
dynamic.run<- run(2)

# Initial State (1 or 2 is static or dynamic, going to start with static): Initial genomes
# STATE IN THIS VERSION IS WEIGHTS!!! EACH GEN CHANGES
gridBot.InitialState <- function(){
  genomes<- createGeneration(N.bots)
  gen<- runGeneration(genomes, x)
  return(gen)
}

#Convert to NN Inputs
gridBot.ConvertStateToNeuralNetInputs <- function(currentGen){
  return (currentGen)
}

#Update State: Takes data of gen's run in Grid, Gives next Genome
gridBot.UpdateState <- function(currentState,neuralNetOutputs){
  
  
  return (currentState)
}

#Update Fitness
gridBot.UpdateFitness <- function(oldState,updatedState,oldFitness){
  return (oldFitness+(heightFitness + heightFitness*centerFitness))
}

#Termination (True or False)
gridBot.CheckForTermination <- function(frameNum,oldState,updatedState,oldFitness,newFitness){

  return(FALSE)  

  
  #Plot
  poleBalance.PlotState <-function(updatedState){
    cart.centerX <- updatedState[[1]]
    cart.centerXDot <- updatedState[[2]]
    cart.centerXDotDot <- updatedState[[3]]
    cart.force <- updatedState[[4]]
    pole.theta <- updatedState[[5]]
    pole.thetaDot <- updatedState[[6]]
    pole.thetaDotDot <- updatedState[[7]]
    
    createSceneFunc(scene.bottomLeftX,scene.bottomLeftY,scene.width,scene.height,
                    main="Simulation of Inverted Pendulum - www.gekkoquant.com",xlab="",
                    ylab="",xlim=c(-0.5*scene.width,0.5*scene.width),
                    ylim=c(-0.5*scene.height,0.5*scene.height))
    
    createBoxFunc(track.x,track.y,track.limit*2,track.height,track.colour)
    createBoxFunc(leftBuffer.x,leftBuffer.y,leftBuffer.width,leftBuffer.height,leftBuffer.colour)
    createBoxFunc(rightBuffer.x,rightBuffer.y,rightBuffer.width,
                  rightBuffer.height,rightBuffer.colour)
    createBoxFunc(cart.centerX-0.5*cart.width,cart.centerY+0.5*cart.height,cart.width,cart.height,
                  cart.colour)
    drawPoleFunc(cart.centerX,cart.centerY,2*pole.length,pole.theta,pole.colour)
    drawPendulum(cart.centerX,cart.centerY,2*pole.length,pole.theta,pendulum.radius,pendulum.colour)
    
  }
  
 #Run
  config <- newConfigNEAT(7,1,500,50)
  poleSimulation <- newNEATSimulation(config, poleBalance.InitialState,
                                      poleBalance.UpdatePoleState,
                                      poleBalance.ConvertStateToNeuralNetInputs,
                                      poleBalance.UpdateFitness,
                                      poleBalance.CheckForTermination,
                                      poleBalance.PlotState)
  
  nMax <- 1 #Number of generations to run
  for(i in seq(1,nMax)){
    poleSimulation <- NEATSimulation.RunSingleGeneration(poleSimulation)
    #poleSimulation <- NEATSimulation.RunSingleGeneration(poleSimulation,T,"videos",
    #                                            "poleBalance",1/simulation.timestep)
  }
  
  
