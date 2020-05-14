######################## Sufficient Cause Learning #############################
######################## Version 20.05.2019

########## Minor functions ############

#' Function used as part of other functions
#'
#' Function used as part of other functions
#'
#' @param r rows in matrix
#' @param c columns in matrix
#' @param v mean
#' @export

random <- function(r,c,v) {
  w1 <- matrix(NA,nrow = r, ncol = c)
  w1 <- sapply(w1,function(x){rnorm(1,v,.01)})
  w1 <- matrix(w1,nrow = r, ncol = c)
  return(w1)
}

#' Function used as part of other functions
#'
#' Function used as part of other functions
#'
#' @param input input in the relu function
#' @export

relu <- function(input) {
  #  return(ifelse(input<0,0,input))
  return((input>0)*input)
}


########## Wrappers ##############

#' SRCL motivating example
#'
#' To reproduce the synthetic data from the paper Synergistic Cause Learning.
#'
#' @param n number of observations for the synthetic data
#' @export
#' @examples
#' library(SRCL)
#' library(graphics)
#' colours <- c("grey","dodgerblue","red","orange","green")
#'
#' # Data simulation
#' set.seed(12345678)
#' data <- SRCL_0_motivating_example(100) # use 40 000 to replicate the paper
#'
#' # Code data monotonisticly
#' lm(Y~.,data)
#' recode <- lm(Y~.,data)$coefficients<0
#' for (i in 2:ncol(data)) {
#'   if(recode[i]==TRUE) colnames(data)[i] <- paste0("Not_",colnames(data)[i])
#'   if(recode[i]==TRUE) data[,i] = 1 - data[,i]
#' }
#' summary(lm(Y~.,data))
#' exposure_data <- data[,-1]
#' outcome_data <- data[,1]
#'
#' # Model fit
#' model <- SRCL_1_initiate_neural_network(inputs=ncol(exposure_data),hidden=5)
#' for (lr_set in c(0.01,0.001,0.0001,0.00001)) {
#'   model <- SRCL_2_train_neural_network(exposure_data,outcome_data,model,
#'            lr = lr_set, epochs=1000,patience = 10,plot_and_evaluation_frequency = 50)
#' }
#'
#'
#' par(mfrow=c(1,3))
#'
#' # Performance
#' plot(model$train_performance, type='l',yaxs='i', ylab="Mean squared error",
#'            xlab="Epochs",main="Performance")
#'
#' # Model visualisation
#' SRCL_3_plot_neural_network(model,names(exposure_data),5)
#'
#' # AUC
#' library(pROC)
#' pred <- SRCL_4_predict_risks(exposure_data,model)
#' plot(roc(outcome_data,pred),print.auc=TRUE)
#'
#' # Risk contributions
#' r_c <- SRCL_5_layerwise_relevance_propagation(exposure_data,model)
#'
#'
#' # Clustering
#' summary(prcomp(r_c))
#' plot(prcomp(r_c))
#' plot(prcomp(r_c)$x[,1:2])
#' # Depending on the number of influencial principal components
#' # (In this example, the two first principal components are used)
#' d_r_c <- as.data.frame(prcomp(r_c)$x[,1:2])
#' library(fastcluster)
#' p_d <- hclust(dist(d_r_c),"ward.D")
#' groups = 4
#' clus = cutree(p_d, groups)
#' plot(prcomp(r_c)$x[,1:2],col=colours[clus])
#'
#' # Plot results
#' library(robustbase)
#' layout(matrix(c(1,2,3,3,4,4,5,5,6,6,7,7), 3, 4, byrow = TRUE))
#' par(mar=c(4,5,3,0))
#' plot(prcomp(r_c),main="PCA: Proportion of variance")
#' plot(prcomp(r_c)$x[,1:2],pch=16,col=colours[clus],main="PCA: Biplot",frame.plot=FALSE)
#' par(mar=c(4,12,3,4))
#' plot(0,0,type='n',xlim=c(0,1),asp=1,ylim=c(0,1),xaxs='i',yaxs='i',
#' axes=FALSE,ylab="Risk",xlab="Exposed",frame.plot=FALSE,main="Combined plot")
#' axis(1,seq(0,1,.2));axis(2,seq(0,1,.2))
#' rect(0,0,1,1)
#' prev0 = 0; total = 0
#' for (i in 1:groups) {
#'   prev <- sum(clus==i)/length(clus)
#'   risk <- sum(colMedians(as.matrix(r_c[clus==i,])))
#'   rect(xleft = prev0,ybottom = 0,xright = prev+prev0,ytop = risk, col=colours[i])
#'   prev0 = prev + prev0
#' total = total + risk * prev
#' }
#' arrows(x0=0,x1=1,y0=median(r_c$Baseline_risk),lty=1,length=0)
#' par(mar=c(3,10,5,3))
#' for (i in 1:groups) {
#'   prev <- sum(clus==i)/length(clus)
#'   risk <- sum(colMedians(as.matrix(r_c[clus==i,])))
#'   risk_obs <- mean(outcome_data[clus==i])
#'   boxplot(r_c[clus==i,],horizontal = TRUE,las=2, outline = FALSE ,
#'                             range = 0.000000001,col=colours[i])
#'   title(paste0("Elaborated plot - sub-group ",i,"\n",sum(clus==i),
#'             " (prev=",round(prev*100,1),"%, risk=",round(risk*100),
#'             "%,\nexcess=",round(prev*(risk-median(r_c$Baseline_risk))/total*100,1),
#'             "%, observed risk=",round(risk_obs*100),"%)"),col.main=colours[i])
#' }


SRCL_0_motivating_example <- function(n) {
  #n = 20000
  Genes = sample(1:0,n,prob=c(0.05,0.95),replace=TRUE)
  Living_area = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)

  Low_SES = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  Non_smoking = sample(1:0,n,prob=c(0.8,0.2),replace=TRUE)
  for (i in 1:n) {
    if (Low_SES[i] == 1 & sample(1:0,1,prob=c(.2,.8)) ) Non_smoking[i] <- 0
  }

  Mutation_X = rep(0,n)
  for (i in 1:n) {
    if (Genes[i] == 1 & sample(1:0,1,prob=c(.95,.05)) ) Mutation_X[i] <- 1
  }
  LDL = sample(1:0,n,prob=c(0.3,0.7),replace=TRUE)
  for (i in 1:n) {
    if (Genes[i] == 1 & sample(1:0,1,prob=c(.15,.85)) ) LDL[i] <- 1
  }
  Night_shifts = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  for (i in 1:n) {
    if (Living_area[i] == 1 & sample(1:0,1,prob=c(.1,.9)) ) Night_shifts[i] <- 1
  }
  Air_pollution = sample(1:0,n,prob=c(0.2,0.8),replace=TRUE)
  for (i in 1:n) {
    if (Living_area[i] == 1 & sample(1:0,1,prob=c(.3,.7)) ) Air_pollution[i] <- 1
  }

  Y <-  sample(1:0,n,prob=c(0.05,0.95),replace = TRUE)
  for (i in 1:n) {
    if (Non_smoking[i] == 0 & LDL[i] == 1 & Night_shifts[i] == 1 & sample(1:0,1,prob=c(.15,0.85)) ) {
      Y[i] <- 1
    }
    if (Mutation_X[i] == 1 & Air_pollution[i] == 1 & sample(1:0,1,prob=c(.1,0.9)) ) {
      Y[i] <- 1
    }
  }

  #  C = rep(0,n)

  data <- data.frame(Y,Non_smoking,Low_SES,Mutation_X,LDL,Night_shifts,Air_pollution) #,C)
  for (i in 1:ncol(data))   data[,i] <- as.numeric(data[,i])
  return(data)
}



#' Initiates a monotonistc neural network
#'
#' This function initiates a monotonistc neural network. The one-hidden layer monotonistic neural network is designed to resemble a DAG with hidden synergistic components. With the model, we intend to learn the various synergistic interactions between the exposures and outcome. The model needs to be monotonistic and estimate the risk on an additive scale. Neural networks include hidden activation functions (if the sum of the input exceeds a threshold, information is passed on), which can model minimum threshold values of interactions between exposures. We need to specify the upper limit of the number of possible hidden activation functions and through model fitting, the model may be able to learn both stand-alone and synergistically interacting factors.
#'
#' @param inputs The number of exposures.
#' @param hidden Number of hidden nodes.
#' @param confounder Allows to control away a confounder (connected to the output layer)
#' @export
#' @examples
#' #See the example under SRCL_0_motivating_example
#'


SRCL_1_initiate_neural_network <- function(inputs,hidden,confounder=FALSE) {
  # Weight initiation
  w1 <- abs(random(inputs,hidden,0.01))
  b1 <- -abs(random(1,hidden,0.00001))
  w2 <- abs(random(hidden,1,0.01))
  b2 <- abs(random(1,1,0.01))
  if (confounder==TRUE)  c2 <- abs(random(1,1,0.01))
  performance <- NA
  best_epoch <- NA
  epochs <- NA
  if (confounder == FALSE)   return(list(w1,b1,w2,b2,performance,epochs,best_epoch))
  if (confounder == TRUE)   return(list(w1,b1,w2,b2,c2,performance,epochs,best_epoch))
}








#' Training the monotonistic neural network
#'
#' This function trains the monotonistic neural network. Fitting the model is done in a step-wise procedure one individual at a time, where the model estimates individual's risk of the disease outcome, estimates the prediction's residual error and adjusts the model parameters to reduce this error. By iterating through all individuals for multiple epochs (one complete iterations through all individuals is called an epoch), we end with parameters for the model, where the errors are smallest possible for the full population. The model fit follows the linear expectation that synergism is a combined effect larger than the sum of independent effects. The initial values, derivatives, and learning rates are described in further detail in the Supplementary material. The monotonistic model ensures that the predicted value cannot be negative. The model does not prevent estimating probabilities above 1, but this would be unlikely, as risks of disease and mortality even for high risk groups in genereal are far below 1. The use of a test dataset does not seem to assist deciding on the optimal number of epochs possibly due to the constrains due to the monotonicity assumption. We suggest splitting data into a train and test data set, such that findings from the train data set can be confirmed in the test data set before developing hypotheses.
#'
#' @param X The exposure data
#' @param Y The outcome data
#' @param model The fitted monotonistic neural network
#' @param lr Learning rate
#' @param epochs Epochs
#' @param patience The number of epochs allowed without an improvement in performance.
#' @param plot_and_evaluation_frequency The interval for plotting the performance and checking the patience
#' @export
#' @examples
#' #See the example under SRCL_0_motivating_example


SRCL_2_train_neural_network <- function(X, Y, model, lr = 0.01,
                            epochs = 50000, patience = 500,
                            plot_and_evaluation_frequency = 50) {

  performance = model$train_performance
  par(mfrow=c(1,1));par(mar=c(3,5,3,1))
    for(rounds in 1:ceiling(c(epochs/plot_and_evaluation_frequency))) {
      model <- SRCL_cpp_train_network_relu(x=as.matrix(X),y=as.matrix(Y),testx=as.matrix(X),testy=as.matrix(Y),lr = lr, maxepochs  = plot_and_evaluation_frequency, W1_input = model[[1]],B1_input = model[[2]],W2_input = model[[3]],B2_input = model[[4]])
      performance <- c(performance,model$train_performance)
      plot(performance, type='l',yaxs='i' ylab="Mean squared error",
           xlab="Epochs",main="Performance")
      if(length(performance)-which.min(performance)>patience) break
    }
  model$train_performance <- c(performance)
  model$test_performance <- NA
  return(model)
  }




#' Training the monotonistic neural network with a confounder connected to the output layer
#'
#' This function trains the monotonistic neural network with a confounder connected to the output layer. This functions allows one to devide the training process into several steps.
#'
#' @param X The exposure data
#' @param Y The outcome data
#' @param C The confounder data
#' @param model The fitted monotonistic neural network
#' @param lr Learning rate
#' @param epochs Epochs
#' @param patience The number of epochs allowed without an improvement in performance.
#' @param plot_and_evaluation_frequency The interval for plotting the performance and checking the patience
#' @export
#' @examples
#' #See the example under SRCL_0_motivating_example

SRCL_2_train_neural_network_with_confounder <- function(X, Y, C, model, lr = 0.01,
                                      epochs = 50000, patience = 500,
                                      plot_and_evaluation_frequency = 50) {
  C <- as.matrix(C)
  performance = model$train_performance
  par(mfrow=c(1,1));par(mar=c(3,5,3,1))
  for(rounds in 1:ceiling(c(epochs/plot_and_evaluation_frequency))) {
    model <- SRCL_cpp_train_network_relu_with_confounder(as.matrix(X),as.matrix(Y),as.matrix(C),as.matrix(X),as.matrix(Y),as.matrix(C),
           lr = lr, maxepochs  = plot_and_evaluation_frequency, W1_input = model[[1]],B1_input = model[[2]],W2_input = model[[3]],B2_input = model[[4]],C2_input = model[[5]])
    performance <- c(performance,model$train_performance)
    plot(performance, type='l',yaxs='i', ylab="Mean squared error",
         xlab="Epochs",main="Performance")
    if(length(performance)-which.min(performance)>patience) break
  }
  model$train_performance <- c(performance)
  model$test_performance <- NA
  return(model)
}



#' Plotting the monotonistic neural network
#'
#' This function plots the monotonistic neural network
#'
#' @param model The fitted monotonistic neural network
#' @param names Labels of each exposure
#' @param arrow_size defines the arrow_size for the model illustration in the reported training progress.
#' @export
#' @examples
#' #See the example under SRCL_0_motivating_example

SRCL_3_plot_neural_network <- function(model,names,arrow_size = 2) {
  par(mar=c(0,0,2,0))
  plot(0,0,type='n',xlim=c(0,4),ylim=c(-max(nrow(model[[1]]),nrow(model[[3]]))-1,0),axes=FALSE,ylab="",xlab="",main="Model")
  #abline(h=0)
  points(rep(1,nrow(model[[1]])),-c(1:nrow(model[[1]])),cex=10)
  points(rep(2,ncol(model[[1]])),-c(1:ncol(model[[1]])),cex=10)
  points(3,-(ncol(model[[1]])+1)/2,cex=10)
  for (g in 1:nrow(model[[1]])) {
    for (h in 1:ncol(model[[1]])) {
      arrows(x0=1,x1=2,y0=-g,y1=-h,lwd=abs(model[[1]][g,h])*arrow_size,col=ifelse(model[[1]][g,h]>0,"green","white"),length=0)
      #      text(1,-g,round(model[[1]][g,h],2),pos=3)
    }
  }
  for (g in 1:nrow(model[[3]])) {
    arrows(x0=2,x1=3,y0=-g,y1= -(ncol(model[[1]])+1)/2,lwd=abs(model[[3]][g,1])*arrow_size,col=ifelse(model[[3]][g,1]>0,"green","white"),length=0)
    #   text(2,-g,round(model[[3]][g,1],2),pos=3)
  }
  for (i in 1:nrow(model[[1]])) {
    text(rep(1,nrow(model[[1]]))[i],-c(1:nrow(model[[1]]))[i],names[i])
  }
  text(rep(2,ncol(model[[1]])),-c(1:ncol(model[[1]])),paste0("b=",round(model[[2]][1,],2)),pos=1)
  text(3,-(ncol(model[[1]])+1)/2,paste0("B=",round(model[[4]][1,1],2)),pos=1)
  par(mar=c(5.1,4.1,4.1,2.1))
  #  points(3,-(ncol(model[[1]])+1)/2+1,cex=10)
  #  arrows(x0=3,x1=3,y0=-(ncol(model[[1]])+1)/2+1,y1= -(ncol(model[[1]])+1)/2,lwd=abs(model[[5]][1,1])*arrow_size,col=ifelse(model[[5]][1,1]>0,"green","white"),length=0)
}


#' Predict the risk of the outcome using the fitted monotonistic neural network
#'
#' Predict the risk of the outcome using the fitted monotonistic neural network
#'
#' @param X The exposure data
#' @param model The fitted the monotonistic neural network
#' @export
#' @examples
#' #See the example under SRCL_0_motivating_example


SRCL_4_predict_risks <- function(X,model) {
  H <- relu(t(t(as.matrix(X) %*% as.matrix(model[[1]])) + as.vector(model[[2]])))
  o = relu(as.vector(H %*% model[[3]][,1] + as.vector(model[[4]][1,1])))
  return(o)
}



#' Layer-wise relevance propagation of the fitted monotonistic neural network
#'
#' Calculates risk contributions for each exposure and a baseline using layer-wise relevance propagation of the fitted monotonistic neural network and data.
#'
#' @param X The exposure data
#' @param model The fitted the monotonistic neural network
#' @export
#' @examples
#' #See the example under SRCL_0_motivating_example


SRCL_5_layerwise_relevance_propagation <- function(X,model) {
  #model <- model_2_c
  #X = X_flip

  labels <- colnames(X)
  X = as.matrix(X)

  # Forward
  R_X <- matrix(0,ncol=ncol(X),nrow=nrow(X))
  U_B = NULL

  H_all <- relu(t(t(as.matrix(X) %*% as.matrix(model[[1]])) + as.vector(model[[2]])))
  o_all = as.vector(H_all %*% model[[3]][,1] + as.vector(model[[4]][1,1]))

  for (i in 1:nrow(X)) {
    if (i / 1000 == i %/% 1000) {print(i)}
    H <- H_all[i,]
    o <- as.numeric(o_all[i])

    # Layer-wise relevance propagation (LRP)
    Pos1 = model[[3]][,1]
    Pos1 = ifelse(Pos1>0,Pos1,0)
    Pos1_sum = sum(H*Pos1) #+ifelse(model[[5]][1,1] * C[i]>0,model[[5]][1,1] * C[i],0)
    Pos1_sum <- ifelse(is.na(Pos1_sum)|Pos1_sum==0,1,Pos1_sum)
    Pos1 = (H*Pos1)/Pos1_sum

    model[[3]][,1] < 0 # ensure none are below 0


    o <- as.numeric(o * (1-  (relu(model[[4]]) / o))) # Subtracting the first U_B
    R_H = Pos1 * o

    Pos2 = model[[1]]
    Pos2 = ifelse(Pos2>0,Pos2,0)
    model[[1]] < 0 # ensure none are below 0

    for (g in 1:length(H)) {
      Pos2_sum = sum(X[i,]*Pos2[,g])
      Pos2_sum <- ifelse(is.na(Pos2_sum)|Pos2_sum==0,1,Pos2_sum)
      R_X[i,] = R_X[i,] + (X[i,]*Pos2[,g])/Pos2_sum * R_H[g] #+ sum((((X[i,g][X[i,g]>0])*Neg2[g,])/Neg2_sum) * (-b) * R_H)
    }

    U_B[i] <- model[[4]]
    #R_X[i,] <- ((o_all[i]*(1-(U_B[i]/o_all[i])))/sum(R_X[i,]))*R_X[i,]
    if (sum(R_X[i,])==0 | is.na(sum(R_X[i,]))) R_X[i,] = 0
  }


  Baseline_risk <- U_B
  R_X <- data.frame(cbind(R_X,Baseline_risk))
  colnames(R_X) <- c(labels,"Baseline_risk")
  return(R_X)
}
