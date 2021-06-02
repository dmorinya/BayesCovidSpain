#########################################
##     Correcting Under-Reporting      ##
##              Functions              ##
#########################################

# Function to simulate Intrinsic Conditional Autoregressive Fields
# by Devin Johnson (not associated with authors of paper).
ricar_simple <- function(Q){
  v <- eigen(Q, TRUE)
  val.inv <- sqrt(ifelse(v$values>sqrt(.Machine$double.eps), 1/v$values, 0))
  P <- v$vectors
  sim <- P%*%diag(val.inv)%*%rnorm(dim(Q)[1], 0, 1)
  X <- rep(1,length(sim))
  if(sum(val.inv==0)==2) X <- cbind(X, 1:length(sim))
  sim <- sim-X%*%solve(crossprod(X), crossprod(X,sim))
  return(sim)
}

# Rewritten as the above function crashes R if used in a for loop.
ricar <- nimbleFunction(run=function(Q=double(2),epsilon=double(0)){
  v <- nimEigen(Q, symmetric=TRUE)
  v.inv<-numeric(length(v$values))
  index <- v$values>sqrt(epsilon)
  v.inv[index] <- sqrt(1/v$values[index])
  v.inv[!index] <- 0
  P <- v$vectors
  sim <- P%*%diag(v.inv)%*%rnorm(dim(Q)[1], 0, 1)
  if(sum(v.inv==0)==2){
    X=matrix(nrow=length(sim),ncol=2)
    X[,1]<- rep(1,length(sim))
    X[,2] <- 1:length(sim)
    sim <- sim-X%*%solve(X%*%X,X%*%sim)
  }else{
    Y<- rep(1,length(sim))
    sim <- sim-Y%*%solve(Y%*%Y,Y%*%sim)
  }
  returnType(double(2))
  return(sim)
})

ricar_compiled <- compileNimble(ricar)

# This function was found online at:
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

tran_var=function(x){(x-mean(x))/sd(x)}
