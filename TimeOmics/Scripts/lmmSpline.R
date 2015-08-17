
other.reshape <- function(Rep, Time, Data){
  lme.data<-NULL
  lme.data <- data.frame(Time=Time,Rep=Rep,as.matrix(Data))
  lme.data$Time = factor(drop.levels(lme.data$Time))
  lme.data$Rep = factor(drop.levels(lme.data$Rep))
  melt.lme.data <-NULL
  melt.lme.data <- melt(lme.data)
  cast.lme.data  <- NULL
  cast.lme.data <- dcast(melt.lme.data, variable+ Rep ~ Time)
  melt.lme.data2 <- NULL
  melt.lme.data2 <-  melt(data.frame(cast.lme.data))
  names(melt.lme.data2) <- c("Molecule",  "Rep", "Time", "Expr")
  melt.lme.data2$Time <- factor(gsub("^X", "", as.character(melt.lme.data2$Time)))
  
  return(as.data.frame(melt.lme.data2))
}

lmmSpline <- function(data, time, sampleID, timePredict, deri, basis, knots,keepModels, numCores){
  library(reshape2)
  library(nlme)
  library(lmeSplines)
  library(parallel)
  library(gdata)
  if(missing(keepModels))
    keepModels <- F
  if(missing(timePredict))
    timePredict <- sort(unique(time))
  if(missing(basis))
    basis <- "cubic"
  
  if(missing(deri)){
    deri <- FALSE
  }else{
    deri <- deri
  }
  
  basis.collection <-  c("cubic","p-spline","cubic p-spline")
  if(!basis%in% basis.collection)
    stop(cat("Chosen basis is not available. Choose:", paste(basis.collection,collapse=', ')))
  if(diff(range(c(length(sampleID),length(time),nrow(data))))>0)
    stop("Size of the input vectors rep, time and nrow(data) are not equal")
  if(missing(knots)& (basis=="p-spline"|basis=='cubic p-spline'))
    warning("The number of knots is automatically estimated")
  if(deri & basis=='cubic')
    stop('To calculate the derivative choose either "p-spline" or "cubic p-spline" as basis')
  
  options(show.error.messages = TRUE) 
  
  i <- NULL
  fits <- NULL
  error <- NULL
  
  if(missing(numCores)){
    num.Cores <- detectCores()
  }else{
    num.Cores <- detectCores()
    if(num.Cores<numCores){
      warning(paste('The number of cores is bigger than the number of detected cores. Using the number of detected cores',num.Cores,'instead.'))
    }else{
      num.Cores <- numCores
    }
    
  }
  Molecule <- ''
  
  derivLme <- function(fit){ 
    #random slopes
    
    if(class(fit)=='lm'){
      beta.hat <- rep(fit$coefficients[2],length(unique(fit$model$time)))
      return(beta.hat)
      
    }else if(class(fit)=='lme'){
      u <- unlist(fit$coefficients$random$all)
      beta.hat <- fit$coefficients$fixed[2]
      Zt <-  fit$data$Zt[!duplicated(fit$data$Zt),]>0
      deriv.all <-    beta.hat + rowSums(Zt%*%t(u)) 
      return(deriv.all)
    }
  }
  
  #penalized cubic
  
  derivLmeCubic <- function(fit){ 
    #random slopes
    if(class(fit)=='lm'){
      beta.hat <- rep(fit$coefficients[2],length(unique(fit$model$time)))
      return(beta.hat)
      
    }else if(class(fit)=='lme'){
      u <- unlist(fit$coefficients$random$all)
      beta.hat <- fit$coefficients$fixed[2]
      PZ <-  fit$data$Zt[!duplicated(fit$data$Zt),]
      PZ <-PZ^(1/3)
      deriv.all <-    beta.hat + rowSums((PZ*PZ)%*%(t(u)*3)) 
      return(deriv.all)
    }
    
  }
  
  if(missing(knots))
    knots <-NULL
  nMolecules <- NULL
  nMolecules <- ncol(data)
  
  
  lme <- nlme::lme
  cl <- makeCluster(num.Cores,"SOCK")
  clusterExport(cl, list('data','lm','try','class','unique','anova','drop.levels','pdDiag','time','sampleID','melt','dcast','predict','derivLme','knots','derivLmeCubic','lme','keepModels','basis','data','other.reshape'),envir=environment())
  
  models <-list()
  
  
  new.data <- parLapply(cl,1:nMolecules,fun = function(i){
    
    
    
    
    
    expr <- data[,i]
    
    dataM <- as.data.frame(other.reshape(Rep=sampleID,Time=time,Data=unlist(expr)))
    dataM$all = rep(1, nrow(dataM))
    dataM$time = as.numeric(as.character(dataM$Time))
    dataM$Expr = as.numeric(as.character(dataM$Expr))
    
    
    #### CUBIC SPLINE BASIS ####
    if(basis=="cubic"){
      dataM$Zt <- lmeSplines::smspline(~ time, data=dataM)
      knots <- sort(unique(time))[2:(length(unique(time))-1)]
    }
    #### PENALIZED SPLINE BASIS#####
    if(basis%in%c("p-spline","cubic p-spline")){
      
      if(is.null(knots)){
        K <- max(6,min(floor(length(unique(dataM$time))/4),40))
      }else{
        K <- max(knots,6)
      }
      knots <- quantile(unique(dataM$time),seq(0,1,length=K+2))[-c(1,K+2)]
      if(min(knots)<=min(dataM$time) | max(knots)>=max(dataM$time))
        stop(cat('Make sure the knots are within the time range',range(dataM$time)[1],'to',range(dataM$time)[2]))
      PZ <- outer(dataM$time,knots,"-")
      if(basis=="cubic p-spline")
        PZ <- PZ^3
      PZ <- PZ *(PZ>0)
      dataM$Zt <- PZ 
      
    }
    
    
    
    if(deri){
      pred.spline = rep(NA,length(timePredict))
    }else{
      pred.spline =rep(NA,length(timePredict))
      pred.df <- data.frame(all=rep(1,length(timePredict)), time=timePredict)
      pred.df$Zt = lmeSplines::approx.Z(dataM$Zt, dataM$time, timePredict)
      
    }

    
    #library(nlme)
    fit0 <- NULL
    
    fit0  <- try(lm(Expr ~ time, data=dataM ))
    if(class(fit0) == 'try-error') { 
      error <- c(error,i)
    }
    fit1 <- NULL
    fit1 <- try(lme(Expr ~ time, data=dataM, random=list(all=pdIdent(~Zt - 1)),
                    na.action=na.exclude, control=lmeControl(opt = "optim"))) 
    pvalue <-1
    if(class(fit1) != 'try-error') { 
      
      pvalue <- anova(fit1, fit0)$'p-value'[2]
      
    }
    
    if(pvalue <= 0.05){  
      
      fit2 <- NULL
      fit2 <- try(lme(Expr ~ time, data=dataM, 
                      random=list(all=pdIdent(~Zt - 1), Rep=pdIdent(~1)), 
                      na.action=na.exclude, control=lmeControl(opt = "optim")))
      
      if(class(fit2) != 'try-error') {  # to prevent errors stopping the loop
        
        pvalue = anova(fit1, fit2)$'p-value'[2]
      }else{ 
        pvalue=1
      }
      
      if(pvalue <= 0.05){  
        fit3 <-NULL
        fit3 <- try(lme(Expr ~ time, data=dataM, 
                        random=list(all=pdIdent(~Zt - 1), Rep=pdDiag(~time)), 
                        na.action=na.exclude, control=lmeControl(opt = "optim")))  
        
        if(class(fit3) != 'try-error') {  # to prevent errors stopping the loop
          pvalue = anova(fit2, fit3)$'p-value'[2]
          
        }else{
          pvalue=1
        }
        if(pvalue <= 0.05){
          fits <- 3
          models<- fit3
          if(deri){
            if(basis=='p-spline')
              pred.spline = derivLme(fit3)
            if(basis=='cubic p-spline')
              pred.spline = derivLmeCubic(fit3)
            
          }else{
            pred.spline = predict(fit3, newdata=pred.df, level=1, na.action=na.exclude)
          }
        }else{ # choose simpler model: fit2
          fits <- 2
          models <- fit2
          if(deri){
            if(basis=='p-spline')
              pred.spline = derivLme(fit2)
            if(basis=='cubic p-spline')
              pred.spline = derivLmeCubic(fit2)
          }else{
            pred.spline = predict(fit2, newdata=pred.df, level=1, na.action=na.exclude)
          }
        } 
        
      }else{ 
        models <- fit1
        fits <- 1
        if(deri){
          if(basis=='p-spline')
            pred.spline = derivLme(fit1)
          if(basis=='cubic p-spline')
            pred.spline = derivLmeCubic(fit1)
        }else{
          pred.spline = predict(fit1, newdata=pred.df, level=1, na.action=na.exclude)
        }
      }
    }else{ 
      
      models <- fit0
      fits <-0
      if(deri){
        pred.spline = rep(fit0$coefficients[2],length(unique(dataM$time)))    
      }else{
        
        pred.spline = predict(fit0, newdata=pred.df, level=1, na.action=na.exclude)
      }
    }
    
    if(!keepModels)
      keepModels <- list()
    return(list(pred.spl=pred.spline,fit=fits,models=models,error=error,knots=knots))
    
  })
  stopCluster(cl)
  knots <- sort(unique(as.vector((sapply(new.data,'[[','knots')))))
  pred.spl <- matrix(sapply(new.data,'[[','pred.spl'),nrow=nMolecules,byrow=T)
  fits <-  unlist(sapply(new.data,'[[','fit'))
  error <-  unlist(sapply(new.data,'[[','error'))
  models <-list()
  if(keepModels){
    models <- sapply(new.data,'[[','models')
    
    if(is.matrix(models))
      models <- sapply(new.data,'[','models')
  }
  
  pred.spl = as.data.frame(pred.spl)
  MolNames <- as.character(unlist(colnames(data)))
  
  if(is.null(MolNames)| sum(is.na(MolNames))>0)
    MolNames <- 1:nrow(pred.spl)
  if(nrow(pred.spl)==length(MolNames))
    rownames(pred.spl)<-MolNames
  if(ncol(pred.spl)==length(timePredict))
    colnames(pred.spl) <- timePredict
  error2 <- "All features were modelled"
  if(length(error)>0){
    warning('The following features could not be fitted',paste(MolNames[error],' ',sep='\n'))
    error2 <- c()
    error2 <- rownames(pred.spline)[error]
    pred.spline <- pred.spline[-error,]
    
  }
  
  l <-list('lmmspline',predSpline=pred.spl,modelsUsed=fits,basis=basis,knots=knots,errorMolecules=error2,models=models, derivative=deri)
  return(l)
  
}
