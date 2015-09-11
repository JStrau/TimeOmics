# 
# Copyright (C) <2015>  <Jasmin Straube>
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>


library(shiny)
library(cluster)
library(kohonen)
library(Rmixmod)
library(mclust)
library(ggplot2)
library(googleVis)
library(lmms)

ymax <- NULL
ymin <- NULL
ymin.fc <- NULL
ymax.fc <- NULL
selDataTableOutput <- NULL
data <- NULL
path <- ""
group.path <-""
time.path <- ""
rep.path <- ""
annot.path <- ""
sep <- ""
header <- ""
current_group <- ""
group <-NULL
time <-NULL
annot <-NULL
rep <-NULL
lmm <- NULL
indexFinal <- NULL
lmm.de <- NULL
ce <- NULL
v <-"m"
data2 <- NULL
num <-0
core <-T
class <- NULL
enrich <- NULL
classifi <- NULL
f.slider <-NULL
m.slider<-NULL
fc2 <- NULL
numMis <- NULL
df <- NULL
changedPath.Exp <- F
log <- TRUE
investNoiseData1 <- investNoiseData2 <- NULL

plot <-grSelDE<- hcChanged <- kmChanged <-somChanged <-modelChanged <-ClRangeChanged1 <- ClRangeChanged2 <-matrixChanged<-pamChanged <-0

shinyServer(function(input, output,session) {
ExampleExp <- NULL
########UPLOAD FUNCTIONS################
 
ExpData <- reactive({

    inFile <- input$ExpData
    
    withProgress(message = 'Uploading data', value = 0.1, {
    if (is.null(inFile))
      return(list(data=NULL,changedPath=F))
    
    changedPath.Exp <- inFile$datapath!=path 
    
  if(is.null(data) | changedPath.Exp | header!=input$header | sep!=input$sep| !is.null(input$AnnotData)){
    ExampleData <- NULL
    lmm <<- NULL

    data <<- read.csv(file=inFile$datapath, header=input$header, sep=input$sep)

    indexFinal <<- rep(TRUE,ncol(data))
    
    path <<- inFile$datapath
    header <<-input$header
    sep <<-input$sep
  if(is.null(GroupData()$data)){
   group <<- rep("1",nrow(data)) 
  }
    annotation <- as.character(unlist(AnnotData()))
    
    if(!is.null(annotation)){

      colnames(data) <- annotation
    }
        
    }
  })

    return(list(data=data,changedPath=changedPath.Exp))
  })
 
 ##### UPLOAD GROUP DATA #######
 GroupData <- reactive({
   
   inFile <- input$GroupData
   
   if (is.null(inFile) ){
     return(list(data=NULL,changedPath=F))
   }
   changedPath.group <- group.path!=inFile$datapath
   if(is.null(group)| changedPath.group){
     group <<- NULL
     group <<- as.character(unlist(read.csv(file=inFile$datapath,header=F)))
     group.path <<- inFile$datapath
     return(list(data=group,changedPath=changedPath.group))
   }

 })

##### UPLOAD TIME DATA #######
 
TimeData <- reactive({
   
   inFile <- input$TimeData
   
   if(is.null(inFile))
     return(list(data=NULL,changedPath=F))
   
   changedPath.time<- time.path!=inFile$datapath
   
   if(is.null(time)|changedPath.time){
     print("Upload time data")
     time <<- unlist(read.csv(file=inFile$datapath,header=F))
     time.path <<- inFile$datapath
   }
   return(list(data=time,changedPath=changedPath.time))
 })

##### UPLOAD SAMPLE ID DATA #######
 
RepData <- reactive({
   
   inFile <- input$ReplicateData
   
   if (is.null(inFile))
     return(list(data=NULL,changedPath=F))
   
   changedPath.rep  <- rep.path!=inFile$datapath
   if(is.null(rep)|changedPath.rep){
     print("Upload sampleID data")
     rep <<- unlist(read.csv(file=inFile$datapath,header=F))
     rep.path <<- inFile$datapath
   }
   return(list(data=rep,changedPath=changedPath.rep))
 })

##### UPLOAD ANNOTATION DATA #######

AnnotData <- reactive({
  
  inFile <- input$AnnotData
  
  if (is.null(inFile)){
    return(NULL)
  }
  
  if(is.null(annot)|annot.path!=inFile$datapath){
    annot <<- unlist(read.csv(file=inFile$datapath,header=F))
    annot.path <<- inFile$datapath
  }
  annot
})

##### TRACK CHANGED file path #######
changedPath <- reactive({
  if(!is.null(ExpData()$data) & !is.null(TimeData()$data) & !is.null(GroupData()$data) & !is.null(RepData()$data)){
    if(TimeData()$changedPath | GroupData()$changedPath| ExpData()$changedPath | RepData()$changedPath){

      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }
  
})

### LOAD EXAMPLE DATA ON BUTTON RUNEXAMPLE####

Example <- observe({
 
  if(input$RunExample & is.null(ExampleExp)){
   # print(getwd())
    updateTabsetPanel(session, 'Tabs', selected = 'Upload')
    load('ExampleData/Example.RData')
    ExampleExp <<- ExampleExp
    ExampleGroup <<- ExampleGroup
    ExampleTime <<- ExampleTime
    ExampleSample <<- ExampleSample
  
    indexFinal <<- rep(TRUE,ncol(ExampleExp))
    
  }

  lmm.de <<- NULL
  lmm <<- NULL
})

  output$HistPlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    ExpData <- ExpData()$data
    
    if(input$RunExample){
      #Example()
      ExpData <- ExampleExp
    }
    
    if(!is.null(ExpData)){
     
      group <<-NULL
    
      if(is.null(GroupData()$data) & !input$RunExample){
        group <<- rep(1,nrow(ExpData))
      }else{
        if(input$RunExample)group<<- as.character(unlist(ExampleGroup))else group<<- as.character(GroupData()$data)
        g <- input$GroupsSel
        if(is.null(g))
          g <- unique(na.omit(group))[1]
        
         index.g <- group%in%g
         ExpData <- ExpData[index.g,]
         group <<- group[index.g]
      }

      mes <- data.frame(Expression=as.vector(unlist(ExpData)),Group=as.factor(rep(group,ncol(ExpData))))
   
      #density plot
      #m <- qplot(Expression, data=mes, geom='density',fill="red") + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())
    if(input$dens){
        m <- qplot(Expression, color=Group, data=mes, geom='density', alpha=0.8)+  theme_bw()# +  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())#+ scale_fill_brewer(palette="Dark2") 
      }else{
        m <- ggplot(mes, aes(x=Expression,color=Group,fill=Group)) 
        #m <- m + geom_histogram(aes(fill = ..count..)) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) 
        m <- m + geom_histogram(alpha=0.8) + theme_bw() #theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) #+ scale_fill_brewer(palette="Dark2") 
      }
      print(m)

    }
  })
  
  
  

obsB <- observe({
  print('observe')
  input$GroupsSel

})


output$Boxplot <- renderPlot({

  ExpData <- ExpData()$data

  if(input$RunExample){
   # Example()
    ExpData <- ExampleExp
  }
  
  if(!is.null(ExpData)){
    numberRow <- nrow(ExpData)
    numberCol <- ncol(ExpData)
    if(is.null(GroupData()$data) & !input$RunExample){
      group <<- factor(1:numberRow)
    }else{
      if(input$RunExample)group<<- as.character(unlist(ExampleGroup))else group<<- as.character(GroupData()$data)
      g <- input$GroupsSel
      if(is.null(g))
        g <- unique(na.omit(group))[1]
      
      index.g <- group%in%g
      ExpData <- ExpData[index.g,]
      group <- group[index.g]
    }
  mes <- data.frame(Expression=as.vector(unlist(ExpData)),Group=factor(rep(group,each=ncol(ExpData))),Sample=as.factor(rep((1:sum(index.g)),each=ncol(ExpData))))

  geom <- 'boxplot'
  main <-"Boxplot of the samples"
  if(input$densSample){
    geom <-'density'
    main <-"Density of the samples"

    q <-qplot(Expression,group=Sample,color=Group,geom =geom ,data=mes)+theme_bw()+ theme(legend.position="none")#theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),legend.position="none")#+ scale_fill_brewer(palette="Dark2") 
    
  }else{
    q <-qplot(y=Expression,x=Sample,group=Sample,main=main,fill=Group,geom =geom ,data=mes)+theme_bw()+ theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1))#theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),legend.position="none")#+ scale_fill_brewer(palette="Dark2") 
    
  }
   print(q)
  }
})

#output$BoxplotGvis <- renderGvis({
  
  # generate an rnorm distribution and plot it
  
#  ExpData <- ExpData()$data
#  if(!is.null(ExpData)){
#    numberRow <- dim(ExpData)[1]
    #matri <- matrix(ExpData,nrow=numberRow[1])
 
#    b <- boxplot(t(ExpData),main="Boxplot of the samples",col=rainbow(numberRow))$stats
#    df.b <- data.frame(low=b[1,],open=b[2,],time=c(1:ncol(b)),close=b[4,],high=b[5,])
#    g <- gvisCandlestickChart(df.b, xvar="time", low="low", open="open", close="close",high="high",options=list(legend='none'))
##    g
#  }
#})



#####################  FILTER FUNCTIONS  #################


output$result <- renderText( {
  g <- input$GroupsSel
  
  group <- GroupData()$data
  if(is.null(group) & is.null(g))
    return('Please upload a data set or tick the `Run example` checkbox in the `Example and Help` tab.')
  
  if(input$RunExample){
   # Example()
    group <- as.character(unlist(ExampleGroup))
  }
  ##There always needs to be one group selected
  if(length(g)>2|length(g)==0){

    cb_options <- list()
    group <- unique(na.omit(group))
    for(x in group){
      cb_options[[x]] <- x
    }
    updateCheckboxGroupInput(session,"GroupsSel",choices=cb_options,selected=cb_options[[1]],inline = T)
  }
  paste('Analysis is going to be performed on group/s:',paste(unlist(input$GroupsSel),collapse = ' and '),collapse='')

 
})


output$Group_Checkbox <- renderUI({
  group <- GroupData()$data
  if(input$RunExample){
  #  Example()
    group <- as.character(unlist(ExampleGroup))
  }
    
  if(is.null(group))
    return()
  group <- as.character(unique(na.omit(group)))
  cb_options <- list()
  for(x in group){
  cb_options[[x]] <- x
  }

  text <-'Detected group'
  if(length(group)>1)
    text <- "Select 1 or 2 of the detected groups"
  checkboxGroupInput("GroupsSel", text,
               choices=cb_options,selected=cb_options[[1]],inline = T)
   
  
})




noiseDatanew <- reactive({
  withProgress(message = 'Filtering in progress', value = 0.1, {
  ExpData <- ExpData()$data
  time <- TimeData()$data
  group <- as.character(GroupData()$data)
  replicate <- RepData()$data
  logfc <- input$logfc
 ## f <- FALSE
#  m <- FALSE
  summaMis <- summaFC <- summa <-''
  if(input$RunExample){
   # Example()
    ExpData <- ExampleExp
    time <- unlist(ExampleTime)
    group <- as.character(unlist(ExampleGroup))
    replicate <- unlist(ExampleSample)
  }
  
  if((!is.null(ExpData) & !is.null(time) & !is.null(group) & !is.null(replicate)) | changedPath()|logfc!=log){

      indexFilterFC <- indexFilterMiss <- indexFilterRatios <- rep(TRUE, ncol(ExpData))

    grp <- input$GroupsSel
    print(grp)
    
    #### Check if group selection changed
    equal_gr <- FALSE
    ########## if two groups what group is selected #####
    current_filter_input <- input$filter_gr
    
    if(length(current_filter_input)==0|is.null(current_filter_input)){
      equal_gr <- FALSE
      current_filter_input <- grp[1]
    }else{
      current_filter_input<- as.character(current_filter_input )
      equal_gr <- current_filter_input!=current_group
    }
    gr <- 1:length(unlist(time))
    if(length(current_filter_input)!=0){
      gr <- gr[group==input$filter_gr]
      current_group <<- current_filter_input
    }

    ### two groups calculate RT and RI for each group ####
    gr1 <- which(group==grp[1])
    print(gr1)
    if(is.null(investNoiseData1)){
      investNoiseData1 <<-investNoise(data = ExpData[gr1,],time =time[gr1] ,sampleID=replicate[gr1])
    }
    index.na1 <- which(!is.na(investNoiseData1@RT)&!is.na(investNoiseData1@RI)&!is.infinite(investNoiseData1@RI)&!is.infinite(investNoiseData1@RT))
    
    if(length(grp)==2){

      if(is.null(investNoiseData2)){
        gr2 <- which(group==grp[2])
      investNoiseData2 <<-investNoise(data = ExpData[gr2,],time =time[gr2] ,sampleID=replicate[gr2])
      }
      
      index.na2 <- which(!is.na(investNoiseData2@RT)&!is.na(investNoiseData2@RI)&!is.infinite(investNoiseData2@RI)&!is.infinite(investNoiseData2@RT))
      
      if(input$numMissingUsed){
        misdata <- input$miss
  
        indexFilterMiss[investNoiseData2@propMissing>=misdata|investNoiseData1@propMissing>=misdata] <- FALSE
        summaMis<- paste(sum(indexFilterFC,na.rm=T),' molecules remaining \n after filtering using a proportion of missing values threshold of',misdata,'.', sep=" ")
        
      }
      #### is filter by FC selected #####
      if(input$fcUsed){
        fcfilt <- input$fcs
        indexFilterFC[investNoiseData2@foldChange<=fcfilt|investNoiseData1@foldChange<=fcfilt] <- FALSE
        summaFC<- paste(sum(indexFilterFC,na.rm=T),' molecules remaining \n after filtering using a fold change threshold of',fcfilt,'.', sep=" ")
      }
    }else{
      if(input$numMissingUsed){
        misdata <- input$miss
        indexFilterMiss[investNoiseData1@propMissing>=misdata] <- FALSE
           summaMis<- paste(sum(indexFilterFC,na.rm=T),' molecules remaining \n after filtering using a proportion of missing values threshold of',misdata,'.', sep=" ")
        
        
      }
      if(input$fcUsed){
        fcfilt <- input$fcs

        indexFilterFC[investNoiseData1@foldChange<=fcfilt] <- FALSE
        summaFC<- paste(sum(indexFilterFC,na.rm=T),' molecules remaining \n after filtering using a fold change threshold of',fcfilt,'.', sep=" ")
        
      }
      
    }
    
    ##### FILTERING Based on Filter ratios #####

    
    if(input$FilterRad!="Non"){

      if(length(grp)==2){
      ###### FILTERING based on model based clustering
    if(input$FilterRad=="model"){
      class1<- try(mixmodCluster(data.frame(cbind(investNoiseData1@RT,investNoiseData1@RI)[index.na1&index.na2,]),nbCluster =2)@bestResult@partition)
      class2<- try(mixmodCluster(data.frame(cbind(investNoiseData2@RT,investNoiseData2@RI)[index.na1&index.na2,]),nbCluster =2)@bestResult@partition)
      
      if(class(class1)!='try-error' & class(class2)!='try-error'){
      cl1 <- unique(class1)
      tcl1 <- ifelse(mean(investNoiseData1@RT[index.na1&index.na2][class1==cl1[1]],na.rm=T)<mean(investNoiseData1@RT[index.na1&index.na2][class1==cl1[2]],na.rm=T),1,2)
        cl2 <- unique(class2)
      tcl2 <- ifelse(mean(investNoiseData2@RT[index.na2&index.na1][class2==cl2[1]],na.rm=T)<mean(investNoiseData2@RT[index.na2&index.na1][class2==cl2[2]],na.rm=T),1,2)
      #take the missing data in account
      indexFilterRatios[index.na2&index.na1] <- (class1==cl1[tcl1] | class2==cl2[tcl2])
      indexFilterRatios[!(index.na2&index.na1)] <-FALSE
      summa <- paste(sum(indexFilterRatios,na.rm=T),' molecules remaining \n after filtering with model based clustering. ', sep=" ")
      }
      }else{
      ####### FILTERING BASED ON fixed RT and RI
      
      RT <- as.numeric(input$RT_Filter)
      RI <- as.numeric(input$RI_Filter)
      index1 <- c(investNoiseData1@RI<=RI & investNoiseData1@RT<=RT )
      index2 <- c(investNoiseData2@RI<=RI & investNoiseData2@RT<=RT )
      
      indexFilterRatios <- (index1 | index2) 
      summa <- paste(sum(indexFilterRatios,na.rm=T),' molecules remaining \n after filtering with fixed RT=',RT,'and RI=',RI,'.', sep=" ")
    }
      }else{
        ###### FILTERING based on model based clustering
        if(input$FilterRad=="model"){
          class1<- try(mixmodCluster(data.frame(cbind(investNoiseData1@RT,investNoiseData1@RI)[index.na1,]),nbCluster =2)@bestResult@partition)
          if(class(class1)!='try-error'){
            cl1 <- unique(class1)
            tcl1 <- ifelse(mean(investNoiseData1@RT[index.na1][class1==cl1[1]],na.rm=T)<mean(investNoiseData1@RT[index.na1][class1==cl1[2]],na.rm=T),1,2)
           }
          indexFilterRatios[!(index.na1)] <-FALSE
          indexFilterRatios[index.na1] <- (class1==cl1[tcl1])
          summa <- paste(sum(indexFilterRatios,na.rm=T),' molecules remaining \n after filtering with model based clustering. ', sep=" ")
        }else{
          RT <- as.numeric(input$RT_Filter)
          RI <- as.numeric(input$RI_Filter)
          ####### FILTERING BASED ON fixed RT and RI
          indexFilterRatios <- c(investNoiseData1@RI<=RI & investNoiseData1@RT<=RT )
          summa <- paste(sum(indexFilterRatios,na.rm=T),' molecules remaining \n after filtering with fixed RT=',RT,'and RI=',RI,'.', sep=" ")
          
        }
        
        
      }
      
    
  print(summaMis)
  print(summaFC)
  print(summa)
  indexFinal <<- indexFilterRatios & indexFilterMiss & indexFilterFC
 
  if(current_filter_input==grp[1]){
  data2 <<- data.frame(RT=signif(investNoiseData1@RT,2),RI=signif(investNoiseData1@RI,2),propMiss=signif(investNoiseData1@propMissing,2),FC=signif(investNoiseData1@foldChange,2),Name=investNoiseData1@name)[index.na1,]
  }else{
    data2 <<- data.frame(RT=signif(investNoiseData2@RT,2),RI=signif(investNoiseData2@RI,2),propMiss=signif(investNoiseData2@propMissing,2),FC=signif(investNoiseData2@foldChange,2),Name=investNoiseData2@name)[index.na2,]
    
  }
  }
  if(sum(indexFinal)==0)
    indexFinal <<- rep(T,length(indexFinal))
  }
  })
  return(data2[indexFinal,])
 
})

#obd <- observe({
#  input$ResetFilter
#    indexFinal <<- rep(T,ncol(indexFilter))
#})
# 
# noiseData <- reactive({
#   
#   ExpData <- ExpData()$data
#   time <- TimeData()$data
#   group <- as.character(GroupData()$data)
#   replicate <- RepData()$data
#   logfc <- input$logfc
#   f <- FALSE
#   m <- FALSE
#   
#   if(input$RunExample!=0){
#     print('example')
#     ExpData <- ExampleExp
#     time <- ExampleTime
#     group <- ExampleGroup
#     replicate <- ExampleSample
#     
#     
#   }
#   
# 
#   if((!is.null(ExpData) & !is.null(time) & !is.null(group) & !is.null(replicate)) | changedPath()|logfc!=log){
# 
#     print('reactive noise')
#     if(input$fcUsed){
#       if(is.null(fc2)){
#         f <- TRUE
#       }else{
#         if(is.null(input$fcs)){
#           f<- FALSE
#         }else{
#           f <- fc2!=input$fcs
#           
#         }
#       }
#     }
#     
#     if(input$numMissingUsed){
#       if(is.null(numMis)){
#         m <- TRUE
#       }else{
#         if(is.null(input$miss)){
#           m <- FALSE
#           
#         }else{
#           m <- numMis!=input$miss
#         }
#         
#       }
#     }
#     
#     
#     
#     equal_gr <- FALSE
#     current_filter_input <- input$filter_gr
#     
#     if(length(current_filter_input)==0){
#       equal_gr <- FALSE
#     }else{
#       current_filter_input<- as.character(current_filter_input )
#       equal_gr <- current_filter_input!=current_group
#     }
# 
#     if(is.null(df)|equal_gr|m|f|logfc!=log){ 
#       
#       if(is.null(df) |equal_gr|logfc!=log){
#         gr <- 1:length(unlist(time))
#         if(length(current_filter_input)!=0){
#           gr <- gr[group==input$filter_gr]
#           current_group <<- current_filter_input
#         }
# 
#         n <-investNoise(data = ExpData[gr,],time =time[gr] ,sampleID=replicate[gr],log = logfc)
#         index.na <- which(!is.na(n@RT)|!is.na(n@RI)|!is.infinite(n@RI)|!is.infinite(n@RT))
#         data2 <<- data.frame(RT=signif(n@RT,2),RI=signif(n@RI,2),propMiss=signif(n@propMissing,2),FC=signif(n@foldChange,2),Name=n@name)[index.na,]
#       }
#       
#       if(!is.null(data2)){
#         index <- rep(TRUE,length(data2$RT))
#         
#         if(input$numMissingUsed & input$fcUsed){      
#           
#           if(!is.null(input$fcs)&!is.null(input$miss)){
#             index[((data2$propMiss>input$miss) | (data2$FC<input$fcs))] <- FALSE
#             fc2 <<-input$fcs
#             numMis <<-input$miss
#           }
#         }
#         if((!input$fcUsed) & input$numMissingUsed){
#           if(!is.null(input$miss)){
#             index[data2$propMiss>input$miss] <- FALSE
#             numMis <<-input$miss
#           }
#         }
#         if((!input$numMissingUsed) & input$fcUsed){
#           if(!is.null(input$fcs)){
#             index[data2$FC<input$fcs] <- FALSE
#             fc2 <<-input$fcs
#           }
#         }
#         
#         if(is.null(index))
#           index <- rep(TRUE,nrow(data2))
#         
#         df <<- data2[index,]
#       }
#       
#     }
#   }
#   df <<- df
#   log <<- input$logfc
#   return(df)
# })
# 

## Scatterplot of filter ratios colored by fold change values ##
output$BubbleGvisFC <- renderGvis({
  df <- noiseDatanew()
  bubble2 <- NULL
  if(!is.null(df)){
    bubble2 <- gvisBubbleChart(df, idvar="Name", xvar="RT", yvar="RI", sizevar="FC",options=list(title='Fold change',hAxis="{title: 'R_T'}",vAxis="{title: 'R_I'}",colorAxis="{colors: ['blue', 'orange']}",bubble="{stroke:'none',opacity:0.4,textStyle:{color: 'none'}}",sizeAxis="{minValue: 0,  maxSize: 5}"))
  }
  return(bubble2)
  
})

## Scatterplot of filter ratios colored by proportion of missing values ##
output$BubbleGvisMissProp <- renderGvis({
  df <- noiseDatanew()
  bubble <- NULL
  if(!is.null(df)){
    bubble <- gvisBubbleChart(df, idvar="Name", xvar="RT", yvar="RI", sizevar="propMiss",options=list(title='Proportion of missing values', hAxis="{title: 'R_T'}",vAxis="{title: 'R_I'}", colorAxis="{colors: ['green','blue', 'red']}",bubble="{stroke:'none',opacity:0.4,textStyle:{color: 'none'}}",sizeAxis="{minValue: 0,  maxSize: 5}"))
  }
  return(bubble)
})

### Model based clustering on filter ratios ##

output$MCLUST <- renderPlot({
  df <- noiseDatanew()
  clustplot <- NULL
  if(!is.null(df)){
    #Show progress bar
   # withProgress(message = 'Clustering data', value = 0.1, {
      index.na <- which(!is.na(df$RT)&!is.na(df$RI)&!is.infinite(df$RT)&!is.infinite(df$RI))
      class1<- try(mixmodCluster(data.frame(cbind(df$RT,df$RI)[ index.na,]),nbCluster = 2)@bestResult@partition)
   
      if(class(class1)!='try-error'){
        cl <- unique(class1)
        tcl <- ifelse(mean(df$RT[class1==cl[1]],na.rm=T)<mean(df$RT[class1==cl[2]],na.rm=T),2,1)
        keep <- rep('yes',length(class1))
        keep[class1==cl[tcl]] <- 'no'
        df$keep <- keep
        clustplot <- qplot(RT,RI,data=df[index.na,],colour=keep,size=2,type="n",xlab="R_T",ylab="R_I",main="Classification using \n model based clustering and two clusters")+ theme_bw()#+ theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) 
      }
  #  })
    return(clustplot)
  }else{
    return()
  }
})



output$filter_group <- renderUI({
  group <- input$GroupsSel
  if(!is.null(group)){
    selectInput("filter_gr", "Choose a group:", 
                choices = group)
  }
})


output$RI_Filter <- renderUI({
  df <- noiseDatanew()
  if(!is.null(df))
    numericInput("RI_Filt", HTML(paste("R", tags$sub("I"), sep = "")), 0.3,
                 min = min(df$RT,na.rm=T), max = max(df$RT,na.rm=T),step=0.1)
  
})

output$RT_Filter <- renderUI({
  df <- noiseDatanew()
  if(!is.null(df))
    numericInput("RT_Filt", HTML(paste("R", tags$sub("T"), sep = "")), 0.9,
                 min = min(df$RI,na.rm=T), max = max(df$RI,na.rm=T),step=0.1)
})

output$fc_slider <- renderUI({
  df <- noiseDatanew()
  if(!is.null(df)){
    fc3 <-df$FC
    if(is.null(f.slider)) {
      #  if(is.null(f.slider) | changeFC){
      ymin.fc <<- min(fc3,na.rm = T)
      ymax.fc <<- max(fc3,na.rm = T)
      f.slider <<- ymin.fc
    }else{
      if(length(input$fcs)==0 | is.null(input$fcs)){
        inp.fc <- 0
      }else{
        inp.fc <- input$fcs
      }
      f.slider <<- ifelse(inp.fc<ymin.fc,ymin.fc,inp.fc)
    }
    sliderInput("fcs", "Fold change",
                min = signif(ymin.fc,2), max = signif(ymax.fc,2),
                value = signif(f.slider,2))
  }
})




output$missing_slider <- renderUI({
  df <- noiseDatanew()
  if(!is.null(df)){
    numM <-df$propMiss
    
    if(is.null(m.slider)){
      ymin <<- min(numM,na.rm = T)
      ymax <<- max(numM,na.rm = T)
      m.slider <<- signif(ymax,2)
    }else{
      m.slider <<- input$miss
      
    }
    
    ticks=T
    if(ymin==ymax)
      ticks <- F
    rsm <- 0
    if(is.numeric(m.slider))
      rsm <- signif(m.slider,2)
    sliderInput("miss", "Proportion of missing values",
                min = signif(ymin,2), max = signif(ymax,2),
                value = rsm, animate = F,ticks=ticks)
  }
})

#Action buttons are annoying
output$resetFilter <- renderText({
  if(input$ResetFilter==0)
    return('')
  indexFinal <<- rep(T,length(indexFinal))
  lmm <<- NULL
  lmm.de <<- NULL
  return('Reset all filters.')
})


output$summaryFilter <- renderText({
  if(input$ApplyFilter==0 )
    return('')
  
    lmm <<- NULL
    lmm.de <<- NULL
    if(input$ApplyFilter){
      return(paste('Leaving ',sum(indexFinal),'molecules after filtering.'))
      }else{
        indexFinal <<- rep(T,length(indexFinal))
        return(paste('No filtering applied for further analysis'))
      
    }
})



# FilterSoftvals <- function(){
#   summa <- ""
#   ExpData <- isolate(ExpData()$data)
#   time <- isolate(TimeData()$data)
#   group <- isolate(as.character(GroupData()$data))
#   replicate <- isolate(RepData()$data)
#  
# 
#   if(input$RunExample!=0){
#     ExpData <- ExampleExp
#     time <- ExampleTime
#     group <- unlist(ExampleGroup)
#     replicate <- ExampleSample
#   }
#   
#   grp <- unique(na.omit(as.character(group)))
#   
#   if((!is.null(ExpData) & !is.null(time) & !is.null(group) & !is.null(replicate)) | changedPath()){
#     
#     
#     if(length(grp)==2){
#       gr1 <- which(group==grp[1])
#       n1 <-investNoise(data = ExpData[gr1,],time =time[gr1] ,sampleID=replicate[gr1])
#      
#       da1 <- data.frame(time=n1@RT,ind=n1@RI)
#       gr2 <- which(group==grp[2])
#       n2 <-investNoise(data = ExpData[gr2,],time =time[gr2] ,sampleID=replicate[gr2])
#   
#       da2 <- data.frame(time=n2@RT,ind=n2@RI)
#     
#       if(input$numMissingUsed){
#         misdata <- isolate(input$miss)
#         print(misdata)
#         indexFinal[n2@propMissing>=misdata|n1@propMissing>=misdata] <- FALSE
#     
#     }
#   
#       if(input$fcUsed){
#         fcfilt <- isolate(input$fcs)
#         print(fcfilt)
#         indexFinal[n2@foldChange<=fcfilt|n1@foldChange<=fcfilt] <- FALSE
#       }
#   
#     
#     }else{
#       n1 <-investNoise(data = ExpData,time =time ,sampleID=replicate)
#       da1 <- data.frame(time=n1@RT,ind=n1@RI)
#   
#       if(input$numMissingUsed){
#      
#       misdata <- isolate(input$miss)
#       print(misdata)
#       indexFinal[n1@propMissing>=misdata] <- FALSE
#     
#     }
#   
#     if(input$fcUsed){
#    
#       fcfilt <-  isolate(input$fcs)
#       print(fcfilt)
#       indexFinal[n1@foldChange<=fcfilt] <- FALSE
#     }
# }
# 
# }
# indexFinal <<- indexFinal
# 
# summa <- paste("Removed molecules", sum(!indexFinal,na.rm=F), 'after soft filtering.')
# }
  
# Filtervals <- function(){
#   summa <- ""
#   ExpData <- isolate(ExpData()$data)
#   time <- isolate(TimeData()$data)
#   group <- isolate(as.character(GroupData()$data))
#   replicate <- isolate(RepData()$data)
# 
# 
#   if(input$RunExample & input$RunExample!=0){
#     ExpData <- ExampleExp
#     time <- ExampleTime
#     group <- ExampleGroup
#     replicate <- ExampleSample
#   }
#   grp <- unique(na.omit(as.character(group)))
#   if((!is.null(ExpData) & !is.null(time) & !is.null(group) & !is.null(replicate)) | changedPath()){
#     
#     if(length(grp)==2){
#       gr1 <- which(group==grp[1])
#       n1 <-investNoise(data = ExpData[gr1,],time =time[gr1] ,sampleID=replicate[gr1])
#       index.na1 <- which(!is.na(n1@RT)&!is.na(n1@RI)&!is.infinite(n1@RI)&!is.infinite(n1@RT))
#       da1 <- data.frame(time=n1@RT,ind=n1@RI)
#       gr2 <- which(group==grp[2])
#       n2 <-investNoise(data = ExpData[gr2,],time =time[gr2] ,sampleID=replicate[gr2])
#       
#       index.na <- which(!is.na(n2@RT)&!is.na(n2@RI)&!is.infinite(n2@RI)&!is.infinite(n2@RT))
#       da2 <- data.frame(time=n2@RT,ind=n2@RI)
#   
#       print(input$FilterRad)
#       if(input$FilterRad=="model"){
#         class1<- mixmodCluster(data.frame(cbind(da1$time,da1$ind)[index.na1 & index.na,]),nbCluster =2)@bestResult@partition
#         cl1 <- unique(class1)
#         tcl1 <- ifelse(mean(da1$time[index.na&index.na1][class1==cl1[1]],na.rm=T)<mean(da1$time[index.na&index.na1][class1==cl1[2]],na.rm=T),1,2)
#       
#         class2<- mixmodCluster(data.frame(cbind(n2@RT,n2@RT)[index.na1 & index.na,]),nbCluster =2)@bestResult@partition
#         cl2 <- unique(class2)
#         tcl2 <- ifelse(mean(da2$time[index.na&index.na1][class2==cl2[1]],na.rm=T)<mean(da2$time[index.na&index.na1][class2==cl2[2]],na.rm=T),1,2)
#     #take the missing data in account
#         indexFinal[index.na&index.na1] <<- (class1==cl1[tcl1] | class2==cl2[tcl2])& indexFinal
#         indexFinal[!(index.na&index.na1)] <<-FALSE
#         summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with model based clustering. ', sep=" ")
#       }else{
#         RT <- as.numeric(input$RT_Filter)
#         RI <- as.numeric(input$RI_Filter)
#         index1 <- c(n1@RI<=RI & n1@RT<=RT & indexFinal)
#         index2 <- c(n2@RI<=RI & n2@RT<=RT & indexFinal)
#       
#         indexFinal <<- (index1 | index2) 
#         summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with fixed RT=',RT,'and RI=',RI,'.', sep=" ")
#     }
#     }else{
#       n1 <-investNoise(data = ExpData,time =time ,sampleID=replicate)
#       index.na1 <- which(!is.na(n1@RT)&!is.na(n1@RI)&!is.infinite(n1@RI)&!is.infinite(n1@RT))
#       da1 <- data.frame(time=n1@RT,ind=n1@RI)
#       
#       if(input$FilterRad=="model"){
#         class1<- mixmodCluster(data.frame(cbind(da1$time,da1$ind)[index.na1,]),nbCluster = 2)@bestResult@partition
#         cl1 <- unique(class1)
#         tcl1<- ifelse(mean(da1$time[class1==cl1[1]],na.rm=T)<mean(da1$time[class1==cl1[2]],na.rm=T),1,2)
#         indexFinal[index.na1] <<- class1==cl1[tcl1] & indexFinal
#         indexFinal[!index.na1] <<- FALSE
#         summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with model based clustering. ', sep=" ")
#     }else{
#         RT <- as.numeric(input$RT_Filter)
#         RI <- as.numeric(input$RI_Filter)  
#         indexFinal <<- c(n1@RI<=RI & n1@RT<=RT & indexFinal)
#         summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with fixed RT=',RT,'and RI=',RI,'.', sep=" ")
#     }
#   }
#   
#   
# }
# 
#   if(sum(indexFinal,na.rm=T)==0){
#     indexFinal <<- rep(TRUE,length(indexFinal))
#     summa <- "No molecules left. Please check your settings."
#   }
#   return(summa)
# 
# }

  ################# Modelling functions ###########
  
LMMData <- reactive({
  #lmm <- NULL
  if (is.null(lmm) & input$Modelling==0)
    return()
  ExpData <- ExpData()$data
  if(is.null(indexFinal)& !is.null(ExpData)){
    indexFinal <<- rep(T,ncol(ExpData))
    ExpData <- ExpData[,indexFinal]
  }
  time <- TimeData()$data
  group <- GroupData()$data
  replicate <- RepData()$data
  annotation <- AnnotData()
  
  if(input$RunExample){
   # Example()
     if(is.null(indexFinal))
      indexFinal <<- rep(T,ncol(ExampleExp))
    ExpData <-ExampleExp[,indexFinal]
    time <- unlist(ExampleTime)
    replicate <- unlist(ExampleSample)
    group <- unlist(ExampleGroup)
    lmm <<- NULL
  }
  g <- input$GroupsSel
  if(is.null(g))
    g <- na.omit(unique(group))

  if(!is.null(annotation)){
    colnames(ExpData) <- as.character(unlist(annotation))[indexFinal]
  }
  if(!is.null(ExpData)&!is.null(time)&!is.null(replicate)&is.null(lmm)){
    withProgress(message = 'Modelling in progress', value = 0.1, {
      if(length(g)==1){
        gr1 <- which(group%in%g)
      lmm <<- lmmSpline(data = ExpData[gr1,],time =time[gr1] ,sampleID=replicate[gr1], basis=isolate(input$Basis),keepModels = F)
      
      }else{
        l <- list()
        for(i in g){
          gr1 <- which(group%in%i)
          lmm <<- lmmSpline(data = ExpData[gr1,],time =time[gr1] ,sampleID=replicate[gr1], basis=isolate(input$Basis),keepModels = F,timePredict = na.omit(sort(unique(time))))
        l[[i]] <- lmm
          }
        lmm <<- l
      }
    })
  }else{
    warning("Please check the availability of the expression data, group, time and replicates.")
  }
  lmm
})  

output$ModelTable<- renderText({

  input$Modelling
  lmm <- LMMData()
  if(is.null(lmm))
    return()
  if(class(lmm)=='lmmspline'){
  p <- paste(names(table(lmm@modelsUsed)),as.vector(table(lmm@modelsUsed)),sep=":")
  HTML(paste("Table of models used to model the molecule expression:", paste(p,collapse='<br>'),sep="<br>"))
  }else{
    gr <- input$GroupsSel
    ht <-''
    for(i in 1:length(gr)){
      p <- paste(names(table(lmm[[i]]@modelsUsed)),as.vector(table(lmm[[i]]@modelsUsed)),sep=":")
      ht <- paste(ht,paste("<br> Table of models used to model the molecule expression group ",gr[i],':', paste(p,collapse='<br>'),sep=" "))
    }
    
    HTML(ht)
  }
})


  
output$downloadModel <- downloadHandler(
  print('download'),
  filename = function() { paste('ModelledTrajectories',Sys.Date(),'.csv', sep='') },
  content = function(file) {
    l <- LMMData()
    if(class(l)=='lmmspline'){
    write.csv(l@predSpline, file,row.names=F)
    }else{
      write.csv(rbind(l[[1]]@predSpline,l[[2]]@predSpline), file,row.names=F)
    }
  }
)

output$textModels <- renderText({
    HTML("0: Linear model <br/>
    1: Linear Mixed effect Model Spline (LMMS) <br/>
    2: LMMS with subject-specific random intercept <br/>
    3: LMMS with subject specific intercept and slope"
    )
  })

output$ModelTables =  DT::renderDataTable({
  if (input$Modelling== 0)
    return()
  lmm <- LMMData()
  if(is.null(lmm))
    return()
  gr <- isolate(input$GroupsSel)
  if(class(lmm)=='lmmspline'){
    df <- data.frame(Molecule=rownames(lmm@predSpline),Model=lmm@modelsUsed,Group=rep(gr,each=length(lmm@modelsUsed)))
    
  }else{
    df <- data.frame(Molecule=c(rownames(lmm[[1]]@predSpline),rownames(lmm[[2]]@predSpline)),Model=c(lmm[[1]]@modelsUsed,lmm[[2]]@modelsUsed),Group=rep(gr,each=length(lmm[[1]]@modelsUsed)))
  }

  dt <- datatable(df,selection = 'single')
  
  return(dt)
}, options = list(server=TRUE,rownames=TRUE))


output$ModelPlot <- renderPlot({
  l <- LMMData()
  if(is.null(l))
    return()
  
  s <- input$ModelTables_rows_selected
  v <- NULL
  if (!length(s))
    s <- 1
  
  ## Currently a data table bug as it should select only single arguments
  ### CHANGE IF DT R package is updated
  v <- as.numeric(s)[length(s)]
  # v <- which(l@DE$Molecule%in%input$DETable[1])
  ExpData <- ExpData()$data[,indexFinal]
  time <- TimeData()$data
  group <- GroupData()$data
  if(input$RunExample){
    #Example()
    ExpData <- ExampleExp[,indexFinal]
    time <- unlist(ExampleTime)
    group <- unlist(ExampleGroup)
    
  }
  if(class(l)=='lmmspline'){
    p <- plot(l,v,data=ExpData,type=input$Radio_DEplot,time = time,mean=input$ModelPlotMean,smooth=input$ModelPlotSmooth)+theme_bw()
  }else{
    len <- length(l[[1]]@modelsUsed)

    if(v<len){
      g <- 1
    }else{
      g <- 2
      v <- v-len
    }
    index <-group==input$GroupsSel[g]
    p <- plot(l[[g]],v,data=ExpData[index,],type=input$Radio_DEplot,time = time[index],mean=input$ModelPlotMean,smooth=input$ModelPlotSmooth)+theme_bw()
  }

  print(p)
})

output$textAnaModel <- renderText({
  if(!is.null(input$GroupsSel)){
    if(input$RunExample){
    p <- paste('Analysis is going to be performed on the Example data set with group/s:',paste(unlist(input$GroupsSel),collapse = ' and '),'. <br>Select a basis and press the `Model` button to model the data.',collapse='')
    }else{
      p <- paste('Analysis is going to be performed on group/s:',paste(unlist(input$GroupsSel),collapse = ' and '),'. <br>Select a basis and press the `Model` button to model the data.',collapse='')
    
    }
    
  }else{
      p <- 'Please upload a data set or tick the `Run example` checkbox in the `Example and Help` tab.'
    }
  HTML(p)
})

  ########Clustering functions########
  
  formulaText <- reactive({
    var <- switch(input$variable,
           "hc"="Hierarchical clustering", 
           "diana"  =  "Diana" , 
           "km"="Kmeans" ,
            'model'= 'Model based clustering',
           'som'='Self-organizing maps',
           'pam'='PAM')
    paste(var,"with",input$cluster_num,'cluster')
  })
  
  formulaText2 <- reactive({
    nam <- paste(c("HC"," Kmeans"," SOM"," Mclust"," PAM")[c(input$hierarchical,input$kmeans,input$som,input$model,input$pam)],collapse=",")
    paste(nam,"with",min(input$cluster_range),"-",max(input$cluster_range),"cluster")
  })
  

  makeValiPlot <- function(list,range,correlation){
    cluster.sel <- c(1:5)[list]
    #print(input$submitVali)
    range<-seq(range[1],range[2],1)
      lmms <- LMMData()
    if(is.null(lmms)){
      lmms <- t(ExpData()$data)
    }else{
      if(class(lmms)=='lmmspline'){
        lmms <- lmms@predSpline
      }else{
        lmms <- rbind(lmms[[1]]@predSpline,lmms[[2]]@predSpline)
        }
    
    }
    
    stab <- NULL
    
    withProgress(message = 'Cluster validation in progress', value = 0.1, {
      stab <- clValid2(lmms, range, clMethods=c("hierarchical","kmeans","som","model","pam")[cluster.sel],validation="stability",metric=correlation,maxitems=nrow(lmms))
      ano.plot <- ano.boxplot(stab$measures,clMethods=c("hc","km","som","model","pam")[cluster.sel],measurement=c("CPN"))
    })
  }
  
  
  cluster <- function(algo,num.cluster,cor){

    withProgress(message = 'Clustering in progress', value = 0.1, {
    tmp.data<-LMMData()
    if(is.null(tmp.data)){
      tmp.data <- t(ExpData()$data)
    }else{
    
      if(class(tmp.data)=='lmmspline'){
        tmp.data <- tmp.data@predSpline
      }else{
        tmp.data <- rbind(tmp.data[[1]]@predSpline,tmp.data[[2]]@predSpline)
      }
    }

    if(input$Radio_Correlation=='correlation')
      tmp.data <- 1-cor(t(tmp.data),use="pairwise.complete.obs")
    if(algo=="hc"){
      ifelse(cor,t <-as.dist(tmp.data),t <- dist(tmp.data))
      tree <- hclust(t,method="ward.D")
      classifi <<-  cutree(tree,k=num.cluster)
    }
    if(algo=="km"){
      k <- kmeans(tmp.data,centers= num.cluster)
      classifi <<- k$cluster
    }
    if(algo=="pam"){
      k <- pam(tmp.data,k= num.cluster)
      classifi <<- k$clustering
    }
    if(algo=="model"){
      k <- Mclust(data.frame(tmp.data),G= num.cluster)
      classifi <<- k$partition
    }
    if(algo=="som"){
      m <- t(matrix(unlist(t(tmp.data)),nrow=ncol(tmp.data)))
      som.data <- kohonen::som(m,grid = somgrid(1,num.cluster, 'hexagonal'))
      deriv.map <- NULL
      deriv.map <- kohonen::map(som.data)
      #classification
      classifi <<- deriv.map$unit.classif
    }

    })
    return(classifi)
  }
  

  
  makePlot <- function(algo,num_cluster,cor){

    input$submitVisu
    data.lmm <-LMMData()

    if(is.null(data.lmm)){
      data.lmm <- t(ExpData()$data)
      nr <- nrow(data.lmm)
    }else{

     # data.lmm <- data.lmm@predSpline[indexFinal,]
      if(class(data.lmm)=='lmmspline'){
        nr <- nrow(data.lmm@predSpline)
        data.lmm  <- data.lmm@predSpline
        group <- rep(input$GroupsSel,each=nr)
      }else{
        nr <- nrow(data.lmm[[1]]@predSpline)
        data.lmm  <- rbind(data.lmm[[1]]@predSpline,data.lmm[[2]]@predSpline)
        group <- rep(input$GroupsSel,each=nr)

      }
    }


    if(is.null(rownames(data.lmm)))
      rownames(data.lmm) <- 1:nrow(data.lmm)
    
    time <- TimeData()$data
    if(is.null(time)){
     # Example()
      time <- unlist(ExampleTime)
    }

    t.s <-sort(unique(na.omit(time)))
    classification <- isolate(classif())
    u <- sort(unique(classification))
    l <- tapply(classification,classification,length)
    header <- paste("C",u,", n=",l,"",sep="")
    
    if(input$Smoothed){
      s <- apply(data.lmm,1, function(x)spline(t.s,x,n=50)$y)
      t <- seq(min(time,na.rm = T),max(time,na.rm = T),length.out=50)
         tl <- length(t)
      pred.data <- data.frame(Intensity=as.vector(s),Molecule=rep(rownames(data.lmm),each=tl),Time=rep(t,nrow(data.lmm)),Cluster=header[rep(classification,each=tl)],Group=rep(group,each=tl))
      
    }else{
      t.sl<- length(t.s)
       pred.data <- data.frame(Intensity=as.vector(unlist(t(data.lmm))),Molecule=rep(rownames(data.lmm),each=t.sl),Time=rep(t.s,nrow(data.lmm)),Cluster=header[rep(classification,each=t.sl)],Group=rep(group,each=t.sl))      
      
    }
    if(!input$colorGroup)
      pred.data$Group <- pred.data$Cluster

    p0 <- qplot(x=Time,y=Intensity,group=Molecule,color=Group,geom='line',data=pred.data,facets = ~Cluster) + stat_summary(fun.data = "mean_cl_boot",aes(x=Time,y=Intensity,group=Cluster),size=1.2,data=pred.data, geom="line",color='black')+theme_bw()#+stat_summary(mean_cl_boot, geom="line",stat_summary( group = 1, fun.data = mean_cl_boot, color = "black", alpha = 0.2, size = 1.1))
    #scales='free_y',facets = ~Cluster,data=pred.data,)
    print(p0)

  }
  
  ###### RENDER PLOTS############
  
  # Return the formula text for printing as a caption

  
  output$caption1 <- renderText({
    if (input$submitVali== 0)
      return()
    isolate(formulaText2())
  })


  
  output$cluster_slider <- renderUI({
    ymin <- 2
    ymax <- 20
    
    sliderInput(inputId = "cluster_num",
                label = paste("Number of clusters"),
                min = ymin, max = ymax, value = 3)
  })
  
  output$clusterValidation <- renderPlot({
    if (input$submitVali==0)
      return()
    
    if(isolate(input$hierarchical)!=hcChanged | isolate(input$kmeans)!= kmChanged| isolate(input$som)!=somChanged | isolate(input$model)!=modelChanged|isolate(input$pam)!=pamChanged| isolate(input$cluster_range)[1]!=ClRangeChanged1|isolate(input$cluster_range)[2]!=ClRangeChanged2|matrixChanged!=isolate(input$Radio_Correlation)){
      plot <<- makeValiPlot(c(isolate(input$hierarchical),isolate(input$kmeans),isolate(input$som),
                           isolate(input$model),isolate(input$pam)),isolate(input$cluster_range),isolate(input$Radio_Correlation))
      hcChanged <<- isolate(input$hierarchical)
      kmChanged <<- isolate(input$kmeans)
      somChanged <<- isolate(input$som)
      modelChanged <<- isolate(input$model)
      ClRangeChanged1 <<- isolate(input$cluster_range)[1]
      ClRangeChanged2 <<- isolate(input$cluster_range)[2]
      matrixChanged <<-isolate(input$Radio_Correlation)
      pamChanged <<- isolate(input$pam)
      }else{
        plot
      }
      })
  
  
  output$cluster_range_slider <- renderUI({
    ymin <- 2
    ymax <- 20
    
    sliderInput(inputId = "cluster_range",
                label = paste("Cluster range"),
                min = ymin, max = ymax, value = c(2, 3))
  })
  
  output$caption <- renderText({
    if (input$submitVisu== 0)
      return()
    #input$submitVisu
    isolate(formulaText())
  })
  
  output$clusterPlot <- renderPlot({
    if (input$submitVisu== 0)
      return()
    isolate(makePlot(input$variable,input$cluster_num,input$Radio_Correlation))
    })
  
  
  classif <- reactive({
    if(input$variable!=v |input$cluster_num!=num |input$Radio_Correlation!=core){
      v <<-input$variable
      num <<-input$cluster_num
      core <<-input$Radio_Correlation
      ce <<- cluster(v,num,core)
    }
    ce
   })
  
  output$GOEnrichment =  DT::renderDataTable({
    if (input$updateTable==0)
      return()
 
    datasetInput()
  }, options = list(bSortClasses = TRUE))
  
  ##write data
  datasetInput <- reactive({
    l <- LMMData()

    if(is.null(l)){
      l <- ExpData()$data
      a <-  as.character(unlist(AnnotData()))
      annonames <- if(is.null(colnames(l))) a else colnames(l)
    }else{
      if(class(l)=='lmmspline'){
        annonames <- rownames(l@predSpline)
      }else{
        annonames <- rownames(l[[1]]@predSpline)
      }
    }
    
    cl <- isolate(classif())
    if(is.null(cl))
      return()
    
    if(length(class)!=length(cl)| !all(cl==class)){
      class <<- cl
      
      withProgress(message = 'GO enrichment analysis in progress', value = 0.1, {
      enrich <<-biological.homogenity(cl,ident=annonames,identifier="IPI",ontology=input$ontology,db=input$organism)     
      })
    }
    enrich
    
  })
  
  ##write data
  datasetClassifInput <- reactive({
    l <- LMMData()

    if(is.null(l)){
      l <- ExpData()$data
      a <- as.character(unlist(AnnotData()))
      annonames <- ifelse(is.null(colnames(l)),a,colnames(l))
    }else{
     
      if(class(l)=='lmmspline'){
        annonames <- rownames(l@predSpline)
      }else{
        annonames <- rownames(l[[1]]@predSpline)
      }
    }
    data.frame(ID=annonames, Cluster=isolate(classif()))
  })
  
  output$downloadClassifData <- downloadHandler(
    
    filename = function() { paste('Classification',input$variable,input$cluster_num,Sys.Date(),'.csv', sep='') },
    content = function(file) {
      write.csv(datasetClassifInput(), file,row.names=F)
    })
  
  # output$downloadClusterPlot <- downloadHandler(
  #   filename <- function() {
  #     paste('Plot_',input$variable,input$cluster_num,Sys.Date(),'.ps',sep='') },
  #   content <- function(file) {
  #     postscript(file, width = 4.92, height = 3.2,
  #                family = "Arial", paper = "special", onefile = FALSE,
  #                horizontal = FALSE, pointsize = 12,bg = "white")
  #     top6.plot <- makePlot(input$variable,input$cluster_num,input$correlation)
  #     print(top6.plot)
  #     dev.off()},
  #   contentType = 'image/ps'
  # )
  
  output$downloadData <- downloadHandler(
    filename = function() { paste('GOEnrichment',input$variable,input$cluster_num,Sys.Date(),'.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file,row.names=F)
    }
  ) 
  
  output$textAnaModelCluster <- renderText({
    if(!is.null(input$GroupsSel)){
      if(input$RunExample){
        p <- paste('Analysis is going to be performed on the Example data set with group/s:',paste(unlist(input$GroupsSel),collapse = ' and '),'.<br>Perform a cluster validation to find the number of cluster and algorithm with the highest stability (lowest CPNs) or immidialty cluster and visualize the data.',collapse='')
      }else{
        p <- paste('Analysis is going to be performed on group/s:',paste(unlist(input$GroupsSel),collapse = ' and '),'.<br> Perform a cluster validation to find the number of cluster and algorithm with the highest stability (lowest CPNs) or immidialty cluster and visualize the data.',collapse='')
        
      }
      
    }else{
      p <- 'Please upload a data set or tick the `Run example` checkbox in the `Example and Help` tab.'
    }
    HTML(p)
  })
  
  
  
############### Differential Expression functions #############

  output$textAnaModelDE <- renderText({
    if(!is.null(input$GroupsSel)){
      if(input$RunExample){
        p <- paste('Analysis is going to be performed on the Example data set with group/s:',paste(unlist(input$GroupsSel),collapse = ' and '),'. <br>Select a differential expression analysis over time, between groups or time and group interaction.',collapse='')
      }else{
        p <- paste('Analysis is going to be performed on group/s:',paste(unlist(input$GroupsSel),collapse = ' and '),'. <br>Perform a cluster validation to find the number of cluster and algorithm with the highest stability (lowest CPNs) or immidialty cluster and visualize the data.',collapse='')
        
      }
      
    }else{
      p <- 'Please upload a data set or tick the `Run example` checkbox in the `Example and Help` tab.'
    }
    HTML(p)
  })
  
output$DEInfoText <- renderText({
  if (input$DEtable== 0)
    return()
  paste("Analysed",sum(indexFinal,na.rm=T),"molecules for differential expression.")
})


output$DETable =  DT::renderDataTable({
  if (input$DEtable== 0|is.null(DEoutput()))
    return()
  de <- datatable(DEoutput()@DE,selection = 'single')
  return(de)
}, options = list(server=TRUE,rownames=TRUE))



DEoutput <- reactive({
  if (input$DEtable== 0 & is.null(lmm.de))
    return()

  if(is.null(ExpData()$data) & is.null(ExampleExp))
    return()
  if(is.null(indexFinal) & !is.null(ExpData()$data))
    indexFinal <<- rep(T,ncol(ExpData()$data))
  current_groupSel <- isolate(input$GroupsSel)
  #if(grSelDE!=current_groupSel|length(grSelDE)!=length(current_groupSel))
  #  lmm.de <<- NULL
    
  grSelDE <<-current_groupSel
  
  if(!is.null(ExpData()$data) & !input$RunExample){
   # Example()
    group <- GroupData()$data
    if(!is.null(grSelDE)){ gr <- grSelDE}else{gr <- as.character(unique(group))}
    print(gr)
    grIndex <- which(as.character(group)%in%gr)
    print(grIndex)
    replicate <- RepData()$data[grIndex]
    ExpData <- ExpData()$data[grIndex,indexFinal]
    time <- TimeData()$data[grIndex]
    group <- group[grIndex]
    print(grIndex)
  }

  if(input$RunExample){
   # Example()
    if(is.null(indexFinal))
      indexFinal <<- rep(T,ncol(ExpData))
    group <- unlist(ExampleGroup)
    if(!is.null(grSelDE)){ gr <- grSelDE}else{gr <- as.character(unique(group))}
    print(gr)
    grIndex <- which(as.character(group)%in%gr)
    print(grIndex)
    ExpData <-  ExampleExp[grIndex,indexFinal]
    time <- unlist(ExampleTime)[grIndex]
    replicate <- unlist(ExampleSample)[grIndex]
    group <- group[grIndex]
  }
  
  print(!is.null(ExpData))
  print(is.null(lmm.de))
  print(!is.null(replicate))
  print(!is.null(group))
  print(!is.null(time))
  if((!is.null(ExpData)&!is.null(time)&!is.null(replicate)&!is.null(group)&is.null(lmm.de))| changedPath()){
    withProgress(message = 'Differential expression analysis in progress', value = 0.1, {
      lmm.de <<- lmmsDE(data=ExpData,sampleID=replicate, time=time, group=group,
                        type=isolate(input$Type),basis=isolate(input$Basis),experiment=isolate(input$experiment))
    })
     }else{
    warning("Please check the availability of the expression data, group, time and replicates ")
  }
  lmm.de
})

output$DEPlot <- renderPlot({
  l <- DEoutput()
  if(is.null(l))
    return()
  s <- input$DETable_rows_selected
  if (!length(s))
    s <-1
## Currently a data table bug as it should select only single arguments
  ### CHANGE IF DT R package is updated
  v <- as.numeric(s)[length(s)]
  grSel <- isolate(input$GroupsSel)
  if(!is.null(ExpData()$data) & !input$RunExample){
  if(is.null(indexFinal))
    indexFinal <<- rep(T,ncol(ExpData))
  group <- unlist(ExampleGroup)
  
 if(!is.null(grSel)){ gr <- grSel}else{gr <-as.character( unique(group))}
  print(gr)
  grIndex <- which(as.character(group)%in%gr)
  print(grIndex)
  ExpData <-  ExampleExp[grIndex,indexFinal]
  time <- unlist(ExampleTime)[grIndex]
}
  if(input$RunExample){
   # Example()
    if(is.null(indexFinal))
      indexFinal <<- rep(T,ncol(ExampleExp))
    if(!is.null(grSel)){ gr <- grSel}else{gr <- as.character(unique(group))}
    print(gr)
    grIndex <- which(as.character(group)%in%gr)
    ExpData <-  ExampleExp[grIndex,indexFinal]
    time <- unlist(ExampleTime)[grIndex]
    print(grIndex)
    replicate <- unlist(ExampleSample)[grIndex]
    group <- group[grIndex]
  }
  type <- input$Radio_DEplot
  if(length(unique(group))==1)
    type='time'
  p <- plot(l,v,data=ExpData,type=input$Radio_DEplot,group = group,
            time = time,mean=input$DEPlotMean,smooth=input$DEPlotSmooth)+theme_bw()
  print(p)
})


  
output$downloadDEData <- downloadHandler(
  filename = function() { paste('DEAnalysis',input$Type,Sys.Date(),'.csv', sep='') },
  content = function(file) {
    write.csv(DEoutput()@DE, file,row.names=F)
  }
)


###### PDF VIEWER ########
output$pdfviewer <- renderText({
  if(input$ShowUserGuide==0)
    return()
 # pdf <- '<iframe style="height:600px; width:100%" src="TimeOmics_userguide.pdf",target="_self"></iframe>'
  observeEvent(input$ShowUserGuide, {
    print('event')
    pdf <- '<iframe style="height:600px; width:100%" src="TimeOmics_userguide.pdf",target="_self"></iframe>'
  })
  return(pdf)
  })
  
})