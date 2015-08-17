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
library(lattice)
library(cluster)
library(kohonen)
library(Rmixmod)
library(ggplot2)
library(googleVis)
library(extrafont)
library(lmms)
library(latticeExtra)

loadfonts()

ymax <- NULL
ymin <- NULL
ymin.fc <- NULL
ymax.fc <- NULL
bubble2 <- NULL
bubble <- NULL
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
resetValue <- 0
changedPath.Exp <- F
log <- TRUE
shinyServer(function(input, output) {

########UPLOAD FUNCTIONS################
 ExpData <- reactive({

    inFile <- input$ExpData
    
    withProgress(message = 'Uploading data', value = 0.1, {
    if (is.null(inFile))
      return(list(data=NULL,changedPath=F))
    
    changedPath.Exp <- inFile$datapath!=path 
  if(is.null(data) | changedPath.Exp | header!=input$header | sep!=input$sep| !is.null(input$AnnotData)){

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



 
  
  output$HistPlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    ExpData <- ExpData()$data

    if(!is.null(ExpData)){
     
      group <<-NULL
    
      if(is.null(GroupData()$data)){
        group <<- rep("1",nrow(ExpData))
      }else{
        group <<- GroupData()$data
      }
      print('hist')
      print(group)
      group.tmp <-NULL
      
      group.tmp <- as.factor(rep(group,ncol(ExpData)))

      mes <- data.frame(Expression=as.vector(unlist(ExpData)),Group=group.tmp)
      print(head(mes))
      #density plot
      #m <- qplot(Expression, data=mes, geom='density',fill="red") + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())
    
      m <- ggplot(mes, aes(x=Expression,color=Group,fill=Group)) 
      #m <- m + geom_histogram(aes(fill = ..count..)) + theme_bw() + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) 
      m <- m + geom_histogram(alpha=0.8) + theme_bw() #theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) #+ scale_fill_brewer(palette="Dark2") 
      print(m)
      #hist(as.numeric(unlist(ExpData)),main="Histogram of the expression data")

    }
  })
 
output$DensityPlot <- renderPlot({
  # The palette with grey:
  #cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # The palette with black:
  #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  # To use for fills, add
  scale_fill_manual(values=cbPalette)
  
  # To use for line and point colors, add
  scale_colour_manual(values=cbPalette)
  # generate an rnorm distribution and plot it
  ExpData <- ExpData()$data
  
  if(!is.null(ExpData)){
    group <<-NULL
    
    ifelse(is.null(GroupData()$data),group <<- rep("1",nrow(ExpData)),group <<- GroupData()$data)
    print(head(group))
    group.tmp <-NULL
    group.tmp <- as.factor(rep(unlist(group),ncol(ExpData)))
    print(head(group.tmp))
    mes <- data.frame(Expression=as.vector(unlist(ExpData)),Group=group.tmp)
    #density plot
    #m <- qplot(Expression, data=mes, geom='density',fill="red") 
    m <- qplot(Expression, color=Group, data=mes, geom='density', alpha=0.2)+  theme_bw()# +  theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())#+ scale_fill_brewer(palette="Dark2") 
    print(m)
  }

})
 
 
#   output$Summary <- renderPrint({
#     ExpData <- ExpData()$data
#     if(!is.null(ExpData))
#     summary(ExpData())
#   })

 
output$Boxplot <- renderPlot({

  ExpData <- ExpData()$data
  if(!is.null(ExpData)){
    
  numberRow <- nrow(ExpData)
  numberCol <- ncol(ExpData)
  mes <- data.frame(Expression=as.vector(unlist(ExpData)),Sample=1:numberRow)
  geom <- 'boxplot'
  main <-"Boxplot of the samples"
  if(input$densSample){
    geom <-'density'
    main <-"Density of the samples"
    q <-qplot(Expression,color=as.factor(Sample),geom =geom ,data=mes)+theme_bw()+ theme(legend.position="none")#theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),legend.position="none")#+ scale_fill_brewer(palette="Dark2") 
    
  }else{
    q <-qplot(y=Expression,x=Sample,main=,fill=as.factor(Sample),geom =geom ,data=mes)+theme_bw()+ theme(legend.position="none")#theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank(),legend.position="none")#+ scale_fill_brewer(palette="Dark2") 
    
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

noiseData <- reactive({
  
  ExpData <- ExpData()$data
  time <- TimeData()$data
  group <- as.character(GroupData()$data)
  replicate <- RepData()$data
  logfc <- input$logfc
  f <- FALSE
  m <- FALSE
  if((!is.null(ExpData) & !is.null(time) & !is.null(group) & !is.null(replicate)) | changedPath()|logfc!=log){
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size)
    print('reactive noise')
    if(fcUsed()){
      if(is.null(fc2)){
        f <- TRUE
      }else{
        if(is.null(fcInput())){
          f<- FALSE
        }else{
          f <- fc2!=fcInput()
          
        }
      }
    }
    
    if(missUsed()){
      if(is.null(numMis)){
        m <- TRUE
      }else{
        if(is.null(input$miss)){
          m <- FALSE
          
        }else{
          m <- numMis!=input$miss
        }
        
      }
    }
    
    equal_gr <- FALSE
    current_filter_input <- input$filter_gr
    
    if(length(current_filter_input)==0){
      equal_gr <- FALSE
    }else{
      current_filter_input<- as.character(current_filter_input )
      equal_gr <- current_filter_input!=current_group
    }

    if(is.null(df)|equal_gr|m|f|logfc!=log){ 
      
      if(is.null(df) |equal_gr|logfc!=log){
        gr <- 1:length(unlist(time))
        if(length(current_filter_input)!=0){
          gr <- gr[group==input$filter_gr]
          current_group <<- current_filter_input
        }
        


        n <-investNoise(data = ExpData[gr,],time =time[gr] ,sampleID=replicate[gr],log = logfc)
        index.na <- which(!is.na(n@RT)|!is.na(n@RI)|!is.infinite(n@RI)|!is.infinite(n@RT))
        data2 <<- data.frame(RT=signif(n@RT,2),RI=signif(n@RI,2),propMiss=signif(n@propMissing,2),FC=signif(n@foldChange,2),Name=n@name)[index.na,]
      }
      
      if(!is.null(data2)){
        index <- rep(TRUE,length(data2$RT))
        
        if(missUsed() & fcUsed()){      
          
          if(!is.null(fcInput())&!is.null(input$miss)){
            index[((data2$propMiss>input$miss) | (data2$FC<fcInput()))] <- FALSE
            fc2 <<-fcInput()
            numMis <<-input$miss
          }
        }
        if((!fcUsed()) & missUsed()){
          if(!is.null(input$miss)){
            index[data2$propMiss>input$miss] <- FALSE
            numMis <<-input$miss
          }
        }
        if((!missUsed()) & fcUsed()){
          if(!is.null(fcInput())){
            index[data2$FC<fcInput()] <- FALSE
            fc2 <<-fcInput()
          }
        }
        
        if(is.null(index))
          index <- rep(TRUE,nrow(data2))
        
        df <- data2[index,]
      }
      
    }
  }
  log <<- input$logfc
  return(df)
})



missUsed <- reactive({
  return(input$numMissingUsed)
})

fcUsed <- reactive({
  return(input$fcUsed)
})

missInput <- reactive({
  return(input$miss)
})

fcInput <-  reactive({
  return(input$fcs)
})
output$BubbleGvis <- renderGvis({
  
  # generate an rnorm distribution and plot it
  df <- noiseData()
  
  if(!is.null(df)){
    bubble2 <- gvisBubbleChart(df, idvar="Name", xvar="RT", yvar="RI", sizevar="FC",options=list(title='Fold change',hAxis="{title: 'R_T'}",vAxis="{title: 'R_I'}",colorAxis="{colors: ['blue', 'orange']}",bubble="{stroke:'none',opacity:0.4,textStyle:{color: 'none'}}",sizeAxis="{minValue: 0,  maxSize: 5}"))
  }
  
  return(bubble2)
  
})

output$BubbleGvis2 <- renderGvis({
  df <- noiseData()
  if(!is.null(df)){
    bubble <- gvisBubbleChart(df, idvar="Name", xvar="RT", yvar="RI", sizevar="propMiss",options=list(title='Proportion of missing values', hAxis="{title: 'R_T'}",vAxis="{title: 'R_I'}", colorAxis="{colors: ['green','blue', 'red']}",bubble="{stroke:'none',opacity:0.4,textStyle:{color: 'none'}}",sizeAxis="{minValue: 0,  maxSize: 5}"))
  }
  return(bubble)
})


output$MCLUST <- renderPlot({
  
  # generate an rnorm distribution and plot it
  
  df <- noiseData()
  
  if(!is.null(df)){
    withProgress(message = 'Clustering data', value = 0.1, {
    index.na <- which(!is.na(df$RT)&!is.na(df$RI)&!is.infinite(df$RT)&!is.infinite(df$RI))
    class<- mixmodCluster(data.frame(cbind(df$RT,df$RI)[ index.na,]),nbCluster = 2)@bestResult@partition
    cl <- unique(class)
    tcl <- ifelse(mean(df$RT[class==cl[1]],na.rm=T)<mean(df$RT[class==cl[2]],na.rm=T),2,1)

    keep <- rep('yes',length(class))
    
    keep[class==cl[tcl]] <- 'no'
    df$keep <- keep
    clustplot <- qplot(RT,RI,data=df[index.na,],colour=keep,size=2,type="n",xlab="R_T",ylab="R_I",main="Classification using \n model based clustering and two clusters")+ theme_bw()#+ theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) 
    })
    return(clustplot)
  }else{
    return()
  }
})



output$filter_group <- renderUI({
  
  if(!is.null(GroupData()$data) | GroupData()$changedPath){
    grpf <- unique(as.character(GroupData()$data))
    selectInput("filter_gr", "Choose a group:", 
                choices = grpf)
  }
})


filterGroupSelected <- reactive({
  
  group <- as.character(GroupData()$data)
  g <- unique(na.omit(group))
  if(length(g)>1){
    return(switch(input$filter_gr,
                  "G1" = g[1],
                  "G2" = g[2]))
  }else{
    return('G1')  
  }
  
})


output$RI_Filter <- renderUI({
  noiseData()
  if(!is.null(df))
    numericInput("RI_Filt", HTML(paste("R", tags$sub("I"), sep = "")), 0.3,
                 min = min(df$RT,na.rm=T), max = max(df$RT,na.rm=T),step=0.1)
  
})

output$RT_Filter <- renderUI({
  
  noiseData()
  if(!is.null(df))
    numericInput("RT_Filt", HTML(paste("R", tags$sub("T"), sep = "")), 0.9,
                 min = min(df$RI,na.rm=T), max = max(df$RI,na.rm=T),step=0.1)
  
})

output$fc_slider <- renderUI({
  df <<- noiseData()
  if(!is.null(df)){
    fc3 <-df$FC
    if(is.null(f.slider)) {
      #  if(is.null(f.slider) | changeFC){
      ymin.fc <<- min(fc3,na.rm = T)
      ymax.fc <<- max(fc3,na.rm = T)
      f.slider <<- ymin.fc
    }else{
      if(length(fcInput())==0 | is.null(fcInput())){
        inp.fc <- 0
      }else{
        inp.fc <- fcInput()
      }
      f.slider <<- ifelse(inp.fc<ymin.fc,ymin.fc,inp.fc)
    }
    
    
    sliderInput("fcs", "Fold change",
                min = signif(ymin.fc,2), max = signif(ymax.fc,2),
                value = signif(f.slider,2))
  }
  
  
})




output$missing_slider <- renderUI({
  noiseData()
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
output$resetedFilter <- renderText({
  
  if(input$ResetFilter==0)
    return('')

    indexFinal <<- rep(TRUE,length(indexFinal))
    lmm <<- NULL
    lmm.de <<- NULL
    return('Reset all filters.')
  
})


output$summaryFilter <- renderText({
  if(input$ApplyFilter==0 )
    return('')
  
    lmm <<- NULL
    lmm.de <<- NULL
    return(Filtervals())

})

output$summarySoftFilter <- renderText({
  if(input$ApplyFilterSoft==0)
    return('')
  
  lmm <<- NULL
  lmm.de <<- NULL
  return(FilterSoftvals())

})



FilterSoftvals <- function(){
  summa <- ""
  ExpData <- isolate(ExpData()$data)
  time <- isolate(TimeData()$data)
  group <- as.character(GroupData()$data)
  replicate <- RepData()$data
  grp <- unique(na.omit(group))

  
  if((!is.null(ExpData) & !is.null(time) & !is.null(group) & !is.null(replicate)) | changedPath()){
    
    
    if(length(grp)==2){
      gr1 <- which(group==grp[1])
      n1 <-investNoise(data = ExpData[gr1,],time =time[gr1] ,sampleID=replicate[gr1])
     
      da1 <- data.frame(time=n1@RT,ind=n1@RI)
      gr2 <- which(group==grp[2])
      n2 <-investNoise(data = ExpData[gr2,],time =time[gr2] ,sampleID=replicate[gr2])
  
      da2 <- data.frame(time=n2@RT,ind=n2@RI)
    
      if(input$numMissingUsed){
        misdata <- isolate(input$miss)
        print(misdata)
        indexFinal[n2@propMissing>=misdata|n1@propMissing>=misdata] <- FALSE
    
    }
  
      if(input$fcUsed){
        fcfilt <- isolate(fcInput())
        print(fcfilt)
        indexFinal[n2@foldChange<=fcfilt|n1@foldChange<=fcfilt] <- FALSE
      }
  
    
    }else{
      n1 <-investNoise(data = ExpData,time =time ,sampleID=replicate)
      da1 <- data.frame(time=n1@RT,ind=n1@RI)
  
      if(input$numMissingUsed){
     
      misdata <- isolate(input$miss)
      print(misdata)
      indexFinal[n1@propMissing>=misdata] <- FALSE
    
    }
  
    if(input$fcUsed){
   
      fcfilt <-  isolate(fcInput())
      print(fcfilt)
      indexFinal[n1@foldChange<=fcfilt] <- FALSE
    }
}

}
indexFinal <<- indexFinal

summa <- paste("Removed molecules", sum(!indexFinal,na.rm=F), 'after soft filtering.')
}
  
Filtervals <- function(){
  summa <- ""
  ExpData <- isolate(ExpData()$data)
  time <- isolate(TimeData()$data)
  group <- as.character(GroupData()$data)
  replicate <- RepData()$data
  grp <- unique(na.omit(group))

  if((!is.null(ExpData) & !is.null(time) & !is.null(group) & !is.null(replicate)) | changedPath()){
    
    if(length(grp)==2){
      gr1 <- which(group==grp[1])
      n1 <-investNoise(data = ExpData[gr1,],time =time[gr1] ,sampleID=replicate[gr1])
      index.na1 <- which(!is.na(n1@RT)&!is.na(n1@RI)&!is.infinite(n1@RI)&!is.infinite(n1@RT))
      da1 <- data.frame(time=n1@RT,ind=n1@RI)
      gr2 <- which(group==grp[2])
      n2 <-investNoise(data = ExpData[gr2,],time =time[gr2] ,sampleID=replicate[gr2])
      index.na <- which(!is.na(n2@RT)&!is.na(n2@RI)&!is.infinite(n2@RI)&!is.infinite(n2@RT))
      da2 <- data.frame(time=n2@RT,ind=n2@RI)
  
      print(input$FilterRad)
      if(input$FilterRad=="model"){
        class1<- mixmodCluster(data.frame(cbind(da1$time,da1$ind)[index.na1 & index.na,]),nbCluster =2)@bestResult@partition
        cl1 <- unique(class1)
        tcl1 <- ifelse(mean(da1$time[index.na&index.na1][class1==cl1[1]],na.rm=T)<mean(da1$time[index.na&index.na1][class1==cl1[2]],na.rm=T),1,2)
      
        class2<- mixmodCluster(data.frame(cbind(n2@RT,n2@RT)[index.na1 & index.na,]),nbCluster =2)@bestResult@partition
        cl2 <- unique(class2)
        tcl2 <- ifelse(mean(da2$time[index.na&index.na1][class2==cl2[1]],na.rm=T)<mean(da2$time[index.na&index.na1][class2==cl2[2]],na.rm=T),1,2)
    #take the missing data in account
        indexFinal[index.na&index.na1] <<- (class1==cl1[tcl1] | class2==cl2[tcl2])& indexFinal
        indexFinal[!(index.na&index.na1)] <<-FALSE
        summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with model based clustering. ', sep=" ")
      }else{
        RT <- as.numeric(input$RT_Filter)
        RI <- as.numeric(input$RI_Filter)
        index1 <- c(n1@RI<=RI & n1@RT<=RT & indexFinal)
        index2 <- c(n2@RI<=RI & n2@RT<=RT & indexFinal)
      
        indexFinal <<- (index1 | index2) 
        summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with fixed RT=',RT,'and RI=',RI,'.', sep=" ")
    }
    }else{
      n1 <-investNoise(data = ExpData,time =time ,sampleID=replicate)
      index.na1 <- which(!is.na(n1@RT)&!is.na(n1@RI)&!is.infinite(n1@RI)&!is.infinite(n1@RT))
      da1 <- data.frame(time=n1@RT,ind=n1@RI)
      
      if(input$FilterRad=="model"){
        class1<- mixmodCluster(data.frame(cbind(da1$time,da1$ind)[index.na1,]),nbCluster = 2)@bestResult@partition
        cl1 <- unique(class1)
        tcl1<- ifelse(mean(da1$time[class1==cl1[1]],na.rm=T)<mean(da1$time[class1==cl1[2]],na.rm=T),1,2)
        indexFinal[index.na1] <<- class1==cl1[tcl1] & indexFinal
        indexFinal[!index.na1] <<- FALSE
        summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with model based clustering. ', sep=" ")
    }else{
        RT <- as.numeric(input$RT_Filter)
        RI <- as.numeric(input$RI_Filter)  
        indexFinal <<- c(n1@RI<=RI & n1@RT<=RT & indexFinal)
        summa <- paste(sum(indexFinal,na.rm=T),' molecules remaining \n after filtering with fixed RT=',RT,'and RI=',RI,'.', sep=" ")
    }
  }
  
  
}

  if(sum(indexFinal,na.rm=T)==0){
    indexFinal <<- rep(TRUE,length(indexFinal))
    summa <- "No molecules left. Please check your settings."
  }
  return(summa)

}

  ################# Modelling functions ###########
  
LMMData <- reactive({
  print('iso')
  print(isolate(input$Modelling)==0)
  #lmm <- NULL
  if (is.null(lmm) & input$Modelling==0)
    return()
  
  print(length(indexFinal))
  ExpData <- ExpData()$data[,indexFinal]

  time <- TimeData()$data
  #group <- GroupData()$data
  replicate <- RepData()$data
  annotation <- as.character(unlist(AnnotData()))[indexFinal]
  
  if(!is.null(annotation)){
    colnames(ExpData) <- annotation
  }

#  print(group)
  if(!is.null(ExpData)&!is.null(time)&!is.null(replicate)&is.null(lmm)){
 # if(!is.null(ExpData)&!is.null(time)&!is.null(replicate)&!is.null(group)&is.null(lmm)){
    
   withProgress(message = 'Modelling in progress', value = 0.1, {
  #  lmm <- 

  #  lmm <-new('lmmspline',predSpline= as.data.frame(matrix(rnorm(100,1,2),ncol=4)),modelsUsed=rep(1,25),basis='basis',knots=c(1,2),errorMolecules="NULL",models=list(), derivative=F)
    
    # rm(lmmSpline)
    # source('Scripts/lmmSpline.R')
     
    lmm <<- lmmSpline(data=ExpData,sampleID=replicate, time=time, basis=isolate(input$Basis),keepModels = F)
    
    summary(lmm)
    })
  }else{
    warning("Please check the availability of the expression data, group, time and replicates.")
  }
  
  lmm
  
})  

output$ModelTable<- renderText({
  print('renderTab')
  input$Modelling
  lmm <- LMMData()
  if(is.null(lmm))
    return()
  p <- paste(names(table(lmm@modelsUsed)),as.vector(table(lmm@modelsUsed)),sep=":")
  print(p)
  HTML(paste("Table of models used to model the molecule expression:", paste(p,collapse='<br>'),sep="<br>"))
  
})


  
output$downloadModel <- downloadHandler(
  print('download'),
  filename = function() { paste('ModelledTrajectories',Sys.Date(),'.csv', sep='') },
  content = function(file) {
    write.csv(LMMData()@predSpline, file,row.names=F)
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
  df <- data.frame(Molecule=rownames(lmm@predSpline),Model=lmm@modelsUsed)
  dt <- datatable(df,selection = 'single')
  
  return(dt)
}, options = list(server=TRUE,rownames=TRUE))


output$ModelPlot <- renderPlot({
  
  s <- input$ModelTables_rows_selected
  if (!length(s))
    return()
  
  l <- LMMData()
  ## Currently a data table bug as it should select only single arguments
  ### CHANGE IF DT R package is updated
  v <- as.numeric(s)[length(s)]
  # v <- which(l@DE$Molecule%in%input$DETable[1])
  
  
  
  ExpData <- ExpData()$data[,indexFinal]
  time <- TimeData()$data
  group <- GroupData()$data
  
  p <- plot(l,v,data=ExpData,type=input$Radio_DEplot,time = time,mean=input$ModelPlotMean,smooth=input$ModelPlotSmooth)+theme_bw()
  print(p)
})


  ########Clustering functions########
  limit_data_range <- function() {
    # ------------------------------------------------------------------
    # Because we're using reactiveUI for x_range and y_range, they start
    # out as null, then get resolved after the client and server talk a bit.
    # If they are not yet set, there will be some errors in this function, so
    # do nothing for now (this function will be run again).
    if (is.null(input$x_range) || is.null(input$y_range)) {
      return(NULL)
    }
  }
  
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$mpgPlot expressions
  formulaText <- reactive({
    var <- switch(input$variable,
           "hc"="Hierarchical clustering", 
           "diana"  =  "Diana" , 
           "km"="Kmeans" ,
            'model'= 'Model based clustering',
           'som'='Self-organizing maps')
    paste(var,"with",input$cluster_num,'cluster')
  })
  
  formulaText2 <- reactive({
    nam <- paste(c("HC"," Kmeans"," SOM"," Mclust"," PAM")[c(input$hierarchical,input$kmeans,input$som,input$model,input$pam)],collapse=",")
    paste(nam,"with",min(input$cluster_range),"-",max(input$cluster_range),"cluster")
  })
  

  
  # Generate a plot of the requested variable 
  
  
  makeValiPlot <- function(list,range,correlation){
    cluster.sel <- c(1:5)[list]
    #print(input$submitVali)
    range<-seq(range[1],range[2],1)
    #isolate(ifelse(input$submitVali==0,range<-,range<-2:3))
    
    #ifelse(is.null(input$cluster_range),range <- c(2:3),range <- seq(input$cluster_range[1],input$cluster_range[2],1))
    lmms <- LMMData()
    print('validation')
    if(is.null(lmms)){
      lmms <- t(ExpData()$data[,indexFinal])
    }else{
      lmms <- lmms@predSpline
    }
    
    stab <- NULL
    ifelse(correlation,metric <- "correlation",metric <- "euclidean")
    withProgress(message = 'Cluster validation in progress', value = 0.1, {
    stab <- clValid2(lmms, range, clMethods=c("hierarchical","kmeans","som","model","pam")[cluster.sel],validation="stability",metric=metric,maxitems=nrow(lmms))

    ano.plot <- ano.boxplot(stab$measures,clMethods=c("hc","km","som","model","pam")[cluster.sel],measurement=c("CPN"))
    })
  }
  
  
  cluster <- function(algo,num.cluster,cor){
    
    #ifelse(is.null(input$cluster_num),num.cluster <-3 ,num.cluster <-input$cluster_num)

    withProgress(message = 'Clustering in progress', value = 0.1, {
    tmp.data<-LMMData()
    print('cluster')
    if(is.null(tmp.data)){
      tmp.data <- t(ExpData()$data[,indexFinal])
    }else{
      tmp.data <-tmp.data@predSpline
    }
    
    if(cor)
      tmp.data <- 1-cor(t(tmp.data),use="pairwise.complete.obs")
    if(algo=="hc"){
      ifelse(cor,t <-as.dist(tmp.data),t <- dist(tmp.data))
      tree <- hclust(t,method="ward")
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
      k <- mixmodCluster(data.frame(tmp.data),nbCluster = num.cluster)
      classifi <<- k@bestResult@partition
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
    print('cluster plot')
    if(is.null(data.lmm)){
     data.lmm <- t(ExpData()$data[,indexFinal])
    }else{
      data.lmm <- data.lmm@predSpline
    }
  
    time <- TimeData()$data
    t.s <-sort(unique(time))
    classification <- isolate(classif())
    if(input$Smoothed){
      s <- apply(data.lmm,1, function(x)spline(t.s,x,n=50)$y)
      t <- seq(range(time)[1],range(time)[2],length.out=50)
      pred.data <- data.frame(intensity=as.vector(s),Protein=rep(rownames(data.lmm),each=length(t)),Time=rep(t,dim(data.lmm)[1]),Cluster=factor(rep(classification,each=length(t)),levels=sort(unique(classification))))
      
    }else{

      pred.data <- data.frame(intensity=as.vector(unlist(t(data.lmm))),Protein=rep(rownames(data.lmm),each=length(t.s)),Time=rep(t.s,dim(data.lmm)[1]),Cluster=factor(rep(classification,each=length(t.s)),levels=sort(unique(classification))))      
      
    }
   
    u <- sort(unique(classification))
    l <- tapply(classification,classification,length)
    header <- paste("C",u," ",", p=",l,"",sep="")
    
    p0 <- xyplot(intensity~Time|Cluster, data=pred.data, type="l", groups=Protein,
                 xlab="Time", ylab = "Intensity", col="grey", lty = 2:5,strip = strip.custom(factor.levels=header),par.settings = list(strip.background=list(col="white")),cex = 0.3,layout=c(length(u),1))
    p1 <- xyplot(intensity~Time|Cluster, data=pred.data, type="a",
                 xlab="Array", ylab = "Intensity", col="black", lty = 1,
                 lwd = 2)
    
    print(p0 + p1)
#     plot <- xyplot(intensity~Time|Cluster, data=pred.data, type="l", groups=Protein,col="black", ylab = "Intensity")
#     plot1 <- xyplot(intensity~Time|Cluster, data=pred.data, type="a", groups=Protein,col="red",lwd = 2, ylab = "Intensity")
#     p <- plot+plot1
#     print(p)
#     #return(p)
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
    if (input$submitVali== 0)
      return()
    if(input$submitVali){
      isolate(makeValiPlot(c(input$hierarchical,input$kmeans,input$som,
                           input$model,input$pam),input$cluster_range,input$correlation))
      }else{
        return()
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
    isolate(makePlot(input$variable,input$cluster_num,input$correlation))
    })
  
  
  classif <- reactive({
   
    if(input$variable!=v |input$cluster_num!=num |input$correlation!=core){
      v <<-input$variable
      num <<-input$cluster_num
      core <<-input$correlation
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
      l <- ExpData()$data[,indexFinal]
      a <-  as.character(unlist(AnnotData()))
      annonames <- if(is.null(colnames(l))) a else colnames(l)
    }else{
      
      annonames <- rownames(l@predSpline)
    }
    
    cl <- isolate(classif())
    if(is.null(cl))
      return()
    
    if(length(class)!=length(cl)| !all(cl==class)){
      class <<- cl
      print('GO enrich')
      
      withProgress(message = 'GO enrichment analysis in progress', value = 0.1, {
      enrich <<-biological.homogenity(cl,ident=annonames,identifier="IPI",ontology=input$ontology,db=input$organism)     
      })
    }
    
    print(head(enrich))
    enrich
    
  })
  
  
############### Differential Expression functions #############


output$DEInfoText <- renderText({
  if (input$DEtable== 0)
    return()
  #input$submitVisu
  print('Update text')
  paste("Analysed",sum(indexFinal,na.rm=T),"molecules for differential expression.")
})


output$DETable =  DT::renderDataTable({
  if (input$DEtable== 0)
    return()
 
   de <- datatable(DEoutput()@DE,selection = 'single')
  
   return(de)
}, options = list(server=TRUE,rownames=TRUE))



DEoutput <- reactive({
  if (input$DEtable== 0 & is.null(lmm.de))
    return()

  ExpData <- ExpData()$data[,indexFinal]
  
  time <- TimeData()$data
  group <- GroupData()$data
  replicate <- RepData()$data
  if((!is.null(ExpData)&!is.null(time)&!is.null(replicate)&!is.null(group)&is.null(lmm.de))| changedPath()){
    withProgress(message = 'Differential expression analysis in progress', value = 0.1, {
    lmm.de <<- lmmsDE(data=ExpData,sampleID=replicate, time=time, group=group,type=isolate(input$Type),basis=isolate(input$Basis),experiment=isolate(input$experiment))
    })
     }else{
    warning("Please check the availability of the expression data, group, time and replicates ")
  }
  lmm.de
})

output$DEPlot <- renderPlot({
 
  s <- input$DETable_rows_selected
  if (!length(s))
    return()
  
  l <- DEoutput()
## Currently a data table bug as it should select only single arguments
  ### CHANGE IF DT R package is updated
  v <- as.numeric(s)[length(s)]
 # v <- which(l@DE$Molecule%in%input$DETable[1])



  ExpData <- ExpData()$data[,indexFinal]
  time <- TimeData()$data
  group <- GroupData()$data

  p <- plot(l,v,data=ExpData,type=input$Radio_DEplot,group = group,time = time,mean=input$DEPlotMean,smooth=input$DEPlotSmooth)+theme_bw()
  print(p)
})

##write data
datasetClassifInput <- reactive({
  l <- LMMData()
  print('LMMData')
  if(is.null(l)){
    l <- ExpData()$data[,indexFinal]
    a <- as.character(unlist(AnnotData()))[indexFinal]
    
    # print(colnames(l))
    annonames <- if(is.null(colnames(l))) a else colnames(l)
    #print(annonames)
  }else{
    annonames <- rownames(l@predSpline)
  }

  data.frame(ID=annonames, Cluster=isolate(classif()))
})
  
output$downloadClassifData <- downloadHandler(

  filename = function() { paste('Classification',input$variable,input$cluster_num,Sys.Date(),'.csv', sep='') },
  content = function(file) {
    write.csv(datasetClassifInput(), file,row.names=F)
  }
)

output$downloadClusterPlot <- downloadHandler(
  filename <- function() {
    paste('Plot_',input$variable,input$cluster_num,Sys.Date(),'.ps',sep='') },
  content <- function(file) {
    postscript(file, width = 4.92, height = 3.2,
               family = "Arial", paper = "special", onefile = FALSE,
               horizontal = FALSE, pointsize = 12,bg = "white")
    top6.plot <- makePlot(input$variable,input$cluster_num,input$correlation)
    print(top6.plot)
    dev.off()},
  contentType = 'image/ps'
)

  output$downloadData <- downloadHandler(
    filename = function() { paste('GOEnrichment',input$variable,input$cluster_num,Sys.Date(),'.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file,row.names=F)
    }
  )
  
output$downloadDEData <- downloadHandler(
  filename = function() { paste('DEAnalysis',input$Type,Sys.Date(),'.csv', sep='') },
  content = function(file) {
    write.csv(DEoutput()@DE, file,row.names=F)
  }
)


###### PDF VIEWER ########
output$pdfviewer <- renderText({
  pdf <- '<iframe style="height:600px; width:100%" src="TimeOmics_userguide.pdf",target="_self"></iframe>'
  observeEvent(input$ShowUserGuide, {
    print('event')
    pdf <- '<iframe style="height:600px; width:100%" src="TimeOmics_userguide.pdf",target="_self"></iframe>'
  })
  return(pdf)
  })
  
})