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

biological.homogenity <- function(cluster,ident,ontology="all",identifier="Gene symbol",db="org.Hs.eg.db",background=NULL){
  
  if(length(grep("^IPI",ident[1:10]))>0)
    identifier <-"IPI"

  getPackage <- function(pkg, load = TRUE, silent = FALSE, repos = "http://cran.us.r-project.org") {
    if(!suppressMessages(suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE)))) {
      cat(paste("Package", pkg, "not found, attempting download from Bioconductor\n", 
                sep = " "))
      
      source("http://bioconductor.org/biocLite.R")
      try(biocLite(pkg),silent = T)
    }
    if(load) suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
    if(load & !silent) message("Loaded ", pkg)
  }

  d <- c('GO.db','annotate','plyr','AnnotationDbi','annotate',db)
   lapply(d, getPackage, silent = TRUE)
  
  identifiers <- c("Gene symbol","IPI")
  if(!identifier %in% identifiers)
    stop(paste("Please select one of the following identifiers:", identifiers,collapse = " "))
  
  ont.selection  <-c("BP","CC","MF","all")

  size <- length(ident)
  if(!(ontology%in%ont.selection))
    stop(paste("Choose an ontology from the selection", ont.selection[1],ont.selection[2],ont.selection[3],ont.selection[4]))
  
  if(length(ident)!=length(cluster))
    stop("Annotation and cluster must have the same length")
  if(identifier =="IPI"){
    #h <- toTable(org.Hs.egPROSITE)
    
    allAnnots <-AnnotationDbi::select(org.Hs.eg.db, keys=keys(org.Hs.eg.db), columns = c("GO","IPI"))
  
    if(!is.null(background))
     allAnnots <-AnnotationDbi::select(org.Hs.eg.db, keys=background, columns = c("GO","IPI"),keytype="IPI")
     
 
    
    #need to remove dublicated ipi_ids for downstream analysis
   # h$ipi_id <-  gsub(pattern=" ",replacement="",h$ipi_id)
   # h$gene_id <- gsub(pattern=" ",replacement="",h$gene_id)
   # h <- h[!duplicated(h$ipi_id),]
   # allAnnots <- merge(h, toTable(org.Hs.egGO), by.x="gene_id", by.y="gene_id")
  
   #allAnnots <- select(org.Hs.eg.db, keys=ident, columns = c("GO","IPI"),keytype="IPI")
   
  # allAnnots <- allAnnots[!duplicated(allAnnots[,1:2]),]
    names(allAnnots)[which(names(allAnnots)=="IPI")] <- "ident_id"
   names(allAnnots)[2] <- "go_id"
   names(allAnnots)[4] <- "Ontology"
  }else{
    #take care same symbols can have different gene_ids
    t <-toTable(org.Hs.egSYMBOL2EG)
    if(!is.null(background))
      t <- t[t$symbol%in%background,]
    allAnnots <- merge(t, toTable(org.Hs.egGO), by.x="gene_id", by.y="gene_id")
    names(allAnnots)[2] <- "ident_id"
    
  }
  
  #duplicated allAnnots$ident_id + allAnnots$go_id 
  allAnnots <- allAnnots[!duplicated(data.frame(allAnnots$ident_id,allAnnots$go_id)),]
  num.mol.all <- length(unique(allAnnots$ident_id))

  ont.index <- 1:dim(allAnnots)[1]
  #obviously ids they have white spaces and the comparison 
 # allAnnots$Ontology <- gsub(pattern=" ",replacement="",allAnnots$Ontology)
 # allAnnots$ident_id <- gsub(pattern=" ",replacement="",allAnnots$ident_id)
 # allAnnots$go_id <- gsub(pattern=" ",replacement="",allAnnots$go_id)
 
  
  if(ontology!="all")
    ont.index <- which(allAnnots$Ontology==ontology)
  
  selectedAnnots <- allAnnots[intersect(which(allAnnots$ident_id%in%ident),ont.index),]
  
  if(dim(selectedAnnots)[1]==0)
    stop("Identifiers do not match. Please check identifiers")

  array <- array(list(),dim=length(unique(cluster)))
  
  rows <-NULL
  bhi <- NULL
  bhis <-  NULL
  clusters <- NULL
  GODescrps <- NULL
  adj.p.values <-   NULL
  p.values <- NULL
  Counts <-  NULL
  Num.Mols <- NULL
  GOs <-  NULL
  Cluster.size <- NULL
  Ontology <- NULL
  log.odds <-  NULL
  
  for(i in sort(unique(cluster))){
    ident.c <- NULL
    ident.c <- ident[which(cluster==i)]

    drawn <- length(ident.c)
      w <- which(selectedAnnots$ident_id%in%ident.c)
      if(length(w)==0)
        next
    
    tmp.annots <- selectedAnnots[sort(w),]
      

#     sum <- NULL
#     for(j in 1:drawn-1){
#       
#       for(k in (j+1):drawn){
#         
#         # GOs<- c(GOs,intersect(tmp.annots$go_id[which(tmp.annots$ident_id==ident.c[j])],tmp.annots$go_id[which(tmp.annots$ident_id==ident.c[k])]))
#         sum <- sum(sum,(any(tmp.annots$go_id[which(tmp.annots$ident_id%in%ident.c[j])] %in% tmp.annots$go_id[which(tmp.annots$ident_id%in%ident.c[k])])))
#       }
#     }

    tGOs <- table(tmp.annots$go_id)
    #break
    df1 <- c()
    for(l in 1:length(tGOs)){
      num.mol.cluster <- NULL
      num.mol.cluster <-tGOs[l]
      
      num.mol <- NULL
      num.mol <- length(which(allAnnots$go_id%in%names(tGOs)[l]))
         
      num.sample <- size-num.mol
      
      #q number of obs molecules in cluster
      #m number of molecules in the data
      #n all-m
      #k cluster size
      DrawnTable <- matrix(c(num.mol.cluster, (drawn-num.mol.cluster), (num.mol-num.mol.cluster), (num.mol.all-(num.mol-num.mol.cluster))),nrow = 2,dimnames =list(c("Function", "Other-Function"),c("drawn", "not-drawn")))

      f <- fisher.test(DrawnTable,alternative="greater")
      p <- f$p.value
      if(num.mol.cluster> drawn){
        warning("Number of enriched molecules is bigger than the cluster size")
      }
      
     # p <-signif(phyper(q=num.mol.cluster-1,m=num.mol,n=num.mol.all-num.mol,k=drawn,lower.tail=F),2)
     # print(paste("fish",f.p,"phyp",p))
      log.odd <- log(f$estimate,2)
      df1 <- rbind(df1,c(names(tGOs)[l],num.mol.cluster,num.mol,drawn,p,log.odd))
    }
    df1 <- as.data.frame(df1)
    colnames(df1) <- c("GO","Count","Num.Mol","Cluster.size","p.value","log.odds.ratio")
    df1$p.value <- as.numeric(as.character(df1$p.value))
    df1$log.odds.ratio <- as.numeric(as.character(df1$log.odds.ratio))
    df1$Count <- as.numeric(as.character(df1$Count))
    df1$Num.Mol <- as.numeric(as.character(df1$Num.Mol))
    
    df1 <- arrange(df1,p.value,desc(Count))
    rows <-dim(df1)[1]
    #bhi <- (sum/(drawn*(drawn-1))/2)
   # bhis <- c(bhis,rep(bhi,rows))
    Cluster.size <- c(Cluster.size,rep(as.numeric(as.character(drawn)),rows))

    clusters <- c(clusters,rep(i,rows))
    GODescrps <- c(GODescrps,AnnotationDbi::Term(as.character(df1$GO)))
    adj.p.values <-  c(adj.p.values,signif(p.adjust(df1$p.value,"BH"),2))
    p.values <- c(p.values,df1$p.value)
    log.odds <- c(log.odds,df1$log.odds.ratio)
    Counts <- c(Counts, df1$Count)
    Num.Mols <- c(Num.Mols,df1$Num.Mol)
    GOs <- c(GOs,as.character(df1$GO))
    Ontology <- c(Ontology,Ontology(as.character(df1$GO)))
 
  } 
    

  
    #df <- (data.frame(Cluster=clusters,Counts=Counts,Num.Mols=Num.Mols,Cluster.size=Cluster.size,GO=GOs,GODescrp=GODescrps,Ontlogy=Ontology,BHI=bhis,p.values=p.values,adj.p.value=adj.p.values,log.odds.ratio=log.odds,stringsAsFactors=FALSE))
  df <- (data.frame(Cluster=clusters,Counts=Counts,Num.Mols=Num.Mols,Cluster.size=Cluster.size,GO=GOs,GODescrip=GODescrps,Ontology=Ontology,p.values=p.values,adj.p.value=adj.p.values,log.odds.ratio=log.odds,stringsAsFactors=FALSE))
    
  return(df)
}

IPItoGOID <- function(ipi){
  require(org.Hs.eg.db)
  mapping <- merge(toTable(org.Hs.egPROSITE), toTable(org.Hs.egGO), by.x="gene_id", by.y="gene_id")
  idents<- mapping[which(mapping$ipi_id%in%ipi),]
  idents <- idents[!duplicated(idents$ipi_id,idents$go_id),]
  return(idents)
}

IPItoGENESymbol <- function(ipi){
  require(org.Hs.eg.db)
  mapping <- merge(toTable(org.Hs.egPROSITE), toTable(org.Hs.egSYMBOL), by.x="gene_id", by.y="gene_id")
  idents<- mapping[which(mapping$ipi_id%in%ipi),]
  idents <- idents[!duplicated(idents$ipi_id,idents$go_id),]
  return(idents)
}
