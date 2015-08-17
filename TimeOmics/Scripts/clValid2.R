clValid2 <- function (obj, nClust, clMethods = "hierarchical", validation = "stability", 
        maxitems = 600, metric = "euclidean", method = "average", 
        neighbSize = 10, annotation = NULL, GOcategory = "all", goTermFreq = 0.05, 
        dropEvidence = NULL, verbose = FALSE, ...) {
  
  
  
  require(cluster)
  clMethods <- tolower(clMethods)
  clMethods <- match.arg(clMethods, c("hierarchical", "kmeans", "diana", "fanny", "som", "model", "sota", "pam", "clara", "agnes"), several.ok = TRUE)

  getPackage <- function(pkg, load = TRUE, silent = FALSE, repos = "http://cran.us.r-project.org") {
    if(!suppressMessages(suppressWarnings(require(pkg, character.only = TRUE, quietly = TRUE)))) {
      try(install.packages(pkg, repos = repos), silent = TRUE)
    }
    if(load) suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE))
    if(load & !silent) message("Loaded ", pkg)
  }
  d <- c('kohonen','Rmixmod')
  lapply(c(d), getPackage, silent = TRUE)
  
  if ("som" %in% clMethods) {
    if (!require(kohonen)) {
      stop("package 'kohonen' required for clustering using SOM")
    }
  }
  if ("model" %in% clMethods) {
    if (!require(Rmixmod)) {
      stop("package 'Rmixmod' required for model-based clustering")
    }
  }
  
  
  validation <- match.arg(validation, c("stability", "internal", 
                                        "biological"), several.ok = TRUE)
  metric <- match.arg(metric, c("euclidean", "correlation", 
                                "manhattan",'MIC'))
  method <- match.arg(method, c("ward", "single", "complete", 
                                "average"))
  GOcategory <- match.arg(GOcategory, c("all", "BP", "CC", 
                                        "MF"))
  switch(class(obj), matrix = mat <- obj, ExpressionSet = mat <- Biobase::exprs(obj), 
         data.frame = {
           if (any(!sapply(obj, class) %in% c("numeric", "integer"))) stop("data frame 'obj' contains non-numeric data")
           mat <- as.matrix(obj)
         }, stop("argument 'obj' must be a matrix, data.frame, or ExpressionSet object"))
  if (nrow(mat) > maxitems) {
    if (interactive()) {
      cat("\nThe number of items to be clustered is larger than 'maxitems'\n")
      cat("The memory and time required may be excessive, do you wish to continue?\n")
      cat("(y to continue, any other character to exit) ")
      ans <- tolower(substr(readLines(n = 1), 1, 1))
      if (ans != "y") {
        stop("Exiting clValid, number of items exceeds 'maxitems'")
      }
    }
    else {
      stop("The number of items to be clustered is larger than 'maxitems'\n  Either decrease the number of rows (items) or increase 'maxitems'\n")
    }
  }
  if ("clara" %in% clMethods & metric == "correlation") 
    warning("'clara' currently only works with 'euclidean' or 'manhattan' metrics - metric will be changed to 'euclidean'  ")
  if (is.character(annotation) & length(grep(".db", annotation)) == 
        0) {
    annotation <- paste(annotation, ".db", sep = "")
  }
  if (is.list(annotation)) {
    if (is.null(rownames(mat))) {
      stop("rownames of data must be present to specify biological annotation from file")
    }
    annotation <- annotationListToMatrix(annotation, genenames = rownames(mat))
  }
  if ("biological" %in% validation & is.null(annotation)) {
    stop("annotation must be specified in order to use biological validation")
  }
  if ("biological" %in% validation & is.character(annotation)) {
    if (!require(Biobase) | !require(GO.db) | !require(annotate)) {
      stop("packages 'Biobase', 'GO.db', and 'annotate' required for 2nd type of biological validation \n\nthese can be downloaded from Bioconductor (www.bioconductor.org)")
    }
  }
  if ("biological" %in% validation & is.null(rownames(mat))) {
    stop("rownames of data must be present to use biological validation")
  }
  nClust <- floor(nClust)
  if (any(nClust < 1)) 
    stop("argument 'nClust' must be a positive integer vector")
  
  if(metric == "correlation"){
    Dist <- as.dist(1 - cor(t(mat), use = "pairwise.complete.obs"))
  }else if(metric=='MIC'){
    Dist <- as.dist(1 - mine(t(mat))$MIC)
  }else{ 
    Dist <- dist(mat, method = metric)
}
  clusterObjs <- vector("list", length(clMethods))
  BPsObjs <- vector("list", length(clMethods))
  MFsObjs <- vector("list", length(clMethods))
  CCsObjs <- vector("list", length(clMethods))
  ALLObjs <- vector("list", length(clMethods))
  names(clusterObjs) <- clMethods
  #print("stab")
  #measures <- c(if ("stability" %in% validation) c("APN", "CPN","AD", "ADM", "FOM"), if ("internal" %in% validation) c("Connectivity", "Dunn", "Silhouette"), if ("biological" %in% validation) c("BHI", "BSI"))
  measures <- c(if ("stability" %in% validation) c("APN", "CPN","AD", "ADM", "FOM"), if ("internal" %in% validation) c("Connectivity", "Dunn", "Silhouette"), if ("biological" %in% validation) c("BHI"))

  validMeasures <- array(dim = c(length(measures)*max(nClust,na.rm = TRUE), length(nClust), 
                                 length(clMethods)))
  dimnames(validMeasures) <- list(paste(rep(measures,each=max(nClust,na.rm = TRUE)),1:max(nClust,na.rm = TRUE),sep="."), nClust, clMethods)
  for (i in 1:length(clMethods)) {
    cvalid <- vClusters2(mat, clMethods[i], nClust, validation = validation, 
                        Dist = Dist, method = method, metric = metric, annotation = annotation, 
                        GOcategory = GOcategory, goTermFreq = goTermFreq, 
                        neighbSize = neighbSize, dropEvidence = dropEvidence, 
                        verbose = verbose, ...)
    clusterObjs[[i]] <- cvalid$clusterObj
    BPsObjs[[i]] <- cvalid$BPs
    MFsObjs[[i]] <- cvalid$MFs
    CCsObjs[[i]] <- cvalid$CCs
    ALLObjs[[i]] <- cvalid$ALL
    validMeasures[, , i] <- cvalid$measures
  }
  if (is.null(rownames(mat))) {
    rownames(mat) <- 1:nrow(mat)
    warning("rownames for data not specified, using 1:nrow(data)")
  }

  return(list( clusterObjs = clusterObjs, measures = validMeasures, measNames = measures, clMethods = clMethods,BPs=BPsObjs,MFs=MFsObjs,CCs=CCsObjs,ALL=ALLObjs))
  #new("clValid", clusterObjs = clusterObjs, measures = validMeasures, measNames = measures, clMethods = clMethods, labels = rownames(mat), 
     # nClust = nClust, validation = validation, metric = metric, method = method, neighbSize = neighbSize, GOcategory = GOcategory, 
     # goTermFreq = goTermFreq, annotation = annotation, BPs=BPsObjs,call = match.call())
}

vClusters2 <- function (mat, clMethod, nClust, nclustMax, validation, Dist, 
          method, metric, annotation, GOcategory, goTermFreq, neighbSize, 
          dropEvidence, verbose, ...) 
  
{
  #measNames <- c(if ("stability" %in% validation) c("APN", "CPN","AD", "ADM", "FOM"), if ("internal" %in% validation) c("Connectivity", "Dunn", "Silhouette"), if ("biological" %in% validation) c("BHI","BSI"))
  measNames <- c(if ("stability" %in% validation) c("APN", "CPN","AD", "ADM", "FOM"), if ("internal" %in% validation) c("Connectivity", "Dunn", "Silhouette"), if ("biological" %in% validation) c("BHI"))
  #change form initial 0 to NA  but problem with sum later because NA+number=NA ?    
  measures <- matrix(0, nrow = (length(measNames)*length(1:max(nClust,na.rm = TRUE))), ncol = length(nClust))
  rownames(measures) <- paste(rep(measNames,each=length(1:max(nClust,na.rm = TRUE))),c(1:max(nClust,na.rm = TRUE)),sep=".")
 
  #print(rownames(measures))
  colnames(measures) <- nClust
  BPs.tmp <- vector("list",length(nClust))
  MFs.tmp <- vector("list",length(nClust))
  CCs.tmp <- vector("list",length(nClust))
  ALL.tmp <- vector("list",length(nClust))
  switch(clMethod, hierarchical = {
    clusterObj <- hclust(Dist, method)
  }, diana = {
    clusterObj <- diana(Dist, ...)
  }, kmeans = {
    clusterObj <- vector("list", length = length(nClust))
    names(clusterObj) <- nClust
    clusterObjInit <- hclust(Dist, method)
  }, agnes = {
    clusterObj <- agnes(Dist, method = method, ...)
  }, {
    clusterObj <- vector("list", length = length(nClust))
    names(clusterObj) <- nClust
  })
  ind <- 1
  for (nc in nClust) {
    switch(clMethod, kmeans = {
      initial <- tapply(mat, list(rep(cutree(clusterObjInit, 
                                             nc), ncol(mat)), col(mat)), function(x) mean(x, 
                                                                                          na.rm = TRUE))
      if (length(dup <- which(duplicated(initial))) > 0) {
        for (dupi in dup) initial[dupi, ] <- initial[dupi, 
                                                     ] + jitter(initial[dupi, ])
      }
      dimnames(initial) <- list(NULL, dimnames(mat)[[2]])
      clusterObj[[ind]] <- kmeans(mat, initial, ...)
      cluster <- clusterObj[[ind]]$cluster
      
    }, fanny = {
      clusterObj[[ind]] <- fanny(Dist, nc, ...)
      cluster <- clusterObj[[ind]]$clustering
    }, model = {
      print(dim(mat))
      t <- mixmodCluster(as.data.frame(mat), nbCluster = nc, ...)
      print(t@bestResult@partition)
      clusterObj[[ind]] <- mixmodCluster(as.data.frame(mat), nbCluster = nc, ...)
      
      cluster <- clusterObj[[ind]]@bestResult@partition
      print(cluster)
    }, som = {
      clusterObj[[ind]] <- som(mat, grid = somgrid(1, nc), 
                               ...)
      cluster <- clusterObj[[ind]]$unit.classif
    }, pam = {
      clusterObj[[ind]] <- pam(Dist, nc, ...)
      cluster <- clusterObj[[ind]]$clustering
    }, clara = {
      clusterObj[[ind]] <- clara(mat, nc, metric = ifelse(metric == 
                                                            "correlation", "euclidean", metric), ...)
      cluster <- clusterObj[[ind]]$clustering
    }, sota = {
      clusterObj[[ind]] <- sota(mat, nc - 1)
      cluster <- clusterObj[[ind]]$clust
    }, {
      cluster <- cutree(clusterObj, nc)
      #print(cluster)
    })
    if (length(table(cluster)) != nc) {
      warning(paste(clMethod, "unable to find", nc, "clusters, returning NA for these validation measures"))
      measures[, ind] <- NA
      ind <- ind + 1
      next()
    }
    if ("internal" %in% validation) {

      measures[paste("Dunn",nc,sep="."), ind] <- dunn(Dist, cluster)
      si <- silhouette(cluster, dmatrix = as.matrix(Dist))
      sil <- tapply(si[,3],si[,1],mean,na.rm=T)
      measures[paste("Silhouette",1:length(sil),sep="."), ind] <-sil
      measures[paste("Connectivity",nc,sep="."), ind] <- connectivity(Dist, 
                                                    cluster, neighbSize = neighbSize)
      if (verbose) 
        print(paste("Finished internal validation,", 
                    clMethod, nc, "clusters"))
    }
    if ("biological" %in% validation) {
      bhi.t <- NULL
      bhi.t <- BHI2(cluster, annotation = annotation, names = rownames(mat), category = GOcategory, dropEvidence = dropEvidence)
      #print("vcl")
      #print(bhi.t)
      bhi <- c(bhi.t$bhi,rep(NA,max(nClust,na.rm = TRUE)-length(bhi.t$bhi)))
      #@@@changed code bits
      measures[paste("BHI",1:length(bhi),sep="."), ind] <- bhi
   
      BPs.tmp[[ind]] <-  bhi.t$BPs
      MFs.tmp[[ind]] <-  bhi.t$MFs
      CCs.tmp[[ind]] <-  bhi.t$CCs
      ALL.tmp[[ind]] <-  bhi.t$ALL
    
        print(paste("Finished BHI,", clMethod, nc, "clusters"))
    }
    if ("stability" %in% validation | "biological" %in% validation) {
      co.del <- 0
      for (del in 1:ncol(mat)) {
        matDel <- mat[, -del]
        if (metric == "correlation") {
          DistDel <- as.dist(1 - cor(t(matDel), use = "pairwise.complete.obs"))
        }else if(metric == 'MIC'){
          DistDel <- as.dist(1 - mine(t(matDel))$MIC)
        }
        else {
          DistDel <- dist(matDel, method = metric)
        }
        switch(clMethod, hierarchical = clusterObjDel <- hclust(DistDel, 
                                                                method), kmeans = clusterObjInitDel <- hclust(DistDel, 
                                                                                                              method), diana = clusterObjDel <- diana(DistDel, 
                                                                                                                                                      ...), agnes = clusterObjDel <- agnes(DistDel, 
                                                                                                                                                                                           method = method, ...), clara = clusterObjDel <- clara(matDel, 
                                                                                                                                                                                                                                                 nc, metric = ifelse(metric == "correlation", 
                                                                                                                                                                                                                                                                     "euclidean", metric), ...))
        switch(clMethod, kmeans = {
          initialDel <- tapply(matDel, list(rep(cutree(clusterObjInitDel, 
                                                       nc), ncol(matDel)), col(matDel)), function(x) mean(x, 
                                                                                                          na.rm = TRUE))
          if (length(dup <- which(duplicated(initialDel))) > 
                0) {
            for (dupi in dup) initialDel[dupi, ] <- initialDel[dupi, 
                                                               ] + jitter(initialDel[dupi, ])
          }
          dimnames(initialDel) <- list(NULL, dimnames(matDel)[[2]])
          kmdel <- kmeans(matDel, initialDel, ...)
          clusterDel <- kmdel$cluster
        }, fanny = {
          hfdel <- fanny(DistDel, nc, ...)
          clusterDel <- hfdel$clustering
        }, model = {
          clusterDel <- mixmodCluster(as.data.frame(matDel), nbCluster = nc, ...)@bestResult@partition
         
        }, som = {
          hsdel <- try(som(matDel, grid = somgrid(1, 
                                                  nc), ...))
          clusterDel <- hsdel$unit.classif
        }, pam = {
          clusterDel <- pam(DistDel, nc, cluster.only = TRUE, 
                            ...)
        }, clara = {
          clusterDel <- clusterObjDel$clustering
        }, sota = {
          clusterDel <- sota(matDel, nc - 1)$clust
        }, {
          clusterDel <- cutree(clusterObjDel, nc)
        })
        if ("stability" %in% validation) {
         stabmeas <- stability2(mat, Dist, del, cluster, clusterDel=clusterDel,num.cluster=max(nClust,na.rm = TRUE))
     
         measures[paste("APN",1:length(stabmeas[,"APN"]),sep="."),ind] <- measures[paste("APN",1:length(stabmeas[,"APN"]),sep="."),ind] + stabmeas[,"APN"]
         measures[paste("CPN",1:length(stabmeas[,"CPN"]),sep="."),ind ] <- measures[paste("CPN",1:length(stabmeas[,"CPN"]),sep="."),ind ] + stabmeas[,"CPN"]
         measures[paste("AD",1:length(stabmeas[,"AD"]),sep="."),ind] <- measures[paste("AD",1:length(stabmeas[,"AD"]),sep="."), ind] +  stabmeas[,"AD"]
         measures[paste("ADM",1:length(stabmeas[,"ADM"]),sep="."), ind] <- measures[paste("ADM",1:length(stabmeas[,"ADM"]),sep="."),ind ] + stabmeas[,"ADM"]
         measures[paste("FOM",1:length(stabmeas[,"FOM"]),sep="."), ind] <-measures[paste("FOM",1:length(stabmeas[,"FOM"]),sep="."),ind ] + stabmeas[,"FOM"]
        
        }
#        if ("biological" %in% validation) {
         # print("BSI")
          #tmp <- BSI2(cluster, clusterDel, annotation = annotation, 
     #                names = rownames(mat), category = GOcategory, 
     #                goTermFreq = goTermFreq, dropEvidence = dropEvidence)
          #tmp <- c(tmp,rep(NA,max(nClust)-length(tmp)))
         # print(tmp)
    #      measures[paste("BSI",1:length(tmp),sep="."), ind] <- measures[paste("BSI",1:length(tmp),sep="."), ind] + tmp
       # }
 
      }
      if (verbose & "stability" %in% validation) 
        print(paste("Finished stability validation,", 
                    clMethod, nc, "clusters"))
      if (verbose & "biological" %in% validation) 
        print(paste("Finished BSI,", clMethod, nc, "clusters"))
    }
    ind <- ind + 1
  }
  if ("stability" %in% validation) {
 	#mean stability of clusters
     measures[paste("APN",1:max(nClust,na.rm = TRUE),sep="."), ] <- measures[paste("APN",1:max(nClust,na.rm = TRUE),sep="."), ]/ncol(mat)
     measures[paste("CPN",1:max(nClust,na.rm = TRUE),sep="."), ] <- measures[paste("CPN",1:max(nClust,na.rm = TRUE),sep="."), ]/ncol(mat)
     measures[paste("AD",1:max(nClust,na.rm = TRUE),sep="."), ] <- measures[paste("AD",1:max(nClust,na.rm = TRUE),sep="."), ]/ncol(mat)
     measures[paste("ADM",1:max(nClust,na.rm = TRUE),sep="."), ] <- measures[paste("ADM",1:max(nClust,na.rm = TRUE),sep="."), ]/ncol(mat)
     measures[paste("FOM",1:max(nClust,na.rm = TRUE),sep="."), ] <- measures[paste("FOM",1:max(nClust,na.rm = TRUE),sep="."), ]/ncol(mat)
  }
  
  list(clusterObj = clusterObj, measures = measures,BPs=BPs.tmp,MFs=MFs.tmp,CCs=CCs.tmp,ALL=ALL.tmp)
}

connectivity <- function (distance = NULL, clusters, Data = NULL, neighbSize = 10, 
          method = "euclidean") 
{
  if (is.null(distance) & is.null(Data)) 
    stop("One of 'distance' or 'Data' is required")
  if (is.null(distance)) 
    distance <- as.matrix(dist(Data, method = method))
  if (class(distance) == "dist") 
    distance <- as.matrix(distance)
  nearest <- apply(distance, 2, function(x) sort(x, ind = TRUE)$ix[2:(neighbSize + 
                                                                        1)])
  nr <- nrow(nearest)
  nc <- ncol(nearest)
  same <- matrix(clusters, nrow = nr, ncol = nc, byrow = TRUE) != 
    matrix(clusters[nearest], nrow = nr, ncol = nc)
  conn <- sum(same * matrix(1/1:neighbSize, nrow = nr, ncol = nc))
  return(conn)
}

dunn <- function (distance = NULL, clusters, Data = NULL, method = "euclidean") 
{
  if (is.null(distance) & is.null(Data)) 
    stop("One of 'distance' or 'Data' is required")
  if (is.null(distance)) 
    distance <- as.matrix(dist(Data, method = method))
  if (class(distance) == "dist") 
    distance <- as.matrix(distance)
  nc <- max(clusters,na.rm = TRUE)
  interClust <- matrix(NA, nc, nc)
  intraClust <- rep(NA, nc)
  for (i in 1:nc) {
    c1 <- which(clusters == i)
    for (j in i:nc) {
      if (j == i) 
        intraClust[i] <- max(distance[c1, c1],na.rm = TRUE)
      if (j > i) {
        c2 <- which(clusters == j)
        interClust[i, j] <- min(distance[c1, c2],na.rm = TRUE)
      }
    }
  }
  dunn <- min(interClust, na.rm = TRUE)/max(intraClust,na.rm = TRUE)
  return(dunn)
}

stability2 <- function (mat, Dist = NULL, del, cluster, clusterDel, method = "euclidean",num.cluster) 
{

  any.na <- any(is.na(mat))
  obsNum <- 1:nrow(mat)
  nc1 <- length(table(cluster))
  nc2 <- length(table(clusterDel))
  stabmeas <- matrix(rep(NA,5*num.cluster),nrow=num.cluster,ncol=5)
  colnames(stabmeas) <- c("APN", "AD", "ADM", "FOM","CPN")
  overlap <- xtabs(~cluster + clusterDel)

  dij <- matrix(rep(NA, nc1 * nc2), nc1, nc2)
  if (is.null(Dist)) 
    matDist <- as.matrix(dist(mat, method = method))
  if (class(Dist) == "dist") 
    matDist <- as.matrix(Dist)
  if (class(Dist) == "matrix") 
    matDist <- Dist
  dij2 <- matrix(rep(NA, nc1 * nc2), nc1, nc2)
  ii <- 1
  for (i in sort(unique(cluster))) {
    jj <- 1
    xbari <- apply(mat[cluster == i, , drop = FALSE], 2, 
                   function(x) mean(x, na.rm = TRUE))
    for (j in sort(unique(clusterDel))) {
      clusi <- obsNum[cluster == i]
      clusdelj <- obsNum[clusterDel == j]
      cl <- length(clusi) * length(clusdelj)
      if (cl > 0) 
        dij[ii, jj] <- mean(matDist[clusi, clusdelj], 
                            na.rm = TRUE)
      xbarj <- apply(mat[clusterDel == j, , drop = FALSE], 
                     2, function(x) mean(x, na.rm = TRUE))
      diff <- xbari - xbarj
      if (length(diff) > 0) {
        if (any.na) {
          diff <- diff[!is.na(diff)]
          dij2[ii, jj] <- sqrt(mean(diff^2))
        }
        else {
          dij2[ii, jj] <- sqrt(sum(diff^2))
        }
      }
      else {
        dij2[ii, jj] <- 0
      }
      jj <- jj + 1
    }
    ii <- ii + 1
  }
  rs <- matrix(rowSums(overlap), nrow = nrow(overlap), ncol = ncol(overlap), 
               byrow = FALSE)
  cs <- matrix(colSums(overlap), nrow = nrow(overlap), ncol = ncol(overlap), 
               byrow = TRUE)
  ###new code bits
  stabmeas[1:nrow(overlap),"CPN"] <- 1 - apply(overlap^2/rs,1,sum)/rs[1:nrow(rs),1]
  
  stabmeas[1,"APN"] <- 1 - sum(overlap^2/rs)/sum(overlap)
  
  stabmeas[1,"AD"] <- sum(overlap * dij)/nrow(mat)
  stabmeas[1,"ADM"] <- sum(overlap * dij2)/nrow(mat)
  xbar <- tapply(mat[, del], clusterDel, function(x) mean(x, na.rm = TRUE))
  stabmeas[1,"FOM"] <- sqrt(mean((mat[, del] - xbar[as.character(clusterDel)])^2, na.rm = TRUE))/sqrt((nrow(mat) - nc1)/nrow(mat))

  return(stabmeas)
}

BSI2 <- function (statClust, statClustDel, annotation, names = NULL, 
          category = "all", goTermFreq = 0.05, dropEvidence = NULL) 
{
  if (is.matrix(annotation)) {
    nFC <- ncol(annotation)
    nAnnot <- apply(annotation, 2, sum)
    if (is.null(names(statClust))) {
      names(statClust) <- names
      names(statClustDel) <- names
    }
    bsi <- numeric(nFC)
    overlap <- xtabs(~statClust + statClustDel)
    rsums <- rowSums(overlap)
    for (FCk in 1:nFC) {
      osum <- 0
      g.idx <- which(annotation[, FCk])
      if (length(g.idx) < 2) 
        next
      for (gx in g.idx) {
        for (gy in g.idx) {
          if (gx != gy) {
            i <- statClust[gx]
            j <- statClustDel[gy]
            osum <- osum + overlap[i, j]/rsums[i]
          }
        }
      }
      bsi[FCk] <- osum/(nAnnot[FCk] * (max(nAnnot[FCk] - 
                                             1, 1,na.rm = TRUE)))
    }
    return(mean(bsi,na.rm=T))
    #return(mean(bsi, na.rm = TRUE))
  }
  tab <- xtabs(~statClust + statClustDel)
  rs <- rowSums(tab)
  n <- length(statClust)
  if (!require(annotation, character.only = TRUE)) {
    cat(paste("package", annotation, "not found, attempting download from Bioconductor\n", 
              sep = " "))
    source("http://bioconductor.org/biocLite.R")
    try(biocLite(annotation))
  }
  goTerms <- getGO(names, annotation)
  if (!is.null(dropEvidence)) 
    goTerms <- lapply(goTerms, dropECode, dropEvidence)
  switch(category, BP = goIDs <- sapply(goTerms, function(a) sapply(a, 
                                                                    function(x) x[1][x[3] == "BP"])), CC = goIDs <- sapply(goTerms, 
                                                                                                                           function(a) sapply(a, function(x) x[1][x[3] == "CC"])), 
         MF = goIDs <- sapply(goTerms, function(a) sapply(a, function(x) x[1][x[3] == 
                                                                                "MF"])), all = goIDs <- sapply(goTerms, function(a) sapply(a, 
                                                                                                                                           function(x) x[1])))
  goTab <- table(unlist(goIDs))
  keepTerms <- names(goTab)[goTab > floor(n * goTermFreq)]
  termMat <- matrix(0, ncol = length(keepTerms), nrow = n)
  for (i in 1:length(keepTerms)) {
    termMat[, i] <- sapply(goIDs, function(x) keepTerms[i] %in% 
                             unlist(x))
  }
  bsi <- apply(termMat, 2, function(a) {
    out <- outer(statClust[as.logical(a)], statClustDel[as.logical(a)], 
                 function(x, y) tab[cbind(x, y)]/rs[x])
    (sum(out) - sum(diag(out)))/(sum(a) * (ifelse(sum(a) > 
                                                    1, sum(a) - 1, 1)))
  })
  return(mean(bsi, na.rm = TRUE))
}

BHI2 <- function (statClust, annotation, names = NULL, category = "all", dropEvidence = NULL) {
  if (is.matrix(annotation)) {
    bhi <- numeric(length(unique(statClust)))
    names(bhi) <- unique(statClust)
    for (k in unique(statClust)) {
      Ck.bhi <- 0
      Ck.idx <- which(statClust == k)
      if (length(Ck.idx) < 2) 
        next
      for (i in Ck.idx) {
       # print(i)
        #print(annotation)
        B <- which(annotation[i, ] == TRUE)
        if (length(B) == 0) 
          next
        #T and F for anno at column B 
        annot <- annotation[Ck.idx[Ck.idx != i], B]
        if (length(B) == 1) 
          Ck.bhi <- Ck.bhi + sum(annot)
        else if (length(B) > 1) 
          Ck.bhi <- Ck.bhi + sum(rowSums(annot) > 0)
      }
      nk <- sum(rowSums(annotation[Ck.idx, ]) > 0)
      if (nk > 1) 
        bhi[k] <- Ck.bhi/(nk * (nk - 1))
    }
    ##@@@ changes in the code
    l <- list(bhi=bhi)
    return(l)
    #return(mean(bhi, na.rm = TRUE))
  }
  if (!require(annotation, character.only = TRUE)) {
    cat(paste("package", annotation, "not found, attempting download from Bioconductor\n", 
              sep = " "))
    source("http://bioconductor.org/biocLite.R")
    res <- try(biocLite(annotation))
    if (class(res) == "try-error") {
      stop(paste("attempted download of package", annotation, 
                 "failed, exiting"))
    }
    else {
      library(annotation, character.only = TRUE)
    }
  }
  require(annotate)
  goTerms <- getGO(names, annotation)
  #print(head(goTerms))
  if (!is.null(dropEvidence)) 
    goTerms <- lapply(goTerms, dropECode, dropEvidence)
  bhi.tmp <- NULL
  bhi.tmp <- tapply(goTerms, statClust, function(x) matchGO2(x, category))
  #print("BHI2")
  #print(sapply(bhi.tmp, function(x){as.list(x[2])}))
  #stop
  bhi <- sapply(bhi.tmp, function(x){as.numeric(x[1])})
  BPs <- NULL
  BPs <- sapply(bhi.tmp, function(x){as.list(x[2])})
  MFs <- NULL
  MFs <- sapply(bhi.tmp, function(x){as.list(x[3])})
  CCs <- NULL
  CCs <- sapply(bhi.tmp, function(x){as.list(x[4])})
  ALL <- NULL
  ALL <- sapply(bhi.tmp, function(x){as.list(x[5])})
  ##@@changed code
  #print(paste("anno",bhi))
  #return(mean(bhi, na.rm = TRUE))
  l <- list(bhi=bhi,BPs=BPs,MFs=MFs,CCs=CCs,ALL=ALL)
#   print(l)
  return(l)
}

matchGO2 <- function (gg, category) 
{
  goIDs <- lapply(gg, function(a) sapply(a, function(x) x[1]))
  ont <- lapply(gg, function(a) sapply(a, function(x) x[3]))
  switch(category, BP = {
    goBP <- sapply(ont, function(x) any(x %in% "BP"))
    goIDs <- goIDs[goBP]
    
  }, CC = {
    goCC <- sapply(ont, function(x) any(x %in% "CC"))
    goIDs <- goIDs[goCC]
  }, MF = {
    goMF <- sapply(ont, function(x) any(x %in% "MF"))
    goIDs <- goIDs[goMF]
  }, all = {
    goAll <- sapply(ont, function(x) any(x %in% c("BP", "CC", 
                                                  "MF")))
    goIDs <- goIDs[goAll]
  })
  n <- length(goIDs)
  if (n < 2) 
    return(NA)
  sum <- 0
  counter <- 0
  BPs <- NULL
  MFs <- NULL
  CCs <- NULL
  ALL <- NULL
  for (i in 1:(length(goIDs) - 1)) {
    ##code bits
   
    for (j in (i + 1):length(goIDs)) {
      counter <- counter+1
      
      BPs <- c(BPs,unique(goIDs[[i]][ont[[i]] == "BP"][which(goIDs[[i]][ont[[i]] == "BP"] %in% goIDs[[j]][ont[[j]] == "BP"])]))
      MFs <- c(MFs,unique(goIDs[[i]][ont[[i]] == "MF"][which(goIDs[[i]][ont[[i]] == "MF"] %in% goIDs[[j]][ont[[j]] == "MF"])]))
      CCs <- c(CCs,unique(goIDs[[i]][ont[[i]] == "CC"][which(goIDs[[i]][ont[[i]] == "CC"] %in% goIDs[[j]][ont[[j]] == "CC"])]))
      ALL <- c(ALL,unique(goIDs[[i]][which(goIDs[[i]] %in% goIDs[[j]])]))
      
      switch(category, all = sum <- sum + any(goIDs[[i]] %in% 
                                                goIDs[[j]]), BP = sum <- sum + any(goIDs[[i]][ont[[i]] == 
                                                                                                "BP"] %in% goIDs[[j]][ont[[j]] == "BP"]), CC = sum <- sum + 
               any(goIDs[[i]][ont[[i]] == "CC"] %in% goIDs[[j]][ont[[j]] == 
                                                                  "CC"]), MF = sum <- sum + any(goIDs[[i]][ont[[i]] == 
                                                                                                             "MF"] %in% goIDs[[j]][ont[[j]] == "MF"]))
    }
  }
  #print(counter)
  tableBPs <- table(as.character(BPs))
  tableALL <- table(as.character(ALL))
  tableMFs <- table(as.character(MFs))
  tableCCs <- table(as.character(CCs))

  sum <- sum/(n * (n - 1))
  return(list(bhi=sum,BPs=tableBPs,MFs=tableMFs,CCs=tableCCs,ALL=tableALL))
}

