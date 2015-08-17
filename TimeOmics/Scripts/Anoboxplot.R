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

ano.boxplot <- function(array, clMethods,measurement=c("BHI","BSI","APN2")){
  num.methods <- NULL
  num.methods <- length(clMethods)
  num.cluster <- ncol(array)
  s <- NULL
  s <- seq(-0.3,0.3,l=num.methods)
  for(m in measurement){
    
    if(length(grep(m,rownames(array)))==0){stop("Requested measurement not in data")}
    index <- NULL
    index <-   grep(m,rownames(array))
    
    #(index)
    #print(dim(array))
    maxY <- max(array[index,,1:num.methods],na.rm=T)
    
    add=F
    if(m=="APN2")
      m <- "CPN"
    for(i in 1:num.methods){
      print(array[index,,i])
      boxplot(array[index,,i], at=1:ncol(array)+s[i],boxwex=0.6/num.methods,add=add,ylim=range(0,maxY),col=rainbow(num.methods)[i],axes=F,xlim=range(0.5,num.cluster+0.5),ylab=m,xlab="Number of cluster")
      add=T
    }
    axis(1,at=1:num.cluster,labels=colnames(array))
    axis(2,at=round(seq(0,maxY,l=5),digits=2),)
    legend("topright",legend=clMethods,fill=rainbow(num.methods),cex=0.7)
  }
}
