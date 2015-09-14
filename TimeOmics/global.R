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

selDataTableOutput <- function (outputId) 
{
  tagList(singleton(tags$head(tags$link(rel = "stylesheet", 
    type = "text/css", href = "shared/datatables/css/DT_bootstrap.css"),
    tags$style(type="text/css", ".rowsSelected td{
               background-color: rgba(112,164,255,0.2) !important}"),
    tags$style(type="text/css", ".selectable div table tbody tr{
               cursor: hand; cursor: pointer;}"),
    tags$script(src = "shared/datatables/js/jquery.dataTables.min.js"), 
    tags$script(src = "shared/datatables/js/DT_bootstrap.js"),
    tags$script(src = "js/DTbinding.js"))), 
    div(id = outputId, class = "shiny-datatable-output selectable"))
}




#Upload panel gloabel variables
path <- group.path <-rep.path <- sep <- header <-annot.path <-time.path <-current_group <- ""
data <-group <- time <-annot <- annotation <- rep <-NULL
changedPath.Exp <- F

# Filtering global variables
ymax <-ymin.fc <- ymin <- ymax.fc <-indexFinal <-f.slider<- m.slider<- NULL
investNoiseData1 <- investNoiseData2 <- NULL
log <- TRUE

#Modelling global variables
lmm <- NULL

#Clustering global variables
core <-T
clusterAlgo <- ''
enrich <-class <- classifi <-ce <- stab <- NULL
num <-valiplot <-grSelDE<- hcChanged <- kmChanged <-somChanged <-modelChanged <-ClRangeChanged1 <- ClRangeChanged2 <-matrixChanged<-pamChanged <-0

#Differential expression global variables
data2 <- NULL
lmm.de <- NULL
ExampleExp <- NULL
