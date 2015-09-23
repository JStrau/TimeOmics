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




#################

shinyUI(pageWithSidebar(
  # Application title
#tags$head(tags$style("body  {background-color: orange;}")
  titlePanel(windowTitle='TimeOmics',list(HTML(' <img src="Picture5.png" alt="TimeOmics" height="60" width="120" align="left"></td>'))),
             #,list(HTML(' <img src="Logo3.png" alt="TimeOmics" height="60" width="120" align="right"></td>'))),
  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  
  sidebarPanel(   
    
    ####### Upload Panel ################
    conditionalPanel(condition = "input.Tabs == 'Upload'",
    wellPanel(
      p(strong("Upload")),
      fileInput('ExpData', 'Expression matrix',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'),multiple=FALSE),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator:',
                   c("Comma"=',',
                     "Semicolon"=';',
                     "Tab"='\t')),
      helpText("Note: Please have the samples as row and the molecules as column."),
      fileInput("TimeData", "Time vector", multiple=FALSE),
      fileInput("ReplicateData", "Sample ID vector", multiple=FALSE),
      helpText("Note: The time, the sample ID and the group vector needs to have the same length as the expression matrix row."),
      fileInput("GroupData", "Group vector", multiple=FALSE),
      helpText("Note: If your samples belong to one group upload a vector of '1' with the same length as the  expression matrix row."),
      
      fileInput("AnnotData", "Molecule annotation", multiple=FALSE),
      helpText("Note: In order to perform the GO term enrichment analysis please upload the Entrez Gene identifier matching the expression matrix columns.")
      )
    ),
    
  
    
    ################## Filtering Panel ##################
    
    conditionalPanel(condition = "input.Tabs == 'Filter'",
                     wellPanel(
                       p(strong("Filter:")),
                       fluidRow(
                         shiny::column(width=4,offset = 1,checkboxInput(inputId = "numMissingUsed",
                                                                  label = strong("Missing data"),
                                                                  value = FALSE)),
                         shiny::column(width=4, offset = 1, uiOutput("missing_slider"))),
                       
                        # shiny::column(6,offset = 1,checkboxInput(inputId = "modelColor",
                        #                                          label = strong("Color by missing values"),
                        #                                          value = TRUE))),
                       fixedRow(
                         shiny::column(width = 4,offset=1,checkboxInput(inputId = "fcUsed",
                                                                label = strong("Fold change"),
                                                                value = FALSE)),
                         shiny::column(width=4, offset = 1,
                                       uiOutput("fc_slider"))),
                       helpText("Note: Filtering of molecules based on the proportion missing values and fold change is only applied if the boxes are selected."),
                       #actionButton('ApplyFilterSoft', 'Apply'),
                       br(),
                       hr(),
                       fluidRow(radioButtons("FilterRad", "Filter on filter ratios using:",c("Don't use filter ratios"="Non","Model based clustering" = "model","Fixed R_T and R_I" = "fixed"))),
                       hr(),
                       fluidRow(textInput("RT_Filter",'R_T',0.9),textInput("RI_Filter","R_I",0.3), checkboxInput('ApplyFilter', 'Use filtered data for further analysis',FALSE)))),
                       
                     #  fluidRow(actionButton('ResetFilter', 'Reset all filters')))),
   

    
    ################## Modelling Panel ##################
    
    conditionalPanel(condition = "input.Tabs == 'Model'",
                     wellPanel(
                       p(strong("Modelling")),
                       selectInput("Basis", "Basis:",
                                   list("Cubic" = "cubic",
                                        "P-spline" = "p-spline", 
                                        "Cubic p-spline" = "cubic p-spline")),
                       actionButton("Modelling","Model"),
                       p(strong(" ")),
                       p(strong("Model info:")),
                       htmlOutput("textModels"),
                       p(strong(" ")),
                       htmlOutput("ModelTable"),
                       p(strong(" ")),
                       downloadButton('downloadModel', 'Download modelled data')
                     )
    ),
    
    
    
    ################## Clustering Panel ##################
    
    conditionalPanel(condition = "input.Tabs == 'Cluster'",
    wellPanel(
     # HTML(tags$style(".span12 {background-color: black;}"))
      radioButtons("Radio_Correlation", "Select distance matrix",c("Correlation" = "correlation","Euclidean"="euclidean")),
      
      p(strong("Cluster validation")),
      #checkboxInput(inputId = "correlation", label = "Correlation", value = TRUE),
        p(strong("Select algoritms:")),
      checkboxInput(inputId = "hierarchical", label = "Hierarchical Clustering", value = TRUE),
      checkboxInput(inputId = "kmeans", label = "Kmeans", value = TRUE),
      checkboxInput(inputId = "pam", label = "PAM", value = FALSE),
      checkboxInput(inputId = "som", label = "Self-Organizing Maps", value = FALSE),
      checkboxInput(inputId = "model", label = "Model based clustering", value = FALSE),
      uiOutput("cluster_range_slider"),
      actionButton("submitVali","Update View")
    ),
    
    
    wellPanel(
      p(strong("Cluster visualisation")),
      selectInput("clusterAlgo", "Algorithm:",
                  list("Hierarchical Clustering" = "hc", 
                       "Diana" = "diana", 
                       "Kmeans" = "km",
                       'Model based clustering'='model',
                       'Self-Organizing maps'='som',
                       'PAM'='pam')),
      uiOutput("cluster_slider"),
      checkboxInput(inputId = "Smoothed", label = "Smoothed", value = FALSE),
      actionButton("submitVisu","Update View"),
      downloadButton('downloadClassifData', 'Download classification')
    ),
    
    wellPanel(
      p(strong("GO Enrichment")),
      selectInput("organism", "Organism:",
                  list("Homo sapiens" = "org.Hs.eg.db" 
                     #  "Mus musculus" = "org.Mm.eg.db", 
                     #  "Drosophila melanogasta" = "org.Dm.eg.db",
                    #   'Arabidopsis thaliana'='org.At.tair.db',
                     #  'Anopheles'='org.Ag.eg.db',
                     #  'Danio rerio'='org.Dr.eg.db'
                    )),
      selectInput("ontology", "Ontology:",
                  list("All" = "all", 
                       "Biological Process" = "BP", 
                       "Molecular Function" = "MF",
                       'Cellular Compartment'='CC')),
      actionButton("updateTable","Update table"),
      downloadButton('downloadData', 'Download enrichment analysis')
    )),
    
    ############ Differential expression ############
    conditionalPanel(condition = "input.Tabs == 'Differential Expression'",
                     wellPanel(
                       p(strong("Differential Expression")),
                       selectInput("BasisDE", "Basis:",
                                   list("Cubic" = "cubic",
                                        "P-spline" = "p-spline", 
                                        "Cubic p-spline" = "cubic p-spline")),
                       selectInput("Type", "Type:",
                                   list("All"="all",
                                        "Time" = "time",
                                        "Group" = "group", 
                                        "Group and Time interaction" = "grouptime")),
                       selectInput("experiment", "Experiment:",
                                   list("All"="all",
                                        "Time course" = "timecourse",
                                        "Longitudinal 1" = "longitudinal1", 
                                        "Longitudinal 2" = "longitudinal2")),
                       actionButton("DEtable","Analyse"),
                       downloadButton('downloadDEData', 'Download DE analysis')
                     )
    ),
    
    ############# Example and Help tab ##############
    conditionalPanel(condition = "input.Tabs == 'Example and Help'",
                     wellPanel(
                       p(strong("Example")),
                       helpText("Please click on 'Run Example' to upload multi sample example data."),
                       checkboxInput("RunExample","Run example",value = F),
                       p(strong("User guide")),
                       helpText("Note: If your browser cannot display the pdf within the shiny application the file is also available in the www folder."),
                       actionButton("ShowUserGuide","User guide")
                     )
    )


    ,#end tabsetPanel
    HTML('<footer>
         <td valign="top" >  <img src="index.gif" alt="QFAB" height="60" width="60" align="center"><img src="crc.gif" alt="CRC" align="center" height="50" width="148" style="display:inline;"></td></footer>')
    ),
  

  mainPanel(
    
    tabsetPanel(type = 'pills',
    #tags$main(tags$style("body {background-color: black; }")),
    #HTML('<body background-color="#FF00FF">January</body>'),
    
    ############### UPLOAD #################
    
    tabPanel("Upload",
              fluidRow( uiOutput("Group_Checkbox")),
              textOutput("result"),
              checkboxInput(inputId = "dens",
                           label = strong("Show density"),
                           value = FALSE),
                              plotOutput("HistPlot"),
              checkboxInput(inputId = "densSample",
                           label = strong("Show density"),
                           value = FALSE),
             
               plotOutput("Boxplot")),
               #htmlOutput("BoxplotGvis")),
    
    ########## FILTERING ##########
    
    tabPanel("Filter", uiOutput("filter_group"),
               fluidRow(
                 shiny::column(width=6,htmlOutput("BubbleGvisMissProp")),
                 shiny::column(width=6,htmlOutput("BubbleGvisFC"),checkboxInput('logfc', 'Log fold change', TRUE))),
                fluidRow(plotOutput("MCLUST")),
                hr(),
                fluidRow(shiny::column(width=6, textOutput('summaryFilter')))),
               # fluidRow(shiny::column(width=6, textOutput('resetFilter')))),
    
    ############### MODELLING #################
    
    tabPanel("Model",
             htmlOutput("textAnaModel"),
                  checkboxInput(inputId = "ModelPlotMean",
                           label = strong("Show mean"),
                           value = FALSE),
             checkboxInput(inputId = "ModelPlotSmooth",
                           label = strong("Show smoothed fit"),
                           value = FALSE),
          #   radioButtons("Radio_plotmodel", "Show profile:",c("Smooth" = "smooth","Mean"="mean")),
             plotOutput("ModelPlot"),
             DT::dataTableOutput('ModelTables')),
      
    ############### Clustering #################       
    
    tabPanel("Cluster",
      htmlOutput("textAnaModelCluster"),
      h3(textOutput("caption1")),
      plotOutput("clusterValidation"),
      h3(textOutput("caption")),
      checkboxInput(inputId='colorGroup', label = strong("Color by group"),value= TRUE),
      plotOutput("clusterPlot"),
      DT::dataTableOutput('GOEnrichment')),
      
    ############### Differential Expression #################
      
    tabPanel("Differential Expression",
             htmlOutput("textAnaModelDE"),
             textOutput("DEInfoText"),
             fluidRow(shiny::column(width=6,radioButtons("Radio_DEplot", "Show plot:",c("All"='all',"Time fit" = "time","Group fit" = "group","Group and time interaction fit"="group*time"))),                      
                                    shiny::column(width=6, checkboxInput(inputId = "DEPlotMean",
                                                                         label = strong("Show mean"),
                                                                         value = FALSE),
                                                            checkboxInput(inputId = "DEPlotSmooth",
                                                                label = strong("Show smoothed fit"),value = FALSE))),
             plotOutput("DEPlot"),
             DT::dataTableOutput('DETable')),
    
    
    ############## Run Example and Help Page ###############
    tabPanel("Example and Help",
             fluidRow( h3('TimeOmics workflows')),
             fluidRow(img(src="Workflow.png",width=600,height=200)),
             conditionalPanel(condition = "input.ShowUserGuide != 0",
             tags$iframe(src="TimeOmics_userguide.pdf", width="900", height="600"))),
             #htmlOutput('pdfviewer')
     
    
    id ="Tabs"
    )    )#End mainpanel
)
)