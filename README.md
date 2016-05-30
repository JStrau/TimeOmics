# TimeOmics in a Nutshell#
TimeOmics is a user-friendly application to filter, visualize and analyse high dimensional time course `omics' data. TimeOmics enables a variety of functions, for molecule expression experiments measured on multiple biological replicates over multiple time points to enable: 

1.  Filtering of not expressed or noisy molecules.
2.  Modelling of time course expression profiles.
3.  Clustering of modelled expression profiles.
4.  Analysing differential expression over time (between groups and time and group interaction, if two groups are available).

Examples can be found in the [user guide](https://github.com/JStrau/TimeOmics/tree/master/TimeOmics/www/UserGuide.pdf)/.

## Quick start ##
### What do I need to run TimeOmics? ###
TimeOmics version 1.0 functionality was tested on R version 3.2 and RStudio version 0.99. 
Follow the subsequent steps to install R (>= 3.2), RStudio (>= 0.99) and TimeOmics:

1. Download and install the latest version of R for your machine from [here](https://cran.r-project.org/bin/windows/base/).
2. Download and install the latest version of  **RStudio Desktop** for your machine from [here](https://www.rstudio.com/products/rstudio/#Desktop).
3. Once you have R and RStudio running, download TimeOmics from GitHub [here](https://github.com/JStrau/TimeOmics)
4. You will also need to install the R package shiny in order to run TimeOmics. Open RStudio and type into the RStudio console:

    
    install.packages('shiny')



### Run TimeOmics ###
Run TimeOmics by typing the following commands in the console:

    library(shiny)
    runApp('C:/filepath/to/TimeOmics')

**Note:** The first time you launch TimeOmics may take time as many package dependencies need to be  automatically installed. 

### Citing TimeOmics ###

The statistical methods can be cited as:
Straube J, Gorse A-D, Huang BE and Lê Cao K-A (2015)
A linear mixed model spline framework for analyzing time course `omics' data.
PLoS ONE 10(8): e0134540. doi: 10.1371/journal.pone.0134540


### Who do I talk to? ###
The [user guide](https://github.com/JStrau/TimeOmics/tree/master/TimeOmics/www/UserGuide.pdf) and references will hopefully answer most questions about TimeOmics. However, additional questions can be directed to j.straube[at]qfab.org.
We appreciate bug reports in the software or R functions and welcome any suggestions or comments for improvements.
