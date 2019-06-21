
# Symptoms Length Analysis Tool

![LeAFtool Logo](/inst/app/www/SLATtransparent.png)


## About this package

Web application made whith shiny in order to visualize and analyze the mesure of symptom intensity as quantitative variable in function of experimental factors.


## Installation
### Regular installation

# Install SLAT

```
library(devtools)
install_github("aucomte/SLAT")

```

## Running LeAFtool with GUI

  * To run the application SLAT

```
library(SLAT)
runSLAT()
```

### Regular installation

  * Main Program: Please copy and paste the following command to R console.
  * Upgrading R and Rstudio to the latest version (R >= 3.5, Rstudio > 1.0.0) is strongly recommended.

```
# Dependecies that needs to be manually installed.
# You may need to paste the following code line by line
# and choose if previously installed packages should be updated (recommended).

# list of packages required

list.of.packages <- c("shiny","shinythemes","shinyBS","stringr","shinydashboard","shinyjs","shinyWidgets","DT","shinyhelper","colourpicker",
"shinyFeedback","readr","data.table","ggplot2","dplyr","lubridate","RColorBrewer","shinycssloaders","plotly","ggvis","gplots","ade4",
"factoextra","rmarkdown","knitr","heatmaply")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

