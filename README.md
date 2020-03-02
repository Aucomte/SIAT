

![SIAT Logo](./inst/app/www/SIATtransparent.png)

## Table of Contents
<!-- TOC depthFrom:2 depthTo:3 withLinks:1 updateOnSave:1 orderedList:0 -->

- [Table of Contents](#table-of-contents)
- [About this package](#about-this-package)
- [Installation](#installation)
- [Running SIAT with GUI](#running-siat-with-gui)
- [Citation](#citation)
- [License](#license)

<!-- /TOC -->

## About this package

Web application made whith shiny in order to visualize and analyze the mesure of symptom intensity as quantitative variable in function of experimental factors.

[Shiny server hosting the application online](http://bioinfo-shiny.ird.fr:3838/AnalyseSymptoms/)

## Installation

  * Main Program: Please copy and paste the following command to R console.
  * Upgrading R and Rstudio to the latest version (R >= 3.5, Rstudio > 1.0.0) is strongly recommended.

``` ruby
#### Install or update SIAT
install.packages("remotes")
remotes::install_github("aucomte/SIAT")

```

## Running SIAT with GUI

  * To run the application SIAT

```ruby
library(SIAT)
runSIAT()
```

## Citation
The paper is currently in prep.

## License

LGPL-3 | file LICENSE
