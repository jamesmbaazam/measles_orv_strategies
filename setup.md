# Setup requirements
## Programming language and IDEs
1. To run this script, the user must have R installed. Install R from [here](https://cran.r-project.org/bin/windows/base/). 
2. R studio is not a necessity but it is worth noting that it was used to script this project and using a different IDE might lead to some quirks I am currently unaware of. Install R studio [here](https://rstudio.com/products/rstudio/download/).

## Project dependencies
Most of the scripts are intentionally written to depend of base R functions but a number of external packages need to be installed to run the analyses, wrangling, and plotting.
    - ggplot2
    - dplyr
    - reshape2
    - purrr
    - gridExtra
    - tidyr
    - shiny
    - deSolve
    - tibble
    - ggthemes
    - ggpubr
You can batch install the dependencies with install.packages(c('tidyverse', 'reshape2', 'gridExtra', 'shiny','ggthemes', 'ggpubr')).

## Operating systems
The scripts were written in R and have not been tested on Ubuntu and MacOSX. I would like for the project to run irrespective of operating system.

## Tools
1. I use Sublime text 3 to edit all my plain text files, i.e., Markdown, text files, etc.