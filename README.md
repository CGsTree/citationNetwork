# quickQrtPCR
![Shiny App](https://img.shields.io/badge/Shiny-App-blue)

## Introduction
#### What is citationNetwork ?
quickQrtPCR is a shiny APP which can analyze the citation network and retrieve the reference information.



## Installation
Download the entire project files, open them in RStudio, and click 'Run App' to start.

Before running, you can install the required R packages as shown below.

```r
# Define the required packages
packages <- c("shiny", "httr", "igraph", "dplyr", "tibble", "visNetwork", "shinycssloaders", "xml2", "colourpicker", "shinyhelper", "shinythemes", "shinyWidgets", "htmlwidgets", "jsonlite", "DT")

# Check and install missing packages
install_if_missing <- function(p) { 
  if (!requireNamespace(p, quietly = TRUE)) 
    install.packages(p) 
}

# Iterate over all required packages
invisible(lapply(packages, install_if_missing))
```

## Acceleration step
Refer to the following link to apply for an NCBI API Key, enter the string into line 29 as ncbi_api_key = "", and then run the app.

[How do I get an enhanced API key (exceeding 10 rps) for NCBI APIs?
](https://support.nlm.nih.gov/kbArticle/?pn=KA-05318)

[How do I obtain an API Key through an NCBI account?
](https://support.nlm.nih.gov/kbArticle/?pn=KA-05317)


## APP Interface
Citations page

![Alt1](/image/img1.png)

References page

![Alt2](/image/img2.png)
