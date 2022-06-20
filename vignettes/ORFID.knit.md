---
title: "Introduction to ORFID"
author: "Hugo Marques and Annika Putt"
date: "2022-06-20"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Introduction to ORFID}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




## Overview

ORFID is a package for compiling and summarizing passive integrated transponder (PIT) data collected using Oregon RFID OMRM and OSRS antenna readers. 


## Installation

ORFID is currently available as a development package on Github:


```r
devtools::install_github("hugo-marques/ORFID")
library(ORFID)
```


## View field names

The output from Oregon RFID antenna readers is highly customizable. Available data fields and their descriptions can be viewed using:






















