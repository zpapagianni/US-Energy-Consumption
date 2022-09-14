# US Energy Consumption analysis 

## Description

This repository shows a set of R markdown files that I used to analyse the data provided in Table 2.2 of the US Energy Information Administration’s online data browser (https://www.eia.gov/totalenergy/data/browser/). The data set contains multiple time series, detailing:

* Primary US residential energy consumption patterns per energy source (Fossil Fuels and Renewable Energy) from 1949 until 2021.
* Electricity Retail Sales.
* Electrical System Energy Losses.

The aim of this study is to analyse the monthly residential energy consumption in the US, since January 2000. The analysis contains parts:

* Data cleaning.
* Analysis of Missing Values.
* Visualisations.
* Univariate Time series Analysis.
* Multivariate Time series Analysis.

Here are the different files:
* [US-Energy-Consumption.Rmd](./US-Energy-Consumption.Rmd):The rmarkdown file that contains the detailed step of the analysis.
* [US-Energy-Consumption-Report.Rmd](./US-Energy-Consumption-Report.Rmd): The rmarkdown file used to produce the github friendly document that can be found [here](US-Energy-Consumption-Report.md).
* [US-Energy-Consumption.R](./US-Energy-Consumption.R): Contains the code that was used for the analysis.
* [data.csv](./data.csv): Input data.

## Environment

* [Rstudio]([https://www.rstudio.com/])

## Requirements

* [R version 4.1.1 (2021-08-10)](https://www.r-project.org/)

## Dependencies

Choose the latest versions of any of the dependencies below:
* MASS
* ggplot2
* tidyverse
* tseries
* lubridate
* naniar

## License

MIT. See the LICENSE file for the copyright notice.
