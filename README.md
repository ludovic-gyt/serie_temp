# Time Series Modeling and Forecasting with ARIMA

## Overview
This GitHub repository presents a school project (for time series ENSAE course) focused on time series modeling and forecasting using the ARIMA (AutoRegressive Integrated Moving Average) methodology. The project's goal is to analyze and predict a time series observed in France. 

## Project Details
The project, conducted in R, is structured as follows:

### Part I: Data
1. **Understanding the Chosen Time Series**: This section explores the selected time series, providing insights into its sector, potential treatments, and whether transformations like logarithmic scaling are necessary.

2. **Stationarity Transformation**: If required, the time series is transformed to achieve stationarity. This may involve differentiation or the removal of deterministic trends.

### Part II: ARMA Models
3. **Selection of ARMA Model**: An ARMA(p,q) model is selected for the transformed time series (Xt). The model parameters are estimated and its validity verified.

4. **ARIMA Representation**: The ARIMA(p,d,q) model for the chosen time series is expressed.

### Part III: Forecasting
5. **Confidence Region Equation**: The equation defining the confidence region of level α for future values (XT+1, XT+2) is derived and the confidence region is graphically depicted.

9. **Improvement of XT+1 prediction**: The improvement of XT+1 prediction based on the availability of a stationary series Yt, with YT+1 potentially being accessible earlier than XT+1 is discussed as well as conditions under which this information could enhance XT+1 forecasting, along with potential testing methods.

## Repository Contents
This GitHub repository contains the following components :

- Report in PDF format (time_series_analysis.pdf).
- R script used for analysis (input/analysis.R).
- Explanations of the R code in Markdown format.
  - [HTML rendering of code explanations](./code_explanation/code_md.html)
  - [PDF rendering of code explanations](./code_explanation/code_md.pdf)
