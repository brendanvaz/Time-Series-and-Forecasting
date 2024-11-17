East Midlands Housing Prices

Overview: 
This project utilizes time series modeling to forecast monthly average housing prices in the East Midlands region. Using historical data from 2010 to 2019, the analysis applies ARIMA and Seasonal ARIMA (SARIMA) models to predict housing price trends for the first half of 2020.

Objectives

Develop a time series model to capture housing price trends and seasonality.
Evaluate various SARIMA models to identify the best fit.
Forecast monthly average housing prices for January–June 2020.

Dataset

Source: Historical housing price data.
Timeframe: January 2010–December 2019.
Features: Monthly average house prices in GBP for the East Midlands region.

Methodology

Exploratory Data Analysis (EDA):

Identified upward trends and seasonality in the data.
Applied seasonal differencing to achieve stationarity.

Modeling:

Evaluated multiple SARIMA models.
Selected ARIMA (1,1,2) × (0,1,1)[12] as the best-performing model based on Akaike Information Criterion (AIC) and residual independence.
Forecasting:

Forecasted monthly housing prices for January–June 2020.
Visualized forecasted values alongside historical data.

Results

Best Model: ARIMA (1,1,2) × (0,1,1)[12]
AIC: 1790.89
Forecasted Prices (GBP):
January 2020: £193,930.5
February 2020: £194,836.4
March 2020: £194,401.0
April 2020: £196,204.1
May 2020: £197,202.1
June 2020: £197,933.9

Insights:

Predicted continued upward trends in housing prices, with a slight dip in March 2020.

Technologies Used

Programming Language: R
Libraries: forecast, base R functions for time series analysis

Future Work

Incorporate additional economic factors for more robust predictions.
Extend the analysis to forecast beyond six months.
Experiment with advanced machine learning models for comparative analysis.

Author

Brendan Ezekiel Agnelo Vaz
MSc Data Science, University of Nottingham

