# Forecasting-VIX

The CBOE volatility index (VIX) stands as a representative barometer of the overall sentiment and volatility of the financial market. This project seeks to apply random forest and its variable importance measure to forecasting the VIX index. 

Compared to the previous research which finds it difficult to beat the pure HAR process in forecasting the VIX index due to its persistent nature, random forest could produce forecasts that are significantly more accurate than the HAR and augmented HAR models for multi-days forecasting horizons. Also, the superior predictability of random forest compared to the linear models becomes more evident for the longer forecasting horizons. 

The forecasting accuracy of random forest could further be improved by systematically selecting the optimal number of the most important variables from a dataset of 298 macro-finance variables, using the Boruta algorithm based on random forest’s variable importance measure.
