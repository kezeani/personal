import pandas as pd
from sklearn.metrics import mean_absolute_error

# Read data into pandas + set index to date
dfinit = pd.read_csv('boston_weather.csv', index_col="DATE")

# Isolate the primary columns and rename them
dfcore = dfinit[["PRCP", "TMAX", "TMIN", "SNOW", "SNWD"]].copy()
dfcore.columns = ['precip', 'temp_max', 'temp_min', 'snow', 'snow_depth']

# Remove all null values
dfcore = dfcore.fillna(0)

# Ensure index is in datetime format
dfcore.index = pd.to_datetime(dfcore.index)

# Create "target" column based on the next day's temp
dfcore["target"] = dfcore.shift(-1)["temp_max"]
dfcore = dfcore.iloc[:-1,:].copy()

# Initialize the machine learning model
from sklearn.linear_model import Ridge
reg = Ridge(alpha=.1)

dfcore["30_day_avg"] = dfcore["temp_max"].rolling(30).mean()
dfcore["month/max"] = dfcore["30_day_avg"]/dfcore["temp_max"]
dfcore = dfcore.iloc[30:,:].copy()

dfcore["month_avg"] = dfcore["temp_max"].groupby(dfcore.index.month).apply(lambda x: x.expanding(1).mean())
dfcore["day_of_year_avg"] = dfcore["temp_max"].groupby(dfcore.index.day_of_year).apply(lambda x: x.expanding(1).mean())

# Predictors: The columns used to predict the temp
predictors = ["precip", "temp_max", "temp_min", "30_day_avg", "month/max"]

def create_predictions (dfcore, predictors, reg):
    # Training set and testing set
    training = dfcore.loc[:"2021-12-31"]
    testing = dfcore.loc["2022-01-01":]

    # Fit model to data and generate predictions
    reg.fit(training[predictors], training["target"])
    predictions = reg.predict(testing[predictors])

    # Evaluate error
    error = mean_absolute_error(testing["target"], predictions)

    # Combine actual and predicted
    combined = pd.concat([testing["target"], pd.Series(predictions, index=testing.index)], axis=1)
    combined.columns = ["actual", "predictions"]
    return error, combined


