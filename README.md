# Description

Every data science project has a lifecycle - from data exploration to aggregation to feature engineering and model building and tuning. Courtesy of Kaggle's [customer churning dataset](https://www.kaggle.com/blastchar/telco-customer-churn), this project attempts to walk through the lifecycle of the project. The intention here is to build a predictive model that could predict the churning rate of a customer to help reduce attrition rate through early intervention or commerical incentives. And following are the three cycles (or tabs in the dashboard) in the project:

1. Data Exploration: What factors influence the churning rate? Is it demographics, monthly payments, or tenureship? This tab allows you to explore and unearth the patterns among churners. It's like finding the right ingredients for a recipe.
2. Data Table: Once you identify features that they think is important to build the model, you will be able to aggregate them in this tab and better, download the table as well.
3. Model Building: Now that you have the right ingredients (features) for our recipe, it's time to cook (i.e. build the model). In this tab, you can select the features you want to use for the Logistic Regression and assess how your model varies. The accuracy rate and goodness of fit help you gauge the model's performance. And so does the QQ-plot of the residuals. Unlike linear regression, logistic regression does not assume the error terms (residuals) to be normally distributed.

**Note:** Every time user you select a new feature, the model runs again, computes all the evaluation metrics, and redraws the QQ-plot. Thus, you might notice some lag, and you should wait for the model to update before you try new combinations.

Ready? Let's dive into the kitchen:
https://prasunshrestha.shinyapps.io/churning_modeling_project_lifecycle/
