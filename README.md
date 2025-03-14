# Data Processing Application

## Quick Start

1. **Check out our deployed application**

deployed to <https://ruangkawprc.shinyapps.io/applieddatascience_project2/>

2. **Clone the Repository:**
```

   git clone https://github.com/ruangkawprc/AppliedDataScience_Project2.git
```
## Loading Datasets

Our application support .csv, .xlsx and .txt files. We provide one sample dataset below:

-   **Korean Drama Dataset (2015-2023)**\
    *Source:* Kaggle\
    *Description:* Comprises production details, broadcast information, content summaries, and user ratings for Korean dramas. It serves purposes in entertainment trend analysis and recommendation modeling.
  
The uploaded dataset in this tab will be used in the other tabs.
   
## Data Cleaning

From the uploaded dataset from Loading Datasets tab, we can perform the following preprocessing steps.

1.  **Handling Missing Values**
    -   Mean Imputation
    -   Median Imputation
    -   Mode Imputation

2.  **Duplicate Removal**\
    Identified and removed duplicate records to ensure uniqueness.

3.  **Outlier Treatment**\
    Used the Interquartile Range (IQR) method to detect outliers, removing or capping extreme values at calculated thresholds.

For instance, after applying removing duplicates and removing outliers, the data table and number of rows before and after will be shown.

## Feature Engineering:

The app can do feature transformation and one-hot encoding:

For Feature transformation:

You can select up to three variables to do the following: 

No change/log-transformation/Normalizing/Standardizing

When you uploaded a file, the app will automatically show three variables. To modify which variables to use, just use the delete button in your keyboard and delete existing variables. And use the drop down button to add new variables to it. By choosing the suitable transformation and clicking on "Apply Transformation", you will see the histrgram of the transformed feature. 

For One-hot Encoding: 

You can select one variable to do the one-hot encoding, but make sure you choose the categorical variable. After selecting one variable and selecting the "One hot encoding" drop-down button and clicking "Apply Transformation" button, the tab-One-hot Encoded Data will show the output of the one-hot encoding. 

## Exploratory Data Analysis (EDA)

The EDA module offers a suite of visualization and analytical tools that enable users to gain deeper insights into the dataset.

## Basic Statistics and Missing Value Analysis

Upon uploading a CSV or JSON file, users immediately see a **comprehensive summary** of the dataset. For numeric columns, key descriptive statistics—such as quartiles and mean values—are provided. Additionally, a **missing values table** helps assess the extent of data gaps.

## Visualization Tools

-   **Histogram**\
    Allows users to see the distribution of a chosen column. For numeric variables, the app uses a classic histogram (`geom_histogram()`); for categorical variables, it uses a bar chart (`geom_bar()`) to depict frequency counts.

-    **Boxplot**\
    Focused on numeric columns, the boxplot reveals the data distribution and highlights potential outliers. This helps in identifying anomalies and understanding the overall spread of the data.

-   **Correlation Analysis**

    -   **Correlation Matrix**: Displays a table of correlation coefficients among numeric columns.\
    -   **Correlation Heatmap**: Offers a more visual approach to correlation. When datasets have numerous columns, you can enable “Show Top 15 in Correlation Heatmap” to focus on the most correlated variables.\

 ## Data Filtering and Export

After reviewing basic statistics and visualizations, users can **filter** the dataset by selecting desired columns. By clicking the download button, the app saves these selected columns to a **new CSV file**. This feature streamlines the workflow by letting users isolate a subset of data for deeper investigation or modeling.

## User Interface (UI) and User Experience (UX)

This Shiny UI uses the Cerulean theme and CSS styles in blue color to enhance the appearance and customizes the navigation bar, buttons, tabs and tables. Instruction boxes are also added for clarification on how to use the program. 





