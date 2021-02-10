# Clinflow-a-Shiny-app-for-exploring-longitudinal-clinical-data
## Online demo
An online demo of this app can be found [here](https://oanastoicescu11.shinyapps.io/Clinflow-a-Shiny-app-for-exploring-longitudinal-clinical-data/)
## To run locally:
- Download the source code
- Install the R software on your computer. R version 3.6.3 can be found [here]( https://cran.r-project.org/bin/windows/base/old/3.6.3/)
- Source the Libraries.R file
- Use the command:
  `shiny::runApp()`

### Input
The applicaiton expects a csv or tsv file as input. 

### Original data and Preprocessed data
In the Original Data Summary tab a sample of the first rows of the uploaded raw table is displayed, along with a box that displays a summary and structure of the data. 
The following tab, Preprocessed Data displays two tables, Visit data and Patient data, along with buttons to download or display the summary and structure of each table.
Patient data contains only the constant(non time series) variables, such as patient's sex. Visit data contains all variables (time-series and constant).

### Filter
The next tab is the Filter Data tab, a functionality that allows the user to choose either visit data or patient data, and to select variables to group and filter.
The filter elements on the left are used to group the data, a pre-filter. This is used to create a subset of data to apply the filter on, while the rest of the data remains unchanged.

For the numeric variables, the filtering is done via a slider, and for the categorical ones, a multi-choice selector. If the variables contain missing entries, there is also a switch for filtering out the rows with missing values in the chosen variables.

The filtered table is updated dynamically as the user filters the data, and the bar above the table shows in percents the amount of data preserved after filtering. The button "Update Table" updates both the visit and patient data displayed below, to include only the filtered information and it also updates the filter options accordingly. There is an option for resetting the table, which brings back the unfiltered preprocessed table from the start, and resets the filter options. This page also contains a box for displaying a summary of the variables and the variable types of the filtered data, and buttons for downloading the updated patient and visit tables. 

### Missing data

In this tab, the user can choose to display a missing data map of either the visit or the patient table, and a scatterplot of missing vs. observed values from two chosen variables, in order to study the missing data mechanism. This plot can show whether the data is missing completely at random (MCAR), or not. If the data is not MCAR, this table helps the user to study the missing mechanism and make a decision on how to deal with the missing values without introducing bias in the analysis results. 

The interactive plot allows to select the points by clicking or dragging, then display them in a separate table, and delete them via the "Delete selected entries" button.

### Create a new patient feature

This option allows the user to aggregate time-series data from a certain period of time, and attach it to the Patient table as a constant variable. The options for aggregation of numeric variables are: sum, max, min, mean. For the categorical variables, user is asked to select a categorical level. If that level appears in the time interval, the variable will have the value TRUE, otherwise FALSE. For example: If the patient had a respiratory infection during days 1-10, mark TRUE, otherwise FALSE.

### Create a new categorical feature

This tab allows the user to create a new feature, either in the patient table or the visit table, based on another numerical feature in that table. The user must choose a numerical variable in the drop down menu, then type the cut-off points, separated by comma, for splitting the variable into intervals closed on the left and open on the right. The last interval is closed on both sides. A new categorical variable is created with as many levels as there are intervals. The "Updated Table" displays a preview of the table containing the new categorical variable, and the "Save Table" button adds the new variable to the dataset. The new feature can then be used for conducting group-based analysis.

### Outliers

This tab allows the user to plot a scatterplot and a boxplot of two chosen variables, and colour the points by a categorical variable, in order to identify outliers. The scatterplot shows the points in the data and a regression line. The points that are further from the regression line should be investigated as possible outliers. The boxplot shows a representation of the distribution of values on the Y axis, as a box with the edges as the first and third quartile and a median line in between. If the X axis is a categorical variable, the boxplot shows distributions of the data for each category. The values that are far away from the median and the quartiles should be investigated as outliers. The data can be either patient or visit data, and the plots are interactive. Clicking on a point in the scatterplot, or selecting multiple points by dragging, will move those entries from the original table into the Outlier table, where the user can study them and decide whether they should be removed from the data or not. Once the user presses the "Delete Outliers" button, the new data table without outliers is saved, and the selected entries are deleted.

### Visualize

The second navigation bar includes a tab for constructing and viewing the table to be analyzed, tabs for different clustering methods, and a tab for various charts. In the My data tab, the user can select either the patient or the visit table, then choose the variables and missing data treatment to use in the clustering methods. The numeric variables are used for the clustering, and the categorical variables can be used to customize the colours and the shapes of the points in the cluster plots. 

The user can choose how to treat missing values in the numeric variables used for clustering, either by deleting the rows containing missing values, estimating the missing values using bayesian PCA, or not treating them at all. The clustering methods give an error if missing values are present in the numeric data, so the user must make an informed decision on how to proceed. It is recommended to study the missing mechanisms in the data and make a subset, using the data filter, that doesn't include missing values that are not MCAR. Imputation or deletion of the missing entries that are not MCAR can introduce bias in the analysis. If the categorical variables contain missing values, the NA entries are automatically assigned the label "Missing" and they appear as a category in the data, in order to preserve as much information as possible. The clustering visualizations allow for user customization of some parameters, rotation of the 3D plot, and saving the plots in various formats.

Clustering is useful in identifying groups of similar entries in the table. Combined with the coloring and categorization options, the user can explore the reasons behind the similarities found in the data points and identify important relationships between variables.

### Panel Data

This is a panel data creation tool that turns the visit time into a timestamp and allows the user to construct a time-series longitudinal panel according to preferences. User can either cut off time points and aggregate data for each patient at each time interval, or keep all the time points.

The Panel data charts offer interactive visualizations for time-series values. Clicking on the points or lines in the plots, representing the values of individual patients, will display a table with all the entries of that patient.

### Survival

This functionality allows the user to plot survival curves with a Cox PH model and explore the effect of diferent covariates on the survival.
In the left panel, the user selects the Time-to-event variable (or follow-up time), an event variable and event value, and more covariates if needed. The app fits a coxPH model on the data, then predicts the survival times of a new patient with the chosen covariates values. The user can explore the effect of different covariates on the survival curves. The model details can be investigated in the Model Summary tab, and a Hazard forest plot can be visualized in the Model Forest Plot tab.
