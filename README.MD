Getting and Cleaning Data Course Project
====

## How to run this code:
The file run_analysis.R contains all the functions used to generate the final data set.

Type into R console the command: run_analysis() 

This function will download the original data set, merge and filter data, generate a new data set and save the result into a txt file called output.txt

For visualize the new data set generated by run_analysis() just call read_output() at R console


## Organizing data and generating a new data set:

This section will explain how the original data set was modified to generate the final data set submitted in this project

### Step 1: Merging files

The first part was merge the information distributed in 6 different txt files together. For this I follow this steps:

1. Merge rows from files: X_train.txt and X_test.txt into a single data frame called "complete_df"
2. Use information from file features.txt to define columns names of complete_df
3. Select only the columns that indicates mean measures and standard deviation measures

   (Only measures of mean and standard deviation of the features was important for this script. Using information in        features.txt I was capable to identify which columns I need to consider. Angle related measures from original dataset    was not included)

4. Merge rows from files: y_train.txt and y_test.txt and insert values in a new column at complete_df
5. Merge rows from files: subject_train.txt.txt and subject_test.txt.txt and insert values in a new column at complete_df


### Step 2: Renaming measures variables

In the original data set columns names are confusing and not very descriptive. In this step all measures names was modified to a more descriptive name.

### Step 3: Generating a new data set

In this step the objective was to generate a new data set with the average of each variable for each activity and each subject. To achieve this two dplyr functions was used: group by and summarize. First complete_df was grouped by subject and activity then summarize function was applied to calculate the mean of each measure.