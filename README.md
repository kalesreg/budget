---
title: "Budget WalkThrough"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
# The functions included in the functions.R file provide the capability to read in various instiutional transaction data, categorize those transactions, and then summarize income and expenses. See below for a walkthrough.

# Step 1: Download CSV files from different bank and credit card accounts. It is suggested that you save these files into a raw-data folder in your working directory.
* Most, if not all, bank and credit organizations have a download or export button. Select the option to export to csv or excel.
* Use setwd() to tell the environment which top folder to go to. 

For example:
```{r}
setwd("C:\Users\kales\OneDrive\Documents\GitHub\budget")
```

# Step 2: Use source() to read the functions.R file into the environment.

```{r}
source("functions.R")
```


# Step 3: Read CSV files into R using read.transaction or read.transaction.folder function. Follow instructions as they show up in the console.
* The read.transaction function reads one file in at a time.
* The read.transaction.folder function reads in all files in a particular folder. The file names must be in a particular format: "AMZ-yyyy-mm.csv", BoA-Check-yyyy-mm.csv", "BoA-Credit-yyyy-mm.csv", and "Discover-yyyy-mm.csv".
        
For example:
```{r}
BoACheckJan2020 <- read.transaction("./raw-data/BoA-Check-2020-01.csv", "BoA", "checking")
BoACheckFeb2020 <- read.transaction("./raw-data/BoA-Check-2020-02.csv", "BoA", "checking")
BoACheckMar2020 <- read.transaction("./raw-data/BoA-Check-2020-03.csv", "BoA", "checking")

data <- read.transaction.folder("./raw-data")
```

# Step 4: If you used read.transaction.folder, skip this step. Otherwise, combine data sets and sort by month and year using rbind.transaction function.

```{r}
data <- rbind.transaction(BoACheckJan2020, BoACheckFeb2020, BoACheckMar2020)
```


# Step 5: Categorize data using category.sort function. Add category label based on previously saved descriptions. Previously unsaved descriptions will be labeled as Other.

```{r}
data <- category.sort(data)
```


# Step 6: Use review.category to review the labels for all the data or for a particular category. Specifically, make sure to go through the Other category as it would be better to categorize it into a more specific category when possible. Follow instructions as they show up in the console.
* You can use rename.category to rename a category if needed.

```{r}
data <- review.category(data, "Other")
data <- review.category(data, "Rent")
data <- review.category(data, "All")
```


# Step 7: Use fwrite to save combined tidy data. (May need to combine with previously saved data using fread and rbind.)

```{r}
fwrite(data, "./tidy-data/2020-01.csv")
```


# Step 8: Summarize data using month.sum and year.sum functions

```{r}
summary <- month.sum(data, 2020, 1:3)
year2020 <- year.sum(data, 2020)
```


# Step 9: Use fwrite to save summary data.

```{r}
fwrite(summary, "./summary-data/summary-data-2020-03.csv")
fwrite(year2020, "./summary-data/2020.csv")
```
