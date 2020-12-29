## The budget functions streamline the budget functions for those that don't
## have a deeper understanding of R. 

source("functions.R")

data <- data.table()

## budget walks the user through each step in the process. (Complete)

budget <- function(folder = "raw-data") {
        categorize(folder)
        summary <- readline(prompt = "Would you like a summary of your data? ")
        if (summary == 1 || toupper(summary) == "YES") {
                year <- readline(prompt = "Year: ")
                month <- readline(prompt = "Month(s): ")
                if (str_detect(month, ":") == TRUE) {
                        split <- strsplit(month, ":")
                        split <- as.numeric(split[[1]])
                        months <- seq(split[[1]], split[[2]])
                } else {
                        months <- as.numeric(month)
                }
                summarize(as.numeric(year), months)
        }
}

## categorize reads in transaction data, categorizes it, and saves tidy data. (Complete)

categorize <- function(folder = "raw-data") {
        data <<- read.transaction.folder(folder)
        data <<- category.sort(data)
        data <<- other.desc(data)
        review <- readline(prompt = "Is there any category that you would like to review? ")
        while (review == 1 || toupper(review) == "YES" || toupper(review) == "Y") {
                category <- readline(prompt = "Which category would you like to review? ")
                data <<- review.category(data, category)
                review <- readline(prompt = "Is there any category that you would like to review? ")
        }
        tidy <- readline(prompt = "File name for the saved data: ")
        file <- paste("./tidy-data/", tidy, ".csv", sep = "")
        fwrite(data, file)
}

## summarize completes the month and year summaries and saves it. (Complete)

summarize <- function(year, months) {
        month_summary <<- month.sum(data, as.numeric(year), months)
        month_save <- readline(prompt = "File name for month summary data: ")
        month_name <- paste("./summary-data/", month_save, ".csv", sep = "")
        fwrite(month_summary, month_name)
        year_summary <<- year.sum(data, as.numeric(year))
        year_save <- readline(prompt = "File name for year summary data: ")
        year_name <- paste("./summary-data/", year_save, ".csv", sep = "")
        fwrite(year_summary, year_name)
}
