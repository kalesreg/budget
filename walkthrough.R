## The budget functions streamline the budget functions for those that don't
## have a deeper understanding of R. 

source("budgetFunctions.R")

transaction.data()

## budget walks the user through each step in the process. (Complete)

budget <- function() {
        category <- readline(prompt = "Would you like to categorize your data? ")
        if (category == 1 || toupper(category) == "Y" || toupper(category) == "YES") {
                categorize()
        }
        summary <- readline(prompt = "Would you like a summary of your data? ")
        while (summary == 1 || toupper(summary) == "Y" || toupper(summary) == "YES") {
                summarize()
                summary <- readline(prompt = "Would you like another summary of your data? ")
        }
        bottom <- readline(prompt = "Would you like to save balances and/or calculate the bottom line? ")
        if (bottom == 1 || toupper(bottom) == "Y" || toupper(bottom) == "YES") {
                bottom.line()
        }
}

## categorize reads in transaction data, categorizes it, and saves tidy data. (Complete)

categorize <- function() {
        folder <- readline(prompt = "Which folder would you like to import transactions from (e.g. ./raw-data or ./raw-data/archive)? ")
        cur_transactions <- read.transaction.folder(folder)
        cur_transactions <- category.sort(cur_transactions)
        cur_transactions <- other.desc(cur_transactions)
        review <- readline(prompt = "Is there any category that you would like to review? ")
        while (review == 1 || toupper(review) == "YES" || toupper(review) == "Y") {
                category <- readline(prompt = "Which category would you like to review? ")
                cur_transactions <- review.category(cur_transactions, category)
                review <- readline(prompt = "Is there any category that you would like to review? ")
        }
        transactions <<- rbind(transactions, cur_transactions)
        tidy <- readline(prompt = "File name for the saved data: ")
        file <- paste("./tidy-data/", tidy, ".csv", sep = "")
        fwrite(transactions, file)
}

## summarize completes the month and year summaries and saves it. (Complete)

summarize <- function() {
        year <- readline(prompt = "Relevant Year to summarize: ")
        month <- readline(prompt = "Relevant Month(s) to summarize: ")
        if (str_detect(month, ":") == TRUE) {
                split <- strsplit(month, ":")
                split <- as.numeric(split[[1]])
                months <- seq(split[[1]], split[[2]])
        } else {
                months <- as.numeric(month)
        }
        month_summary <<- month.sum(transactions, as.numeric(year), months)
        month_save <- readline(prompt = "File name for month summary data: ")
        month_name <- paste("./summary-data/", month_save, ".csv", sep = "")
        fwrite(month_summary, month_name)
        year_summary <<- year.sum(transactions, as.numeric(year))
        year_save <- readline(prompt = "File name for year summary data: ")
        year_name <- paste("./summary-data/", year_save, ".csv", sep = "")
        fwrite(year_summary, year_name)
}

# bottom.line saves the months starting and ending balances and caluclates the months bottom line (Complete)

bottom.line <- function() {
        balance <- readline(prompt = "Would you like to save an institution's starting and ending balances for a particular month? ")
        while (balance == 1 || toupper(balance) == "Y" || toupper(balance) == "YES") {
                institution <- readline(prompt = "Which Institution is this balance for? ")
                type <- readline(prompt = "Which type of institution is this balance for? ")
                year <- readline(prompt = "Which Year is this balance for? ")
                month <- readline(prompt = "Which Month is this balance for? ")
                start <- readline(prompt = "What is the starting balance? ")
                end <- readline(prompt = "What is the ending balance? ")
                add.balance(institution, type, year, month, start, end)
                balance <- readline(prompt = "Would you like to save another month/institution's balances? ")
        }
        bottom <- readline(prompt = "Would you like to see a month's bottom line? ")
        while (bottom == 1 || toupper(bottom) == "Y" || toupper(bottom) == "YES") {
                year <- readline(prompt = "Which Year is relevant to this bottom line calculation? ")
                month <- readline(prompt = "Which Month is relevant to this bottom line calculation? ")
                bottom.line.check(transactions, year, month)
                bottom <- readline(prompt = "Would you like to see another month's bottom line? ")
        }
}
