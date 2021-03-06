## All functions required to sort data from multiple bank and credit 
## organizations into relevant categories, save the tidy data, and create
## summary data.

library(data.table)
library(stringr)
library(lubridate)

## institution.data is used to check to see if the institutions data has been 
## read into the environment. If it hasn't, it reads it in or creates the file
## and reads it in. (Complete)

institution.data <- function() {
        if (any(str_detect(ls(envir=.GlobalEnv), "institutions")) == FALSE) {
                if (any(str_detect(list.files(), "institutions.csv")) == TRUE) {
                        institutions <<- fread("institutions.csv") 
                } else {
                        institutions <<- setNames(data.table(matrix(nrow = 0, ncol = 8)), 
                                                  c("Institution", "Type", "Skip", 
                                                    "Date_Column", "Description_Column", "Amount_Column",
                                                    "File_Name", "Special_Instructions"))
                        new <- data.table(Institution = toupper("BoA"), Type = toupper("Checking"), Skip = 6,
                                          Date_Column = "Date", Description_Column = "Description", Amount_Column = "Amount",
                                          File_Name = "BoA-Check-", Special_Instructions = "delete first row")
                        institutions <<- rbind(institutions, new)
                        setkey(institutions, "Institution", "Type")
                        fwrite(institutions, "institutions.csv")
                }
        }
}

## new.institution is used to add new organizations to a file that saves the known 
## organization formats. (Complete)

new.institution <- function(institution, file_name, type = "credit", skip = 0, date_col = "Date", desc_col = "Description", amt_col = "Amount", special_instructions = "None") {
        institution.data()
        for (i in 1:nrow(institutions)) {
                if (toupper(institution) == institutions[i, "Institution"] && toupper(type) == institutions[i, "Type"]) {
                        message <- paste("Institution and Type already exist: row ", i)
                        stop(message)        
                }
        }
        new <- data.table(Institution = toupper(institution), Type = toupper(type), Skip = skip, 
                          Date_Column = date_col, Description_Column = desc_col, Amount_Column = amt_col, 
                          File_Name = file_name, Special_Instructions = special_instructions)
        institutions <<- rbind(institutions, new)
        setkey(institutions, "Institution", "Type")
        fwrite(institutions, "institutions.csv")
        print("New institution added.")
}

## transaction.data is used to check to see if the transaction data has been 
## read into the environment. If it hasn't, it reads it in or creates the file
## and reads it in. (Complete)

transaction.data <- function() {
        if (month(Sys.Date()) == 1) {
                year <- year(Sys.Date())-1
                month <- 11
        } else if (month(Sys.Date()) == 2) {
                year <- year(Sys.Date())-1
                month <- 12
        } else {
                year <- year(Sys.Date())
                if (month(Sys.Date()) == 12) {
                        month <- month(Sys.Date())-2
                } else {
                        month <- paste("0", month(Sys.Date())-2, sep = "")
                }
        }
        file <- paste("./tidy-data/", year, "-", month, ".csv", sep = "")
        if (any(str_detect(ls(envir=.GlobalEnv), "transactions")) == FALSE) {
                if (any(str_detect(list.files("./tidy-data", full.names = TRUE), file)) == TRUE) {
                        transactions <<- fread(file) 
                } else {
                        transactions <<- setNames(data.table(matrix(nrow = 0, ncol = 6)), c("Date", "Description", "Amount", "Month", "Year", "Category"))
                        transactions <<- transactions[, .(Date = as.IDate(Date), Description = as.character(Description), Amount = as.numeric(Amount), Month = as.numeric(Month), Year = as.numeric(Year), Category = as.character(Category))]
                        setkey(transactions, "Date")
                        fwrite(transactions, file)
                }
        }
}

## read.transaction.folder is used to read all csv files from multiple different bank and
## credit organizations into the environment. Corresponds to Step 3 of the 
## walkthrough. (Complete)

read.transaction.folder <- function(folder = "./raw-data") {
        institution.data()
        data <- data.table()
        files <- list.files(folder, full.names = TRUE)
        for (i in 1:length(files)) {
                for (j in 1:nrow(institutions)) {
                        if (str_detect(files[i], institutions[[j, "File_Name"]])) {
                                temp <- read.transaction(files[i], institutions[[j, "Institution"]], institutions[[j, "Type"]])
                                data <- rbind(data, temp)
                                break
                        }
                }
        }
        setkey(data, "Date")
        return(data)
}

## read.transaction is used to read different bank and credit organizations csv files 
## into the environment. Corresponds to Step 3 in the walkthrough. (Complete)

read.transaction <- function(file, institution, type = "Credit") {
        institution.data()
        read.data <- function(file, institution, type) {
                data <- fread(file, skip = institutions[[i, "Skip"]],
                              select = c(institutions[[i, "Date_Column"]], institutions[[i, "Description_Column"]], institutions[[i, "Amount_Column"]]),
                              col.names = c("Date", "Description", "Amount"))
                if (institutions[i, "Special_Instructions"] == "delete first row" || institutions[i, "Special_Instructions"] == "delete" || institutions[i, "Special_Instructions"] == "-1") {
                        data <- data[-1]
                }
                if (institutions[i, "Special_Instructions"] == "negate amount" || institutions[i, "Special_Instructions"] == "negate" || institutions[i, "Special_Instructions"] == "-Amount") {
                        data$Amount <- data[, -Amount]
                }
                return(data)
        }
        for (i in 1:nrow(institutions)) {
                if (institutions[i, "Institution"] == toupper(institution) && institutions[i, "Type"] == toupper(type)) {
                        data <- read.data(file, institution, type)
                        break
                } else if (i == nrow(institutions)) {
                        if (askYesNo("Do you want to add a new institution? ") == TRUE) {
                                file_name <- readline(prompt = "Template for file_name: ")
                                skip <- readline(prompt = "Number of lines to skip: ")
                                date_col <- readline(prompt = "Label for date_col: ")
                                desc_col <- readline(prompt = "Label for desc_col: ")
                                amt_col <- readline(prompt = "Label for amt_col: ")
                                special_instructions <- readline(prompt = "Any special_instructions: ")
                                institutions <<- new.institution(institution, file_name, type, skip, date_col, desc_col, amt_col, special_instructions)
                                setkey(categories, "Category")
                                fwrite(categories, "desc_category.csv")
                                data <- read.data(file, institution, type)
                        } else {
                                print("Institution and Type of Account unavailable")
                        }
                } else {
                        next
                }
        }
        data$Date <- mdy(data$Date)
        data$Date <- as.IDate(data$Date)
        data$Month <- month(data$Date)
        data$Year <- year(data$Date)
        return(data)
}

## rbind.transaction is used to combine data from multiple sources and then sets the
## key to the Date for future analysis. Corresponds to Step 4 in the 
## walkthrough. (Complete)

rbind.transaction <- function(data = "ALL") {
        if(length(data) == 1 && data == "ALL") {
                vars <- ls(envir=.GlobalEnv)
                func <- lsf.str()
                irrel <- c(func, "institutions", "categories", "balances")
                rel_vars <- setdiff(vars, irrel)
                num_vars <- 1:length(rel_vars)
                all_data <- rbindlist(lapply(num_vars, function(x) get(rel_vars[x])))
        } else {
                all_data <- rbindlist(data)
        }
        setkey(all_data, "Date")
        all_data$Month <- month(all_data$Date) ## Consider just using date
        all_data$Year <- year(all_data$Date) ## Consider just using date
        return(all_data)
}

## category.data function checks to see if the category data set has been read
## into the environment. If it hasn't, it either reads in the file or creates 
## and reads in the file. (Complete)

category.data <- function() {
        if (any(str_detect(ls(envir=.GlobalEnv), "categories")) == FALSE) {
                if (any(str_detect(list.files(), "desc_category.csv")) == TRUE) {
                        categories <<- fread("desc_category.csv") 
                } else {
                        categories <<- setNames(data.table(matrix(nrow = 0, ncol = 2)), c("Description", "Category"))
                        categories <<- categories[, lapply(.SD, as.character)]
                        setkey(categories, "Category")
                        fwrite(categories, "desc_category.csv")
                }
        }
}

## category.sort is used to assign each observation a category to be used in 
## later analysis. Corresponds to Step 5 in the walkthrough. (Complete)

category.sort <- function(data) {
        category.data()
        if (any(str_detect(colnames(data), "Category")) == FALSE) {
                data$Category <- "Unassigned"
        }
        for (i in 1:nrow(data)) {
                if (any(str_detect(toupper(data[i, "Description"]), categories$Description)) == TRUE) {
                        data[i, "Category"] <- categories[str_detect(toupper(data[i, "Description"]), categories$Description), "Category"]
                } else if (any(str_detect(data[i, "Category"], categories$Category)) == TRUE) {
                        next
                } else {
                        data[i, "Category"] <- "Other"
                }
        }
        setkey(data, "Date", "Category")
        return(data)
}

## review.category is used to check the descriptions under a given category 
## belong there. Corresponds to Step 6 in the walkthrough. (Complete)

review.category <- function(data, category = "all") {
        category.data()
        category_options <- unique(categories$Category)
        if (category == "Other") {
                data <- other.desc(data)
        } else if (any(str_detect(toupper(category), category_options))) { 
                writeLines(c("Type 1 for Yes",
                             "Type 2 for No"))
                for (i in 1:nrow(data)) {
                        if (data[i, "Category"] == toupper(category)) {
                                print(data[i, 1:3])
                                answer <- readline(prompt="Do you want to change the category? ")
                                if (answer == 1 || toupper(answer) == "YES") {
                                        cat <- readline(prompt="Enter desired category: ")
                                        if (any(str_detect(toupper(cat), toupper(categories$Category))) == TRUE) {
                                                data[i, "Category"] <- toupper(cat) 
                                                print("Category changed")
                                        } else {
                                                add.category("No Description", cat)
                                                data[i, "Category"] <- toupper(cat)
                                        }
                                }
                        }
                }
                message <- paste("You went through all the descriptions listed under ", category)
                print(message)
        } else if (toupper(category) == "ALL") {
                print("Other")
                data <- other.desc(data)
                for (j in 1:length(category_options)) {
                        print(category_options[j])
                        for (k in 1:nrow(data)) {
                                if (data[k, "Category"] == category_options[j]) {
                                        print(data[k, 1:3])
                                        answer <- readline(prompt="Is this under the correct category? ")
                                        if (answer == 2 || toupper(answer) == "NO") {
                                                cat <- readline(prompt="Enter desired category: ")
                                                if (any(str_detect(toupper(cat), toupper(categories$Category))) == TRUE) {
                                                        data[k, "Category"] <- toupper(cat) 
                                                        print("Category changed")
                                                } else {
                                                        add.category("No Description", toupper(cat))
                                                        data[k, "Category"] <- toupper(cat)
                                                }
                                        }
                                }
                        }
                }
                print("You went through all the data and their categories")
        } else {
                print("Category does not exist")
        }
        return(data)
}

## other.desc is used to identify observations labeled as other and create new 
## categories when needed. (Complete)

other.desc <- function(data) {
        category.data()
        writeLines(c("Type 1 for Yes",
                   "Type 2 for No"))
        for (i in 1:nrow(data)) {
                if (data[i, "Category"] == "Other") {
                        question <- paste("Do you want to add the description: \n", data[i, "Description"], "? ")
                        answer <- readline(prompt = question)
                        if (answer == 1 || toupper(answer) == "YES") {
                                desc <- readline(prompt="Enter description: ")
                                cat <- readline(prompt="Enter category: ")
                                add.desc(toupper(desc), toupper(cat))
                                print("Data recategorized.")
                        } else {
                                print(data[i, 1:3])
                                answer <- readline(prompt="Do you want to change the category? ")
                                if (answer == 1 || toupper(answer) == "YES") {
                                        cat <- readline(prompt="Enter desired category: ")
                                        if (any(str_detect(toupper(cat), toupper(categories$Category))) == TRUE) {
                                                data[i, "Category"] <- toupper(cat) 
                                                print("Category changed")
                                        } else {
                                                add.category("No Description", toupper(cat))
                                                data[i, "Category"] <- toupper(cat)
                                        }
                                }
                        }
                        if (i == nrow(data)) {
                                print("No more new descriptions")
                        }
                } else if (data[i, "Category"] != "Other" && i == nrow(data)) {
                        print("No more new descriptions")
                } else {
                        next
                }
        }
        return(data)
}

## add.desc is used to add a new description to a category for future use. It 
## is used by other.desc to simplify the categorization process. (Complete)

add.desc <- function(desc, cat) {
        category.data()
        x <- str_detect(toupper(desc), categories$Description)
        if (any(x) == TRUE) {
                category <- categories[x, "Category"]
                message <- paste("Description already exists under", category, 
                                 "\n Would you like to recategorize it? ")
                answer <- readline(prompt = message)
                if (toupper(answer) == "YES" || answer == 1) {
                        if (any(str_detect(toupper(cat), toupper(categories$Category))) == TRUE) {
                                categories[x, "Category"] <<- toupper(cat)
                                setkey(categories, "Category")
                                fwrite(categories, "desc_category.csv")
                                print("Description recategorized")
                        } else {
                                add.category(desc, cat)
                        }
                        
                }
        } else {
                if (any(str_detect(toupper(cat), toupper(categories$Category))) == TRUE) {
                        new <- data.table(Description = toupper(desc), Category = toupper(cat))
                        categories <<- rbind(categories, new)
                        setkey(categories, "Category")
                        fwrite(categories, "desc_category.csv")
                        print("New description added")
                } else {
                        add.category(desc, cat)
                }
        }
}

## add.category is used to add a new category with a given description. Used in 
## add.desc and review.category. (Complete) 

add.category <- function(desc, cat) {
        category.data()
        if (askYesNo("Do you want to add a new category?") == TRUE) {
                new <- data.table(Description = toupper(desc), Category = toupper(cat))
                categories <<- rbind(categories, new)
                setkey(categories, "Category")
                fwrite(categories, "desc_category.csv")
                print("New category added")
        } else {
                print("Category not entered")
        }
}

## rename.category is used to rename a category.(Complete)

rename.category <- function(previous, new, data = NA) {
        category.data()
        for (i in 1:nrow(categories)) {
                if (categories[i, "Category"] == toupper(previous)) {
                        categories[i, "Category"] <<- toupper(new)
                }
        }
        setkey(categories, "Category")
        for (j in 1:nrow(data)) {
                if (data[j, "Category"] == toupper(previous)) {
                        data[j, "Category"] <- toupper(new)
                }
        }
        return(data)
}

## month.sum is used to get a summary of each category for each month or a 
## specified month or group of months. Corresponds to Step 8 in the walkthrough. (Complete)

month.sum <- function(data, year, months = 1:12) {
        each.sum <- function(data, year, month) {
                output <- data[Year == year & Month == month, .(total = sum(Amount)), by = .(Category)] ## Consider just using date
                setnames(output, "total", paste("total", year, month, sep = "_"))
                setkey(output, "Category")
        }
        month_list <- lapply(months, function(x) each.sum(data, year, x))
        comb <- Reduce(function(...) merge(..., all = TRUE), month_list)
        return(comb)
}

## year.sum is used to get a summary of each category for a specified year or 
## multiple years. Corresponds to Step 8 in the walkthrough. (Complete)

year.sum <- function(data, years) {
        each.sum <- function(data, year) {
                output <- data[Year == year, .(total = sum(Amount)), by = .(Category)] ## Consider just using date
                setnames(output, "total", paste("total", year, sep = "_"))
                setkey(output, "Category")
        }
        years_list <- lapply(years, function(x) each.sum(data, x))
        comb <- Reduce(function(...) merge(..., all = TRUE), years_list)
        return(comb)
}
                       
## balance.data function checks to see if the balance data set has been read
## into the environment. If it hasn't, it either reads in data from 2 months ago
## or creates and reads in the file. (Complete)

balance.data <- function() {
        if (month(Sys.Date()) == 1) {
                year <- year(Sys.Date())-1
                month <- 11
        } else if (month(Sys.Date()) == 2) {
                year <- year(Sys.Date())-1
                month <- 12
        } else {
                year <- year(Sys.Date())
                if (month(Sys.Date()) == 12) {
                        month <- month(Sys.Date())-2
                } else {
                        month <- paste("0", month(Sys.Date())-2, sep = "")
                }
        }
        file <- paste("./bottom-line/balances-", year, "-", month, ".csv", sep = "")
        if (any(str_detect(ls(envir=.GlobalEnv), "balances")) == FALSE) {
                if (any(str_detect(list.files("./bottom-line", full.names = TRUE), file)) == TRUE) {
                        balances <<- fread(file) 
                } else {
                        balances <<- setNames(data.table(matrix(nrow = 0, ncol = 6)), c("Institution", "Type", "Year", "Month", "Start_Balance", "End_Balance"))
                        balances <<- balances[, lapply(balances[, 3:6], as.numeric), lapply(balances[, 1:2], as.character)]
                        setkey(balances, "Institution", "Type", "Year", "Month")
                        fwrite(balances, file)
                }
        }
}

## bottom.line.data is used to check to see if the bottom_line data has been 
## read into the environment. If it hasn't, it reads it in or creates the file
## and reads it in. (Complete)

bottom.line.data <- function() {
        if (month(Sys.Date()) == 1) {
                year <- year(Sys.Date())-1
                month <- 11
        } else if (month(Sys.Date()) == 2) {
                year <- year(Sys.Date())-1
                month <- 12
        } else {
                year <- year(Sys.Date())
                if (month(Sys.Date()) == 12) {
                        month <- month(Sys.Date())-2
                } else {
                        month <- paste("0", month(Sys.Date())-2, sep = "")
                }
        }
        file <- paste("./bottom-line/bottom-line-", year, "-", month, ".csv", sep = "")
        if (any(str_detect(ls(envir=.GlobalEnv), "bottom_line")) == FALSE) {
                if (any(str_detect(list.files("./bottom-line", full.names = TRUE), file)) == TRUE) {
                        bottom_line <<- fread(file) 
                } else {
                        bottom_line <<- setNames(data.table(matrix(nrow = 0, ncol = 5)), c("Year", "Month", "Start", "End", "Change"))
                        bottom_line <<- bottom_line[, lapply(bottom_line, as.numeric)]
                        setkey(bottom_line, "Year", "Month")
                        fwrite(bottom_line, file)
                }
        }
}

## add.balance adds the balance start and end for each institution, for each
## month of the year. Checks if previous month's end_balance matches inputted 
## months start_balance. Checks if next month's start_balance matches inputted 
## months end_balance. Adds row if needed. (Complete)

add.balance <- function(institution, type = "Credit", year, month, start_balance, end_balance) {
        balance.data()
        previous.month <- function(institution, type, year, month, start_balance) {
                if (month == 1) {
                        previous_month <- 12
                        previous_year <- year-1
                } else {
                        previous_month <- month-1
                        previous_year <- year
                }
                previous_row <- data.table(Institution = toupper(institution), Type = toupper(type), Year = as.integer(previous_year), Month = as.integer(previous_month), Start_Balance = NA, End_Balance = start_balance)
                for (i in 1:nrow(balances)) {
                        if (all(balances[i, 1:4] == previous_row[, 1:4])) {
                                if (is.na(balances[i, "End_Balance"]) == TRUE) {
                                        balances[i, "End_Balance"] <- start_balance
                                        print("Previous Month's End Balance added.")
                                } else {
                                        if (start_balance != balances[i, "End_Balance"]) {
                                                message <- paste("Previous Month's End balance updated from", balances[i, "End_Balance"])
                                                balances[i, "End_Balance"] <- start_balance
                                                print(message)
                                        } else {
                                                print("Previous Month's End Balance correct.")
                                        }
                                }
                                break
                        }
                        if (i == nrow(balances)) {
                                balances <- rbind(balances, previous_row)
                                setkey(balances, "Institution", "Type", "Year", "Month")
                                print("Added previous month's end balance")
                        }
                }
                return(balances)
        }
        next.month <- function(institution, type, year, month, end_balance) {
                if (month == 12) {
                        next_month <- 1
                        next_year <- year+1
                } else {
                        next_month <- month+1
                        next_year <- year
                }
                next_row <- data.table(Institution = toupper(institution), Type = toupper(type), Year = as.integer(next_year), Month = as.integer(next_month), Start_Balance = end_balance, End_Balance = NA)
                for (k in 1:nrow(balances)) {
                        if (all(balances[k, 1:4] == next_row[, 1:4])) {
                                if (is.na(balances[k, "Start_Balance"]) == TRUE) {
                                        balances[k, "Start_Balance"] <- end_balance
                                        print("Next Month's Start Balance added.")
                                } else {
                                        if (end_balance != balances[k, "Start_Balance"]) {
                                                message <- paste("Next Month's Start balance updated from", balances[k, "Start_Balance"])
                                                balances[k, "Start_Balance"] <- end_balance
                                                print(message)
                                        } else {
                                                print("Next Month's start balance correct.")
                                        }
                                }
                                break
                        }
                        if (k == nrow(balances)) {
                                balances <- rbind(balances, next_row)
                                setkey(balances, "Institution", "Type", "Year", "Month")
                                print("Added next month's start balance")
                        }
                }
                return(balances)
        }
        new_row <- data.table(Institution = toupper(institution), Type = toupper(type), Year = as.integer(year), Month = as.integer(month), Start_Balance = start_balance, End_Balance = end_balance)
        if (any(sapply(1:nrow(balances), function(x) all(balances[x, 1:2] == new_row[, 1:2]))) == FALSE) {
                if (askYesNo("Do you want to add a new institution and type to the balance sheet? ") == FALSE) {
                        stop("Not added to balance sheet due to invalid institution and type.")
                }
        }
        for (j in 1:nrow(balances)) {
                if (all(balances[j, 1:4] == new_row[, 1:4])) {
                        if (is.na(balances[j, "Start_Balance"]) == TRUE) {
                                balances[j, "Start_Balance"] <<- start_balance
                                print("Start Balance updated.")
                                balances <<- previous.month(institution, type, year, month, start_balance)
                        } else {
                                if (start_balance != balances[j, "Start_Balance"]) {
                                        message <- paste("Start balance updated from", balances[j, "Start_Balance"])
                                        balances[j, "Start_Balance"] <<- start_balance
                                        print(message)
                                        balances <<- previous.month(institution, type, year, month, start_balance)
                                } else {
                                        print("Previous start balance correct.")
                                }
                        }
                        if (is.na(balances[j, "End_Balance"]) == TRUE) {
                                balances[j, "End_Balance"] <<- end_balance
                                print("End Balance added.")
                                balances <<- next.month(institution, type, year, month, end_balance)
                        } else {
                                if (end_balance != balances[j, "End_Balance"]) {
                                        message <- paste("End balance updated from", balances[j, "End_Balance"])
                                        balances[j, "End_Balance"] <<- end_balance
                                        print(message)
                                        balances <<- next.month(institution, type, year, month, end_balance)
                                } else {
                                        print("Previous end balance correct.")
                                }
                        }
                        break
                } 
                if (j == nrow(balances)) {
                        balances <<- rbind(balances, new_row)
                        setkey(balances, "Institution", "Type", "Year", "Month")
                        print("Month balances added.")
                        balances <<- previous.month(institution, type, year, month, start_balance)
                        balances <<- next.month(institution, type, year, month, end_balance)
                }
        }
        if (month(Sys.Date()) == 1) {
                sys_year <- year(Sys.Date())-1
                sys_month <- 12
        } else {
                sys_year <- year(Sys.Date())
                if (month(Sys.Date()) > 10) {
                        sys_month <- month(Sys.Date())-1
                } else {
                        sys_month <- paste("0", month(Sys.Date())-1, sep = "")
                }
        }
        file <- paste("./bottom-line/balances-", sys_year, "-", sys_month, ".csv", sep = "")
        fwrite(balances, file)
}

## month.bottom.line used to calculate the bottom line for a specified month. (Complete)

month.bottom.line <- function (year, month) {
        balance.data()
        bottom.line.data()
        month_data <- balances[Year == year & Month == month]
        month_data[Type == "CREDIT", Start_Balance := -Start_Balance]
        month_data[Type == "CREDIT", End_Balance := -End_Balance]
        start <- sum(month_data[, "Start_Balance"])
        end <- sum(month_data[, "End_Balance"])
        change <- end-start
        for (i in 1:nrow(bottom_line)) {
                if (bottom_line[i, "Year"] == year && bottom_line[i, "Month"] == month) {
                        if (bottom_line[i, "Start"] == start && bottom_line[i, "End"] == end && bottom_line[i, "Change"] == change) {
                                print("Bottom line previously calculated.")
                        } else {
                                bottom_line[i, "Start"] <<- start 
                                bottom_line[i, "End"] <<- end 
                                bottom_line[i, "Change"] <<- change
                                print("Bottom line data updated.")
                        }
                        print(bottom_line[i])
                        break
                } 
                if (i == nrow(bottom_line)) {
                        new_row <- data.table(Year = year, Month = month, Start= start, End = end, Change = change)
                        bottom_line <<- rbind(bottom_line, new_row)
                        print("New bottom line added.")
                        setkey(bottom_line, "Year", "Month")
                        print(new_row)
                }
        }
        if (month(Sys.Date()) == 1) {
                sys_year <- year(Sys.Date())-1
                sys_month <- 12
        } else {
                sys_year <- year(Sys.Date())
                if (month(Sys.Date()) > 10) {
                        sys_month <- month(Sys.Date())-1
                } else {
                        sys_month <- paste("0", month(Sys.Date())-1, sep = "")
                }
        }
        file <- paste("./bottom-line/bottom-line-", sys_year, "-", sys_month, ".csv", sep = "")
        fwrite(bottom_line, file)
}

## bottom.line.check makes sure bottom line change matches change represented by transactions. (Complete)

bottom.line.check <- function(trans_data, year, month) {
        bottom.line.data()
        summary <- month.sum(trans_data, year, month)
        trans_data <- sum(summary[, 2])
        month.bottom.line(year, month)
        rel_row <- data.table(Year = year, Month = month)
        for (i in 1:nrow(bottom_line)) {
                if (all(bottom_line[i, 1:2] == rel_row)) {
                        rel_bottom_line <- bottom_line[i]
                }
        }
        check <- trans_data-rel_bottom_line[, "Change"]
        if (abs(check) <= 0.01) {
                print("Transactions and bottom line match.")
        } else {
                print("Transactions and bottom line don't match.")
                return(check)
        }
}
