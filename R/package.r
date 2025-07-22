#load base library (not really needed)
library("base");

#load dplyr library
library("dplyr");

#' Rename columns based on the camelCase standart.
#'
#' @description
#' Changes the list of provided names to be in camelCase. (also removes spaces and other format problems)
#'
#' @details More information
#' It modifies the standart [base::make.names()] function to the camelCase standart
#' and can then be used to apply them with the [dplyr::rename_with()] function to a `dataframe`.
#' INFO: The names are first converted to snake snake_case internally via [make.snakeCase()].
#'
#' @param names A `collection` of names that needs to be cleaned.
#'
#' @seealso [make.snakeCase()] to do the same with snake_case.
#' @examples
#' ```r
#'  # use rename with to get the colnames and apply the changes
#'  `rename_with(data, make.namesCamel)`
#'  #"x_0_test_string_123" => "x0TestString123"
#' ```
make.namesCamel <- function (names) {
    #do it again to remove overlap between single single characters  ("X0_testString123" => X0TestString123")
    gsub("([[:alnum:]])_([[:alnum:]])", "\\1\\U\\2", perl = TRUE,
            #replace all charaters/digits between a "_" with ""  ("x_0_test_string_123" => "x0_testString123")
            gsub("([[:alnum:]])_([[:alnum:]])", "\\1\\U\\2", perl = TRUE,
                #apply snake case to create the basic structure of the names
                make.namesSnake(names)
        )
    );
}

#' Rename columns based on the snake_case standart.
#'
#' @description
#' Changes the list of provided names to be in snake_case. (also removes spaces and other format problems)
#'
#' @details More information
#' It modifies the standart [base::make.names()] function to the snake_case standart
#' and can then be used to apply them with the [dplyr::rename_with()] function to a `dataframe`.
#'
#' @param names A `collection` of names that needs to be cleaned.
#'
#' @return The cleaned up `collection` of names.
#' @seealso [make.camelCase()] to do the same with camelCase.
#' @examples
#' ```r
#'  # use rename with to get the colnames and apply the changes
#'  `rename_with(data, make.namesSnake)`
#'  #"x0TestString123" => "x_0_test_string_123"
#' ```
make.namesSnake <- function (names) {
    #make the name lowercase
    tolower(
        #add a "_" in between all characters before numbers ("X0TestString123" => "X_0TestString_123")
        gsub("([[:lower:]]|[[:upper:]])([[:digit:]])", "\\1_\\2",
            #add a "_" in between all characters after numbers ("X0TestString123" => "X0_TestString123")
            gsub("([[:digit:]])([[:lower:]]|[[:upper:]])", "\\1_\\2",
                #add a "_" in between all small characters and big characters ("X0TestString123" => "X0Test_String123")
                gsub("([[:lower:]])([[:upper:]])", "\\1_\\2",
                    #replace all "." with "_" (`make.names` replaces spaces with ".")
                    gsub("\\.", "_",
                        #apply `make.names` to do initial cleanup (remove duplicates and allow "_")
                        make.names(names, unique = TRUE, allow_ = TRUE)
                    )
                )
            )
        )
    );
}

#' Remove all columns that only contain `NA` or `NaN`.
#'
#' @description
#' It removes columns that only contain `NA` to the [tidyr::drop_na()] function.
#' WARNING: [drop_full_na()] will also drop all `NaN`.
#'
#' @param data The `dataframe` that needs to be cleaned.
#'
#' @return The cleaned up `dataframe`.
#' @export
#' @importFrom dplyr select
#' @importFrom dplyr where
#' @examples
#' ```r
#'  data = data.frame(
#'      part = c(NA, "test", NaN),
#'      na = c(NA, NA, NA),
#'      nan = c(NaN, NaN, NaN),
#'      str = c("NA", "NaN", NA),
#'      ...
#'  );
#'
#'  drop_full_na(data) # => only without the `na` and `nan` column
#' ```
drop_full_na <- function(data) {
    #select only data where the sum of all entries that are not `NA` is more than `0`
    data %>% select(where(~sum(!(is.na(.x))) > 0));
}

#' Remove all columns that only contain `NaN`.
#'
#' @description
#' It removes columns that only contain `NaN` to the [tidyr::drop_na()] function.
#'
#' @param data The `dataframe` that needs to be cleaned.
#'
#' @return The cleaned up `dataframe`.
#' @export
#' @importFrom dplyr select
#' @importFrom dplyr where
#' @examples
#' ```r
#'  data = data.frame(
#'      part = c(NA, "test", NaN),
#'      na = c(NA, NA, NA),
#'      nan = c(NaN, NaN, NaN),
#'      str = c("NA", "NaN", NA),
#'      ...
#'  );
#'
#'  drop_full_na(data) # => only without the `nan` column
#' ```
drop_full_nan <- function(data) {
    #select only data where the sum of all entries that are not `NaN` is more than `0`
    data %>% select(where(~sum(!(is.nan(.x))) > 0));
}

#' Adjust the data type for each column of the boolean type.
#'
#' @description
#' It dynamically and "automatically" changes the datatype of the provided data based on the format for each column.
#'
#' @details More information
#' Converts all columns to `boolean` via the [as.logical()] function,
#' that match this regex `^(NA|true|false)$` for each entry.
#' INFO: Columns containing only `NA` or `NaN` will be ignored.
#'
#'
#' @param data The `dataframe` that needs to be cleaned.
#'
#' @return The cleaned up `dataframe`.
#' @export
#' @examples
#' ```r
#'  #pretty self explanatory how to use it
#'  formatBoolean(data)
#' ```
formatBoolean <- function (data) {
    #iterate all colnames
    for (colname in colnames(data)) {
        #get the column
        col <- data[, colname];

        #check if it is all NA
        if (sum(is.na(col)) == length(col)) {
            #skip
            next;
        }

        if (typeof(col) == "logical") {
            #convert the column to logic (is already of type logical)
            data[,colname] <- as.logical(col);

            #skip to next
            next;
        }

        #get all matches to this expression
        expr <- gregexpr("^(NA|true|false)$", col, ignore.case = TRUE);

        #check for each column whether the data matches the format of a boolean
        #check if the vector contains -1 (no match)
        if (! -1 %in% expr) {
            #convert the column to logic (all entries matched the regex)
            data[,colname] <- as.logical(col);
        }
    }

    #return the changed data
    return(data);
}

#' Adjust the data type for each column of the `number` type.
#'
#' @description
#' It dynamically and "automatically" changes the datatype of the provided data based on the format for each column.
#'
#' @details More information
#' Changes the type to:
#' - `numeric` if only `NaN`
#' - `integer` if only full digits exits
#' - `double` if only `integer`s or digits seperated by `.` or `,` exists
#'
#' @param data The `dataframe` that needs to be cleaned.
#'
#' @return The cleaned up `dataframe`.
#' @export
#' @examples
#' ```r
#'  #pretty self explanatory how to use it
#'  formatNumber(data)
#' ```
formatNumber <- function (data) {
    #iterate all colnames
    for (colname in colnames(data)) {
        #get the column
        col <- data[, colname];

        #check if it is all NaN
        if (sum(is.nan(col)) == length(col)) {
            #set type to numeric
            data[,colname] <- as.numeric(col);

            #skip
            next;
        }

        #check if it is all NA
        if (sum(is.na(col)) == length(col)) {
            #skip
            next;
        }

        #get all matches to this expression
        expr_int <- gregexpr("^(NA|NaN|[[:digit:]]+)$", col, ignore.case = TRUE);

        #check for each column whether the data matches the format of a integer
        #check if the vector contains -1 (no match)
        if (! -1 %in% expr_int) {
            #convert the column to int (all entries matched the regex)
            data[,colname] <- as.integer(col);

            #skip to next
            next;
        }

        #get all matches to this expression
        expr_double <- gregexpr("^(NA|NaN|[[:digit:]]+|[[:digit:]]+\\.[[:digit:]]+)$", col, ignore.case = TRUE);

        #check for each column whether the data matches the format of a double
        #check if the vector contains -1 (no match)
        if (! -1 %in% expr_double) {
            #convert the column to double (all entries matched the regex)
            data[,colname] <- as.double(col);

            #skip to next
            next;
        }

        #get all matches to this expression (use , instead of .) as separator
        expr_double_de <- gregexpr("^(NA|NaN|[[:digit:]]+|[[:digit:]]+\\,[[:digit:]]+)$", col, ignore.case = TRUE);

        #check for each column whether the data matches the format of a double (with ",")
        #check if the vector contains -1 (no match)
        if (! -1 %in% expr_double_de) {
            #convert the column to double (all entries matched the regex)
            data[,colname] <- as.double(sub(",", ".", col));

            #skip to next
            next;
        }
    }

    #return the changed data
    return(data);
}

#' Adjust the data type for each column of the `date` type.
#'
#' @description
#' It dynamically and "automatically" changes the datatype of the provided data based on the format for each column.
#'
#' @details More information
#' Changes the type to a `date` if the `string` has the format .
#' - dd/mm/yyyy
#' - dd-mm-yyyy
#' - dd.mm.yyyy
#' WARNING: The format needs to be consistent in the column.
#'
#' @param data The `dataframe` that needs to be cleaned.
#'
#' @return The cleaned up `dataframe`.
#' @export
#' @examples
#' ```r
#'  #pretty self explanatory how to use it
#'  formatDate(data)
#' ```
formatDate <- function (data) {
    #iterate all colnames
    for (colname in colnames(data)) {
        #get the column
        col <- data[, colname];

        #check if it is all NA
        if (sum(is.na(col)) == length(col)) {
            #skip
            next;
        }

        #get all matches to this expression
        expr <- gregexpr("^(?:(?:31(\\/|-|\\.)(?:0?[13578]|1[02]))\\1|(?:(?:29|30)(\\/|-|\\.)(?:0?[13-9]|1[0-2])\\2))(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$|^(?:29(\\/|-|\\.)0?2\\3(?:(?:(?:1[6-9]|[2-9]\\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\\d|2[0-8])(\\/|-|\\.)(?:(?:0?[1-9])|(?:1[0-2]))\\4(?:(?:1[6-9]|[2-9]\\d)?\\d{2})$",
        col, ignore.case = TRUE);

        #check for each column whether the data matches the format of a Date
        #check if the vector contains -1 (no match)
        if (! -1 %in% expr) {
            #convert the column to a Date (all entries matched the regex)
            data[,colname] <- as.Date(col, tryFormats = c("%d.%m.%y", "%d/%m/%y", "%d-%m-%y"));

            #skip to next
            next;
        }
    }

    #return the changed data
    return(data);
}

#' Adjust the data type for each column of the `factor` type.
#'
#' @description
#' It dynamically and "automatically" changes the datatype of the provided data based on the format for each column.
#'
#' @details More information
#' Changes the type to a factor if the existing type is `character` and the amount of unique items is less than half of all items.
#'
#' @param data The `dataframe` that needs to be cleaned.
#'
#' @return The cleaned up `dataframe`.
#' @export
#' @examples
#' ```r
#'  #pretty self explanatory how to use it
#'  formatFactor(data)
#' ```
formatFactor <- function (data) {
    #iterate all colnames
    for (colname in colnames(data)) {
        #get the column
        col <- data[, colname];

        #check if it is all NA
        if (sum(is.na(col)) == length(col)) {
            #skip
            next;
        }

        #check for each column whether the data matches the format of a Date
        #check if the vector contains -1 (no match)
        if (typeof(col) == "character" & length(unique(col)) < length(col) / 2) {
            #convert the column to a Date (all entries matched the regex)
            data[,colname] <- factor(col, levels = unique(col));

            #skip to next
            next;
        }
    }

    #return the changed data
    return(data);
}

#' Clean up data to make it easier to work with.
#'
#' @description
#' This function is a wrapper for all the `rcleanup` functions.
#' They can be used to cleanup and restructure the dataset for future use,
#' without noticeably changing the data itself.
#' The changes made include:
#'  - Names of columns (renaming duplicates, changing the format).
#'  - Type of the data in the columns (can also slightly change the data `1,1` => `1.1`).
#'  - Removing unnecessary columns (only `NA` or `NaN`).
#'
#' @details More information
#' ### Rename Columns
#' It combines: [make.namesSnake()], [make.namesCamel()], [base::make.names()]
#' and applies them with the [dplyr::rename_with()] function to the provided data.
#' `colname="camel"` => `rename_with(data, make.namesCamel)`
#' `colname="snake"` => `rename_with(data, make.namesSnake)`
#' `colname="make"` => `rename_with(data, make.names)`
#'
#' ### Change Column Types
#' It dynamically and "automatically" changes the datatype of the provided data based on the format for each column.
#' This is done by using: [formatBoolean()], [formatNumber()], [formatDate()], [formatFactor()].
#' Certain keywords correspond to certain combinations:
#' `auto_format="full"` => `data %>% formatBoolean() %>% formatNumber() %>% formatDate() %>% formatFactor()`
#' `auto_format="partial"` => `data %>% formatBoolean() %>% formatNumber() %>% formatDate()`
#' `auto_format="basic"` => `data %>% formatBoolean() %>% formatNumber()`
#'
#' ### Remove Unnecessary Data
#' It removes columns that only contain `NA` or `NaN` using [drop_full_na()] or [drop_full_nan()] similar to the [tidyr::drop_na()] function.
#' WARNING: [drop_full_na()] will also drop all `NaN`.
#' Therefore setting `drop_nan` to `FALSE` while `drop_na` is `TRUE` will still drop all `NaN`.
#'

#' @param data The `dataframe` that needs to be cleaned.
#' @param colname A `string` indicating the kind of format that should be used for the column names. `"camel"` per default of `"snake"|"camel"|"make"|"none"`.
#' @param auto_format  A `string` indicating the kind of format that should be applied for each column. `"partial"` per default of `"full"|"partial"|"base"|"boolean"|"number"|"date"|"factor"|"none"`.
#' @param drop_na A `boolean` indicating whether columns containing only `NA` or `NaN` should be removed.  `TRUE` per default.
#' @param drop_nan A `boolean` indicating whether columns containing only `NaN` should be removed.  `TRUE` per default.
#'
#' @return The cleaned up `dataframe`.
#' @export
#' @importFrom dplyr rename_with
#' @importFrom magrittr %>%
#' @examples
#' ```r
#' #clean(data, colname="camel", auto_format="partial", drop_na=TRUE, drop_nan=TRUE) <=> clean(data)
#' clean(data)
#'
#' #same only with snake case names
#' clean(data, colname="snake")
#'
#' #same only with names made by `make.names()` and no format
#' clean(data, colname="make")
#'
#' #no col name formatting
#' clean(data, colname="none")
#'
#' #full type formatting
#' clean(data, auto_format="full")
#'
#' #partial type formatting (no factors)
#' clean(data, auto_format="partial")
#'
#' #base type formatting (no factors or dates)
#' clean(data, auto_format="base")
#'
#' #basic type formatting (only boolean)
#' clean(data, auto_format="boolean")
#' ... # for each spezial type
#'
#' #no type formatting
#' clean(data, auto_format="none")
#'
#' #remove all columns containing only NA or NaN
#' clean(data, drop_na=TRUE)
#'
#' #remove all columns containing only NaN
#' clean(data, drop_nan=TRUE)
#' ```
clean <- function(data, colname="camel", auto_format="partial", drop_na=TRUE, drop_nan=TRUE) {
    #apply the dropping of "NA" corresponding to the `drop_na` param (default to drops)
    if (drop_na) {
        #use the function to drop all columns containing only NA
        data <- drop_full_na(data);
    } else if (drop_nan) { #apply the dropping of "NaN" corresponding to the `drop_nan` param (default to drops)
        #use the function to drop all columns containing only NaN
        data <- drop_full_nan(data);
    }

    #apply the cleanup of the colnames corresponding to the colnames param (default to no changes)
    data <- switch(colname,
        "snake" = rename_with(data, make.namesSnake),
        "camel" = rename_with(data, make.namesCamel),
        "make" = rename_with(data, make.names),
        "none" = data,
        .default = data
    );

    #apply the formatting corresponding to the format param (default to no formatting)
    data <- switch(auto_format,
        "full" = data %>% formatBoolean() %>% formatNumber() %>% formatDate() %>% formatFactor(),
        "partial" = data %>% formatBoolean() %>% formatNumber() %>% formatDate(),
        "base" = data %>% formatBoolean() %>% formatNumber(),
        "boolean" = formatBoolean(data),
        "number" =  formatNumber(data),
        "date" = formatDate(data),
        "factor" = formatFactor(data),
        "none" = data,
        .default = data
    );

    #return the end result
    return(data);
}



