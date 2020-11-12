expand_list_columns <- function(records) {
  ## This process is a bit ridiculous
  ## 1.  Record the set of list columns (currently there are 2, but I expect that will change at some point).
    ##     This is handled by appending to the list_columns scalar.
    ## 2.  Check the length of the elements in the list columns, right now one of them is empty
    ##     So I am just going to drop it.
    ## 3.  Figure out the structure of the lists from the first one
    ##     These are little dataframes where the first column is the names and second column values.
    ## 4.  Figure out if the data from the internal lists already exists, if so, ignore it,
    ##     if not, make an empty column to hold that information.
    ## 5.  Fill in the empty columns on a row-by-row basis; which is slow but at least clear.
    ## 6.  Drop the original columns so they do not screw us later.
    list_columns <- c()
    for (col_num in 1:length(colnames(records))) {
        if (class(records[[col_num]])[1] == "list") {
            ## 1. above, write down the lists
            list_columns <- c(col_num, list_columns)
            ## 2. above, drop the empty lists
            if (length(records[1, col_num][[1]]) == 0) {
                next
            }
            ## 3. Get the internal structure from the first element
            internal <- records[1, col_num][[1]]
            new_names <- internal[[1]]
            for (n in 1:length(new_names)) {
                new_name <- new_names[n]
                ## 4. Is this data already in the table?
                if (!new_name %in% colnames(records)) {
                    records[[new_name]] <- ""
                }
            }
            ## 5. Fill in the new columns with the relevant data
            for (r in 1:nrow(records)) {
                for (n in 1:length(new_names)) {
                    new_name <- new_names[n]
                    records[r, new_name] <- internal[n, 2]
                }
            }
        }
    }
    ## 6.  Get rid of the stupid list columns.
    records[, list_columns] <- NULL
    return(records)
}
