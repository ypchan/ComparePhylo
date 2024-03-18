get_parsinomy_stat <- function(iqtree_log_lst) {
    # Define output dataframe
    out_df <- data.frame()
    
    # Iterate over each log file
    for (log_file_path in iqtree_log_lst) {
        # Initialize values list with the file name
        values <- list(file_name = log_file_path)
        
        # Regular patterns to match the values
        patterns <- list(
            "sequences" = "Alignment has (\\d+) sequences with (\\d+) columns,",
            "parsimony" = "(\\d+) parsimony-informative,",
            "model" = "model: (.*) chosen according to"
        )
        
        # Read the log file
        lines <- readLines(log_file_path)
        
        # Iterate over each line in the log file
        for( line in lines)  {
            count = 0
            for (pattern_name in names(patterns)) {
                count = count + 1
                matches <- regmatches(line, regexec(patterns[[pattern_name]], line, ignore.case = TRUE))
                if (length(matches[[1]]) == 3) {
                    # Store matched values
                    values[["taxon"]] <- matches[[1]][2]
                    values[["sites"]] <- matches[[1]][3]
                }
                if (length(matches[[1]]) == 2 & count == 2) {
                    # Store matched values
                    values[["parsinomy_sites"]] <- matches[[1]][2]
                }
                if (length(matches[[1]]) == 2 & count == 3) {
                    # Store matched values
                    values[["model"]] <- matches[[1]][2]
                }
            }
        }
        
        # Convert values list to dataframe row
        row_data <- as.data.frame(t(unlist(values)), stringsAsFactors = FALSE)
        colnames(row_data) <- names(values)
        
        # Append row_data to out_df
        out_df <- rbind(out_df, row_data)
    }
    
    # Return the output dataframe
    return(out_df)
}