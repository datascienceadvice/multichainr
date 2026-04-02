extract_tags <- function(path = ".", pattern = "#\' @family",
                                file_pattern = "\\.(R|r|Rmd|Rmarkdown)$", ...) {
  # Get all files matching the pattern
  files <- list.files(path, pattern = file_pattern,
                      recursive = TRUE, full.names = TRUE, ...)
  
  # Initialize result data frame
  result <- data.frame(file = character(), line = character(),
                       stringsAsFactors = FALSE)
  
  for (f in files) {
    # Try to read the file; skip if any error (e.g., binary file)
    lines <- tryCatch(readLines(f, warn = FALSE), error = function(e) NULL)
    if (is.null(lines)) next
    
    # Find lines containing the pattern
    matches <- grep(pattern, lines, value = TRUE, fixed = FALSE)
    
    if (length(matches) > 0) {
      # Append to result
      result <- rbind(result,
                      data.frame(file = rep(f, length(matches)),
                                 line = matches,
                                 stringsAsFactors = FALSE))
    }
  }
  
  # Return the data frame (invisibly for printing)
  return(result)
}

extract_tags()
