
library(purrr)
library(stringr)

strReplaceAll <- function(string, pattern = NA, replacement = ""){
        if (identical(pattern, NA)){
                return(string)
        }
        if (length(replacement) == 1){
                replacement <- rep(replacement, length(pattern))
        } else {
                if (length(replacement) != length(pattern)){
                        stop("pattern & replacement arguments must be of same length")
                }
        }
        for (strCounter in 1:length(string)){
                for (counter in 1:length(pattern)){
                        string[strCounter] <- stringr::str_replace_all(string[strCounter],
                                                                       pattern = pattern[counter],
                                                                       replacement = replacement[counter])
                }
        }
        return(string)
}

fileInfoCSV <- function(filename, infoLines = 1,
                        strPatterns = NA,
                        strReplacements = ""){
        force(filename)
        force(infoLines)
        function(){
                result <- fileInfo(filename)()
                result$info[["description"]] <- strReplaceAll(readLines(filename, n = infoLines),
                                                              pattern = strPatterns,
                                                              replacement = strReplacements)
                return(result)
        }
}

readCSV <- function(filename, columns = 1:2,
                    columnNames = c("x","y"),
                    rowNames = NULL, sep = ",", skip = 0, header = TRUE,
                    additionalInfo = NA){
        force(filename)
        force(columns)
        force(columnNames)
        force(rowNames)
        force(sep)
        force(skip)
        force(header)
        force(additionalInfo)
        function(){
                tempdf <- utils::read.table(filename, 
                                            row.names = rowNames,
                                            sep = sep, skip = skip,
                                            header = header)[, columns]
                readData(dataFrame = tempdf,
                         columns = 1:length(columns),
                         columnNames = columnNames,
                         rowNames = rowNames,
                         info = ifelseProper(identical(additionalInfo, NA),
                                             list(source = "csv",
                                                  filename = filename),
                                             append(list(source = "csv",
                                                         filename = filename),
                                                    additionalInfo)))()
        }
}