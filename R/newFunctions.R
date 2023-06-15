
library(purrr)
library(stringr)

multiFileInfoThermo <- function(filename, readIndex = TRUE, collapseCharacter ="-"){
        force(filename)
        force(readIndex)
        force(collapseCharacter)
        result <- list()
        for (counter in 1:length(filename)){
                result[[counter]] <- fileInfoThermo(filename = filename[counter],
                                                    readIndex = readIndex,
                                                    collapseCharacter = collapseCharacter)
        }
        return(result)
}

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
                    sep = ",", skip = 0, header = TRUE,
                    additionalInfo = NA){
        force(filename)
        force(columns)
        force(columnNames)
        force(sep)
        force(skip)
        force(header)
        force(additionalInfo)
        function(){
                tempdf <- utils::read.csv(filename, 
                                          sep = sep, skip = skip,
                                          header = header)[, columns]
                readData(dataFrame = tempdf,
                         columns = 1:length(columns),
                         columnNames = columnNames,
                         rowNames = NULL,
                         info = ifelseProper(identical(additionalInfo, NA),
                                             list(source = "csv",
                                                  filename = filename),
                                             append(list(source = "csv",
                                                         filename = filename),
                                                    additionalInfo)))()
        }
}

# essentially a number of csv files joined together
readMultiCSV <- function(filename, nchars = NA, useBytes = FALSE,
                         linesSplit = '\n',
                         startTable = '#\\"',
                         dataSplit = ',',
                         removeChars = c('#\\"','#','\"','\r'),
                         columnClasses = "character"){ # can only use base types as integer, logical and numeric (default if not any of other)
        force(filename)
        force(nchars)
        force(useBytes)
        force(linesSplit)
        force(dataSplit)
        force(startTable)
        function(){
                if (identical(nchars, NA)){
                        nchars <- file.size(filename)
                }
                textFile <- readChar(filename, nchars = nchars, useBytes = useBytes)
                # split into lines
                textFile <- str_split(textFile, pattern = linesSplit)[[1]]
                # split into tables
                tables <- which(grepl(textFile, pattern = startTable))
                tables[length(tables)+1] <- length(textFile)
                result <- list()
                for (counter in 1:(length(tables)-1)){
                        result[[counter]] <- textFile[tables[counter]:(tables[counter+1]-1)]
                }
                result <- map(result, ~strReplaceAll(.x, pattern = removeChars))
                names(result) <- map_chr(result, ~.x[1])
                # remove names of tables
                result <- map(result, ~.x[-1])
                # get column names of tables
                columnNames <- map_chr(result, ~.x[1]) |>
                        unname() |>
                        stringr::str_split(pattern = ",")
                if (length(columnClasses) == 1){
                   columnClasses <- rep(columnClasses, length(columnNames[[1]]))     
                }  
                # remove headers from tables
                result <- map(result, ~.x[-1])
                result <- map(result, ~str_split(.x[-1], pattern = ","))
                for (listCounter in 1:length(result)){
                        for (counter in 1:length(result[[listCounter]])){
                                names(result[[listCounter]][[counter]]) <- columnNames[[listCounter]]
                        }
                }
                result <- map(result, ~dplyr::bind_rows(.x))
                toDo <- which(columnClasses != "character")
                if (length(toDo)>0){
                        for (listCounter in 1:length(result)){
                                for (counter in 1:length(toDo)){
                                        result[[listCounter]][, toDo[counter]] <- BBPersonalR::ifelseProper(columnClasses[toDo[counter]] == "integer",
                                                                                                            as.integer(as.data.frame(result[[listCounter]])[, toDo[counter]]),
                                                                                                            BBPersonalR::ifelseProper(columnClasses[toDo[counter]] == "logical",
                                                                                                                                      as.logical(as.data.frame(result[[listCounter]])[, toDo[counter]]),
                                                                                                                                      as.numeric(as.data.frame(result[[listCounter]])[, toDo[counter]])
                                                                                                            )
                                                                                  )
                                }
                        }
                }
                return(result)
        }
}
