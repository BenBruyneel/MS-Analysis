
library(purrr)
library(stringr)

# multiFileInfoThermo <- function(filename, readIndex = TRUE, collapseCharacter ="-"){
#         force(filename)
#         force(readIndex)
#         force(collapseCharacter)
#         result <- list()
#         for (counter in 1:length(filename)){
#                 result[[counter]] <- fileInfoThermo(filename = filename[counter],
#                                                     readIndex = readIndex,
#                                                     collapseCharacter = collapseCharacter)
#         }
#         return(result)
# }

# ---- need to revisit ----
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
        function(...){
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

# internal function for chromfiles, should not have to call directly
readAgilentExport.Spectrum.memory <- function(textLines, sep = ","){
  force(textLines)
  force(sep)
  function(centroided = FALSE){
    tempComment <- textLines[1] %>%
      str_replace_all(pattern = "\\\"", replacement = "") %>%
      str_replace_all(pattern = "#", replacement = "")
    tempdf <- map_df(textLines[3:length(textLines)], ~as.data.frame(t(str_split(.x, pattern = sep)[[1]]))) %>%
      dplyr::select(2,3)
    colnames(tempdf) <- c("mz", "intensity")
    tempdf$mz <- as.numeric(tempdf$mz)
    tempdf$intensity <- as.numeric(tempdf$intensity)
    readData(dataFrame = tempdf,
             info = list(source = "Agilent",
                         centroided = centroided,
                         comment = tempComment))()
  }
}

# note: also deals with single spectra files
readAgilentExport.Spectrum <- function(filename, sep = ",", seekStart = "#"){
  force(filename)
  force(sep)
  force(seekStart)
  function(centroided = FALSE){
    result <- list()
    tempLines <- readLines(filename)
    starts <- which(grepl(tempLines, pattern = seekStart))[c(T,F)] # first line is start & description, second is header
    starts <- append(starts, length(tempLines)+1)
    for (counter in 1:(length(starts)-1)){
      result[[counter]] <- readAgilentExport.Spectrum.memory(tempLines[starts[counter]:(starts[counter+1]-1)])(centroided = centroided)
    }
    return(result)
  }
}


# internal function for chromfiles, should not have to call directly
readAgilentExport.Chromatogram.memory <- function(textLines, sep = ","){
  force(textLines)
  force(sep)
  function(...){
    tempComment <- textLines[1] %>%
      str_replace_all(pattern = "\\\"", replacement = "") %>%
      str_replace_all(pattern = "#", replacement = "")
    tempdf <- map_df(textLines[3:length(textLines)], ~as.data.frame(t(str_split(.x, pattern = sep)[[1]]))) %>%
      dplyr::select(2,3)
    colnames(tempdf) <- c("rt", "intensity")
    tempdf$rt <- as.numeric(tempdf$rt)
    tempdf$intensity <- as.numeric(tempdf$intensity)
    readData(dataFrame = tempdf,
             info = list(source = "Agilent",
                         comment = tempComment))()
  }
}

# note: also deals with single chromatogram files
readAgilentExport.Chromatogram <- function(filename, sep = ",", seekStart = "#"){
  force(filename)
  force(sep)
  force(seekStart)
  function(...){
    result <- list()
    tempLines <- readLines(filename)
    starts <- which(grepl(tempLines, pattern = seekStart))[c(T,F)] # first line is start & description, second is header
    starts <- append(starts, length(tempLines)+1)
    for (counter in 1:(length(starts)-1)){
      result[[counter]] <- readAgilentExport.Chromatogram.memory(tempLines[starts[counter]:(starts[counter+1]-1)])()
    }
    return(result)
  }
}


fileInfoThermo.UV <- function(filename, infoLinesToRead = 50, fileType = "chromatogram"){
  force(filename)
  force(infoLinesToRead)
  force(fileType)
  function(...){
    startData <- switch(fileType,
                        "chromatogram" = "Chromatogram Data:",
                        "Raw Data:")   # default, same for spectrum & 3D
    result <- fileInfo(filename)()
    theInfo <- read.table(file = filename, sep = ",", nrows = infoLinesToRead, skip = 2)[, 1:2]
    theInfo[,1] <- str_replace_all(theInfo[,1], pattern = "\\(\\.+\\)", replacement = "")
    theInfo <- theInfo[1:which(grepl(theInfo[,1],  pattern = startData)),]
    theInfo <- theInfo[theInfo[,1] != "" & theInfo[,2] != "",] # remove empty rows
    infoData <- theInfo[,2]
    names(infoData) <- theInfo[,1]  
    result[[1]]$info <- append(result[[1]]$info, infoData)
    return(result)
  }
}
# 
# readChro
# readChroma <- function(filename, columns = 1:2,
#                     columnNames = c("x","y"),
#                     sep = ",", skip = 0, header = TRUE,
#                     additionalInfo = NA){
#   force(filename)
#   force(columns)
#   force(columnNames)
#   force(sep)
#   force(skip)
#   force(header)
#   force(additionalInfo)
#   function(...){
#     tempdf <- utils::read.csv(filename, 
#                               sep = sep, skip = skip,
#                               header = header)
#     readDataFrame(dataFrame = list(tempdf),
#                   columns = columns,
#                   columnNames = columnNames,
#                   rowNames = NULL,
#                   info = ifelseProper(identical(additionalInfo, NA),
#                                       list(source = "csv",
#                                            filename = filename),
#                                       append(list(source = "csv",
#                                                   filename = filename),
#                                              additionalInfo)))()
#   }
# }

