# ---- basic data load functions ----

#' basic function factory which generates a function that returns the data in a data.frame
#' 
#' @note the reason these readData functions are function factories is that (depending on
#'  the data source) the actual reading in of data may take quite some time. readData
#'  provides a base function that reads the data in and returns a list of an info data.frame
#'  object with info on the data and a data data.frame object that contains the actual data.
#'  The result can be used as a data source for info objects (and their descendants)
#'   
#' @param dataFrame data.frame containing the data
#' @param columns specifies which columns to take from the dataFrame
#' @param columnNames specifies which column names to give to the data.frame. Should be
#'  same length as the columns argument.
#' @param rowNames specifies which row names to give to the data.frame. Should be same
#'  length as the number of rows in the dataFrame argument
#' @param info named list object that contains info to be put into the info data.frame of
#'  the info objects. The intention is to put some relevant information there, like source
#'  of the data, file names and other information which makes it possible to recreate the
#'  data
#'  
#' @return a function that returns a list of two objects: info and data
#' @export
readData <- function(dataFrame = NA,
                     columns = 1:ncol(dataFrame),
                     columnNames = NA, # c("x","y"),
                     rowNames = NULL,
                     info = list(source = "data")){
        force(dataFrame)
        force(columns)
        force(columnNames)
        force(rowNames)
        force(info)
        function(){
                if (!identical(dataFrame, NA)){
                        if (!identical(columns, NA)){
                                dataFrame <- dataFrame[, columns]
                        }
                        if (!identical(columnNames, NA)){
                                colnames(dataFrame) <- columnNames                                
                        }
                        if (!identical(rowNames, NA)){
                                rownames(dataFrame) = rowNames
                        }
                }
                return(
                        list(
                                info = info,
                                data = dataFrame)
                )
        }
}

#' function factory to read excel data, more or less a demo function that
#'  can be expanded for more complicated sheets/data
#'
#' @note use the 'openxlsx' package to read the data
#'
#' @param filename name of the excel file from which the data to read
#' @param columns columns from the excel file to read
#' @param columnNames new names for the columns of the data read from the excel file
#'  (length column names should be the same as the length of columns)
#' @param rowNames specifies which row names to give to the data.frame. Should be same
#'  length as the number of rows of the data being read
#'
#' @return a function that reads the data from the specified excel file and
#'  returns a list of two objects: info and data
#' @export
readExcel <- function(filename,
                      columns = 1:2, columnNames = c("x","y"),
                      rowNames = NULL){
        force(filename)
        force(columns)
        force(columnNames)
        force(rowNames)
        function(){
                tempdf <- openxlsx::read.xlsx(filename)[,columns]
                readData(dataFrame = tempdf,
                         columns = columns, columnNames = columnNames,
                         rowNames = NULL,
                         info = list(source = "xlsx",
                                     filename = filename))()
        }
}

# ---- read chromatogram data ----

#' function factory that generates a function that reads data from a thermo MS
#'  chromatogram file
#' 
#' @note the file for this function needs to be '.raw' format. Internally the 'rawrr'
#'  package is used to do the actual data extraction
#'
#' @param filename name of the .raw file from which the data to be read
#' @param mz specifies the m/z to make an extracted ion chormatogram of. Ignored unless
#'  the 'type' argument is "xic"
#' @param tolerance spcifies the tolerance to use when extracting an ion chromatogram.
#'  Ignored unless the 'type' argument is "xic". Please note that this value is in 'ppm' and
#'  that a value of 10 is the same as a mass tolerance of 5 ppm in eg the Thermo freestyle
#'  software (the 5 specified there is a range m/z-5 till m/z+5). Here 10 means m/z-5 till
#'  m/z+5
#' @param filter specifies the scan filter to be used, default = "ms". Valid is also eg "ms2"
#'  for ms2 data (if present in the file)
#' @param type specifies the data type to read, possible is: "tic"  (total ion current),
#'  "bpc" (base peak chromatogram) or "xic" (extracted ion chromatogram)
#'
#' @return a function that reads the data from the specified .raw file and returns a
#'  list of two objects: info and data
#' @export
readChromatogramThermo <- function(filename,
                                   mz = NA,
                                   tolerance = 10,
                                   filter = "ms",
                                   type = "xic"){
        force(filename)
        force(mz)
        force(tolerance)
        force(filter)
        force(type)
        function(){
                tempData <- rawrr::readChromatogram(rawfile = filename,
                                                    mass = mz,
                                                    tol = tolerance,
                                                    filter = filter,
                                                    type = type)
                if (type == "xic"){
                        tempData <- tempData[[1]]
                }
                readData(dataFrame = data.frame(rt = as.numeric(tempData$times),
                                                intensity = as.numeric(tempData$intensities)),
                         columnNames = c("rt", "intensity"),
                         info = list(source = "thermo",
                                     filename = filename,
                                     mz = ifelse(is.null(mz),
                                                 NA,
                                                 mz),
                                     tolerance = tolerance,
                                     filter = filter,
                                     type = type))()
        }
}

#' function factory that generates a list of functions that read data from a thermo MS
#'  chromatogram file(s)
#' 
#' @note the file(s) for this function needs to be '.raw' format. Internally the 'rawrr'
#'  package is used to do the actual data extraction
#'  
#' @note each of the variables can be a multi element vector. The functions generate will
#'  attempt to accomodate all possible combinations of these. Please be aware that with
#'  many multi element arguments this will quickly lead to (extremely) long function lists
#'
#' @param filename name of the .raw file from which the data to be read
#' @param mz specifies the m/z to make an extracted ion chormatogram of. Ignored unless
#'  the 'type' argument is "xic"
#' @param tolerance spcifies the tolerance to use when extracting an ion chromatogram.
#'  Ignored unless the 'type' argument is "xic". Please note that this value is in 'ppm' and
#'  that a value of 10 is the same as a mass tolerance of 5 ppm in eg the Thermo freestyle
#'  software (the 5 specified there is a range m/z-5 till m/z+5). Here 10 means m/z-5 till
#'  m/z+5
#' @param filter specifies the scan filter to be used, default = "ms". Valid is also eg "ms2"
#'  for ms2 data (if present in the file)
#' @param type specifies the data type to read, possible is: "tic"  (total ion current),
#'  "bpc" (base peak chromatogram) or "xic" (extracted ion chromatogram)
#'
#' @return list of functions that read the data from the specified .raw file and returns a
#'  list of two objects: info and data
#' @export
readMultipleChromatogramsThermo <- function(filename,
                                            mz = NA,
                                            tolerance = 10,
                                            filter = "ms",
                                            type = "xic"){
        force(filename)
        force(mz)
        force(tolerance)
        force(filter)
        force(type)
        dataInput <- list(filename = filename,
                          mz = mz,
                          tolerance = tolerance,
                          filter = filter,
                          type = type)
        dataLengths <- unlist(lapply(dataInput, length))
        dataMax <- which(dataLengths > 1)
        if (length(dataMax) > 1){
                newMax <- prod(dataLengths[dataMax])
                for (counter in dataMax){
                        lengthToReplicate <- newMax / length(dataInput[[counter]])
                        dataInput[[counter]] <- rep(dataInput[[counter]], lengthToReplicate)
                }
                dataLengths <- unlist(lapply(dataInput, length))
                dataMax <- newMax
        } else {
                dataMax <- max(dataLengths)
        }
        # make repeats for single element values in data input
        if (dataMax > 1){
                for (counter in 1:length(dataInput)){
                        if (dataLengths[counter] == 1){
                                dataInput[[counter]] <- rep(dataInput[[counter]], max(dataLengths))
                        }
                }
                
        }
        result <- list()
        for (counter in 1:length(dataInput[[1]])){
                result[[counter]] <- readChromatogramThermo(
                        filename = dataInput$filename[counter],
                        mz = dataInput$mz[counter],
                        tolerance = dataInput$tolerance[counter],
                        filter = dataInput$filter[counter],
                        type = dataInput$type[counter]
                )
        }
        return(result)
}

# ---- read spectra data ----

#' function factory that generates a function that reads spectral data from a thermo MS
#'  chromatogram file
#' 
#' @note the file(s) for this function needs to be '.raw' format. Internally the 'rawrr'
#'  package is used to do the actual data extraction
#'  
#' @param filename name of the .raw file from which the data to be read
#' @param scan scan number to be extracted
#' @param centroided whether to retrieve the centroided spectrum (TRUE) or not (FALSE).
#'  Please note that it has been observed that when centroided is FALSE, while there is
#'  only centroided data, centroided data will be returned!
#'
#' @return a function that reads data from the specified .raw file and returns a
#'  list of two objects: info and data
#' @export
readSpectrumThermo <- function(filename,
                               scan = NULL,
                               centroided = FALSE){
        force(filename)
        force(scan)
        force(centroided)
        function(){
                if (length(scan) > 1){
                        scan <- scan[1]
                }
                tempData <- rawrr::readSpectrum(rawfile = filename,
                                                scan = scan)
                if (centroided){
                        data <- data.frame(mz = tempData[[1]]$centroid.mZ,
                                           intensity = tempData[[1]]$centroid.intensity)
                } else {
                        data <- data.frame(mz = tempData[[1]]$mZ,
                                           intensity = tempData[[1]]$intensity)
                }
                info <- list(source = "thermo",
                             filename = filename,
                             rt = tempData[[1]]$rtinseconds/60,
                             scan = scan,
                             scanType = tempData[[1]]$scanType,
                             centroided = centroided)
                return(readData(dataFrame = data,
                                columnNames = c("mz","intensity"),
                                info = info)())
        }
}

#' function factory that generates a list of functions that reads spectral data from
#'  a thermo MS chromatogram file 
#' 
#' @note the file(s) for this function needs to be '.raw' format. Internally the 'rawrr'
#'  package is used to do the actual data extraction
#'
#' @param filename name of the .raw file from which the data to read
#' @param scan scan number to be extracted, this can be a multi element vector, eg 101:110 
#' @param centroided whether to retrieve the centroided spectrum (TRUE) or not (FALSE).
#'  Please note that it has been observed that when centroided is FALSE, while there is
#'  only centroided data, centroided data will be returned!
#'
#' @return a list of functions that read data from the specified .raw file and return a
#'  list of two objects: info and data
#' @export
readMultipleSpectrumThermo <- function(filename,
                                       scan = NULL,
                                       centroided = FALSE){
        force(filename)
        force(scan)
        force(centroided)
        result <- list()
        for (counter in 1:length(scan)){
                result[[counter]] <- readSpectrumThermo(filename = filename,
                                                        scan = scan[counter],
                                                        centroided = centroided)
        }
        return(result)
}

# ---- Thermo rawFile Info ----

#' Internal function factory that generates a function for a simple info element for the
#'  'fileInfoThermo'function
#'
#' @param filename name of the file from which the data is to be read
#'
#' @return list of two objects: info (contains only filename) and data (empty)
#' @noRd
fileInfo <- function(filename){
        force(filename)
        function(){
                if (!file.exists(filename)){
                        stop("File does not exist")
                }
                return(
                        list(info = list(filename = filename),
                             data = NA)
                )
        }
}

#' function factory that generates a function for an info & data element with
#'  info on a Thermo .raw mass spectrometry file
#'
#' @param filename name of the file from which the data is to be read
#' @param readIndex logical vector. If TRUE the data element will contain info
#'  on all scans in the file. If FALSE then data element will be empty
#' @param collapseCharacter some of the file info data consists of more than one
#'  element. These will be pasted together where collapseCharacter defines the separator
#'  
#' @return list of two objects: info (contains only file info) and data (empty or
#'  index of scans)
#' @export
fileInfoThermo <- function(filename, readIndex = TRUE, collapseCharacter = "-"){
        force(filename)
        force(readIndex)
        force(collapseCharacter)
        function(){
                if (!file.exists(filename)){
                        stop("File does not exist")
                }
                tempList <- list(filename = filename)
                tempList <- append(tempList , rawrr::readFileHeader(filename))
                tLengths <- unlist(lapply(tempList, length))
                for (counter in which(tLengths > 1)){
                        # paste together multiple element vectors in the list
                        tempList[[counter]] <- paste(tempList[[counter]],
                                                     collapse = collapseCharacter)
                }
                if (readIndex){
                        tempIndex <- rawrr::readIndex(filename)
                } else {
                        tempIndex <- NA
                }
                names(tempList) <- gsub(names(tempList),
                                        pattern = " ", replacement = "")
                names(tempList) <- gsub(names(tempList),
                                        pattern = "\\(", replacement = "")
                names(tempList) <- gsub(names(tempList),
                                        pattern = "\\)", replacement = "")
                
                return(
                        list(info = tempList,
                             data = tempIndex)
                )
        }
}