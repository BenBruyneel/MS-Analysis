

# ---- MSInfo ----

#' R6 Class base for mass specrometry info
#' 
#' @description 
#'  R6 Class dealing with mass spectrometry data in an organised manner. This class
#'   is a descendant of infolist. In principle it holds 4 infoDB or infoDB Variable
#'   items which represent collections of chromatograms, spectra, files & peaks
#'   
#' @note it can be expanded to contain more items obviously, but this is best done
#'  via a descendant
#'
#' @export 
MSInfo <- R6::R6Class(
        "MSInfo",
        inherit = infoList,
        private = list(
                type_ = "infoDatabase"
                # internal variable defining type of info element to be used
                # options are: "dbVar" for infoDBVariable, "db" for infoDB and 
                # "rds" for info (non-database)
        ),
        public = list(
                showWarnings = TRUE,
                #  logical vector which defines whether warnings are to be shown
                #   info object itself only uses this in private evaluate_ function
                
                #' @description create a new info object
                #' 
                #' @param type character vector defining the type of info element
                #'  to be used. By default this will be "dbVar" for infoDBVariable.
                #'  Other options are  "db" for infoDB and  "rds" for info
                #'  (non-database). If somthing else is passed on then an 'empty'
                #'  object will be created
                #'  
                #' @return a new 'info' object
                initialize = function(type = "infoDatabase"){
                        private$type_ <- type
                        if (type == "infoDB"){
                                private$info_[[1]] <- infoDB$new(name = "chromatograms")
                                private$info_[[2]] <- infoDB$new(name = "spectra")
                                private$info_[[3]] <- infoDB$new(name = "files")
                                # private$info_[[4]] <- infoDB$new(name = "peaks")
                        } else {
                                if (type == "rds"){
                                        private$info_[[1]] <- info$new(name = "chromatograms")
                                        private$info_[[2]] <- info$new(name = "spectra")
                                        private$info_[[3]] <- info$new(name = "files")
                                        # private$info_[[4]] <- info$new(name = "peaks")
                                } else {
                                        if (type == "infoDBVariable"){
                                                private$info_[[1]] <- infoDBVariable$new(name = "chromatograms")
                                                private$info_[[2]] <- infoDBVariable$new(name = "spectra")
                                                private$info_[[3]] <- infoDBVariable$new(name = "files")
                                                # private$info_[[4]] <- infoDBVariable$new(name = "peaks")
                                        } else {
                                                if (type == "infoDatabase"){
                                                        private$info_[[1]] <- infoDatabase$new(name = "chromatograms")
                                                        private$info_[[2]] <- infoDatabase$new(name = "spectra")
                                                        private$info_[[3]] <- infoDatabase$new(name = "files")
                                                        # private$info_[[4]] <- infoDatabase$new(name = "peaks")
                                                } else {
                                                        private$info_[[1]] <- NA
                                                        private$info_[[2]] <- NA
                                                        private$info_[[3]] <- NA
                                                        # private$info_[[4]] <- NA
                                                }
                                        }
                                }
                        }
                        names(private$info_) <- c("chromatograms",
                                                  "spectra",
                                                  "files")#,
                        #"peaks")
                        invisible(self)
                },
                #' @description
                #' For printing purposes: prints the names of the "info" items present
                #'  and the number of data_ items in each
                print = function(){
                        cat(paste(c(" File Info\t: ", self$files$length,"\n"), collapse = ""))
                        cat(paste(c(" Chromatograms\t: ", self$chromatograms$length,"\n"), collapse = ""))
                        cat(paste(c(" Spectra\t: ", self$spectra$length,"\n"), collapse = ""))
                        # cat(paste(c(" Peaks  \t: ", self$peaks$length,"\n"), collapse = ""))
                        if (self$length > 3){
                                for (counter in 4:self$length){
                                        cat(paste(c(" ",self$names[counter],"\t: ", self$item(counter)$length,"\n"), collapse = ""))
                                }
                        }
                },
                #' @description 
                #'  Loads the info objects from a database
                #' 
                #' @param db database access 'handle' from which data is to be loaded
                #'  
                #' @note this does not work if "rds" was chosen as type of info object
                #'  (see initialize)
                load = function(db){
                        if (private$type_ %in%  c("infoDB","infoDBVariable","infoDatabase")){
                                tempL <- as.logical()
                                for (counter in 1:self$length){
                                        tempL <- append(tempL, private$info_[[counter]]$load(db = db))
                                }
                                for (counter in 1:self$length){
                                        if (tempL[counter]){
                                                private$info_[[counter]]$index <- max(private$info_[[counter]]$info$id, na.rm = TRUE) + 1
                                        }
                                }
                                if (!self$spectra$empty){
                                        self$spectra$info$centroided <- as.logical(self$spectra$info$centroided)
                                }
                                return(tempL)
                        } else {
                                if (self$showWarnings){
                                        warning(" Cannot automatically load this infoList type.")
                                }
                                return(NA)
                        }
                        invisble()
                },
                #' @description 
                #'  saves the info objects to a database
                #'  
                #' @param db database access 'handle' to which data is to be saved
                #' @param overwrite defines whether existing tables in the database are allowed
                #'  to be overwritten (default is TRUE)
                #'  
                #' @note this does not work if "rds" was chosen as type of info object
                #'  (see initialize)
                save = function(db, overwrite = TRUE){
                        if (private$type_ %in%  c("infoDB","infoDBVariable","infoDatabase")){
                                tempL <- as.logical()
                                for (counter in 1:self$length){
                                        tempL <- append(tempL, private$info_[[counter]]$save(db = db, overwrite = overwrite))
                                }
                                return(tempL)
                        } else {
                                if (self$showWarnings){
                                        warning(" Cannot automatically save this infoList type.")
                                }
                                return(NA)
                        }
                        invisble()
                },
                #' @description 
                #'  Attempts to find the file (on which a chromatogram (indicated by index/id)) in
                #'  the files object (via the filename). If it is found, the files index is returned
                #'
                #' @param index index of the chromatogram (see chromatograms$info); ignored if
                #'  id is not NA. Note: can also be a character vector specifuing a filename
                #' @param id uses the id column of the chromatograms$info object to find the
                #'  correct index of the chromatogram to be used
                #'
                #' @return integer vector or NA
                #' @export
                chromatogram.files.index = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$chromatograms$indexFromId(id = id)
                        }
                        if (!identical(index, NA)){
                                if (!is.character(index)){
                                        if ((index > 0) & (index <= self$chromatograms$length)){
                                                index <- self$chromatograms$info$filename[index]
                                        }
                                }
                                if (index %in% self$files$info$filename){
                                        return(which(index == self$files$info$filename))
                                }
                        }
                        return(NA)
                },
                #' @description 
                #'  returns the filename on which a chromatogram (indicated via index/id) is based
                #'
                #' @param index index of the chromatogram (see chromatograms$info); ignored if
                #'  id is not NA
                #' @param id uses the id column of the chromatograms$info object to find the
                #'  correct index of the chromatogram to be used
                #'
                #' @return integer vector or NA
                #' @export
                chromatogram.filename = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$chromatograms$indexFromId(id = id)
                        }
                        if ((index > 0) & (index <= nrow(self$chromatograms$info))){
                                return(self$chromatograms$info$filename[index])
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  plots one of the chromatograms in the object
                #'
                #' @param index index of the chromatogram (see chromatograms$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the chromatograms$info object to find the
                #'  correct index of the chromatogram to be used
                #' @param ... to pass on additional parameters to the plotChromatogram function
                #'
                #' @return ggplot plot of the specified chromatogram
                #' @export
                chromatogram.plot = function(index = 1, id = NA, ...){
                        if (!identical(id, NA)){
                                index <- self$chromatograms$indexFromId(id = id)
                        }
                        tempdf <- self$chromatograms$item(index)
                        if (!identical(tempdf, NA)){
                                plotChromatogram(tempdf, ...)
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  finds peaks in a chromatogram (inside the object's internal chromatogram section)
                #'
                #' @param index index of the chromatogram (see chromatograms$info) to be used for
                #'  peak detection; ignored if id is not NA
                #' @param id uses the id column of the chromatograms$info object to find the
                #'  correct index of the chromatogram to be used
                #' @param chromatogramDetectPeaksMethod function to detect peaks in the
                #'  selected chromatogram
                #' @param ... to pass on additional parameters to the
                #'  chromatogramDetectPeaksMethod function
                #'
                #' @return the result from the chromatogramDetectPeaksMethod or NA
                #' @export
                chromatogram.findPeaks = function(index = 1, id = NA, chromatogramDetectPeaksMethod = NA, ...){
                        if (!identical(id, NA)){
                                index <- self$chromatograms$indexFromId(id = id)
                        }
                        tempdf <- self$chromatograms$item(index)
                        if (!identical(tempdf, NA)){
                                if (identical(chromatogramDetectPeaksMethod, NA)){
                                        return(chromatogramFindPeaks(trace = tempdf,
                                                                     xtraInfo = list(traceId = self$chromatograms$idFromIndex(index)),
                                                                     ...))
                                } else {
                                        return(chromatogramDetectPeaksMethod(trace = tempdf, ...))
                                }
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  returns a smoothed version of the indicated chromatogram (indicated by index/id)
                #'
                #' @param index index of the chromatogram (see chromatograms$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the chromatograms$info object to find the
                #'  correct index of the chromatogram to be used
                #' @param smoothFunction function to smooth the selected chromatogram
                #' @param ... to pass on additional parameters to the smoothing function
                #'
                #' @return the result from the chromatogramDetectPeaksMethod or NA
                #' @export
                chromatogram.smooth = function(index = 1, id = NA,
                                               smoothFunction = smoothFunction.loess(x = "rt"),
                                               ...){
                        if (!identical(id, NA)){
                                index <- self$chromatograms$indexFromId(id = id)
                        }
                        tempdf <- self$chromatograms$item(index)
                        if (!identical(tempdf, NA)){
                                smoothData(tempdf, 
                                           smoothFunction = smoothFunction,
                                           ...)
                        }
                },
                #' @description 
                #'  returns the maximum intensity in the chromatogram
                #'
                #' @param index index of the spectrum (see spectra$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #'
                #' @return numeric vector or NA
                #' @export
                chromatogram.intensity.max = function(index = 1, id = NA,
                                                      ...){
                        if (!identical(id, NA)){
                                index <- self$chromatograms$indexFromId(id = id)
                        }
                        tempdf <- self$chromatograms$item(index)
                        if (!identical(tempdf, NA)){
                                return(max(tempdf$intensity, na.rm = T))
                        }
                        return(NA)
                },
                #' @description
                #'  Returns the index of the filename in the files$info object
                #'
                #' @param filename filename for which the index is to be found
                #'
                #' @return integer vector or NA
                #' @export
                file.index = function(filename){
                        if (filename %in% self$files$info$filename){
                                return(which(filename == self$files$info$filename))
                        } else {
                                return(NA)
                        }
                },
                #' @description
                #'  Returns the id of the filename in the files$info object
                #'
                #' @param filename filename for which the id is to be found
                #'
                #' @return integer vector or NA
                #' @export
                file.id = function(filename){
                        if (filename %in% self$files$info$filename){
                                return(self$files$info$id[filename == self$files$info$filename])
                        } else {
                                return(NA)
                        }
                },
                #' @description
                #'  Returns the filename of row with index/id in the files$info object
                #'
                #' @param index index of the file (see files$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the files$info object to find the
                #'  correct index of the chromatogram to be used
                #'
                #' @return character vector or NA
                #' @export
                file.name = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$files$indexFromId(id = id)
                        }
                        if ((index > 0) & (index <= self$files$length)){
                                return(self$files$info$filename[index])
                        }
                        return(NA)
                },
                #' @description
                #'  performs the getScans function on the selected scan index in the files$info object
                #'
                #' @param index index of the file (see files$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the files$info object to find the
                #'  correct index of the chromatogram to be used
                #' @param ... to pass on additional parameters to the getScans function
                #'
                #' @return character vector or NA
                #' @export
                file.getScans = function(index = 1, id = NA, ...){
                        if (!identical(id, NA)){
                                index <- self$files$indexFromId(id = id)
                        }
                        tempScanIndex <- self$files$item(index)
                        if (!identical(tempScanIndex, NA)){
                                return(getScans(scanIndex = tempScanIndex, ...))
                        }
                },
                
                # ---- currently not inmplemented ----
                # peaks.sort = function(decreasing = TRUE, intensity = TRUE){
                #         if (self$peaks$length > 0){
                #                 for (counter in 1:self$peaks$length){
                #                         if (intensity) {
                #                                 if (decreasing) {
                #                                         self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                                 dplyr::arrange(desc(peak_intensity))
                #                                 } else {
                #                                         self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                                 dplyr::arrange(peak_intensity)
                #                                 }
                #                         } else {
                #                                 if (decreasing) {
                #                                         self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                                 dplyr::arrange(desc(peak_rt))
                #                                 } else {
                #                                         self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                                 dplyr::arrange(peak_rt)
                #                                 }
                #                                 
                #                         }
                #                 }
                #         }
                #         invisible(self)
                # },
                # # also sorts according to intensity (decreasing = TRUE) if limitNR != NA
                # peaks.filter = function(limitNr = NA, rtLimits = NA,
                #                         intensityLimit = NA, intensityPercentage = TRUE){
                #         if (!identical(rtLimits, NA)){
                #                 for (counter in 1:self$peaks$length){
                #                         self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                 dplyr::filter((peak_rt >= rtLimits[1]) & (peak_rt <= rtLimits[2]))
                #                 }
                #         }
                #         if (!identical(intensityLimit, NA)){
                #                 if (!intensityPercentage){
                #                         for (counter in 1:self$peaks$length){
                #                                 self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                         dplyr::filter(peak_intensity >= intensityLimit)
                #                         }
                #                 } else {
                #                         for (counter in 1:self$peaks$length){
                #                                 maxIntensity <- max(self$peaks$data[[counter]]$peak_intensity, na.rm = TRUE)
                #                                 self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                         dplyr::mutate(perc = peak_intensity/maxIntensity) %>%
                #                                         dplyr::filter(perc >= intensityLimit) %>%
                #                                         dplyr::select(-perc)
                #                         }
                #                         
                #                 }
                #         }
                #         if (!identical(limitNr, NA)){
                #                 if (limitNr < 1){
                #                         stop("limitNr paramerer must be >= 1")
                #                 }
                #                 for (counter in 1:self$peaks$length){
                #                         self$peaks$data[[counter]] <- self$peaks$data[[counter]] %>%
                #                                 dplyr::arrange(desc(peak_intensity)) %>%
                #                                 dplyr::slice(1:limitNr)
                #                 }
                #         }
                # },
                # peaks.fixed.edges = function(index = 1, id = NA, ...){
                #         if (!identical(id, NA)){
                #                 index <- self$peaks$indexFromId(id = id)
                #         }
                #         tempdf <- self$peaks$item(index)
                #         if (!identical(tempdf, NA)){
                #                 if (self$peaks$info$source[index] == "chromatogram"){
                #                         tempdf <- chromatogramPeaksFixedEdges(peaks = tempdf, ...)
                #                         self$peaks$data[[index]] <- tempdf
                #                 } else {
                #                         if (self$showWarnings){
                #                                 warning("Function 'peaks.fixed.edges' only works on peaks coming from chromatograms")
                #                         }
                #                 }
                #         }
                # },
                # peaks.areas = function(index = 1, id = NA, areaMethod = NA, ...){
                #         if (!identical(id, NA)){
                #                 index <- self$peaks$indexFromId(id = id)
                #         }
                #         tempPeaks <- self$peaks$item(index)
                #         if (!identical(tempPeaks, NA)){
                #                 tempTrace <- self$peaks$info[index, ]
                #                 if (tempTrace$source[1] == "chromatogram"){
                #                         tempTrace <- self$chromatograms$item(id = self$peaks$info$id[index])
                #                         if (identical(areaMethod, NA)){
                #                                 areaMethod = chromatogramFindAreas
                #                         }
                #                         tempPeaks <- areaMethod(peaks = tempPeaks,
                #                                                         trace = tempTrace,
                #                                                         ...)
                #                         self$peaks$data[[index]] <- tempPeaks
                #                 } else {
                #                         if (self$showWarnings){
                #                                 warning("Function 'peaks.areas' only works on peaks coming from chromatograms")
                #                         }
                #                 }
                #         }
                # },
                # ---- currently not inmplemented ----
                
                #' @description 
                #'  Attempts to find the file (from which a spectrum (indicated by index/id) originates)
                #'  in the files object (via the filename). If it is found, the files index is returned
                #'
                #' @param index index of the spectrum (see spectrumss$info); ignored if
                #'  id is not NA. Note: can also be a character vector specifuing a filename
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #'
                #' @return integer vector or NA
                #' @export
                spectrum.files.index = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$spectra$indexFromId(id = id)
                        }
                        if (!identical(index, NA)){
                                if (!is.character(index)){
                                        if ((index > 0) & (index <= self$spectra$length)){
                                                index <- self$spectra$info$filename[index]
                                        }
                                }
                                if (index %in% self$files$info$filename){
                                        return(which(index == self$files$info$filename))
                                }
                        }
                        return(NA)
                },
                #' @description 
                #'  returns the filename from which a spectrum (indicated via index/id) originates
                #'
                #' @param index index of the spectrum (see spectra$info); ignored if
                #'  id is not NA
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #'
                #' @return integer vector or NA
                #' @export
                spectrum.filename = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$spectra$indexFromId(id = id)
                        }
                        if ((index > 0) & (index <= nrow(self$spectra$info))){
                                return(self$spectra$info$filename[index])
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  plots one of the spectra in the object
                #'
                #' @param index index of the spectrum (see spectra$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #' @param centroidPlot if NA then it will take the stored (spectra$info object)
                #'  centroid logical vector. Otherwise should be set to TRUE/FALSE, see
                #'  plotSpectrum() function
                #' @param ... to pass on additional parameters to the plotSpectrum function
                #'
                #' @return ggplot plot of the specified spectrum
                #' @export
                spectrum.plot = function(index = 1, id = NA, centroidPlot = NA, ...){
                        if (!identical(id, NA)){
                                index <- self$spectra$indexFromId(id = id)
                        }
                        tempdf <- self$spectra$item(index)
                        if (!identical(tempdf, NA)){
                                if (identical(centroidPlot, NA)){
                                        plotSpectrum(tempdf,
                                                     centroidPlot = self$spectra$info$centroided[index],
                                                     ...)
                                } else {
                                        plotSpectrum(tempdf,
                                                     centroidPlot = centroidPlot,
                                                     ...)
                                }
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  finds peaks in a spectrum (inside the object's internal spectrum section)
                #'
                #' @param index index of the spectrum (see spectra$info) to be used for
                #'  peak detection; ignored if id is not NA
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #' @param spectrumDetectPeaksMethod function to detect peaks in the
                #'  selected spectrum. If NA then the function will attempt to use
                #'  the appropriate spectrumDetectPeaks method by looking at the
                #'  centroided column (in spectra$info) of the selected spectrum
                #' @param ... to pass on additional parameters to the
                #'  spectrumDetectPeaksMethod function
                #'
                #' @return the result from the spectrumDetectPeaksMethod or NA
                #' @export
                spectrum.findPeaks = function(index = 1, id = NA, spectrunDetectPeaksMethod = NA, ...){
                        if (!identical(id, NA)){
                                index <- self$spectra$indexFromId(id = id)
                        }
                        tempdf <- self$spectra$item(index)
                        if (!identical(tempdf, NA)){
                                if (identical(spectrunDetectPeaksMethod, NA)){
                                        if (identical(self$spectra$info$Is_Centroided[index], NA)){
                                                return(spectrumDetectPeaks(dataFrame = tempdf,
                                                                           xtraInfo = list(traceId = self$spectra$idFromIndex(index)),
                                                                           ...))
                                        } else {
                                                if (self$spectra$info$Is_Centroided[index]){
                                                        return(spectrumDetectPeaks.Centroid(xtraInfo = list(traceId = self$spectra$idFromIndex(index)), ...)(dataFrame = tempdf))
                                                } else {
                                                        return(spectrumDetectPeaks.Profile(xtraInfo = list(traceId = self$spectra$idFromIndex(index)), ...)(dataFrame = tempdf))
                                                }
                                        }
                                } else {
                                        return(spectrunDetectPeaksMethod(dataFrame = tempdf))
                                }
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  returns the TIC (total ion current = sum of all intensities) of a spectrum
                #'
                #' @param index index of the spectrum (see spectra$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #'
                #' @return numeric vector or NA
                #' @export
                spectrum.tic = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$spectra$indexFromId(id = id)
                        }
                        tempdf <- self$spectra$item(index)
                        if (!identical(tempdf, NA)){
                                return(sum(tempdf$intensity, na.rm = T))
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  returns the m/z of the base peak (most intense ion) of a spectrum
                #'
                #' @param index index of the spectrum (see spectra$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #'
                #' @return numeric vector or NA
                #' @export
                spectrum.basepeak.mz = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$spectra$indexFromId(id = id)
                        }
                        tempdf <- self$spectra$item(index)
                        if (!identical(tempdf, NA)){
                                return(tempdf$mz[which.max(tempdf$intensity)])
                        } else {
                                return(NA)
                        }
                },
                #' @description 
                #'  returns the intensity of the base peak (most intense ion) of a spectrum
                #'
                #' @param index index of the spectrum (see spectra$info) to be used.
                #'  Ignored if id is not NA
                #' @param id uses the id column of the spectra$info object to find the
                #'  correct index of the spectrum to be used
                #'
                #' @return numeric vector or NA
                #' @export
                spectrum.basepeak.intensity = function(index = 1, id = NA){
                        if (!identical(id, NA)){
                                index <- self$spectra$indexFromId(id = id)
                        }
                        tempdf <- self$spectra$item(index)
                        if (!identical(tempdf, NA)){
                                return(max(tempdf$intensity, na.rm = T))
                        } else {
                                return(NA)
                        }
                }
        ),
        active = list(
                #' @field chromatograms returns chromatograms info object
                chromatograms = function(value){
                        if (missing(value)){
                                return(private$info_[["chromatograms"]])
                        } else {
                                private$info_[["chromatograms"]] <- value
                        }
                },
                #' @field spectra returns spectra info object
                spectra = function(value){
                        if (missing(value)){
                                return(private$info_[["spectra"]])
                        } else {
                                private$info_[["spectra"]] <- value
                        }
                },
                #' @field files returns files info object
                files = function(value){
                        if (missing(value)){
                                return(private$info_[["files"]])                                
                        } else {
                                private$info_[["files"]] <- value
                        }
                },
                
                # ---- currently not inmplemented ----
                # peaks = function(value){
                #         if (missing(value)){
                #                 return(private$info_[["peaks"]])                                
                #         } else {
                #                 private$info_[["peaks"]] <- value
                #         }
                # },
                # ---- currently not inmplemented ----
                
                #' @field blobConvert get/sets the blobConvert of the different
                #'  info objects (files, chromatograms, spectra, etc). This only
                #'  works if the info object classs (private$type_) is 'db', because
                #'  only then the info objects are of the 'infodb'  type
                blobConvert = function(value){
                        if (missing(value)){
                                if (private$type_ == "db"){
                                        return(private$info_[[1]]$blobConvert)
                                } else {
                                        return(NA)
                                }
                        } else {
                                if (is.logical(value)){
                                        if ((private$type_ == "db")){
                                                for (counter in 1:self$length){
                                                        private$info_[[counter]]$blobConvert <- value
                                                }
                                        }
                                }
                        }
                }
        )
)

