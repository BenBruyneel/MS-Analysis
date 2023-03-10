
#' function to generate a series of labels for ions
#' 
#' @note all arguments, except where indicated, are character vectors
#' 
#' @param whichSeries letter or name assigned to the series, eg y for y-ions,
#'  b for b-ions, etc
#' @param theSeries integer vector, eg 1:10 to specify the numbers of the ion series
#' @param whichCharge integer vector, gives the charge state (1,2,3,...)
#' @param positive if TRUE than positive ions series is generated, else negative
#'  ion series
#' @param chargeConnectorPre character vector placed between the charge and the chargeConnector
#' @param chargeConnector character vector to connect the name + charge to the ion series number
#' @param chargeConnectorPost character vector placed between the chargeConnector and the ion series number
#' @param seriesConnectorPre character vector placed just before the series number
#' @param seriesConnectorPost character vector placed just after the series number 
#'
#' @return a character vector or NA
#' @export
createIonSeries <- function(whichSeries = "y",  theSeries = NA, whichCharge = 1, positive = TRUE,
                            chargeConnectorPre = "", chargeConnector = "", chargeConnectorPost = "",
                            seriesConnectorPre = "_", seriesConnectorPost = ""){
        if (!identical(theSeries,NA)){
                whichCharge <- ifelse(whichCharge == 1,
                                      ifelse(is.na(positive),
                                             "",
                                             ifelse(positive,
                                                    "+","-")),
                                      paste(c(as.character(whichCharge), chargeConnector, ifelse(is.na(positive),
                                                                                                 "",
                                                                                                 ifelse(positive,
                                                                                                        "+","-"))), collapse = ""))
                return(unlist(lapply(theSeries,function(x){paste(c(whichSeries,
                                                                   chargeConnectorPre, whichCharge, chargeConnectorPost,
                                                                   seriesConnectorPre, as.character(x), seriesConnectorPost), collapse = "")})))
        } else {
                return(NA)
        }
}

#' origin: developed for spectrum read info thermo
#'
#' @param elementList 
#' @param removeLarger 
#' @param collapseCharacter 
#' @param namesRemoveCharacters 
#' @param namesReplaceCharacters 
#'
#' @return
#' @export
#'
#' @examples
multiToSingleElement <- function(elementList, removeLarger = 10, collapseCharacter = "-",
                                 namesRemoveCharacters = c(":","\\.","\\(","\\)"),
                                 namesReplaceCharacters = data.frame(pattern = c("[:space:]+",
                                                                                 "\001"),
                                                                     replacement = c("_",
                                                                                     "Unk"))){
        elementLengths <- unname(map_int(elementList, ~length(.x)))
        # remove too large elements
        if (!is.na(removeLarger)){
                elementList <- elementList[elementLengths < removeLarger]
                elementLengths <- elementLengths[elementLengths < removeLarger]
        }
        if (!identical(namesRemoveCharacters, NA)){
                names(elementList) <- stringr::str_replace_all(names(elementList),
                                                               pattern = ifelse(length(namesRemoveCharacters) == 1,
                                                                                namesRemoveCharacters,
                                                                                paste(namesRemoveCharacters, collapse = "|")), 
                                                               replacement = "")
        }
        if (!identical(namesReplaceCharacters, NA)){
                for (counter in 1:nrow(namesReplaceCharacters)){
                        names(elementList) <- stringr::str_replace_all(names(elementList),
                                                                       pattern = namesReplaceCharacters$pattern[counter],
                                                                       replacement = namesReplaceCharacters$replacement[counter])
                }
        }
        if (!is.na(collapseCharacter)){
                elementList <- map2(elementList, elementLengths, ~ifelse(.y == 1,
                                                                         .x,
                                                                         paste(.x, collapse = collapseCharacter)))
        }
        return(elementList)
}

#' function to (several) combine spectra into one
#' 
#' @note experimental function
#' 
#' @note centroided plots gives problems with this function, use profile when possible
#' 
#' @param spectrum list of data.frame's containing at least 2 columns: mz (m/z) and intensity
#' @param centroidedSpectrum logical vector. Specifies if the spectra are centroided or profile.
#'  Currently this argument is only used to (automatically) set the mzAccuracy argument
#' @param intensityCombination character vector specfiying whether to average the spectra or 
#'  to add them. Only valid values: 'add' or 'average'
#' @param mzWeighted logical vector which determines whether combined m/z values are calculated
#'  in a weighted manner (which means that m/z is more influenced by higher intensity values)
#' @param intensityZeroCorrection numeric value. If the intensity is below this value. It will
#'  be set to zero. This exists to prevent negative intensity values to occur/have influence
#' @mzAccuracy value that determines how close two m/z values must be to be added or averaged
#'  1 = 10^1, 2 = 10^2, -1 = 10^-1 etc etc. Does not have to be an integer!
combineSpectra <- function(spectrum,
                           centroidedSpectrum = FALSE,
                           intensityCombination = "add",
                           mzWeighted = TRUE,
                           intensityZeroCorrection = -20,
                           mzAccuracy = ifelse(centroidedSpectrum,2,4)){
        if (!(intensityCombination %in% c("add","average"))){
                stop("combineSpectra argument intensityCombination should be 'add' or 'average'" )
        }
        if (intensityCombination == "add"){
                if (mzWeighted){
                        return(bind_rows(spectrum) %>%
                                       arrange(mz, desc(intensity)) %>%
                                       mutate(mzG = round(mz * (10^mzAccuracy)),
                                              intensity = intensity + 10^(intensityZeroCorrection)) %>%
                                       group_by(mzG) %>%
                                       summarize(mz = weighted.mean(x = mz, w = intensity),
                                                 intensity = sum(intensity)) %>%
                                       mutate(intensity = ifelse(intensity <= 10^(intensityZeroCorrection+1),
                                                                 0,
                                                                 intensity)) %>%
                                       dplyr::select(mz, intensity) %>%
                                       dplyr::arrange(mz))
                } else {
                        return(bind_rows(spectrum) %>%
                                       arrange(mz, desc(intensity)) %>%
                                       mutate(mzG = round(mz * (10^mzAccuracy))) %>%
                                       group_by(mzG) %>%
                                       summarize(mz = mean(mz),
                                                 intensity = sum(intensity)) %>%
                                       dplyr::select(mz, intensity) %>%
                                       dplyr::arrange(mz))
                }
        } else {
                if (mzWeighted){
                        return(bind_rows(spectrum) %>%
                                       arrange(mz, desc(intensity)) %>%
                                       mutate(mzG = round(mz * (10^mzAccuracy)),
                                              intensity = intensity + 10^(intensityZeroCorrection)) %>%
                                       group_by(mzG) %>%
                                       summarize(mz = weighted.mean(x = mz, w = intensity),
                                                 intensity = mean(intensity)) %>%
                                       mutate(intensity = ifelse(intensity <= 10^(intensityZeroCorrection+1),
                                                                 0,
                                                                 intensity)) %>%
                                       dplyr::select(mz, intensity) %>%
                                       dplyr::arrange(mz))
                } else {
                        return(bind_rows(spectrum) %>%
                                       arrange(mz, desc(intensity)) %>%
                                       mutate(mzG = round(mz * (10^mzAccuracy))) %>%
                                       group_by(mzG) %>%
                                       summarize(mz = mean(mz),
                                                 intensity = mean(intensity)) %>%
                                       dplyr::select(mz, intensity) %>%
                                       dplyr::arrange(mz))
                }
        }
}
