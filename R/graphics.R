
# ---- General - Non R6 ----

#' Internal function to reduce number of rows to the top or bottom set.
#'  Specific for spectrum labels
#' 
#' @param spectrumLabelDF data.frame with columns: mz, intensity & label
#' @param maxIntensity if TRUE then the top 'number' of intensities will be returned.
#'  If FALSE, then the bottom 'number' of intensities will be returned
#' @param number the 'number' of top or bottom rows to be returned
#'
#' @return data.frame (same as spectrumLabelDF with top/bottom number of rows)
#' @noRd
reduceSpectrumLabelDF <- function(spectrumLabelDF, maxIntensity = TRUE, number = 1){
        if (maxIntensity){
                return(spectrumLabelDF %>% arrange(desc(intensity)) %>% slice(number))
        } else {
                return(spectrumLabelDF %>% arrange(mz) %>% slice(number))
        }
}

#' function to generate a data.frame of labels (each row is a separate label).
#'  Specific for spectrum labels
#'
#' @param dataFrame data.frame (with columns: mz, intensity, label). If this parameter
#'  is not NA, it will override mz, intensity arguments
#' @param mz numeric vector specifying the m/z's of the labels
#' @param intensity numeric vector specifying the intensity of the labels
#' @param label function that specifies the formatting of the label, example is formatDigits(4)
#'  (default)
#' @param top only show labels with top highest intensities
#' @param mzAccuracy integer vector, can be both positive and negative or NA. If NA then this
#'  is not used. Values will group together 10^mzAccuracy values and show only the highest one.
#'  Eg mzAccuracy NA: spectrumLabels(mz = c(100,100.0001)), nothing will happen to labels, 
#'  spectrumLabels(mz = c(100,101.0001), mzAccuracy = 0) nothing will happen, but with value 1
#'  only the highest intensity (or lowest m/z) will still be in data.frame after the function.
#'  In case of spectrumLabels(mz = c(100,100.0001), mzAccuracy = -4), both values will still be
#'  there, if value is 3 then only 100.0000 is there
#' @param maxIntensity if TRUE then the top 'number' of intensities will be returned.
#'  If FALSE, then the bottom 'number' of intensities will be returned
#'
#' @return data.frame (with columns: mz, intensity, label)
#' @export
spectrumLabels <- function(dataFrame = NA, mz = NA, intensity = NA,
                           label = formatDigits(4), top = NA,
                           mzAccuracy = NA, maxIntensity = TRUE){
        if (identical(mz, NA) & identical(intensity,NA) & identical(dataFrame, NA)){
                return(data.frame(mz = as.numeric(), intensity = as.numeric(), label = as.character()))
        } else {
                if (!identical(dataFrame,NA)){
                        mz = dataFrame$mz
                        intensity = dataFrame$intensity
                }
        }
        if (is.Class(label, "function")){
                if (!identical(mz, NA)){
                        dfLabels <- data.frame(mz = mz, intensity = intensity, label = label(mz))
                } else {
                        dfLabels <- data.frame(mz = mz, intensity = intensity, label = as.character(NA))
                }
        } else {
                dfLabels <- data.frame(mz = mz, intensity = intensity, label = label)
        }
        if (!is.na(mzAccuracy)){
                dfLabels <- dfLabels %>%
                        arrange(mz) %>%
                        mutate(mzG = round(mz / (10^mzAccuracy))) %>%
                        group_by(mzG) %>%
                        group_split()
                dfLabels <- map_df(dfLabels, ~reduceSpectrumLabelDF(.x, maxIntensity = maxIntensity, number = 1)) %>%
                        dplyr::select(-mzG)
        }
        if (!is.na(top)){
                dfLabels <- dfLabels %>%
                        arrange(desc(intensity)) %>%
                        slice(1:top)
        }
        return(dfLabels)
}

#' Creates a plot of a spectrum, m/z will be on the x-axis, intensity on the y-axis 
#'
#' @param spectrum a data.frame with "mz" & "intensity" columns
#' @param mzLimits two element numeric vector. Essentially the limits of the x-axis
#'  (m/z)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity) 
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the mzLimits
#'  (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param mzLabelFormat defines the format of the m/z (x) axis labels.
#'  See eg ?formatDigits. Default is ggplot2::waiver() which ensures 'standard'
#'  formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg ?formatDigits
#' @param centroidPlot logical vector that defines whether the plot should be centroid
#'  -like (geom_segment is used) or profile-like (geon_line is used)
#' @param cutOff numeric vector that defines at which intensity level, below which
#'  peaks should not be shown. Default is 0.01 which means that below 1% of maximum
#'  intensity the peaks won't be shown. Currently only works for centroid(ed) spectra.
#' @param labelFormat defines the format of the peak labels. See e.g. ?formatDigits
#' @param spectrumDetectPeaksMethod defines the spectrumDetectPeaks Method to be used.
#'  Works via function factories such as spectrumDetectPeaks.Centroid. By default
#'  uses spectrumDetectPeaks.Centroid or spectrumDetectPeaks.Profile depending on
#'  the centroidPlot argument
#' @param labels should be the result of a spectrumLabels function or NA. By default
#'  attempts to use the spectrumDetectPeaksMethod together with the labelFormat arguments. 
#' @param returnPeaks default is FALSE. If TRUE, then a list object is returned of which
#'  element 1 is the peak list (detected or provided) and element 2 is the ggplot object
#'  of the spectrum
#' @param returnAllPeaks default is FALSE. If TRUE then the whole peak list is returned.
#'  If FALSE then only the peaks visible in the spectrum plot are returned. Argument is
#'  ignored if returnPeaks is FALSE
#' @param labelCutOff numeric vector that defines at which intensity level, peak labels
#'  should not be shown. Default is 0.01 which means that below 1% of maximum intensity
#'  the labels won't be shown
#' @param labelOverlap logical vector that sets the "check_overlap" argument of geom_text()
#'  which is internally used for labels
#' @param labelSize sets the size of the label text
#' @param labelColor sets the color of the label text 
#' @param labelAngle sets the angle of the label text 
#' @param labelNudge_x determines how much the label should be shifted (nudged)
#'  horizontally
#' @param labelNudge_y determines how much the label should be shifted (nudged)
#'  vertically
#' @param annotateMz either NA or a list of annotation objects to be used
#' @param lineAlpha determines alpha of the lines used for plotting the spectrum
#' @param lineColor determines color of the lines used for plotting the spectrum
#' @param lineWidth determines width of the lines used for plotting the spectrum
#' @param lineType determines linetype of the lines used for plotting the spectrum
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector)
#' @param plot.margins.units sets the units for the margin sizes/widths
#' @param mzTitle defines the label for the x-axis (m/z)
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param spectrumTitle sets title of the spectrum
#' @param spectrumSubtitle sets subtitle of the spectrum
#' @param spectrumCaption sets caption of the spectrum
#'
#' @return a ggplot object or list of peak list + ggplot object
#' @export
plotSpectrum <- function(spectrum,
                         mzLimits = NULL, intensityLimits = NULL,
                         incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                         intensityPercentage = FALSE,
                         mzLabelFormat = ggplot2::waiver(),
                         intensityLabelFormat = ifelse(intensityPercentage,
                                                       formatDigits(0),
                                                       formatScientificDigits(4)),
                         centroidPlot = FALSE, cutOff = 0.01,
                         labelFormat = formatDigitsLargeNumbers(4),
                         spectrumDetectPeaksMethod = ifelseProper(centroidPlot,
                                                                  spectrumDetectPeaks.Centroid(),
                                                                  spectrumDetectPeaks.Profile()),
                         labels = spectrumLabels(spectrumDetectPeaksMethod(spectrum), label = labelFormat),
                         returnPeaks = FALSE, returnAllPeaks = FALSE,
                         labelCutOff = ifelse(is.na(cutOff),
                                              0.01,
                                              cutOff),
                         labelOverlap = FALSE, labelSize = 3, labelColor = "black",
                         labelAngle = 0, labelNudge_x = 0.02, labelNudge_y = 0.01,
                         annotateMz = NULL,
                         lineAlpha = 1,
                         lineColor =  "black",
                         lineWidth = 0.25,
                         lineType = "solid",
                         plot.margins.default = FALSE,
                         plot.margins = c(5,15,5,5),
                         plot.margins.units = "points",
                         mzTitle = "m/z",
                         intensityTitle = ifelse(intensityPercentage,
                                                 "Intensity (%)",
                                                 "Intensity"),
                         spectrumTitle = waiver(),
                         spectrumSubtitle = waiver(),
                         spectrumCaption = waiver()){
        if (intensityPercentage){
                yMaxStored <- max(spectrum$intensity)
                spectrum$intensity <- (spectrum$intensity/yMaxStored)*100
        }
        if (!is.null(labels)){
                if (!identical(labels, NA)){
                        if (nrow(labels) < 1){
                                labels <- NULL
                        }
                } else {
                        labels <- NULL
                }
        }
        if ((!scaleIntensityLocal) | (is.null(mzLimits))){
                maxY <- max(spectrum$intensity)
        } else {
                maxY <- max(spectrum[(spectrum$mz >= mzLimits[1]) & (spectrum$mz <= mzLimits[2]),]$intensity)
        }
        if (!is.null(mzLimits)){
                mzRangeSize <- mzLimits[2] - mzLimits[1]
        } else {
                mzRangeSize <- max(spectrum$mz) - min(spectrum$mz)
        }
        if (centroidPlot & !is.na(cutOff)){
                spectrum <- spectrum[spectrum[,2] >= (cutOff*maxY),]
        }
        maxY <- (1+incrScaleIntensity) * maxY
        if (centroidPlot) {
                g <- ggplot(spectrum, aes(x = mz, xend = mz, y = 0, yend= intensity, label = mz))
                g <- g + geom_segment(alpha = lineAlpha, color = lineColor, linewidth = lineWidth, linetype = lineType)
        } else {
                g <- ggplot(spectrum, aes(x = mz, y = intensity, label = mz))
                g <- g + geom_line(alpha = lineAlpha, color = lineColor, linewidth = lineWidth, linetype = lineType)
        }
        g <- g + xlab(mzTitle) + ylab(intensityTitle)
        if (!is.null(labels)){
                if (!is.na(labelCutOff)){
                        labels <- labels %>% dplyr::filter(intensity > (labelCutOff*maxY))
                }
                g <- g + geom_text(data = labels, aes(x = mz, y = intensity, label = label), angle = labelAngle, color = labelColor,
                                   check_overlap = labelOverlap, size = labelSize, nudge_y = labelNudge_y*maxY, nudge_x = labelNudge_x*mzRangeSize)
        }
        if (!is.null(mzLimits)){
                g <- g + scale_x_continuous(expand = c(0,0),limits = mzLimits, labels = mzLabelFormat)
        } else {
                g <- g + scale_x_continuous(labels = mzLabelFormat)
        }
        if (!is.null(intensityLimits)){
                g <- g + scale_y_continuous(expand = c(0,0), limits = intensityLimits, labels = intensityLabelFormat)
        } else {
                g <- g + scale_y_continuous(expand = c(0,0), limits = c(0,maxY), labels = intensityLabelFormat)
        }
        if (!is.null(annotateMz)){
                for (i in 1:length(annotateMz)){
                        g <- annotateMz[[i]]$draw(graphObject = g, maxY = maxY, intensityPercentage = intensityPercentage)
                }
        }
        g <- g + theme_classic()
        if (!plot.margins.default){
                g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
        }
        g <- g + labs(title = spectrumTitle, subtitle = spectrumSubtitle, caption = spectrumCaption,
                      x = mzTitle, y = intensityTitle)
        if (returnPeaks){
                if (!is.null(labels)){
                        if (!returnAllPeaks & !is.null(mzLimits)){
                                labels <- labels %>% dplyr::filter(mz >= mzLimits[1],
                                                                   mz <= mzLimits[2])
                        }
                        if (!identical(mzTitle, NA)){
                                colnames(labels)[1] <- mzTitle
                        }
                        return(list(graph = g, peaks = labels %>% dplyr::select(-label)))
                }
        }
        return(g)
}

#' Creates a plot of a list of spectra, m/z will be on the x-axis, intensity on
#'  the y-axis 
#'
#' @param spectrumList list of data.frame's (spectra)
#' @param spectrumColors vector specifying the colors of the different spectra,
#'  eg c("red","black"). Length 1 or same length as spectrumList argument
#' @param spectrumLineTypes vector specifying the linetypes of the different spectra.
#'  Length 1 or same length as spectrumList argument
#' @param spectrumAlphas vector specifying the alphas of the different spectra.
#'  Length 1 or same length as spectrumList argument
#' @param spectrumWidths vector specifying the linewidths of the different spectra.
#'  Length 1 or same length as spectrumList argument
#' @param spectrumAllowChange default is FALSE, ignores parameters 'spectrumShift'
#'  and 'spectrumScale'. If TRUE then parameters are used. Note that parameters
#'  don't do anything or are unpredictable when parameter 'intensityPercentage'
#'  is set to TRUE
#' @param spectrumShift determines with how much the spectrum intensities are to
#'  be increased: default is 0 (Length 1 or same length as spectrumList argument)
#' @param spectrumScale determines with how much the spectrum intensities are to
#'  be scaled (multiplied): default is 1 (Length 1 or same length as spectrumList
#'  argument)
#' @param mzLimits two element numeric vector. Essentially the limits of the x-axis
#'  (m/z)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity) 
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the mzLimits
#'  (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param mzLabelFormat defines the format of the m/z (x) axis labels.
#'  See eg ?formatDigits. Default is ggplot2::waiver() which ensures 'standard'
#'  formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg ?formatDigits
#' @param centroidPlot logical vector that defines whether the plot should be centroid
#'  -like (geom_segment is used) or profile-like (geon_line is used)
#' @param cutOff numeric vector that defines at which intensity level, below which
#'  peaks should not be shown. Default is 0.01 which means that below 1% of maximum
#'  intensity the peaks won't be shown. Currently only works for centroid(ed) spectra.
#' @param annotateMz either NA or a list of annotation objects to be used
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector)
#' @param plot.margins.units sets the units for the margin sizes/widths
#' @param mzTitle defines the label for the x-axis (m/z)
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param spectrumTitle sets title of the spectrum
#' @param spectrumSubtitle sets subtitle of the spectrum
#' @param spectrumCaption sets caption of the spectrum
#'
#' @return a ggplot object
#' @export
plotSpectrumOverlay <- function(spectrumList,
                                spectrumColors = "black",
                                spectrumLineTypes = "solid",
                                spectrumAlphas = 1,
                                spectrumWidths = 0.25,
                                spectrumAllowChange = F,
                                spectrumShift = 0, spectrumScale = 1,
                                mzLimits = NULL, intensityLimits = NULL,
                                incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                                intensityPercentage = FALSE,
                                mzLabelFormat = ggplot2::waiver(),
                                intensityLabelFormat = ifelse(intensityPercentage,
                                                              formatDigits(0),
                                                              formatScientificDigits(4)),
                                centroidPlot = FALSE, cutOff = 0.01,
                                annotateMz = NULL,
                                plot.margins.default = FALSE,
                                plot.margins = c(5,15,5,5),
                                plot.margins.units = "points",
                                mzTitle = "m/z",
                                intensityTitle = ifelse(intensityPercentage,
                                                        "Intensity (%)",
                                                        "Intensity"),
                                spectrumTitle = waiver(),
                                spectrumSubtitle = waiver(),
                                spectrumCaption = waiver()){
        if (length(spectrumColors) == 1){
                spectrumColors <- rep(spectrumColors, length(spectrumList))
        }
        if (length(spectrumLineTypes) == 1){
                spectrumLineTypes <- rep(spectrumLineTypes, length(spectrumList))
        }
        if (length(spectrumAlphas) == 1){
                spectrumAlphas <- rep(spectrumAlphas, length(spectrumList))
        }
        if (length(spectrumWidths) == 1){
                spectrumWidths <- rep(spectrumWidths, length(spectrumList))
        }
        if (length(centroidPlot) == 1){
          centroidPlot <- rep(centroidPlot, length(spectrumList))
        }
        if (spectrumAllowChange){
                if (length(spectrumShift) == 1){
                        spectrumShift <- rep(spectrumShift, length(spectrumList))
                }
                if (length(spectrumScale) == 1){
                        spectrumScale <- rep(spectrumScale, length(spectrumList))
                }
                for (counter in 1:length(spectrumList)){
                        spectrumList[[counter]]$intensity <- spectrumShift[counter] + (spectrumScale[counter] * spectrumList[[counter]]$intensity)
                }
        }
        yMaxStored <- max(spectrumList[[1]]$intensity)
        if (intensityPercentage){
                spectrumList[[1]]$intensity <- (spectrumList[[1]]$intensity/yMaxStored)*100
        }
        for (counter in 2:length(spectrumList)){
                yMaxStored2 <- max(spectrumList[[counter]]$intensity)
                if (intensityPercentage){
                        spectrumList[[counter]]$intensity <- (spectrumList[[counter]]$intensity/yMaxStored2)*100
                }
                yMaxStored <- max(yMaxStored, yMaxStored2)
        }
        if ((!scaleIntensityLocal) | (is.null(mzLimits))){
                maxY <- yMaxStored
        } else {
                maxY <- max(spectrumList[[1]][(spectrumList[[1]]$mz >= mzLimits[1]) & (spectrumList[[1]]$mz <= mzLimits[2]),]$intensity)
                for (counter in 2:length(spectrumList)){
                        maxY <- max(c(maxY, max(spectrumList[[counter]][(spectrumList[[counter]]$mz >= mzLimits[1]) & (spectrumList[[counter]]$mz <= mzLimits[2]),]$intensity)))
                }
        }
        if (!is.null(mzLimits)){
                minMz <- mzLimits[1]
                maxMz <- mzLimits[2]
        } else {
                maxMz <- max(spectrumList[[1]]$mz)
                minMz <- min(spectrumList[[1]]$mz)
                for (counter in 2:length(spectrumList)){
                        maxMz <- max(c(maxMz, max(spectrumList[[counter]]$mz)))
                        minMz <- min(c(minMz, min(spectrumList[[counter]]$mz)))
                }
        }
        mzRangeSize <- maxMz - minMz
        for (counter in 1:length(spectrumList)){
          if (centroidPlot[counter] & !is.na(cutOff)){
            spectrumList[[counter]] <- spectrumList[[counter]][spectrumList[[counter]][,2] >= (cutOff*maxY),]
          }
        }
        maxY <- (1+incrScaleIntensity) * maxY
        g <- ggplot()
        for (counter in 1:length(spectrumList)){
                if (centroidPlot[counter]) {
                        g <- g + geom_segment(data = spectrumList[[counter]], aes(x = mz, xend = mz, y = 0, yend = intensity),
                                              color = spectrumColors[counter],
                                              alpha = spectrumAlphas[counter],
                                              linetype = spectrumLineTypes[counter],
                                              linewidth = spectrumWidths[counter])
                } else {
                        g <- g + geom_line(data = spectrumList[[counter]], aes(x = mz, y = intensity),
                                           color = spectrumColors[counter],
                                           alpha = spectrumAlphas[counter],
                                           linetype = spectrumLineTypes[counter],
                                           linewidth = spectrumWidths[counter])
                }
        }
        if (!is.null(mzLimits)){
                g <- g + scale_x_continuous(expand = c(0,0),limits = mzLimits, labels = mzLabelFormat)
        } else {
                g <- g + scale_x_continuous(labels = mzLabelFormat)
        }
        if (!is.null(intensityLimits)){
                g <- g + scale_y_continuous(expand = c(0,0), limits = intensityLimits)
        } else {
                g <- g + scale_y_continuous(expand = c(0,0), limits = c(0,maxY))
        }
        if (!is.null(annotateMz)){
                for (i in 1:length(annotateMz)){
                        g <- annotateMz[[i]]$draw(graphObject = g, maxY = maxY, intensityPercentage = intensityPercentage)
                }
        }
        g <- g + theme_classic()
        if (!plot.margins.default){
                g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
        }
        g <- g + labs(title = spectrumTitle, subtitle = spectrumSubtitle, caption = spectrumCaption,
                      x = mzTitle, y = intensityTitle)
        return(g)
}



#' Creates a plot of a chromatogram, rt (retention time) will be on the x-axis,
#'  intensity on the y-axis 
#'
#' @param chromatogram a data.frame with "mz" & "intensity" columns
#' @param rtLimits two element numeric vector. Essentially the limits of the x-axis
#'  (rt, retention time)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity) 
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the mzLimits
#'  (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param rtLabelFormat defines the format of the rt (x) axis labels.
#'  See eg ?formatDigits. Default is ggplot2::waiver() which ensures 'standard'
#'  formatting
#' @param intensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg ?formatDigits
#' @param lineType determines linetype of the lines used for plotting the chromatogram
#' @param lineAlpha determines alpha of the lines used for plotting the chromatogram
#' @param lineColor determines color of the lines used for plotting the chromatogram
#' @param lineWidth determines linewidth of the lines used for plotting the chromatogram
#' @param rtUnits together with rtTitle argument: label for x-axis
#' @param rtTitle together with rtUnits argument: label for x-axis
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param labels currently labels is supposed to be a data.frame like coming from
#'  chromatogramFindPeaks
#' @param labelsLabel defines which column of the labels data.frame to use as label
#'  (default is 'peak_rt')
#' @param labelsLabelFormat defines the format of the labels
#' @param labelCutOffValue defines which column of the labels data.frame to use for
#'  the labelCutOff argument
#' @param labelCutOff numeric vector that defines at which intensity level, peak labels
#'  should not be shown. Default is 0.01 which means that below 1% of maximum intensity
#'  the labels won't be shown. Note: this assumes the column chosen by labelCutOffValue
#'  is intensity related
#' @param labelOverlap logical vector that sets the "check_overlap" argument of geom_text()
#'  which is internally used for labels
#' @param labelSize sets the size of the label text
#' @param labelColor sets the color of the label text 
#' @param labelAngle sets the angle of the label text 
#' @param labelNudge_x determines how much the label should be shifted (nudged)
#'  horizontally
#' @param labelNudge_y determines how much the label should be shifted (nudged)
#'  vertically
#' @param chromatogramTitle sets the title of the chromatogram
#' @param chromatogramSubtitle sets the subtitle of the chromatogram
#' @param chromatogramCaption sets the caption of the chromatogram
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector)
#' @param plot.margins.units sets the units for the margin sizes/widths
#'
#' @return a ggplot object
#' @export
plotChromatogram <- function(chromatogram,
                             rtLimits = NULL, intensityLimits = NULL,
                             incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                             intensityPercentage = FALSE,
                             rtLabelFormat = formatDigits(1), # ggplot2::waiver(),
                             intensityLabelFormat = ifelse(intensityPercentage,
                                                           formatDigits(0),
                                                           formatScientificDigits(4)),
                             lineType = "solid", lineAlpha = 1,
                             lineColor = "black", lineWidth = 0.25,
                             rtUnits = "(mins)",
                             rtTitle = paste0("rt ", rtUnits),
                             intensityTitle = ifelse(intensityPercentage,
                                                     "Intensity (%)",
                                                     "Intensity"),
                             labels = NA,
                             labelsLabel = "peak_rt",
                             labelsLabelFormat = ifelse(labelsLabel == "peak_rt",
                                                        formatDigits(2),
                                                        ifelse(labelsLabel == "area",
                                                               formatScientificDigits(4),
                                                               ifelse(labelsLabel == "peak_intensity",
                                                                      formatScientificDigits(3),
                                                                      formatDigitsWaiver))),
                             labelCutOffValue = ifelse(labelsLabel == "peak_rt",
                                                       "peak_intensity",
                                                       labelsLabel),
                             labelCutOff = 0.01,
                             labelOverlap = FALSE, labelSize = 3, labelColor = "black",
                             labelAngle = 25,
                             labelNudge_x = ifelse(labelsLabel == "peak_rt",
                                                   0.01,
                                                   0.02),
                             labelNudge_y = ifelse(labelsLabel == "peak_rt",
                                                   0.02,
                                                   0.025),
                             chromatogramTitle = waiver(),
                             chromatogramSubtitle = waiver(),
                             chromatogramCaption = waiver(),
                             plot.margins.default = FALSE,
                             plot.margins = c(5,15,5,5),
                             plot.margins.units = "points"){
        if (intensityPercentage){
                yMaxStored <- max(chromatogram$intensity)
                chromatogram$intensity <- (chromatogram$intensity/yMaxStored)*100
        }
        if ((!scaleIntensityLocal) | (is.null(rtLimits))){
                maxY <- max(chromatogram$intensity)
        } else {
                maxY <- max(chromatogram[(chromatogram$rt >= rtLimits[1]) & (chromatogram$rt <= rtLimits[2]),]$intensity)
        }
        if (!is.null(rtLimits)){
                rtRangeSize <- rtLimits[2] - rtLimits[1]
        } else {
                rtRangeSize <- max(chromatogram$rt) - min(chromatogram$rt)
        }
        maxY <- (1+incrScaleIntensity) * maxY
        if (!identical(labels, NA)){
                if (nrow(labels) < 1){
                        labels <- NA
                }
        }
        g <- ggplot(chromatogram, aes(x = rt, y = intensity))
        g <- g + geom_line(linetype = lineType, alpha = lineAlpha,
                           color = lineColor, linewidth = lineWidth)
        if (!identical(labels, NA)){
                if (!is.na(labelCutOff)){
                        if (labelCutOffValue == "peak_rt"){
                                labels <- labels %>% dplyr::filter(peak_intensity > (labelCutOff*maxY))
                        } else {
                                if (is.numeric(labels[, labelCutOffValue])){
                                        if (labelCutOff < 1){
                                                labels <- labels[labels[, labelCutOffValue] > (labelCutOff*max(labels[,labelCutOffValue])),]
                                        } else {
                                                labels <- labels[labels[, labelCutOffValue] > labelCutOff]
                                        }
                                }
                        }
                }
                if (nrow(labels)>0){
                        labels$actualLabels <- labelsLabelFormat(labels[,labelsLabel])
                        g <- g + geom_text(data = labels, aes(x = peak_rt, y = peak_intensity, label = actualLabels), angle = labelAngle,
                                           color = labelColor, check_overlap = labelOverlap, size = labelSize,
                                           nudge_y = labelNudge_y*maxY, nudge_x = labelNudge_x*rtRangeSize)
                        # if (indicateLeftRight){
                        #         g <- g + annotate("linerange",
                        #                           x = c(labels$left, labels$right),
                        #                           ymin = 0,
                        #                           ymax = indicateHeight * maxY,
                        #                           color = rep(indicateColor,nrow(labels)),
                        #                           alpha = indicateAlpha)
                        # }
                }
        }
        if (!is.null(rtLimits)){
                g <- g + scale_x_continuous(expand = c(0,0),limits = rtLimits, labels = rtLabelFormat)
        } else {
                g <- g + scale_x_continuous(labels = rtLabelFormat)
        }
        if (!is.null(intensityLimits)){
                g <- g + scale_y_continuous(expand = c(0,0), limits = intensityLimits, labels = intensityLabelFormat)
        } else {
                g <- g + scale_y_continuous(expand = c(0,0), limits = c(0,maxY), labels = intensityLabelFormat)
        }
        g <- g + labs(title = chromatogramTitle, subtitle = chromatogramSubtitle, caption = chromatogramCaption,
                      x = rtTitle, y = intensityTitle)
        g <- g + theme_classic()
        if (!plot.margins.default){
                g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
        }
        return(g)
}

#' Creates a plot of a list of chromatograms, rt (retention time) will be on the x-axis,
#'  intensity on the y-axis 
#'
#' @param chromatogramList list of data.frame's (chromatograms)
#' @param chromatogramColors vector specifying the colors of the different chromatograms,
#'  eg c("red","black"). Length 1 or same length as chromatogramList argument
#' @param chromatogramLineTypes vector specifying the linetypes of the different chromatograms.
#'  Length 1 or same length as chromatogramList argument
#' @param chromatogramAlphas vector specifying the alphas of the different chromatograms.
#'  Length 1 or same length as chromatogramList argument
#' @param chromatogramWidths vector specifying the linewidths of the different chromatograms.
#'  Length 1 or same length as chromatogramList argument
#' @param chromatogramAllowChange default is FALSE, ignores parameters 'chromatogramShift'
#'  and 'chromatogramScale'. If TRUE then parameters are used. Note that parameters
#'  don't do anything or are unpredictable when parameter 'intensityPercentage'
#'  is set to TRUE
#' @param chromatogramShift determines with how much the chromatogram intensities are to
#'  be increased: default is 0 (Length 1 or same length as chromatogramList argument)
#' @param chromatogramScale determines with how much the chromatogram intensities are to
#'  be scaled (multiplied): default is 1 (Length 1 or same length as chromatogramList
#'  argument)
#' @param rtLimits two element numeric vector. Essentially the limits of the x-axis
#'  (rt, retention time)
#' @param intensityLimits two element numeric vector. Essentially the limits of the
#'  y-axis (intensity) 
#' @param incrScaleIntensity numeric value which specifies the factor with which to
#'  increase the y-axis limit
#' @param scaleIntensityLocal logical vector which determines whether to use global
#'  intensities (FALSE) or only the intensities of m/z's which fall within the rtLimits
#'  (TRUE, default)
#' @param intensityPercentage Whether to display the intensity axis in percentages
#'  (default FALSE)
#' @param rtLabelFormat defines the format of the rt (x) axis labels.
#'  See eg ?formatDigits. Default is ggplot2::waiver() which ensures 'standard'
#'  formatting
#' @param IntensityLabelFormat defines the format of the intensity (y) axis labels.
#'  See eg ?formatDigits
#' @param rtUnits together with rtTitle argument: label for x-axis
#' @param rtTitle together with rtUnits argument: label for x-axis
#' @param intensityTitle defines the label for y-axis (intensity)
#' @param chromatogramTitle sets the title of the chromatogram
#' @param chromatogramSubtitle sets the subtitle of the chromatogram
#' @param chromatogramCaption sets the caption of the chromatogram
#' @param plot.margins.default sets whether default ggplot margins are to be used
#'  (default = FALSE)
#' @param plot.margins sets the plot margin sizes/widths (4 element integer vector)
#' @param plot.margins.units sets the units for the margin sizes/widths
#'
#' @return a ggplot object
#' @export
plotChromatogramOverlay <- function(chromatogramList,
                                    chromatogramColors = "black",
                                    chromatogramLineTypes = "solid",
                                    chromatogramAlphas = 1,
                                    chromatogramWidths = 0.25,
                                    chromatogramAllowChange = F,
                                    chromatogramShift = 0, chromatogramScale = 1,
                                    rtLimits = NULL, intensityLimits = NULL,
                                    incrScaleIntensity = 0.05, scaleIntensityLocal = TRUE,
                                    intensityPercentage = FALSE,
                                    rtLabelFormat = formatDigits(1), # ggplot2::waiver(),
                                    intensityLabelFormat = ifelse(intensityPercentage,
                                                                  formatDigits(0),
                                                                  formatScientificDigits(4)),
                                    rtUnits = "(mins)",
                                    rtTitle = paste0("rt ", rtUnits),
                                    intensityTitle = ifelse(intensityPercentage,
                                                            "Intensity (%)",
                                                            "Intensity"),
                                    chromatogramTitle = waiver(),
                                    chromatogramSubtitle = waiver(),
                                    chromatogramCaption = waiver(),
                                    plot.margins.default = FALSE,
                                    plot.margins = c(5,15,5,5),
                                    plot.margins.units = "points"){
        if (length(chromatogramColors) == 1){
                chromatogramColors <- rep(chromatogramColors, length(chromatogramList))
        }
        if (length(chromatogramLineTypes) == 1){
                chromatogramLineTypes <- rep(chromatogramLineTypes, length(chromatogramList))
        }
        if (length(chromatogramAlphas) == 1){
                chromatogramAlphas <- rep(chromatogramAlphas, length(chromatogramList))
        }
        if (length(chromatogramWidths) == 1){
                chromatogramWidths <- rep(chromatogramWidths, length(chromatogramList))
        }
        if (chromatogramAllowChange){
                if (length(chromatogramShift) == 1){
                        chromatogramShift <- rep(chromatogramShift, length(chromatogramList))
                }
                if (length(chromatogramScale) == 1){
                        chromatogramScale <- rep(chromatogramScale, length(chromatogramList))
                }
                for (counter in 1:length(chromatogramList)){
                        chromatogramList[[counter]]$intensity <- chromatogramShift[counter] + (chromatogramScale[counter] * chromatogramList[[counter]]$intensity)
                }
        }
        yMaxStored <- max(chromatogramList[[1]]$intensity)
        if (intensityPercentage){
                chromatogramList[[1]]$intensity <- (chromatogramList[[1]]$intensity/yMaxStored)*100
        }
        for (counter in 2:length(chromatogramList)){
                yMaxStored2 <- max(chromatogramList[[counter]]$intensity)
                if (intensityPercentage){
                        chromatogramList[[counter]]$intensity <- (chromatogramList[[counter]]$intensity/yMaxStored2)*100
                }
                yMaxStored <- max(yMaxStored, yMaxStored2)
        }
        if ((!scaleIntensityLocal) | (is.null(rtLimits))){
                maxY <- yMaxStored
        } else {
                maxY <- max(chromatogramList[[1]][(chromatogramList[[1]]$rt >= rtLimits[1]) & (chromatogramList[[1]]$rt <= rtLimits[2]),]$intensity)
                
                for (counter in 2:length(chromatogramList)){
                        maxY <- max(c(maxY, max(chromatogramList[[counter]][(chromatogramList[[counter]]$rt >= rtLimits[1]) & (chromatogramList[[counter]]$rt <= rtLimits[2]),]$intensity)))
                }
        }
        if (!is.null(rtLimits)){
                minRt <- rtLimits[1]
                maxRt <- rtLimits[2]
        } else {
                maxRt <- max(chromatogramList[[1]]$rt)
                minRt <- min(chromatogramList[[1]]$rt)
                for (counter in 2:length(chromatogramList)){
                        maxRt <- max(c(maxRt, max(chromatogramList[[counter]]$rt)))
                        minRt <- min(c(minRt, min(chromatogramList[[counter]]$rt)))
                }
        }
        rtRangeSize <- maxRt - minRt
        
        maxY <- (1+incrScaleIntensity) * maxY
        
        g <- ggplot()
        for (counter in 1:length(chromatogramList)){
                g <- g + geom_line(data = chromatogramList[[counter]], aes(x = rt, y = intensity),
                                   color = chromatogramColors[counter],
                                   alpha = chromatogramAlphas[counter],
                                   linetype = chromatogramLineTypes[counter],
                                   linewidth = chromatogramWidths[counter])
        }
        
        if (!is.null(rtLimits)){
                g <- g + scale_x_continuous(expand = c(0,0),limits = rtLimits, labels = rtLabelFormat)
        } else {
                g <- g + scale_x_continuous(labels = rtLabelFormat)
        }
        if (!is.null(intensityLimits)){
                g <- g + scale_y_continuous(expand = c(0,0), limits = intensityLimits, labels = intensityLabelFormat)
        } else {
                g <- g + scale_y_continuous(expand = c(0,0), limits = c(0,maxY), labels = intensityLabelFormat)
        }
        g <- g + labs(title = chromatogramTitle, subtitle = chromatogramSubtitle, caption = chromatogramCaption,
                      x = rtTitle, y = intensityTitle)
        g <- g + theme_classic()
        if (!plot.margins.default){
                g <- g + ggplot2::theme(plot.margin = ggplot2::unit(plot.margins, plot.margins.units))
        }
        return(g)
}
