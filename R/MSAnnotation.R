library(jsonlite)
library(jsonify)

# ---- annotation ----

#' R6 Class base for dealing with annotations in graphs 
#'  
#' @note still somewhat experimental
#' 
#' @description 
#'  R6 Class base for dealing with annotations in graphs, originally intended for
#'  semi-automatic peptide spectrum annotation
#'   
#' @export 
annotation <- R6Class("annotation",
                      private = list(
                              name_ = NA,
                              # internal variable for storing the (optional) name
                              #  for the annotation object
                              
                              #' @description internal function to get all the (public) variables
                              #'  into a list (for saving purposes)
                              #'  
                              #' @return a named list
                              getList = function(){
                                      return(list(
                                              nameX = self$nameX,
                                              nameY = self$nameY,
                                              x = self$x,
                                              labelX = self$labelX,
                                              labelWhere = self$labelWhere,
                                              labelColor = self$labelColor,
                                              labelSize = self$labelSize,
                                              labelAngle = self$labelAngle,
                                              labelConnect = self$labelConnect,
                                              labelConnectColor = self$labelConnectColor,
                                              labelConnectAlpha = self$labelConnectAlpha,
                                              axisConnect = self$axisConnect,
                                              axisConnectColor = self$axisConnectColor,
                                              axisConnectAlpha = self$axisConnectAlpha,
                                              axisConnectWidth = self$axisConnectWidth,
                                              axisConnectLevel = self$axisConnectLevel,
                                              axisConnectWhere = self$axisConnectWhere,
                                              axisConnectType = self$axisConnectType))
                              },
                              #' @description internal function to set all the (public) variables
                              #'  from a named list (for loading purposes)
                              setList = function(theList = NA){
                                      if (!identical(theList, NA)){
                                              self$nameX <- theList$nameX
                                              self$nameY <- theList$nameY
                                              self$x <- theList$x
                                              self$labelX <- theList$labelX
                                              self$labelWhere <- theList$labelWhere
                                              self$labelColor <- theList$labelColor
                                              self$labelSize <- theList$labelSize
                                              self$labelAngle <- theList$labelAngle
                                              self$labelConnect <- theList$labelConnect
                                              self$labelConnectColor <- theList$labelConnectColor
                                              self$labelConnectAlpha <- theList$labelConnectAlpha
                                              self$axisConnect <- theList$axisConnect
                                              self$axisConnectColor <- theList$axisConnectColor
                                              self$axisConnectAlpha <- theList$axisConnectAlpha
                                              self$axisConnectWidth <- theList$axisConnectWidth
                                              self$axisConnectLevel <- theList$axisConnectLevel
                                              self$axisConnectWhere <- theList$axisConnectWhere
                                              self$axisConnectType <- theList$axisConnectType
                                      }
                              }
                      ),
                      public = list(
                              nameX = NA,
                              # character vector specifying the name of the x-axis variable (eg 'mz')
                              nameY = NA,
                              # character vector specifying the name of the y-axis variable (eg 'intensity')
                              x = NA,
                              # numeric vector of the x-values used for the annotation
                              labelX = NA,
                              # character vector of the labels to be used for the annotation
                              labelWhere = 0.9,
                              # numeric vector: position of the labels, 1 = maximum y-axis
                              labelColor = "red",
                              # color of the label
                              labelSize = 3,
                              # size of the label
                              labelAngle = 30,
                              # angle (degrees) of display for the label
                              labelConnect = TRUE,
                              # logical vector that specifies whether the labels should be
                              # 'connected' with a line between them
                              labelConnectColor = "red",
                              # color of the label connect line(s)
                              labelConnectAlpha = 0.25,
                              # alpha of the label connect line(s)
                              axisConnect = TRUE,
                              # logical vector that specifies whether the label should be
                              # (vertically) connected to the (x-)axis by a line
                              axisConnectColor = "red",
                              # color of axis connector line
                              axisConnectAlpha = 0.25,
                              # alpha of the axis connector line
                              axisConnectWidth = 1,
                              # linewidth of the axis connector line
                              axisConnectLevel = NA, 
                              # NA means connect to axis, if not NA then axisConnectType can be set
                              # axisConnectLevel defines to where (how far down) the axisConnect lines should reach
                              axisConnectWhere = 0.05, 
                              # this is how high above the connect, is relative to max Y-axis. To place labels close to their peaks,
                              # this option should only be set/changed by annotation field labelType     
                              axisConnectType = 1,      
                              # 1 means all the way down
                              # if axisConnectLevel
                              # 2 means up till axisConnectLevel above int of m/z
                              # 3 overrides all axisConnect Settings, labels put at axisConnectLevel above int of m/z and axisConnect lines
                              # are removed (2 & 3 only really work for 'checked') annotations. Since the axisConnectType, axisConnectLevel
                              # and labelConnect 'work' together: always use the object's labelType field to change axisConnectType since
                              # it will 'take care' of all settings together. AxisConnectLevel is also influence by the (public) check
                              # method (it attempts to place set the values above the m/z's found)
                              
                              #' @description Initializes an annotation object
                              #'
                              #' @param annotationName set the internal name of the object
                              #' @param nameX sets the name of the x-axis variable (eg m/z or retention time)
                              #' @param nameY sets the name of the y-axis variable (eg intensity)
                              #' @param x x-values (location on x-axis) for the labels
                              #' @param labelX the labels to be used for the annotation. Should have same length
                              #'  as argument 'x'. If NA, then it will be as.character(x)
                              #' @param labelWhere y-axis height where axis labels are to be placed. Expressed as
                              #'  a fraction of the y-axis maximum (default 0.9)
                              #' @param labelColor sets the color of the labels
                              #' @param labelSize sets the size of the labels
                              #' @param labelAngle sets the angle of display of the labels
                              #' @param labelConnect logical vector which defines if the labels are connected
                              #'  horizontally ('along' the x-axis)
                              #' @param labelConnectColor sets the color of the connection lines between the labels
                              #' @param labelConnectAlpha sets the alpha of the connection lines between the labels
                              #' @param axisConnect specifies if the labels should have axis connect lines
                              #' @param axisConnectColor sets the color of the axis connector line(s) 
                              #' @param axisConnectAlpha sets the alpha of the axis connector line(s)
                              #' @param axisConnectWidth sets the width of the axis connector line(s)
                              #' @param axisConnectLevel defines to where (how far down) the axisConnect lines should reach
                              #' @param axisConnectWhere this is how high above the connect, is relative to max Y-axis.
                              #'  To place labels close to their peaks, this option should only be set/changed by
                              #'  annotation field labelType      
                              #' @param axisConnectType at this moment can be 1,2 or 3. 1 means connect all the way down,
                              #'  2 means up till axisConnectLevel above int of m/z. 3 = overrides all axisConnect Settings,
                              #'  labels put at axisConnectLevel above int of m/z and axisConnect lines
                              #'
                              #' @return a new annotation object
                              initialize = function(annotationName = NA,
                                                    nameX = NA, nameY = NA,
                                                    x = NA, labelX = NA,
                                                    labelWhere = 0.9, labelColor = "red",
                                                    labelSize = 3, labelAngle = 30,
                                                    labelConnect = TRUE, labelConnectColor = "red", labelConnectAlpha = 0.25,
                                                    axisConnect = TRUE, axisConnectColor = "red", axisConnectAlpha = 0.25,
                                                    axisConnectWidth = 0.5, axisConnectLevel = NA, axisConnectWhere = 0.05, axisConnectType = 1){
                                      if (!identical(x,NA) & !identical(nameX, NA) & !identical(nameY,NA)){
                                              if (length(labelWhere) == 1){
                                                      labelWhere <- rep(labelWhere, length(x))
                                              } else {
                                                      stopifnot(length(labelWhere) == length(x))                        
                                              }
                                              if (identical(labels, NA)){
                                                      labels = as.character(x)
                                              }
                                              private$name_ <- annotationName
                                              self$nameX <- nameX
                                              self$nameY <- nameY
                                              self$x <- x
                                              self$labelX <- labelX
                                              self$labelWhere <- labelWhere
                                              self$labelColor <- labelColor
                                              self$labelSize <- labelSize
                                              self$labelAngle <- labelAngle
                                              self$labelConnect <- labelConnect
                                              self$labelConnectColor <- labelConnectColor
                                              self$labelConnectAlpha <- labelConnectAlpha
                                              self$axisConnect <- axisConnect
                                              self$axisConnectColor <- axisConnectColor
                                              self$axisConnectAlpha <- axisConnectAlpha
                                              self$axisConnectWidth <- axisConnectWidth
                                              self$axisConnectLevel <- axisConnectLevel
                                              self$axisConnectWhere <- axisConnectWhere
                                              self$axisConnectType <- axisConnectType
                                      } else {
                                              # do nothing, leave at standard values
                                      }
                                      invisible()
                              },
                              #' @description
                              #' For printing purposes: prints the name of the annotation (if not NA)
                              #'  and a table of the labels
                              #' 
                              #' @note no arguments, the function takes care of printing
                              print = function(){
                                      if (!is.na(self$name)){
                                              cat(paste(c("Annotation: ",self$name,"\n"), collapse = ""))
                                      }
                                      print(self$table)
                              },
                              
                              #' @description 
                              #'  saves the annotation object to a json file
                              #'
                              #' @param filename name of the file
                              #' @param overwrite whether to overwrite existing files or not
                              saveToFile = function(filename = NA, overwrite = TRUE){
                                      if (identical(filename, NA)){
                                              if (identical(private$name_, na)){
                                                      invisible()
                                              }
                                      } else {
                                              private$name_ <- filename
                                      }
                                      if (!overwrite){
                                              if (file.exists(private$name_)){
                                                      invisible()
                                              }
                                      }
                                      jsonlite::write_json(private$getList(), path = private$name_)
                                      invisible()
                              },
                              #' @description 
                              #'  loads the annotation object from a json file
                              #'
                              #' @param filename name of the file
                              loadFromFile = function(filename = NA){
                                      if (identical(filename, NA)){
                                              if (identical(private$name_, na)){
                                                      invisible()
                                              }
                                      } else {
                                              private$name_ <- filename
                                      }
                                      private$setList(jsonlite::read_json(path = private$name_, simplifyVector = TRUE))
                                      invisible()
                              },
                              
                              #' @description
                              #'  saves the annotation object to a table in a database 
                              #'
                              #' @param db database handle, preferable via pool library.
                              #'  Note: database must be open when function is called
                              #' @param tableName name of the table
                              #' @param overwrite if table already exist, overwrite?
                              #' @param dbType currently only "SQLite" has been tested
                              saveToDB = function(db, tableName = NA, overwrite = TRUE, dbType = "SQLite"){
                                      if (identical(tableName, NA)){
                                              if (identical(private$name_, NA)){
                                                      invisible()
                                              }
                                      } else {
                                              private$name_ <- tableName
                                      }
                                      tablesPresent <- pool::dbListTables(db)
                                      if (length(tablesPresent)>0){
                                              if (private$name_ %in% tablesPresent){
                                                      if (!overwrite){
                                                              # abort save action
                                                              warning("Table already exists & overwrite is FALSE")
                                                              invisible()
                                                      } else {
                                                              db_deleteTable(db, private$name_)
                                                      }
                                              }
                                      }
                                      db_createTable(db = db,
                                                     tableName = private$name_,
                                                     dataframe = data.frame(data =  vectorToBlob(as.character(toJSON(private$getList())))),
                                                     addPrimary = FALSE,
                                                     dbType = dbType)
                                      invisible()
                              },
                              #' @description
                              #'  loads the annotation object from a table in a database
                              #'
                              #' @param db database handle, preferable via pool library.
                              #'  Note: database must be open when function is called
                              #' @param tableName name of the table
                              loadFromDB = function(db, tableName = NA){
                                      if (identical(tableName, NA)){
                                              if (identical(private$name_, NA)){
                                                      invisible()
                                              }
                                      } else {
                                              private$name_ <- tableName
                                      }
                                      tablesPresent <- pool::dbListTables(db)
                                      if (length(tablesPresent)>0){
                                              if (private$name_ %in% tablesPresent){
                                                      tempdf <- db_getTable(db = db,
                                                                            tableName = private$name_)
                                                      private$setList(theList = jsonlite::fromJSON(jsonify::as.json(blobToVector(tempdf$data[1], vectorClass = "character"))))
                                              }
                                      }
                                      invisible()
                              },
                              
                              #' @description
                              #'  checks whether the x-axis coordinates have 'sufficient' y-value (intensity) labels
                              #'  if not, then the label is dropped from the object
                              #'
                              #' @param dataFrame data.frame with at least the x-column (nameX) and y-column (nameY)
                              #' @param toleranceLow x-axis left tolerance (m/z)
                              #' @param toleranceHigh x-axis right tolerance (m/z)
                              #' @param relativeCutOff is the yCutOff reletive (fraction of maximum y-value (intensity))
                              #' @param yCutOff y-axis cut off y-axis value (intensity) should be above
                              check = function(dataFrame = NULL,
                                               toleranceLow = 0.1, toleranceHigh = toleranceLow,
                                               relativeCutOff = TRUE, yCutOff = ifelse(relativeCutOff, 0.001, 10)){
                                      if (!is.null(dataFrame)){
                                              dataFrame <- as.data.frame(dataFrame)
                                              if (relativeCutOff){
                                                      yCutOff <- yCutOff * max(dataFrame[,self$nameY], na.rm = TRUE)
                                              }
                                              if (yCutOff > 0 ){
                                                      dataFrame <- dataFrame[dataFrame[,self$nameY] >= yCutOff,]
                                              }
                                              remove <- rep(FALSE, length(self$x))
                                              stayY <- rep(0,length(self$x))
                                              for (cntr in 1:length(self$x)){
                                                      tempY <- dataFrame[dataFrame[, self$nameX] >= (self$x[cntr] - toleranceLow) &
                                                                                 dataFrame[, self$nameX] <= (self$x[cntr] + toleranceHigh), self$nameY]
                                                      if (length(tempY)<1){
                                                              remove[cntr] <- TRUE
                                                      } else {
                                                              maxY <- max(tempY, na.rm = TRUE)
                                                              if (maxY < yCutOff){
                                                                      remove[cntr] <- TRUE 
                                                              } else {
                                                                      stayY[cntr] <- maxY
                                                              }
                                                      }
                                              }
                                              # remove non-detected labels
                                              self$x <- self$x[-which(remove)]
                                              self$labelMz <- self$labelMz[-which(remove)]
                                              self$labelWhere <- self$labelWhere[-which(remove)]
                                              if (length(self$labelColor)>1){
                                                      self$labelColor <- self$labelColor[-which(remove)]
                                              }
                                              # add intensity levels for detected ones
                                              self$axisConnectLevel <- stayY[-which(remove)]
                                      }
                                      invisible()
                              }
                      ),
                      active = list(
                              #' @field name provides access to the name of the object
                              name = function(value){
                                      if (missing(value)){
                                              return(private$name_)
                                      } else {
                                              private$name_ <- value
                                      }
                              },
                              #' @field table provides a data.frame of the x-axis values and labels
                              #'  (read only)
                              table = function(value){
                                      if (missing(value)){
                                              return(data.frame(mz = self$x,
                                                                label = self$labelX))
                                      } else {
                                              # do nothing
                                      }
                              },
                              #' @field color returns the axisConnectColor, labelColor and labelConnectColor as vector
                              #'  can be set as single value or 3 element vector
                              color = function(value){
                                      if (missing(value)){
                                              if ((self$axisConnectColor == self$labelColor) & (self$axisConnectColor == self$labelConnectColor)){
                                                      return(self$axisConnectColor)
                                              } else {
                                                      return(c(self$axisConnectColor,
                                                               self$labelColor,
                                                               self$labelConnectColor))
                                              }
                                      } else {
                                              if (length(value) == 1){
                                                      self$axisConnectColor <- value
                                                      self$labelColor <- value
                                                      self$labelConnectColor <- value
                                              } else {
                                                      self$axisConnectColor <- value[1]
                                                      self$labelColor <- value[2]
                                                      self$labelConnectColor <- value[3]
                                              }
                                      }
                              },
                              #' @field labelType gets/sets the axisConnectType
                              #'  When set it takes care of axisConnect & labelConnect at the same time
                              labelType = function(value){
                                      if (missing(value)){
                                              return(self$axisConnectType)
                                      } else {
                                              if (value > 1){
                                                      if (!identical(self$axisConnectLevel, NA)){
                                                              self$axisConnectType <- value
                                                              if (value > 2){
                                                                      self$axisConnect <- FALSE
                                                                      self$labelConnect <- FALSE
                                                              } else {
                                                                      self$axisConnect <- TRUE
                                                                      self$labelConnect <- TRUE
                                                              }
                                                      } else {
                                                              warning("No axis connect level values, axis connect type was not changed.")
                                                      }
                                              } else {
                                                      self$axisConnectType <- value
                                                      self$axisConnect <- TRUE
                                                      self$labelConnect <- TRUE
                                              }
                                      }
                              }
                      )
)

# ---- Spectrum Annotation ----

#' R6 Class base for mass specrometrum annotation
#' 
#' @description 
#'  R6 Class dealing with mass spectrometry annotation data in an organised manner.
#'   This class is a descendant of annotation. Main feature is the draw() method
#'   
#' @export
spectrumAnnotation <- R6Class("spectrumAnnotation",
                              inherit = annotation,
                              public = list(
                                      #' @description Initializes an spectrum annotation object
                                      #'
                                      #' @param annotationName set the internal name of the object
                                      #' @param nameX sets the name of the x-axis variable (eg m/z or retention time)
                                      #' @param nameY sets the name of the y-axis variable (eg intensity)
                                      #' @param mzs m/z-values (location on x-axis) for the labels
                                      #' @param labels the labels to be used for the annotation. Should have same length
                                      #'  as argument mzs. If NA, then it will be as.character(mzs)
                                      #' @param labelWhere y-axis height where axis labels are to be placed. Expressed as
                                      #'  a fraction of the y-axis maximum (default 0.9)
                                      #' @param labelColor sets the color of the labels
                                      #' @param labelSize sets the size of the labels
                                      #' @param labelAngle sets the angle of display of the labels
                                      #' @param labelConnect logical vector which defines if the labels are connected
                                      #'  horizontally ('along' the x-axis)
                                      #' @param labelConnectColor sets the color of the connection lines between the labels
                                      #' @param labelConnectAlpha sets the alpha of the connection lines between the labels
                                      #' @param axisConnect specifies if the labels should have axis connect lines
                                      #' @param axisConnectColor sets the color of the axis connector line(s) 
                                      #' @param axisConnectAlpha sets the alpha of the axis connector line(s)
                                      #' @param axisConnectWidth sets the width of the axis connector line(s)
                                      #' @param axisConnectLevel defines to where (how far down) the axisConnect lines should reach
                                      #' @param axisConnectWhere this is how high above the connect, is relative to max Y-axis.
                                      #'  To place labels close to their peaks, this option should only be set/changed by
                                      #'  annotation field labelType      
                                      #' @param axisConnectType at this moment can be 1,2 or 3. 1 means connect all the way down,
                                      #'  2 means up till axisConnectLevel above int of m/z. 3 = overrides all axisConnect Settings,
                                      #'  labels put at axisConnectLevel above int of m/z and axisConnect lines
                                      #'
                                      #' @return a new spectrum annotation object
                                      initialize = function(annotationName = NA,
                                                            nameX = "mz", nameY = "intensity",
                                                            mzs = NA, labels = NA,
                                                            levelWhere = 0.9, labelColor = "red",
                                                            labelSize= 3, labelAngle = 30,
                                                            labelConnect = TRUE, labelConnectColor = "red", labelConnectAlpha = 0.25,
                                                            axisConnect = TRUE, axisConnectColor = "red", axisConnectAlpha = 0.25,
                                                            axisConnectWidth = 0.5, axisConnectLevel = NA, axisConnectWhere = 0.05, axisConnectType = 1){
                                              super$initialize(annotationName = annotationName,
                                                               nameX = nameX, nameY = nameY,
                                                               x = mzs, labelX = labels,
                                                               labelWhere = levelWhere, labelColor = labelColor,
                                                               labelSize = labelSize, labelAngle = labelAngle,
                                                               labelConnect = labelConnect, labelConnectColor = labelConnectColor, labelConnectAlpha = labelConnectAlpha,
                                                               axisConnect = axisConnect, axisConnectColor = axisConnectColor, axisConnectAlpha = axisConnectAlpha,
                                                               axisConnectWidth = axisConnectWidth, axisConnectLevel = axisConnectLevel, axisConnectWhere = axisConnectWhere,
                                                               axisConnectType = axisConnectType)
                                              invisible()
                                      },
                                      #' @description
                                      #'  checks whether the m/z coordinates have 'sufficient' intensity labels
                                      #'  if not, then the label is dropped from the object
                                      #'
                                      #' @param dataFrame data.frame with at least an m/z column and an intensity column
                                      #' @param toleranceLow left tolerance (m/z)
                                      #' @param toleranceHigh right tolerance (m/z)
                                      #' @param relativeCutOff is the intensity CutOff reletive (fraction of maximum intensity)
                                      #' @param yCutOff y-axis (intensity) cut off y-axis value (intensity) should be above
                                      check = function(spectrum,
                                                       toleranceLow = 0.1, toleranceHigh = toleranceLow,
                                                       relativeCutOff = TRUE, intensityCutOff = ifelse(relativeCutOff, 0.001, 10)){
                                              super$check(dataFrame = spectrum,
                                                          toleranceLow = toleranceLow, toleranceHigh = toleranceHigh,
                                                          relativeCutOff = relativeCutOff, yCutOff = intensityCutOff)
                                              invisible()
                                      },
                                      #' @description
                                      #'  Adds the annotation graphics to a ggplot object
                                      #'
                                      #' @note plotSpectrum has internal code to use this function but can also be used 'manually'
                                      #'
                                      #' @param graphObject ggplot object to add the annotation to, easiest if it's the output
                                      #'  of plotSpectrum
                                      #' @param maxY maximum intensity value for the ggplot object
                                      #' @param intensityPercentage Whether the intensity axis is displayed in percentages
                                      #'  (default FALSE)
                                      #' @return ggplot object
                                      draw = function(graphObject, maxY = NA, intensityPercentage = FALSE){
                                              if (!identical(maxY, NA)){
                                                      if (self$axisConnect){
                                                              if (self$axisConnectType == 1){
                                                                      graphObject <- graphObject +
                                                                              annotate("linerange",x = self$mz, ymin = rep(0,length(self$mz)),
                                                                                       ymax = self$labelWhere * maxY, linewidth = self$axisConnectWidth,
                                                                                       color = self$axisConnectColor, alpha = self$axisConnectAlpha)
                                                              } else {   # atm anything other than 1
                                                                      ymaxs <- self$labelWhere * maxY
                                                                      if (intensityPercentage){
                                                                              ymins <- ((self$axisConnectLevel/maxY) + (self$axisConnectWhere)) * maxY
                                                                      } else {
                                                                              ymins <- self$axisConnectLevel + (self$axisConnectWhere * ymaxs)
                                                                      }
                                                                      graphObject <- graphObject +
                                                                              annotate("linerange",x = self$mz, ymin = ymins,
                                                                                       ymax = ymaxs, linewidth = axisConnectWidth,
                                                                                       color = self$axisConnectColor, alpha = self$axisConnectAlpha)
                                                              }
                                                      }
                                                      if (self$labelConnect){
                                                              if (length(self$mz) > 1){
                                                                      graphObject <- graphObject +
                                                                              annotate("path",x = self$mz, y = maxY*self$labelWhere,
                                                                                       color = self$labelConnectColor,
                                                                                       alpha = self$labelConnectAlpha)
                                                              }                                        
                                                      }
                                                      if (self$axisConnectType == 3){
                                                              if (intensityPercentage){
                                                                      ymaxs <- ((self$axisConnectLevel/maxY) + (self$axisConnectWhere)) * maxY 
                                                              } else {
                                                                      ymaxs <- self$axisConnectLevel + (self$axisConnectWhere * maxY)
                                                              }
                                                              graphObject <- graphObject +
                                                                      annotate("text", x = self$mz, y = ymaxs,
                                                                               label = self$labelMz, size=self$labelSize,
                                                                               angle=self$labelAngle, color = self$labelColor)
                                                      } else {
                                                              graphObject <- graphObject +
                                                                      annotate("text", x = self$mz, y = maxY*self$labelWhere,
                                                                               label = self$labelMz, size=self$labelSize,
                                                                               angle=self$labelAngle, color = self$labelColor)
                                                      }
                                              }
                                              return(graphObject)
                                      }
                              ),
                              active = list(
                                      #' @field mz gets/sets the x values of the labels
                                      mz = function(value){
                                              if (missing(value)){
                                                      return(self$x)
                                              } else {
                                                      self$x <- value
                                              }
                                      },
                                      #' @field labelMz gets/sets the label values
                                      labelMz = function(value){
                                              if (missing(value)){
                                                      return(self$labelX)
                                              } else {
                                                      self$labelX <- value
                                              }
                                      }
                              )
)