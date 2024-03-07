
library(R6)
library(cli)
library(BBPersonalR)

# ---- info ----

#' R6 Class base for info
#' 
#' @description 
#'  R6 Class base dealing with data in lists in an organised manner
#'
#' @export 
info <- R6::R6Class(
  "info",
  private = list(
    name_ = NA,
    # name of file to from or to where information is read/written
    info_ = data.frame(),
    # descriptive data.frame with information on what's in the data_ list
    # Every element in the data_ list has one row in the info_ data.frame
    # Always contains a column named ("id") which allows. The id of a new
    # element in the data_list will always be current max id (index_) + 1
    data_ = list(),
    # the actual data. Each element of the list can be anything, easiest to
    # deal with is if the elements have at least something in common
    index_ = 1L,
    # defines the highest id number that is in the 
    # can be set with the active/field index but must always be at least
    # max id (info_ data.frame) +1
    type_ = "info",
    # defines which info type object this is
    
    #' @description internal function for checking in the length of the data_ list
    #'  and the number of rows in the info_ data.frame match
    #' 
    #' @note will throw an error if false (If showWarnings is TRUE)
    #' 
    #' @return logical vector
    evaluate_ = function(){
      condition = (nrow(private$info_) == length(private$data_))
      if (is.null(condition)){
        condition = FALSE
      }
      if (!condition & self$showWarnings){
        warning("Dimensions of data & info do not match")
      }
      return(condition)
    },
    #' @description internal function that saves either the info_ data.frame or the
    #'  data_ list
    #'  
    #' @param saveWhat defines what should be saved. Can only be "info" or "data"
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a file with the filename to be used
    #'  
    #' @note the filename consists of the filename_ field + saveWhat argument
    #' 
    #' @return logical vector: TRUE if save was successful, FALSE if unsuccessful
    save_ = function(path = "", filename = "", saveWhat = "info", overwrite = TRUE){
      if (saveWhat %in% c("info", "data")){
        if (!is.na(private$name_)){
          fileName <- paste(c(
            ifelse(
              str_detect(path, pattern = "/$"),
              path,
              paste0(path, "/")),
            filename,
            ifelse(filename == "",
                   "",
                   "-"),
            self$name, "-",
            saveWhat, ".rds"),
            collapse = "")
          if ((overwrite) | 
              (!overwrite & (!file.exists(fileName)))){
            if (saveWhat == "info"){
              saveRDS(private$info_, file = fileName)
            } else {
              saveRDS(private$data_, file = fileName)
            }
            if (file.exists(fileName)){
              return(TRUE)
            }
          }
        }
      }
      return(FALSE)
    },
    #' @description internal function that loads either the info_ data.frame or the
    #'  data_ list
    #' 
    #' @param path path where to place file, should end in '/'
    #' @param filename prefix to the filename to be added
    #' @param loadWhat defines what should be loaded. Can only be "info" or "data"
    #'  
    #' @note the filename consists of the filename_ field + saveWhat argument
    #' 
    #' @return logical vector: TRUE if load_ was successful, FALSE if unsuccessful
    load_ = function(path = "", filename = "", loadWhat = "info"){
      if (loadWhat %in% c("info","data")){
        fileName <- paste(c(
          ifelse(
            str_detect(path, pattern = "/$"),
            path,
            paste0(path, "/")),
          filename,
          ifelse(
            filename == "",
            "",
            "-"),
          self$name,
          "-",
          loadWhat, ".rds"),
          collapse = "")
        if (file.exists(fileName)){
          if (loadWhat == "info"){
            private$info_ <- readRDS(file = fileName)
            private$index_ <- max(private$info_$id) + 1
          } else {
            private$data_ <- readRDS(file = fileName)
          }
          return(TRUE)
        }
      }
      return(FALSE)
    }
  ),
  public = list(
    showWarnings = TRUE,
    # logical vector which defines whether warnings are to be shown
    #  info object itself only uses this in private evaluate_ function
    
    #' @description create a new info object
    #' 
    #' @param name character vector, by default this will also be the filename
    #'  to which the object will be save or loaded
    #'  
    #' @return a new 'info' object
    initialize = function(name = ""){
      self$name <- name
      invisible(self)
    },
    #' @description
    #' For printing purposes: prints the info_ data.frame as in tibble format
    #' 
    #' @note no arguments, the function takes care of printing
    print = function(){
      if (sum(dim(private$info_) == 0) != 2){
        print(private$info_ |> tibble::tibble())
      } else {
        cat(NA)
        cat("\n")
      }
    },
    #' @description saves the info_ data.frame and if that is successful saves
    #'  the data_ list. By default both are saved as '.rds' files via the saveRDS
    #'  function
    #'  
    #' @param path path where to place file, should end in '/'
    #' @param filename prefix to the filename to be added
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a file with the filename to be used
    #'  
    #' @return logical vector TRUE if save was completely successful, FALSE if
    #'  one or both were unsuccessful. Note that if the info_ data.frame is
    #'  successfully saved, but the data_ list is not (for whatever reason),
    #'  then FALSE will be returned. Also note that if the info_ data.frame
    #'  could not be saved, that there will be no attempt at saving the data_ list.
    save = function(path = "", filename = "", overwrite = TRUE){
      result <- FALSE
      if (!self$empty){
        if (private$save_(path = path, filename = filename,
                          saveWhat = "info", overwrite = overwrite)){
          result <- private$save_(path = path, filename = filename,
                                  saveWhat = "data", overwrite = overwrite)
        }
      }
      names(result) <- self$name
      return(result)
    },
    #' @description loads the info_ data.frame and if that is successful loads
    #'  the data_ list. By default both are loaded from '.rds' files via the readRDS
    #'  function
    #'  
    #' @param path path where to place file, should end in '/'
    #' @param filename prefix to the filename to be added
    #'  
    #' @return logical vector TRUE if load was completely successful, FALSE if
    #'  one or both were unsuccessful. Note that if the info_ data.frame is
    #'  successfully loaded, but the data_ list is not (for whatever reason),
    #'  then FALSE will be returned. Also note that if the info_ data.frame
    #'  could not be loaded, that there will be no attempt at loading the
    #'  data_ list
    load = function(path = "", filename = ""){
      result <- FALSE
      saveOld <- list(
        name_ = private$name_,
        info_ = private$info_
      )
      if (private$load_(path = path, filename = filename,
                        loadWhat = "info")){
        private$index_ <- max(private$info_$id, na.rm = T) + 1
        if (private$load_(path = path, filename = filename,
                          loadWhat = "data")){
          private$index_ <- max(self$info$id) + 1
          result <- TRUE
        } else {
          # restore old info
          private$name_ <- saveOld$name_
          private$info_ <- saveOld$info_
        }
      }
      names(result) <- self$name
      return(result)
    },
    #' @description finds the row number in the info_ data.frame which
    #'  has a certain id
    #'  
    #' @param id integer vector: the id for which the row number is to be found
    #' 
    #' @return NA or integer vector
    indexFromId = function(id = NA){
      if (!identical(id, NA)){
        if (self$nrow > 0){
          result <- which(self$info$id == id)
          if (length(result) == 0){
            result <- NA
          }
        } else {
          result <- 1
        }
        return(result)
      }
      return(NA)
    },
    #' @description finds the id in a certain row number in the info_ data.frame
    #'  
    #' @param id integer vector: the row number for which the id is to be found
    #' 
    #' @return NA or integer vector
    idFromIndex = function(index = NA){
      if (!identical(index, NA)){
        if ((index > 0) & (index <= self$nrow)){
          return(self$info$id[index])
        }
      }
      return(NA)
    },
    #' @description adds data + info to the object
    #' 
    #' @param data data for the object must be a list of two items. The 'info' item
    #'  should be a data.frame that will be added to the object's info_ data.frame
    #'  Please note: that internally dplyr::bind_rows is used which may cause new
    #'  columns to be added and column classes to change. The 'data' item is the
    #'  data that gets added to the data_ list. There are no real restraints on
    #'  the class, but anything that cannot be easily saved/loaded may cause trouble.
    #'  Since getting the data may be a slow/long process function factories are used
    #'  which take care of doing the actual retrieving of the data. See eg the
    #'  'readData' function
    #'  @param ... allows passing on additional parameters to the function 'data'
    add = function(data = readData(), ...){
      data <- data(...) # retrieve the data
      if (!identical(data, NA)){
        private$data_[[length(private$data_)+1]] <- data$data
        private$info_ <- dplyr::bind_rows(private$info_, data$info)
        private$info_$id[nrow(private$info_)] <- as.integer(private$index_)
        private$index_ <- private$index_ + 1L
      }
      invisible(self)
    },
    #' @description adds multiple (data + info) elements to the object
    #' 
    #' @param data is expected to be a vector of function factories, such as readData(),
    #'  see also 'add'
    #' @param verbose default is FALSE;if TRUE will show a progress bar which
    #'  tracks the adding of the data to the object. Useful when data is coming in slow
    #' @param ... allows passing on additional parameters to the function factories in 'data'.
    #'  Note that all 'extra' parameters get added to all items in 'data'
    addMultiple = function(data = NA, verbose = FALSE, ...){
      if (!identical(data, NA)){
              if (verbose) { cli::cli_progress_bar(name = "Loading data", total = length(data)) }
              for (counter in 1:length(data)){
                      self$add(data = data[[counter]], ...)
                      if (verbose) { cli::cli_progress_update() }
              }
              if (verbose) { cli::cli_process_done() }
      }
      invisible(self)
    },
    #' @description deletes a data item from the data_ list and removes the
    #'  corresponding row from the info_ data.frame
    #'  
    #'  @param index row numbers/list numbers to be deleted. Ignored if 'id'
    #'   is specified
    #'  @param id id's that need to be deleted from the info_ data.frame/ data_ list
    delete = function(index = NULL, id = NULL){
      if (length(id) > 0){
        index <- unlist(lapply(id, self$indexFromId))
      }
      if (length(index) > 0){
        toDelete <- as.integer()
        for (counter in 1:length(index)){
          if (!(index[counter] < 1) | !(index[counter] > self$length)){
            toDelete <- append(toDelete, index[counter])
          }
        }
        if (length(toDelete) > 0){
          private$info_ <- private$info_[-toDelete,]
          private$data_ <- private$data_[-toDelete]
        }
      }
      invisible((self))
    },
    #' @description retrieves an element of the data_ list
    #' 
    #' @param index row numbers/list number of the data_ element to be retrieved.
    #'  Ignored if 'id' is specified
    #' @param id id of item that needs to be retrieved from the data_ list
    item = function(index = 1, id = NA){
      if (identical(index, NA) & identical(id, NA)){
        return(NA)
      }
      if (!identical(id, NA)){
        index <- self$indexFromId(id = id)
      }
      if (identical(index, NA) | (index < 1) | (index > self$length)){
        return(NA)
      }
      return(private$data_[[index]])
    },
    #' @description retrieves a list of elements from the data_ list
    #' 
    #' @param index row numbers/list number of the data_ elements to be retrieved.
    #'  Ignored if 'id' is specified
    #' @param id id's of items that need to be retrieved from the data_ list
    item.list = function(index = 1:self$length, id = NA){
      if (self$length < 1){
        return(NA)
      }
      if (!identical(id, NA)){
        index = unlist(lapply(id, self$indexFromId))
      }
      return(lapply(index, self$item))
    }
  ),
  active = list(
    #' @field filename 'name' of the object, used as filename when saving/loading
    name = function(value){
      if (missing(value)){
        return(private$name_)
      } else {
        private$name_ = value
      }
    },
    #' @field info returns the info_ data.frame. Be careful directly changing
    #'  this data.frame (better not to add/delete especially rows)
    info = function(value){
      if (missing(value)){
        if (sum(dim(private$info_) == 0) != 2){
          return(private$info_)
        } else {
          return(NA)
        }
      } else {
        private$info_ <- value
      }
    },
    #' @field data returns the data_ list. Be careful directly changing
    #'  this list (better not to add/delete items)
    data = function(value){
      if (missing(value)){
        if (length(private$data_) > 0){
          return(private$data_)
        } else {
          return(NA)
        }
      } else {
        private$data_ <- value
      }
    },
    #' @field columns returns column names of the info_ table (read only)
    columns = function(value){
      if (missing(value)){
        if (ncol(private$info_) > 0){
          return(colnames(private$info_))
        } else {
          return(NA)
        }
      } else {
        # do nothing, read only
      }
    },
    #' @field length returns the length of the data_ list (read only).
    #'  Should be the same as nrow field
    length = function(value){
      if (missing(value)){
        return(length(private$data_))
      } else {
        # do nothing, read only
      }
    },
    #' @field dimensions returns the dimensions of the info_ data.frame (read only)
    dimensions = function(value){
      if (missing(value)){
        return(dim(private$info_))
      } else {
        # do nothing, read only
      }
    },
    #' @field ncol number of columns of the info_ data.frame (read only)
    ncol = function(value){
      if (missing(value)){
        return(ncol(private$info_))
      } else {
        # do nothing, read only
      }
    },
    #' @field nrow number of rows of the info_ data.frame (read only).
    #'  Should be the same as the length field
    nrow = function(value){
      if (missing(value)){
        return(nrow(private$info_))
      } else {
        # do nothing, read only
      }
    },
    #' @field valid logical vector which returns the result of the (internal) 'evaluate' function (read only)
    #'  Under normal conditions, simply tests if the number of rows in the info_ data.frame is the same as
    #'  the length of the data_ list
    valid = function(value){
      if (missing(value)){
        return(private$evaluate_())
      } else {
        # do nothing, read only
      }
    },
    #' @field index set & gets the current value of the (private) index_ field
    #'  Note: for debugging the code & solving issues. Should not be manually set
    #'  w/o (good) reason. May be removed in the future
    index = function(value){
      if (missing(value)){
        return(private$index_)
      } else {
        if (self$nrow > 0){
          if (value > max(self$info$id, na.rm = T)){
            private$index_ <- value
          }
        }
      }
    },
    #' @field empty logical vector, determines if the object is (still) empty
    #'  Note that this does not mean that both info & data are empty completely
    #'  (read only)
    empty = function(value){
      if (missing(value)){
        # if either info or data is emptu than object as
        # such is considered empty
        if (identical(private$info_, NA) & identical(private$data_, NA)){
          return(TRUE)
        } else {
          return((self$nrow == 0) | (self$length == 0))
        }
      } else {
        # nothing, read only
      }
    },
    #' @field type character vector which returns the info type object
    #'  (read only)
    type = function(value){
      if (missing(value)){
        return(private$type_)
      } else {
        # nothing, read only
      }
    }
  )
)

# ---- info DB ----

#' R6 Class extension to 'info' class with basic database support
#'  
#' @description 
#'  Essentially gives basic functions to regulate saving/loading to/from a database
#'  
#' @note all elements in the data_ list must be data.frames with the exact same columns
#' 
#' @note slowest of all types (especially when loading), but generates smallest database files
#' 
#' @note this type may be removed at any time as it seems kind of over complicated and data
#'  in the database cannot be readily accessed by other programs. An alternative for it is
#'  'infoDBVariable' 
#'
#' @export 
infoDB <- R6::R6Class(
  "infoDB",
  inherit = info,
  private = list(
    dbType_ = "SQLite",
    # no real function at this time, database workings are handled by database functions
    # of the BBPersonalR package
    infoTablename_ = "info",
    # defines the name of the table name for the info_ data.frame
    dataTablename_ = "data",
    # defines the name of the table name for the data_ list
    # all data.frame's in the data_list are combined together into one big table with each
    # separate data.frame in it's own row. See ?BBPersonalR::convertDFtoDB for more info
    blobConvert_ = TRUE,
    # convert data to raw vector or not, see ?BBPersonalR::convertDFtoDB
    compression_ = "gzip",
    # compression type, see ?memCompress
    type_ = "infoDB",
    # defines which info type object this is
    
    #' @description
    #'  gives the names of the database tables for info_ and data_. Nothing more than pasting togther
    #'  the 'filename'/'name' field with either '_info' and '_data'
    #'  
    #' @param whichTable defines which tablename to return. Can only be "info" or "data"
    #' 
    #' @note convenience function, nothing more
    tableName_ = function(whichTable){
      return(
        switch(whichTable,
               info = ifelse(self$name != "",
                             paste(c(
                               self$name,
                               "_",self$infoTableName), collapse = ""),
                               self$infoTableName
                             ),
               data = ifelse(self$name != "",
                             paste(c(
                               self$name,
                               "_",self$dataTableName), collapse = ""),
                             self$dataTableName)
        )
      )
    },
    #' @description 
    #'  saves either the info_ data.frame or the data_ list (list of data.frame's) to the database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #' @param saveWhat defines what should be saved. Can only be "info" or "data"
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a table with the filename to be used
    #' 
    #' @return logical vector: TRUE if save was successful, FALSE if unsuccessful 
    save_ = function(db, saveWhat = "info", overwrite = TRUE){
      if (saveWhat %in% c("info", "data")){
        tablesPresent <- pool::dbListTables(db)
        if (length(tablesPresent)>0){
          if (private$tableName_(saveWhat) %in% tablesPresent){
            if (!overwrite & self$showWarnings){
              # abort save action
              warning("Table already exists & overwrite is FALSE")
              return(FALSE)
            } else {
              db_deleteTable(db, private$tableName_(saveWhat))
            }
          }
        }
        if (saveWhat == "data"){
          # change empty data_ elements into empty data.frame's
          # there must be at least one data element with a data.frame
          if (sum(is.na(private$data_)) > 0){
            firstDF <- which(!is.na(private$data_))[1]
            emptyData <- which(is.na(private$data_))
            private$data_[[emptyData]] <- private$data_[[firstDF]][0,]
            private$data_[[emptyData]][1,] <- NA
          }
        }
        db_createTable(db = db,
                       tableName = private$tableName_(saveWhat),
                       dataframe = ifelseProper(
                         saveWhat == "info",
                         private$info_,
                         convertDFtoDB(private$data_, saveClasses = TRUE,
                                       toBlob = private$blobConvert_,
                                       type = private$compression_)),
                       addPrimary = FALSE,
                       dbType = private$dbType_)
      }
      return(TRUE)
    },
    #' @description
    #'  internal function that loads either the info_ data.frame or the data_ list from
    #'  the database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #' @param loadWhat defines what should be loaded. Can only be "info" or "data"
    #' 
    #' @return logical vector: TRUE if load_ was successful, FALSE if unsuccessful
    load_ = function(db, loadWhat = "info"){
      if (loadWhat %in% c("info","data")){
        if (private$tableName_(loadWhat) %in% pool::dbListTables(db)){
          if (loadWhat == "info"){
            private$info_ <- db_getTable(db = db,
                                         tableName = private$tableName_(loadWhat))
          } else {
            private$data_ <- convertDBtoDF(db_getTable(db = db,
                                                       tableName = private$tableName_(loadWhat)),
                                           restoreClasses = TRUE,
                                           fromBlob = private$blobConvert_,
                                           type = private$compression_)
          }
          return(TRUE)
        }
      }
      return(FALSE)
    }
  ),
  public = list(
    #' @description saves the info_ data.frame and if that is successful saves
    #'  the data_ list to a database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a table with the tablename to be used
    #'  
    #' @return logical vector TRUE if save was completely successful, FALSE if
    #'  one or both were unsuccessful. Note that if the info_ data.frame is
    #'  successfully saved, but the data_ list is not (for whatever reason),
    #'  then FALSE will be returned. Also note that if the info_ data.frame
    #'  could not be saved, that there will be no attempt at saving the data_ list.
    save = function(db, overwrite = TRUE){
      result <- FALSE
      if (!self$empty){
        if (private$save_(db, saveWhat = "info",
                          overwrite = overwrite)){
          result <- private$save_(db, saveWhat = "data",
                                  overwrite = overwrite)
        }
      }
      names(result) <- self$name
      return(result)
    },
    #' @description loads the info_ data.frame and if that is successful loads
    #'  the data_ list
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #'  
    #' @return logical vector TRUE if load was completely successful, FALSE if
    #'  one or both were unsuccessful. Note that if the info_ data.frame is
    #'  successfully loaded, but the data_ list is not (for whatever reason),
    #'  then FALSE will be returned. Also note that if the info_ data.frame
    #'  could not be loaded, that there will be no attempt at loading the
    #'  data_ list
    #'  
    #' @note the original info_ data.frame will be restored if loading
    #'  the data_ list fails
    load = function(db){
      saveOld <- list(
        info_ = private$info_
      )
      result <- FALSE
      if (private$load_(db = db, loadWhat = "info")){
        if (private$load_(db = db, loadWhat = "data")){
          private$index_ <- max(private$info_$id) + 1
          result <- TRUE
        } else {
          # restore old info
          private$info_ <- saveOld$info_
        }
      }
      names(result) <- self$name
      return(result)
    }
  ),
  active = list(
    #' @field databaseType allows for the setting of the database type
    #'  currently has no function in the object
    databaseType = function(value){
      if (missing(value)){
        return(private$dbType_)
      } else {
        if (value == "SQLite"){
          private$dbType <- value
        } else {
          if (self$showWarnings){
            warning(paste(c("Database type '", value, "' not supported! ")))
          }
        }
      }
    },
    #' @field infoTableName can be used to overwrite the info table name
    infoTableName = function(value){
      if (missing(value)){
        return(private$infoTablename_)
      } else {
        if (value != private$dataTablename_){
          private$infoTablename_ <- value
        } else {
          if (self$showWarnings){
            warning("info & data table names must be different.")
          }
        }
      }
    },
    #' @field dataTableName can be used to overwrite the data table name
    dataTableName = function(value){
      if (missing(value)){
        return(private$dataTablename_)
      } else {
        if (value != private$infoTablename_){
          private$dataTablename_ <- value
        } else {
          if (self$showWarnings){
            warning("info & data table names must be different.")
          }
        }
      }
    },
    #' @field blobConvert convert data to raw vector or not, see ?BBPersonalR::convertDFtoDB
    blobConvert = function(value){
      if (missing(value)){
        return(private$blobConvert_)
      } else {
        if (is.logical(value)){
          private$blobConvert_ <- value 
        }
      }
    },
    #' @field compression gets/sets compression type, see ?memCompress
    compression = function(value){
      if (missing(value)){
        return(private$compression_)
      } else {
        if (value %in% c("gzip", "bzip2", "xz", "none")){
          private$compression_ <- value 
        }
      }
    }
  )
)

# ---- info DB Variable ----

#' R6 Class extension to 'infoDB' class with different database functionality
#'  
#' @description 
#'  The data in the data_ list is saved in separate tables
#'  
#' @note The elements in the data_ list must be data.frames, but do not have to have the exact same columns
#' 
#' @note The name of this class will probably change in the near future. 'infoDBVariable' is only a placeholder
#'
#' @export 
infoDBVariable <- R6::R6Class(
  "infoDBVariable",
  inherit = infoDB,
  private = list(
    type_ = "infoDBVariable",
    # defines which info type object this is
    
    #' @description
    #'  gives the names of the database tables for info_ and data_. The names for the data_ tables
    #'  are numbered (first element is 1, second 2, and so on)
    #'  
    #' @param whichTable defines which table name to return. Can only be "info" or "data"
    #' @param number defines what number to add to the table name (if whichTable is "data")
    #' 
    #' @note convenience function, nothing more
    tableName_ = function(whichTable, number = 1){
      return(
        switch(whichTable,
               info = super$tableName_(whichTable = whichTable),
               data = paste(c(super$tableName_(whichTable = whichTable),
                              "_",toString(number)), collapse = ""))
      )
    },
    #' @description 
    #'  saves either the info_ data.frame or the data_ list (list of data.frame's) to the database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #' @param saveWhat defines what should be saved. Can only be "info" or "data"
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a table with the filename to be used
    #' 
    #' @return logical vector: TRUE if save was successful, FALSE if unsuccessful 
    save_ = function(db, saveWhat = "info", overwrite = TRUE){
      if (saveWhat %in% c("info", "data")){
        tablesPresent <- pool::dbListTables(db)
        if (saveWhat == "data"){
          # change empty data_ elements into empty data.frame's
          # there must be at least one data element with a data.frame
          if (sum(is.na(private$data_)) > 0){
            # firstDF <- which(!is.na(private$data_))[1]
            emptyData <- which(is.na(private$data_))
            private$data_[[emptyData]] <- data.frame(noData = as.character(NA))# private$data_[[firstDF]][0,]
            # private$data_[[emptyData]][1,] <- NA
          }
          for (counter in 1:self$length){
            if (length(tablesPresent)>0){
              if (private$tableName_(saveWhat,
                                     number = counter) %in% tablesPresent){
                if (!overwrite & self$showWarnings){
                  # abort save action
                  warning("Table already exists & overwrite is FALSE")
                  return(FALSE)
                } else {
                  db_deleteTable(db, private$tableName_(saveWhat,
                                                        number = counter))
                }
              }
            }
            db_createTable(db = db,
                           tableName = private$tableName_(saveWhat,
                                                          number = counter),
                           dataframe = private$data_[[counter]],
                           addPrimary = FALSE,
                           dbType = private$dbType_)
          }
        } else {
          if (length(tablesPresent)>0){
            if (private$tableName_(saveWhat) %in% tablesPresent){
              if (!overwrite & self$showWarnings){
                # abort save action
                warning("Table already exists & overwrite is FALSE")
                return(FALSE)
              } else {
                db_deleteTable(db, private$tableName_(saveWhat))
              }
            }
          }
          # save info
          db_createTable(db = db,
                         tableName = private$tableName_(saveWhat),
                         dataframe = private$info_,
                         addPrimary = FALSE,
                         dbType = private$dbType_)
        }
      }
      return(TRUE)
    },
    #' @description
    #'  internal function that loads either the info_ data.frame or the data_ list from
    #'  the database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #' @param loadWhat defines what should be loaded. Can only be "info" or "data"
    #' 
    #' @return logical vector: TRUE if load_ was successful, FALSE if unsuccessful
    load_ = function(db, loadWhat = "info"){
      if (loadWhat %in% c("info","data")){
        if (private$tableName_(loadWhat) %in% pool::dbListTables(db)){
          if (loadWhat == "info"){
            private$info_ <- db_getTable(db = db,
                                         tableName = private$tableName_(loadWhat))
          } else {
            # assumes info has been loaded!
            for (counter in 1:nrow(self$info)){
              private$data_[[counter]] <- db_getTable(db = db,
                                                      tableName = private$tableName_(loadWhat,
                                                                                     number = counter))
            }
          }
          return(TRUE)
        }
      }
      return(FALSE)
    }
  ),
  public = list(),
  active = list()
)

# ---- info Database ----

#' R6 Class extension to 'info' class with different database functionality
#'  
#' @description 
#'  The data in the data_ list is saved in two tables: data & info (or whatever)
#'  
#' @note The elements in the data_ list must be data.frames with the exact same columns
#' @note No column in the data_ data.frames can be named 'id'
#' @note this is preferred type for info 'database' objects. When saved, it's closest
#'  to a 'regular' database
#'
#' @export 
infoDatabase <- R6::R6Class(
  "infoDatabase",
  inherit = info,
  private = list(
    dbType_ = "SQLite",
    # no real function at this time, database workings are handled by database functions
    # of the BBPersonalR package
    infoTablename_ = "info",
    # defines the name of the table name for the info_ data.frame
    dataTablename_ = "data",
    # defines the name of the table name for the data_ list
    # all data.frame's in the data_list are combined together into one big table with each
    # separate data.frame in it's own row. See ?BBPersonalR::convertDFtoDB for more info
    type_ = "infoDatabase",
    # defines which info type object this is
    
    #' @description
    #'  gives the names of the database tables for info_ and data_. The names for the data_ tables
    #'  are numbered (first element is 1, second 2, and so on)
    #'  
    #' @param whichTable defines which table name to return. Can only be "info" or "data"
    #'   
    #' @note convenience function, nothing more
    tableName_ = function(whichTable){
      return(
        switch(whichTable,
               info = ifelse(self$name != "",
                             paste(c(self$name,"_",self$infoTableName), collapse = ""),
                             self$infoTableName),
               data = ifelse(self$name != "",
                             paste(c(self$name,"_",self$dataTableName), collapse = ""),
                             self$dataTableName)
               )
      )
    },
    #' @description 
    #'  saves either the info_ data.frame or the data_ list (list of data.frame's) to the database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #' @param saveWhat defines what should be saved. Can only be "info" or "data"
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a table with the filename to be used
    #' 
    #' @return logical vector: TRUE if save was successful, FALSE if unsuccessful 
    save_ = function(db, overwrite = TRUE){
      if (self$length == 0){
        return(FALSE)
      }
      tablesPresent <- pool::dbListTables(db)
      if (length(tablesPresent)>0){
        infoPresent <- private$tableName_("info") %in% tablesPresent
        dataPresent <- private$tableName_("data") %in% tablesPresent
        if (infoPresent | dataPresent){
          if (!overwrite & self$showWarnings){
            # abort save action
            tablePresent <- ifelse(infoPresent,
                                   ifelse(dataPresent,
                                          paste(c(private$tableName_("info"), " & ",private$tableName_("data")), collapse =""),
                                          private$tableName_("info")),
                                   private$tableName_("data"))
            warning(paste(c("Table(s) ", tablePresent," already exists & overwrite is FALSE"), collapse = ""))
            return(FALSE)
          } else {
            if (infoPresent){
              db_deleteTable(db, private$tableName_("info"))
            }
            if (dataPresent){
              db_deleteTable(db, private$tableName_("data"))
            }
            
          }
        }
      }
      # save info
      db_createTable(db = db,
                     tableName = private$tableName_("info"),
                     dataframe = private$info_,
                     addPrimary = TRUE,
                     primaryKeyName = "id",
                     dbType = private$dbType_)
      # save data
      # change empty data_ elements into empty data.frame's
      # there must be at least one data element with a data.frame
      if (sum(is.na(private$data_)) > 0){
        emptyData <- which(is.na(private$data_))
        private$data_[[emptyData]] <- data.frame(noData = as.character(NA))
      }
      db_createTable(db = db,
                     tableName = private$tableName_("data"),
                     dataframe = dplyr::bind_cols(private$data_[[1]], data.frame(id = private$info_$id[1])),
                     # dataframe = convertDFtoDB(list(private$data_[[counter]]),
                     #                           saveClasses = TRUE,
                     #                           toBlob = private$blobConvert_,
                     #                           type = private$compression_),
                     addPrimary = FALSE,
                     foreignKeys = data.frame(idName = "id", referenceTable = private$tableName_("info"), referencePrimaryKey ="id"),
                     dbType = private$dbType_)
      if (self$length > 1){
        for (counter in 2:self$length){
          db_writeTable(db = db,
                        tableName = private$tableName_("data"),
                        theTable = dplyr::bind_cols(private$data_[[counter]], data.frame(id = private$info_$id[counter])),
                        primaryKeyName = NA)
        }
      }
      return(TRUE)
    },
    #' @description
    #'  internal function that loads either the info_ data.frame or the data_ list from
    #'  the database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #'  
    #' @return logical vector: TRUE if load_ was successful, FALSE if unsuccessful
    load_ = function(db){
      if (private$tableName_("info") %in% pool::dbListTables(db)){
        private$info_ <- db_getTable(db = db,
                                     tableName = private$tableName_("info"))
      } else {
        return(FALSE)
      }
      if (private$tableName_("data") %in% pool::dbListTables(db)){
        columnsInData <- pool::dbListFields(db, private$tableName_("data"))
        columnsInData <- columnsInData[columnsInData != "id"]
        for (counter in 1:nrow(self$info)){
          private$data_[[counter]] <- db_getColumn(db = db,
                                                   tableName = private$tableName_("data"),
                                                   column = columnsInData,
                                                   where = paste(c("id = ",private$info_$id[counter]), collapse = "")
          )
        }
      } else {
        return(FALSE)
      }
      return(TRUE)
    }
  ),
  public = list(
    #' @description saves the info_ data.frame and if that is successful saves
    #'  the data_ list to a database
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a table with the tablename to be used
    #'  
    #' @return logical vector TRUE if save was completely successful, FALSE if
    #'  one or both were unsuccessful. Note that if the info_ data.frame is
    #'  successfully saved, but the data_ list is not (for whatever reason),
    #'  then FALSE will be returned. Also note that if the info_ data.frame
    #'  could not be saved, that there will be no attempt at saving the data_ list.
    save = function(db, overwrite = TRUE){
      result <- FALSE
      if (!self$empty){
        result <- private$save_(db, overwrite = overwrite)
      }
      names(result) <- self$name
      return(result)
    },
    #' @description loads the info_ data.frame and if that is successful loads
    #'  the data_ list
    #'  
    #' @param db database access 'handle' to be closed. Database needs to be open!
    #'  Opening & closing the database shpuld be handled outside the object's code
    #'  
    #' @return logical vector TRUE if load was completely successful, FALSE if
    #'  one or both were unsuccessful. Note that if the info_ data.frame is
    #'  successfully loaded, but the data_ list is not (for whatever reason),
    #'  then FALSE will be returned. Also note that if the info_ data.frame
    #'  could not be loaded, that there will be no attempt at loading the
    #'  data_ list
    #'  
    #' @note the original info_ data.frame will be restored if loading
    #'  the data_ list fails
    load = function(db){
      result <- FALSE
      saveOld <- list(
        info_ = private$info_,
        data_ = private$data_
      )
      if (private$load_(db = db)){
        private$index_ <- max(private$info_$id) + 1
        result <- TRUE
      } else {
        # restore old info
        private$info_ <- saveOld$info_
        private$data_ <- saveOld$data_
      }
      names(result) <- self$name
      return(result)
    }
  ),
  active = list(
    #' @field databaseType allows for the setting of the database type
    #'  currently has no function in the object
    databaseType = function(value){
      if (missing(value)){
        return(private$dbType_)
      } else {
        if (value == "SQLite"){
          private$dbType <- value
        } else {
          if (self$showWarnings){
            warning(paste(c("Database type '", value, "' not supported! ")))
          }
        }
      }
    },
    #' @field infoTableName can be used to overwrite the info table name
    infoTableName = function(value){
      if (missing(value)){
        return(private$infoTablename_)
      } else {
        if (value != private$dataTablename_){
          private$infoTablename_ <- value
        } else {
          if (self$showWarnings){
            warning("info & data table names must be different.")
          }
        }
      }
    },
    #' @field dataTableName can be used to overwrite the data table name
    dataTableName = function(value){
      if (missing(value)){
        return(private$dataTablename_)
      } else {
        if (value != private$infoTablename_){
          private$dataTablename_ <- value
        } else {
          if (self$showWarnings){
            warning("info & data table names must be different.")
          }
        }
      }
    },
    #' @field objectName in stead of filename, a bit redundant perhaps, but exists since
    #'  the info and data are saved to a database and not a (simple) file anymore. filename
    #'  can still be used. A future use could be to put some limits on the objectName since
    #'  it is part of the tablenames in the database
    objectName = function(value){
      if (missing(value)){
        return(private$filename_)
      } else {
        private$filename_ = value
      }
    }
  )
)

# ---- create Info Objects ----


#' Createa R6 object of type info, infoDB, etc
#'
#' @param type character vector setting the info object type. Can only be
#'  'info', 'infoDB', 'infoDBVariable' or 'infoDatabase'
#' @param name character vector: name of new object
#'
#' @return an R6 object of one of the different info types
#' @export
createInfo <- function(type = "infoDatabase", name = ""){
  switch(type,
         "info" = info$new(name = name),
         "infoDB" = infoDB$new(name = name),
         "infoDBVariable" = infoDBVariable$new(name = name),
         "infoDatabase" = infoDatabase$new(name = name),
         NA
  )
}

#' Saves the specified object. Essentially a wrapper to have a unified 'interface' in infoLists
#'  objects with variable types of info objects
#'
#' @param infoObject the R6 info object to be saved. Be careful with references/clone
#' @param db database handle to which the info object is to be saved (if it is a database info type)
#' @param path path to which the info object is to be saved (if it is not a database info type)
#' @param filename filename to which the info object is to be saved (if it is not a database info type) 
#' @param overwrite sets the overwrite logical vector. Default is TRUE (will overwrite existing data)
#'
#' @return logical vector: TRUE if successful, FALSE if not
#' @export
saveInfo <- function(infoObject, db, path = "", filename = "", overwrite = TRUE){
  switch(infoObject$type,
         "info" = infoObject$save(filename = filename, path = path, overwrite = overwrite),
         "infoDB" = infoObject$save(db = db, overwrite = overwrite),
         "infoDBVariable" = infoObject$save(db = db, overwrite = overwrite),
         "infoDatabase" = infoObject$save(db = db, overwrite = overwrite),
         FALSE
  )
} 

#' Loads data into the specified object. Essentially a wrapper to have a unified 'interface'
#'  objects in infoLists with variable types of info objects
#'
#' @param infoObject the R6 info object to which the data is to be loaded. Be careful with references/clone
#' @param db database handle to which the info object is to be saved (if it is a database info type)
#' @param path path to which the info object is to be saved (if it is not a database info type)
#' @param filename filename to which the info object is to be saved (if it is not a database info type) 
#'
#' @return logical vector: TRUE if successful, FALSE if not
#' @export
loadInfo <- function(infoObject, db, path = "", filename = ""){
  switch(infoObject$type,
         "info" = infoObject$load(filename = filename, path = path),
         "infoDB" = infoObject$load(db = db),
         "infoDBVariable" = infoObject$load(db = db),
         "infoDatabase" = infoObject$load(db = db),
         FALSE
  )
} 

# ---- infoList ----

#' R6 Class to deal with a group of "info" objects in an organized manner
#'  
#' @description 
#'  R6 Class to deal with a group of "info" objects in an organized manner
#'  
#' @export 
infoList <- R6::R6Class(
  "infolist",
  private = list(
    info_ = list(),
    # list of "info" objects
    name_ = "",
    # name of the objectList
    
    #' private function to generate warnings or cause stops when
    #'  something is not correct. (Wrapper to generalize warning messages & stops)
    #'
    #' @param message warning or stop message  (character vector)
    #' @param addName whether to add the infoList name to the message (logical vector)
    #'
    #' @return
    stopOrNot = function(message = NA, addName = TRUE){
      if (self$stopOnFail){
        if (!identical(message, NA)){
          stop(paste0(message, addName))
        } else {
          stop(paste0("Error in infoList object ", private$name))
        }
      } else {
        if (self$showWarnings){
          if (!identical(message, NA)){
            warning(paste0(message, addName))
          } else {
            warning(paste0("Error in infoList object ", private$name))
          }
          
        }
      }
    }
  ),
  public = list(
    stopOnFail = TRUE,
    # logical vector which defines if 'certain' errors should cause
    # a 'stop'
    showWarnings = TRUE,
    # logical vector which defines whether warnings are to be shown
    
    #' @description
    #' Create a new infolist object
    #' 
    #' 
    #'
    #' @return a new 'peptide' object
    initialize = function(name = "", names = NA, types = NA){
      private$name_ <- name
      if (!identical(names, NA) & !identical(types, NA)){
        if (length(types) < length(names)){
          types <- rep(types, length(names))
        }
        if (length(types) != length(names)){
          private$stopOrNot(message = "Error names and/or types arguments in initialization of",
                            addName = TRUE)
        }
        for (counter in 1:length(names)){
          self$add(infoObject = createInfo(type = types[counter],
                                           name = names[counter]), name = names[counter])
        }
      }
      invisible(self)
    },
    #' @description
    #' For printing purposes: prints the names of the "info" items present
    #'  and the number of data_ items in each
    print = function(){
      nameAddChars <- purrr::map_int(self$names, ~nchar(.x))
      nameAddChars <- max(nameAddChars) - nameAddChars
      if ((self$length > 0)){
        for (counter in 1:self$length){
          cat(paste(c(" ",paste(c(self$names[counter], rep(" ", times = nameAddChars[counter])), collapse = "") ,"\t: ", self$item(counter, clone = FALSE)$length,"\n"), collapse = ""))
        }
      } else {
        print(NA)
      }
    },
    #' @description 
    #'  Loads the info objects from a database
    #' 
    #' @param db database access 'handle' from which data is to be loaded
    #' @param path path where to place file, should end in '/'
    #' @param filename prefix to the filename to be added
    #'  
    #' @note this does not work if "rds" was chosen as type of info object
    #'  (see initialize)
    load = function(db, path = "", filename = ""){
      tempL <- as.logical()
      for (counter in 1:self$length){
        tempL <- append(tempL, loadInfo(private$info_[[counter]], db = db,
                                        path = path,
                                        filename = filename))
      }
      names(tempL) <- self$names
      return(tempL)
    },
    #' @description 
    #'  saves the info objects to a database
    #'  
    #' @param db database access 'handle' to which data is to be saved
    #' @param path path where to place file, should end in '/'
    #' @param filename prefix to the filename to be added
    #' @param overwrite logical vector that defines what to do if there is already
    #'  a file with the filename or a table with the tablename to be used 
    save = function(db, path = "", filename = "", overwrite = TRUE){
      tempL <- as.logical()
      for (counter in 1:self$length){
        tempL <- append(tempL, saveInfo(private$info_[[counter]], db = db,
                                        path = path,
                                        filename = filename,
                                        overwrite = overwrite))
      }
      names(tempL) <- self$names
      return(tempL)
    },
    #' @description 
    #'  adds an info object to the info_ list
    #'  
    #' @param infoObject info object to be added, must be a descendant class
    #'  of 'info' (or 'info' class itself)
    #' @param name specifies the name for the object to be added. Cannot be empty (string)
    #' 
    #' @note if this 
    add = function(infoObject, name = ""){
      if (is.Class(infoObject, "info") & (name != "")){
        if (!(name %in% self$names)){
          private$info_[[length(private$info_)+1]] <- infoObject
          names(private$info_)[length(private$info_)] <- name
        } else {
          if (self$stopOnFail){
            stop("Object name already exists")
          } else {
            if (self$showWarnings){
              warning("Object not added, name already exists")
            }
          }
        }
      } else {
        if (self$stopOnFail){
          stop("infoObject needs to be class 'info' or descendent and have a valid name")
        } else {
          if (self$showWarnings){
            warning("infoObject not added, it needs to be class 'info' or descendent and have a valid name")
          }
        }
      }
      invisible(self)
    },
    #' @description 
    #'  deletes one of the info items
    #'  
    #' @param index number of the info item to be deleted
    delete = function(index = NA){
      if (!identical(index, NA)){
        if (is.character(index)){
          if (index %in% names(private$info_)){
            index <- which(index == names(private$info_))
          }
        }
      }
      if (!((index < 1) | (index > self$length))){
        private$info_ <- private$info_[-index]
      }
      invisible(self)
    },
    #' @description 
    #'  retrieves one of the info items
    #'  
    #' @param index number or name of the info item to be retrieved
    #' @param clone logical vector specifying whether the returned item
    #'  should be a clone or not. Default = FALSE
    #' 
    #' @return "info" item
    item = function(index = 1, clone = FALSE){
      if (identical(index, NA)){
        return(NA)
      }
      if (is.character(index)){
        if (index %in% names(private$info_)){
          index <- which(index == names(private$info_))
        }
      }
      if ((index < 1) | (index > self$length)){
        return(NA)
      } else {
        if (!clone){
          return(private$info_[[index]])
        } else {
          return(private$info_[[index]]$clone())
        }
      }
    },
    #' @description 
    #'  retrieves one or more of the info items
    #'  
    #' @param index numbers or names of the info item to be retrieved
    #' @param clone logical vector specifying whether the returned items
    #'  should be clones or not. Default = FALSE
    #' 
    #' @return list of "info" items
    item.list = function(index = 1:self$length, clone = FALSE){
      if (self$length < 1){
        return(NA)
      }
      return(lapply(index,
                    function(x){self$item(index = x, clone = clone)}))
    }
  ),
  active = list(
    #' @field contents alternate to print. Generates a data.frame with the names,
    #'  lengths and types of the info object elements in the infoList object
    contents = function(value){
      if (missing(value)){
        if ((self$length > 0)){
          return(data.frame(name = self$names,
                            length = unname(map_int(private$info_, ~.x$length)),
                            type = unname(map_chr(private$info_, ~.x$type))))
        } else {
          return(NA)
        }
      } else {
        # nothing, read only
      }
    },
    #' @field length number of "info" items in the list object
    length = function(value){
      if (missing(value)){
        return(length(private$info_))
      } else {
        # nothing, read only
      }
    },
    #' @field info provides direct access to the list of "info" items
    info = function(value){
      if (missing(value)){
        return(private$info_)
      } else {
        private$info_ <- value
      }
    },
    #' @field name provides direct access to the name of the infoList object
    name = function(value){
      if (missing(value)){
        if (self$length > 0){
          return(names(private$name_))
        } else {
          return(NA)
        }
      } else {
        private$name_ <- value
      }
    },
    
    #' @field names provides direct access to names of the "info" items
    #'  on the list
    names = function(value){
      if (missing(value)){
        if (self$length > 0){
          return(names(private$info_))
        } else {
          return(NA)
        }
      } else {
        private$info_$name <- value
      }
    },
    #' @field classes returns the (main) class of all items on the list
    classes = function(value){
      if (missing(value)){
        if (self$length > 0){
          return(unlist(lapply(private$info_,
                               function(x){class(x)[1]})))
        } else {
          return(NA)
        }
      } else {
        # nothing
      }
      
    },
    #' @field classes.full returns the full class vectors of all items
    #'  on the list
    classes.full = function(value){
      if (missing(value)){
        if (self$length > 0){
          return(lapply(private$info_, class))
        } else {
          return(NA)
        }
      } else {
        # nothing
      }
    }
  )
)
