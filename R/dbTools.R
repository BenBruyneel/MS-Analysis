library(blob)
library(pool)

#' helper function to determine if object == whichClass or a descendant
#'  of whichClass.
#'
#' @param object a data object of some class
#' @param whichClass character string: class name to be tested
#' @return TRUE or FALSE
#' @export
is.Class <- function(object, whichClass){
  return(whichClass %in% class(object))
}

vectorToBlob <- function(dataVector = as.character(NA), type = "gzip"){
  return(dataVector |> jsonify::to_json() |> as.character() |> memCompress(type = type) |> blob::as_blob())
}

blobToVector <- function(blobData = NA, type = "gzip"){
  return(memDecompress(blobData[[1]], type = type) |> rawToChar() |> jsonify::as.json() |> jsonify::from_json())

}

dfToBlob <- function(df, storeRownames = TRUE, storeClasses = TRUE, storeFactors = TRUE, type = "gzip"){
  result <- list()
  result$data <- vectorToBlob(df)
  if (storeRownames){
    result$rownames <- vectorToBlob(rownames(df), type = type)
  }
  if (storeClasses){
    result$classes <- lapply(df, class)
    result$classes[result$classes == "factor"] <- "character"
    result$classes <- vectorToBlob(result$classes, type = type)
  }
  if (storeFactors){
    result$factors <- names(which(unlist(lapply(lapply(df, class), \(x){x == "factor"}))))
    result$factors <- vectorToBlob(result$factors, type = type)
  }
  return(result)
}

blobToDf <- function(blobData, restoreRownames = TRUE, restoreClasses = TRUE, restoreFactors = TRUE, type = "gzip"){
  result <- blobToVector(blobData = blobData$data, type = type)
  if (restoreRownames & ("rownames" %in% names(blobData))){
    rownames(result) <- blobToVector(blobData = blobData$rownames, type = type)
  }
  if (restoreClasses  & ("classes" %in% names(blobData))){
    columnClasses <- blobToVector(blobData = blobData$classes, type = type)
    for (counter in 1:length(columnClasses)){
      class(result[, names(columnClasses)[counter]]) <- columnClasses[[counter]]
    }
  }
  if (restoreFactors  & ("factors" %in% names(blobData))){
    columnFactors <- blobToVector(blobData = blobData$factors, type = type)
    if (length(columnFactors)>0){
      for (counter in columnFactors){
        result[, counter] <- as.factor(result[, counter])
      }
    }
  }
  return(result)
}

#' 'global' variable: default primary key name
#'
#' @note in future: remove from export
#'
#' @export
defaultPrimaryKeyName <- "id_"

#' returns the default primary key name
#'
#' @return character vector
#' @export
defaultPrimaryKey <- function(){
  return("id_")
}

#' Wrapper around pool::dbPool(): opens a database
#'
#' @param  fileName  a character vector specifying the name and location
#'                   of the database
#' @param  drv defines database connection type, default = RSQLite::SQLite()
#' @param  ... to pass on additional parameters to pool::dbPool, exmples are
#'             host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com"
#'             username = "guest"
#'             password = "guest"
#'
#' @return database access 'handle'
#' @note if no file with the name 'fileName' exists, then it will be created
#' (but obviously it will be empty, so most further commands will fail)
#' @note if fileName == ":memory:" the database will be an in-memory database
#' @export
db_open <- function(fileName, drv = RSQLite::SQLite(), ...){
  return(pool::dbPool(drv = drv,
                      dbname = fileName,
                      ...))
}

#' Wrapper around pool::pooClose():  closes an open database
#' (normally opened earlier via eg db_open())
#'
#'
#' @param db            database access 'handle' to be closed
#' @param suppressErrors logical (default = FALSE). For the somewhat unique
#'  situation that a database is possibly already closed. This happens eg when
#'  working with 'in memory' databases as is possible with SQLite; the closing
#'  of this type of database may already have happened when for instance a shiny
#'  application closes/shuts down
#'
#' @export
db_close <- function(db, suppressErrors = FALSE){
  if (suppressErrors){
    try(pool::poolClose(db), silent = TRUE)
  } else {
    pool::poolClose(db)
  }
}

# 'global' variable: default primary key name
defaultPrimaryKeyName <- "id_"

#' to perform general SQL select like statements
#'
#' @param db              database access 'handle'
#' @param SQLString       the SQL query to be performed
#' @param doNotChangePool boolean that sets whether the pool has to be locked/
#'  released inside this function. Usually this needs to be done, so default
#'  is FALSE. Set to TRUE if this locking/release is done outside of the
#'  function. Note that there is a sort of bug when working with the
#'  pool::dbWriteTable : when a sql constraint like UNIQUE is defined for the
#'  table used, this may result in a locked database upon error. It's advisable
#'  to set doNotChangePool to TRUE to prevent this from happening
#'
#' @returns result from the SQL query, usually a data.frame
#' @export
db_getSQLQuery <- function(db, SQLString, doNotChangePool = FALSE){
  if (!doNotChangePool){
    conn <- pool::poolCheckout(db)
    pool::dbBegin(conn)
    result <- pool::dbGetQuery(conn, SQLString)
    pool::dbCommit(conn = conn)
    pool::poolReturn(conn)
  } else {
    result <- pool::dbGetQuery(db, SQLString)
  }
  return(result)
}

#' to execute statements like update a row
#'
#' @param db              database access 'handle'
#' @param SQLString       the SQL query to be performed
#' @param doNotChangePool boolean that sets whether the pool has to be locked/
#'  released inside this function. Usually this needs to be done, so default
#'  is FALSE. Set to TRUE if this locking/release is done outside of the
#'  function. Note that there is a sort of bug when working with the
#'  pool::dbWriteTable : when a sql constraint like UNIQUE is defined for the
#'  table used, this may result in a locked database upon error. It's advisable
#'  to set doNotChangePool to TRUE to prevent this from happening
#' @param invisibleReturn if TRUE (default), then nothing is returned, if FALSE
#'  then the result of the SQLString will be returned (see SQL for what it can
#'  be)
#'
#' @returns result from the SQL query, usually a data.frame
#' @export
db_ExecuteSQL <- function(db, SQLString, doNotChangePool = FALSE,
                          invisibleReturn = TRUE){
  if (!doNotChangePool){
    conn <- pool::poolCheckout(db)
    pool::dbBegin(conn)
    result <- pool::dbExecute(conn, SQLString)
    pool::dbCommit(conn = conn)
    pool::poolReturn(conn)
  } else {
    result <- pool::dbExecute(db, SQLString)
  }
  if (!invisibleReturn){
    return(result)
  } else {
    invisible()
  }
}

# function factory
dbSQLCommand <- function(db, SQLString,
                         execute = FALSE,
                         doNotChangePool = FALSE,
                         invisibleReturn = TRUE){
  force(db)
  force(SQLString)
  force(execute)
  force(doNotChangePool)
  force(invisibleReturn)
  if (!execute){
    function(){
      db_getSQLQuery(db = db,
                     SQLString = SQLString,
                     doNotChangePool = doNotChangePool)
    }
  } else {
    function(){
      db_ExecuteSQL(db = db,
                    SQLString = SQLString,
                    doNotChangePool = doNotChangePool,
                    invisibleReturn = invisibleReturn)
    }
  }
}