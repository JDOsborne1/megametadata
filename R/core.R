
#' makeEmptyMeta
#'
#' Makes an empty metadata object
#'
#' @param df the dataframe in question
#'
#' @return the empty named list
#' @export
#'
#' @examples
makeEmptyMeta <- function(df) {
        meta_list <- rep(NA_character_, length(colnames(df))) %>%
        `names<-`(colnames(df)) %>%
        return()
}

#' assignDfToDict
#'
#' assigns an empty metadata list to the data dictionary for the dataframe in question
#'
#' @param df the dataframe in question
#'
#' @return Void
#' @export
#'
#' @examples
assignDfToDict <- function(df) {
        name <- deparse(substitute(df))
        data_dict <- get0("data_dict", envir = .GlobalEnv, ifnotfound = list())
        data_dict[[name]] <- makeEmptyMeta(df)
        assign("data_dict", data_dict, envir = .GlobalEnv)
}

#' getConnectedMeta
#'
#' Finds the metadata associated with the specified dataframe and column
#'
#' @param df The Dataframe in question
#' @param colname The column name in question
#'
#' @return the associated metadata
#' @export
#'
#' @examples
getConnectedMeta <- function(df, colname) {
        data_dict <- get0("data_dict", envir = .GlobalEnv, ifnotfound = list())
        meta_list <- data_dict[[deparse(substitute(df))]]
        meta_data <- meta_list[[deparse(substitute(colname))]]
        return(meta_data)
}
#' getDataDict
#'
#' Fetches the data_dict list, or creates it if it is missing
#'
#' @return data_dict - Either the existing data dictionary or a new one if it is missing
#' @export
#'
#' @examples
getDataDict <- function(){
        data_dict <- get0("data_dict", envir = .GlobalEnv, ifnotfound = list())
        return(data_dict)
}
# searchDataDict <- function(){
#         list.files(here::here())
# }

