#' Method dispatch for metadictionary
#'
#' @param the_data the data to be documented
#' @param dict the dictionary to append it to
#'
#' @return
#' @export
#'
#' @examples
metaDictionary <- function(the_data, dict) {
        UseMethod("metaDictionary")
        }

#' The default meta_dictionary generator
#'
#' @param the_data the data to be documented
#' @param dict the dictionary to append it to
#'
#' @return
#' @export
#'
#' @examples
metaDictionary.default <- function(the_data, dict = list()){
        dict[["DatasetLevel"]] <- list()
        dict[["DatasetLevel"]][["Description"]] <- "Basic Description of the dataset"
        dict[["DatasetLevel"]][["Name"]] <- "Full name of the dataset"

        dict[["DefaultLevel"]] <- list()

        for(i in colnames(the_data)){
                dict[["DefaultLevel"]] <- metaColnamer(i, dict[["DefaultLevel"]])
        }

        for(i in colnames(the_data)){
                dict[["DefaultLevel"]] <- metaAutoTyper(i, dict[["DefaultLevel"]], the_data)
        }

        for(i in colnames(the_data)){
                dict[["DefaultLevel"]] <- metaAutoClassifier(i, dict[["DefaultLevel"]], the_data)
        }

        dict
}

#' Method for lists in the generic
#'
#' @param the_data the data to be documented
#' @param dict the dictionary to append it to
#'
#' @return
#' @export
#'
#' @examples
metaDictionary.list <- function(the_data, dict = list){
"no method defined yet for lists"

}
