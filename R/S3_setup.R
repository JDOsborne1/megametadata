#' Method dispatch for metadictionary
#'
#' @param the_data the data to be documented
#' @param dict the dictionary to append it to
#' @param levelgen the level to generate the metadata at
#'
#' @return
#' @export
#'
#' @examples
metaDictionary <- function(the_data, dict, levelgen) {
        UseMethod("metaDictionary")
        }

#' The default meta_dictionary generator
#'
#' @param the_data the data to be documented
#' @param dict the dictionary to append it to
#' @param levelgen the level to generate the metadata at
#'
#' @return
#' @export
#'
#' @examples
metaDictionary.default <- function(the_data, dict = list(), levelgen = "DefaultLevel"){
        dict[["DatasetLevel"]] <- list()
        dict[["DatasetLevel"]][["Description"]] <- "Basic Description of the dataset"
        dict[["DatasetLevel"]][["Name"]] <- "Full name of the dataset"

        dict[[levelgen]] <- list()

        for(i in colnames(the_data)){
                dict[[levelgen]] <- metaColnamer(i, dict[[levelgen]])
        }

        for(i in colnames(the_data)){
                dict[[levelgen]] <- metaAutoTyper(i, dict[[levelgen]], the_data)
        }

        for(i in colnames(the_data)){
                dict[[levelgen]] <- metaAutoClassifier(i, dict[[levelgen]], the_data)
        }

        dict
}

#' Method for lists in the generic
#'
#' @param the_data the data to be documented
#' @param dict the dictionary to append it to
#' @param levelgen the level to generate the metadata at
#'
#' @return
#' @export
#'
#' @examples
metaDictionary.list <- function(the_data, dict = list, levelgen = "DefaultLevel"){
"no method defined yet for lists"

}
