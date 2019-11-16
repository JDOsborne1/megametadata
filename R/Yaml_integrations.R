




#' Function to determine the range of an integer list
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
utilRange <- function(x) {
        max(x, na.rm = T) - min(x, na.rm = T)
}


#' Function to evaluate a string name of function on some specified data
#'
#' @param x
#' @param data
#'
#' @return
#' @export
#'
#' @examples
utilEvalOnData <- function(x, data) {
        eval(as.name(x))(data)
}


#' Process to name the columns in the meta_dict
#'
#' @param column
#' @param meta_list
#'
#' @return
#' @export
#'
#' @examples
metaColnamer <- function(column, meta_list){
        meta_list[[column]] <- list()
        meta_list[[column]][["name"]] <- column
        meta_list
}

#' Process to guess the data category of the columns for the meta_dict
#'
#' @param column
#' @param meta_list
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
metaAutoTyper <- function(column, meta_list, dataset){
        meta_list[[column]][["data_category"]] <- guessDataType(dataset[[column]])
        meta_list
}

#' process to append the typing of a column at read time to the meta_dict
#'
#' @param column
#' @param meta_list
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
metaAutoClassifier <- function(column, meta_list, dataset){
        meta_list[[column]][["class"]] <- class(dataset[[column]])
        meta_list
}


#' Process to append the first pass variables to the meta_dict
#'
#' @param a_tibble
#' @param Leveled_meta
#'
#' @return
#' @export
#'
#' @examples
metaVariableAppend <- function(a_tibble, Leveled_meta = list()){
        for(i in colnames(a_tibble)){
                Leveled_meta <- metaColnamer(i, Leveled_meta)
        }

        for(i in colnames(a_tibble)){
                Leveled_meta <- metaAutoTyper(i, Leveled_meta, a_tibble)
        }

        for(i in colnames(a_tibble)){
                Leveled_meta <- metaAutoClassifier(i, Leveled_meta, a_tibble)
        }

        Leveled_meta
}
