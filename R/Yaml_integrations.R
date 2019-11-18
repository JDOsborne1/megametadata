
#' Function to determine the range of an integer list
#'
#' @param x the vector to determine the range of
#'
#' @return
#' @export
#'
#' @examples
util_Range <- function(x) {
        max(x, na.rm = T) - min(x, na.rm = T)
}


#' Function to evaluate a string name of function on some specified data
#'
#' @param x the name of the function to evaluate on the data
#' @param data the data to evaluate the function on
#'
#' @return
#' @export
#'
#' @examples
util_EvalOnData <- function(x, data) {
        eval(as.name(x))(data)
}


#' Process to name the columns in the meta_dict
#'
#' @param column The column to type
#' @param meta_list A dictionary for the dataset
#'
#' @return
#' @export
#'
#' @examples
meta_Colnamer <- function(column, meta_list){
        meta_list[[column]] <- list()
        meta_list[[column]][["name"]] <- column
        meta_list
}

#' Process to guess the data category of the columns for the meta_dict
#'
#' @param column The column to type
#' @param meta_list A dictionary for the dataset
#' @param dataset The dataset to be documenting
#'
#' @return
#' @export
#'
#' @examples
meta_AutoTyper <- function(column, meta_list, dataset){
        meta_list[[column]][["data_category"]] <- guessDataType(dataset[[column]])
        meta_list
}

#' process to append the typing of a column at read time to the meta_dict
#'
#' @param column The column to type
#' @param meta_list A dictionary for the dataset
#' @param dataset The dataset to be documenting
#'
#' @return
#' @export
#'
#' @examples
meta_AutoClassifier <- function(column, meta_list, dataset){
        meta_list[[column]][["class"]] <- class(dataset[[column]])
        meta_list
}


#' Process to append the first pass variables to the meta_dict
#'
#' @param a_tibble The dataset to be documenting
#' @param Leveled_meta the level of the dataset dictionary to be documented
#'
#' @return
#' @export
#'
#' @examples
meta_VariableAppend <- function(a_tibble, Leveled_meta = list()){
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

#' Update dictionary to fit specification
#'
#' @description Function which takes an existing dictionary template and returns an updated
#'    version which fits the specification.
#'
#' @param a_dict A dictionary for the dataset
#' @param a_spec The specification for the dictionary
#' @param a_dataset The dataset to be documenting
#' @param a_level The level to perform the documentation
#'
#' @return
#' @export
#'
#' @examples
meta_UpdateDictWithSpec <- function(a_dict, a_spec, a_dataset, a_level){
        for(i in colnames(a_dataset)){
                a_dict[[a_level]][[i]]  <- a_dict[[a_level]][[i]] %>%  append(
                        a_spec[[a_level]][[
                                a_dict[[a_level]][[i]][["class"]]
                                ]] %>%
                                purrr::map(utilEvalOnData, a_dataset[[i]])
                )
        }
        a_dict
}

