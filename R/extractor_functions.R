

#' Category Extractor
#'
#' @param slice the index of the data field
#' @param metadata the metadata dictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#'
#' @examples
metaCategoryExtract <- function(slice, metadata, extract_level){
        if(class(metadata[[extract_level]][[slice]]) != "list") {stop("Not a field element")}
        metadata[[extract_level]][[slice]][["data_category"]]
}


#' Name Extractor
#'
#' @param slice the index of the data field
#' @param metadata the metadata dictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#'
#' @examples
metaNameExtract <- function(slice, metadata, extract_level){
        if(class(metadata[[extract_level]][[slice]]) != "list") {stop("Not a field element")}
        metadata[[extract_level]][[slice]][["name"]]
}
#' Class Extractor
#'
#' @param slice the index of the data field
#' @param metadata the metadata dictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#'
#' @examples
metaClassExtract <- function(slice, metadata, extract_level){
        metadata[[extract_level]][[slice]] %>% class()
}


#' Variable Type Splitter
#'
#' @param metadata the data fictionary
#' @param extract_level the level to perform the extract on
#'
#' @return
#' @export
#'
#' @examples
metaSplitNominal <- function(metadata, extract_level){
        dict_range <- 1:length(metadata[[extract_level]])

        classes <- dict_range %>% purrr::map(metaClassExtract, metadata, extract_level) %>% unlist()

        data_range <- which(classes == "list")

        types <-  data_range %>% purrr::map(metaCategoryExtract, metadata, extract_level) %>% unlist()

        nominal_slices <- types %in% c("Category", "Tag")
        cont_slices <- !types %in% c("Category", "Tag")

        names <- data_range %>% purrr::map(metaNameExtract, metadata, extract_level) %>% unlist()

        column_split <- list()

        column_split$nominal <- names[nominal_slices]
        column_split$continuous <- names[cont_slices]

        column_split
}


