
makeEmptyMeta <- function(df) {
        meta_list <- rep(NA_character_, length(colnames(df))) %>%
        `names<-`(colnames(df)) %>%
        return()
}

assignNamedMeta <- function(df) {
        name <- deparse(substitute(df)) %>%
                paste0("_meta") %>%
                assign(makeEmptyMeta(df), envir = .GlobalEnv)

}

getConnectedMeta <- function(df, colname) {
        meta_list <- get(paste0(deparse(substitute(df)),"_meta"))
        meta_data <- meta_list[[colname]]
        return(meta_data)
}
