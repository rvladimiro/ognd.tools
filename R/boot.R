#' @author Ricardo Vladimiro
#' @title boot
#' @name boot
#' @description Cleans global environment and sets working directory
#' @details This function prepares any script or RMarkdown file to be ran from scripts directory
#' with a clean global environment. This avoids issues with previously existing variables in the
#' global environment and RStudio's knitting not using the project's working directory.
#' @export
boot = function() {
    
    # Remove all variables from the global environment set_wd
    cat('Cleaning global environment...\n')
    rm(list = ls())
    
    #' If the current directory is not the scripts directory, set it
    if(grepl(pattern = 'scripts$', x = getwd())){
        cat('Working directory already set to scripts.\n')
    } else {
        if('scripts' %in% list.dirs(full.names = F, recursive = F)) {
            cat('Setting working directory to scripts.\n')
            setwd('scripts')
        } else {
            cat('Cannot set working directory to scripts. Directory does not exist.')
        }
    }
}
