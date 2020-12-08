
#' Fix File Names
#'
#' The [MplusAutomation::readModels()] function from the `MplusAutomation` package reads all special characters in file names, such as
#' brackets, dashes, and spaces, in as full stops. This is problematic for determining the name of the file. This function replaces
#' all special characters in file names with underscores, renaming the files. These can then be read in correctly by [MplusAutomation::readModels()].
#' Warning: it is recommended that you back up the files before renaming
#'
#' @import readr
#' @param dir A directory
#' @return Renamed files
#' @export

fix_file_names <- function(dir){

  # Download the files from the right directory
  old_file_names = list.files(dir)
  #files = lapply(paste0(dir, "/", old_file_names), read.delim)

  # Replace any cases of full stops, dashes, spaces or brackets
  new_file_names <- gsub('-|\\(|\\)| ', '_', old_file_names)

  # Write files (if any changes should be made)
  if(length(setdiff(new_file_names, old_file_names)) > 0){
    mapply(file.rename, paste0(dir, "/", old_file_names), paste0(dir, "/", new_file_names))
  }

  print("Success!")
}


