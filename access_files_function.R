access_files <- function(user, folder_id, folder_save, rm_zip = TRUE){
  
  #OAuth
  suppressMessages(
    googledrive::drive_auth(user)
  )
  
  #store all the files 
  files <- googledrive::drive_ls(googledrive::as_id(folder_id), pattern = "*.zip")
  files <- dplyr::pull(files, name)
  
  #download all the files in the folder
  files_local <- purrr::walk(files,
                             ~ glamr::import_drivefile(folder_id, .x, folder_save, zip = FALSE))
  #unzip files
  purrr::walk(files_local,
              ~ unzip(file.path(folder_save, .x), exdir = folder_save))
  
  #remove downloaded zip
  if(rm_zip == TRUE)
    unlink(file.path(folder_save, files_local))
}


