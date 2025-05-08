# produce table of codeowners using repos, codeowner files
# and org members
# creates 
make_codeowner_table <- function(repos, 
                                 members, 
                                 codeowner_files){
  
  repos |> 
    dplyr::left_join(codeowner_files) |> 
    dplyr::mutate(
      co_url = 
        purrr::map_chr(
          co_file, 
          "download_url",
          .default = "No file found"), #gets co url
      
      co_content = 
        purrr::map(
          co_url,
          purrr::possibly(
            readLines,
            otherwise = "No file found"), #reads file at url
        ),
      
      codeowners_raw =
        purrr::map(
          co_content,
          \(x) stringr::str_extract_all(
            x,
            "(?<=@)(?!primary-owner|secondary-owner)[A-Za-z0-9-]+"
          ) |>
            unlist()#returns all @username patterns in codeowner file except primary/secondary
          
        ),
      
      codeowners_matched = 
        stringr::str_extract_all(
          codeowners_raw, 
          members), #returns codeowners that match member usernames
    )
    
  
}
