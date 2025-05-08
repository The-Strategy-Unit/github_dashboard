# get urls for codeowner files in provided repositories
# takes list produced by get_repos() 
# returns message if file doesn't exist

get_codeowner_files <- function(repos){
  
  possibly_gh <- purrr::possibly(gh::gh, 
                                 otherwise = "No file found")
  
  names <- repos$repo_name
  
  names |> 
    purrr::map(\(x){
      
      result <- possibly_gh(
        "GET /repos/{owner}/{repo}/contents/{path}",
        # https://docs.github.com/en/rest/repos/contents?apiVersion=2022-11-28#get-repository-content
        owner = "The-Strategy-Unit",
        repo = x,  
        path = "CODEOWNERS"
      )
      
      tibble::tibble(
        repo_name = x, 
        co_file = list(result)
        )
        
    }) |> purrr::list_rbind()
}
