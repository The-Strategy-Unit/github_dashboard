#' get all repos for the SU repository 
#' .limit = Inf for all repos
get_repos <- function(limit = Inf){
  
  result <- gh::gh(
    "GET /orgs/{org}/repos",
    org = "The-Strategy-Unit",
    .limit = limit
  )
  
  return(result)
}

# turn results from get_repos into tibble of names and public/private
result_to_tibble <- function(result){
  
  repos <- result |> 
    purrr::map(\(x) 
               tibble::tibble(
                 repo_name = purrr::pluck(x, "name"),
                 is_private = purrr::pluck(x, "private")
               )) |> 
    purrr::list_rbind()
  return(repos)
}

# gets organisation member usernames as a vector
get_members <- function(){
  
  members <- gh::gh(
    "GET /orgs/{org}/members",
    org = "The-Strategy-Unit",
    .limit = Inf
  ) |> 
    sapply(\(x) x[["login"]]) |>
    paste(collapse = "|")
  
  return(members)
  
} 
