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