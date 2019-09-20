#' @importFrom jsonlite fromJSON
#' @importFrom readr read_lines
#' @param date date to obtain rankings in 'yyyy-mm-dd' format
#' 
#' @export
get_rankings <- function(gender = c("men", "women"), date = format(Sys.time(), '%Y-%m-%d')){
  
  gen <- match.arg(gender, choices = c("men", "women"))
  
  if(gen == "men") {
    url <- paste0(glue::glue("https://cmsapi.pulselive.com/rugby/rankings/mru?date={date}&language=en&client=pulse"))
  } else {
    url <- paste0(glue::glue("https://cmsapi.pulselive.com/rugby/rankings/wru?date={date}&language=en&client=pulse"))
  }
  
  txt <- readr::read_lines(url)
  json <- jsonlite::fromJSON(txt)
  result <- tibble::tibble(
    team = json$entries$team$name,
    team_abbr = json$entries$team$abbreviation,
    points = json$entries$pts,
    rank = json$entries$pos,
    played = json$entries$matches,
    previous_points = json$entries$previousPts,
    previous_rank = json$entries$previousPos,
    date = as.Date(date, format = '%Y-%m-%d')
  )
  
  return(result)
} 
