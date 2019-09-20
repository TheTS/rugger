#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom xml2 read_html
#' @importFrom rvest html_node
#' @importFrom rvest html_table
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom dplyr mutate_at
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom tidyr separate
#' @importFrom stringr str_to_lower
#' @importFrom glue glue
#' @title Get Match Data for Rugby Teams
#' @description Download data from ESPN's statsguru service. Data includes information
#' on various teams, win ratios, points scored etc.
#' @param country Name of home team.
#' @param opposition If searching for results for the home team by specific opposition,
#' the opposition team name can be used here.
#' @param all \code{TRUE} by default, returns info on all teams, ignoring the previous
#' parameters. 
#' @param type Defaults to "team". Other options are "player", which gets records for 
#' individual players by countries selected, or "matches", which returns match results
#' for all individual games played.
#' @export 
get_team_records <- function(country = NULL, opposition = NULL,
                             all = TRUE, type = c("team", "player", "matches")){
  
  
  tp <- match.arg(type, choices = c("team", "player", "matches"))
  
  if(tp == "matches") {
    url <- "http://www.espnscrum.com/statsguru/rugby/team/"
  } else {
    url <- "http://stats.espnscrum.com/statsguru/rugby/stats/index.html?class=1;"
  }
  
  country_df <- tibble::tibble(
    country = c("england", "scotland", "ireland", "wales", "south africa",
                "australia", "new zealand", "france", "argentina", "united states",
                "romania", "fiji", "samoa", "tonga", "italy", "japan",
                "canada", "uruguay", "russia", "georgia", "pacific islanders",
                "american samoa", "namibia"),
    number = c("1", "2", "3", "4", "5", "6", "8", "9", "10", "11", "12",
               "14", "15", "16", "20", "23", "25", "29", "57", "81",
               "121", "551", "82")
  )
  
  get_data <- function(URL) {
    
    tabl <- xml2::read_html(URL)
    
    if(tp == "matches") {
      tabl <- rvest::html_node(tabl, "#scrumArticlesBoxContent > table:nth-child(4)") %>% 
        rvest::html_table() %>%
        setNames(make.names(names(.), unique = TRUE)) %>% 
        dplyr::rename(result = Result, 
                      `for` = For, 
                      against = Aga,
                      difference = Diff, 
                      for_ht = HTf,
                      against_ht = HTa,
                      opposition = Opposition,
                      ground = Ground,
                      date = Match.Date) %>% 
        dplyr::select(-c(7, 11))
    } else {
      tabl <- rvest::html_node(tabl, "#scrumArticlesBoxContent > table:nth-child(2)") %>% 
        rvest::html_table()
    }
    
    if(tp == "team") {
      tabl <- tabl %>% 
        dplyr::rename(matches = Mat, percent_won = `%`, against = Aga,
                      difference = Diff, conversions = Conv, penalties = Pens,
                      dropgoals = Drop) %>% 
        dplyr::select(-c(15,16))
      
    } else if(tp == "player") {
      tabl <- tabl %>% 
        dplyr::rename(matches = Mat, points = Pts, percent_won = `%`,
                      conversions = Conv, penalties = Pens,
                      dropgoals = Drop) %>%
        dplyr::select(-16)
    }
    
    if(tp != "matches") {
      tabl <- tabl %>%
        tidyr::separate(Span, into = c("start_year", "end_year"), sep = "-") %>% 
        dplyr::mutate_at(2:3, as.numeric)
      
      colnames(tabl) <- stringr::str_to_lower(colnames(tabl))
      tabl <- tibble::as_tibble(tabl)
    }
    
    
    return(tabl)
  }
  
  if(is.null(country)){
    all_teams <- paste0(url, "template=results;", "type=", tp)
    
    df <- get_data(all_teams)
    
    return(df)
  } else if(!is.null(country)){
    
    if(is.null(opposition)){
      
      ct <- stringr::str_to_lower(country)
      
      if(!ct %in% country_df$country) {
        stop("Selected country not available.")
      } else {
        
        team <- dplyr::filter(country_df, country == ct) %>% 
          dplyr::pull(number)
        
        if(tp == "matches") {
          req <- paste0(url, glue::glue("{team}.html?class=1;template=results;type=team;view=results"))
        } else {
          req <- paste0(url, glue::glue("team={team};template=results;type={tp}"))
        }
        
        df <- get_data(req)
        
        return(df)
      }
    } else {
      op <- stringr::str_to_lower(opposition)
      ct <- stringr::str_to_lower(country)
      
      if(!ct %in% country_df$country | !op %in% country_df$country) {
        stop("Selected country not available.")
      } else { 
        team <- dplyr::filter(country_df, country %in% ct) %>% 
          dplyr::pull(number)
        opp <- dplyr::filter(country_df, country %in% op) %>% 
          dplyr::pull(number)
        req <- paste0(url, glue::glue("opposition={opp};team={team};template=results;type={tp}"))
        
        df <- get_data(req)
        
        return(df)
      }
    }
  }
}