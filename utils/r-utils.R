# Rewrite `predict_race` to avoid imputation of missing data 
# for the race prediction.
# Ambiguity from https://cran.r-project.org/web/packages/wru/wru.pdf:
# "race/ethnicity probabilities will be imputed for 
# unmatched names using race/ethnicity distribution 
# for all other names (i.e., not on Census List)"

predict_race <- function (voter.file, census.surname = TRUE, surname.only = FALSE, 
                          surname.year = 2010, census.geo, census.key, census.data = NA, 
                          age = FALSE, sex = FALSE, party, retry = 0, impute.missing = T) 
{
  if (!missing(census.geo) && (census.geo == "precinct")) {
    stop("Error: census_helper function does not currently support merging precinct-level data.")
  }
  vars.orig <- names(voter.file)
  if (surname.only == T) {
    print("Proceeding with surname-only predictions...")
    if (!("surname" %in% names(voter.file))) {
      stop("Voter data frame needs to have a column named surname")
    }
  }
  else {
    if (missing(census.geo) || is.null(census.geo) || is.na(census.geo) || 
        census.geo %in% c("county", "tract", "block", "place") == 
        F) {
      stop("census.geo must be either 'county', 'tract', 'block', or 'place'")
    }
    else {
      print(paste("Proceeding with Census geographic data at", 
                  census.geo, "level..."))
    }
    if (missing(census.data) || is.null(census.data) || is.na(census.data)) {
      if (missing(census.key) || is.null(census.key) || 
          is.na(census.key)) {
        stop("Please provide a valid Census API key using census.key option.")
      }
      else {
        print("Downloading Census geographic data using provided API key...")
      }
    }
    else {
      if (!("state" %in% names(voter.file))) {
        stop("voter.file object needs to have a column named state.")
      }
      if (sum(toupper(unique(as.character(voter.file$state))) %in% 
              toupper(names(census.data)) == FALSE) > 0) {
        print("census.data object does not include all states in voter.file object.")
        if (missing(census.key) || is.null(census.key) || 
            is.na(census.key)) {
          stop("Please provide either a valid Census API key or valid census.data object that covers all states in voter.file object.")
        }
        else {
          print("Downloading Census geographic data for states not included in census.data object...")
        }
      }
      else {
        print("Using Census geographic data from provided census.data object...")
      }
    }
  }
  eth <- c("whi", "bla", "his", "asi", "oth")
  if (census.surname) {
    if (surname.year == 2010) {
      voter.file <- merge_surnames(voter.file, impute.missing = impute.missing)
    }
    else {
      if (surname.year == 2000) {
        voter.file <- merge_surnames(voter.file, surname.year = surname.year, impute.missing = impute.missing)
      }
      else {
        stop(paste(surname.year, "is not a valid surname.year. It should be either 2000 or 2010 (default)."))
      }
    }
  }
  else {
    for (k in 1:length(eth)) {
      if (paste("p", eth[k], sep = "_") %in% names(voter.file) == 
          F) {
        stop(paste("voter.file object needs to have columns named ", 
                   paste(paste("p", eth, sep = "_"), collapse = " and "), 
                   ".", sep = ""))
      }
    }
  }
  if (surname.only) {
    for (k in 1:length(eth)) {
      voter.file[paste("pred", eth[k], sep = ".")] <- voter.file[paste("p", 
                                                                       eth[k], sep = "_")]/apply(voter.file[paste("p", 
                                                                                                                  eth, sep = "_")], 1, sum)
    }
    pred <- paste("pred", eth, sep = ".")
    return(voter.file[c(vars.orig, pred)])
  }
  if (missing(party) == F) {
    voter.file$PID <- voter.file[, party]
    voter.file <- merge(voter.file, get("pid")[names(get("pid")) %in% 
                                                 "party" == F], by = "PID", all.x = T)
  }
  if (census.geo == "place") {
    if (!("place" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named place.")
    }
    voter.file <- census_helper(key = census.key, voter.file = voter.file, 
                                states = "all", geo = "place", age = age, sex = sex, 
                                census.data = census.data, retry = retry)
  }
  if (census.geo == "block") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% 
                                               names(voter.file)) || !("block" %in% names(voter.file))) {
      stop("voter.file object needs to have columns named block, tract, and county.")
    }
    voter.file <- census_helper(key = census.key, voter.file = voter.file, 
                                states = "all", geo = "block", age = age, sex = sex, 
                                census.data = census.data, retry = retry)
  }
  if (census.geo == "precinct") {
    geo <- "precinct"
    stop("Error: census_helper function does not currently support precinct-level data.")
  }
  if (census.geo == "tract") {
    if (!("tract" %in% names(voter.file)) || !("county" %in% 
                                               names(voter.file))) {
      stop("voter.file object needs to have columns named tract and county.")
    }
    voter.file <- census_helper(key = census.key, voter.file = voter.file, 
                                states = "all", geo = "tract", age = age, sex = sex, 
                                census.data = census.data, retry = retry)
  }
  if (census.geo == "county") {
    if (!("county" %in% names(voter.file))) {
      stop("voter.file object needs to have a column named county.")
    }
    voter.file <- census_helper(key = census.key, voter.file = voter.file, 
                                states = "all", geo = "county", age = age, sex = sex, 
                                census.data = census.data, retry = retry)
  }
  if (missing(party)) {
    for (k in 1:length(eth)) {
      voter.file[paste("u", eth[k], sep = "_")] <- voter.file[paste("p", 
                                                                    eth[k], sep = "_")] * voter.file[paste("r", eth[k], 
                                                                                                           sep = "_")]
    }
    voter.file$u_tot <- apply(voter.file[paste("u", eth, 
                                               sep = "_")], 1, sum, na.rm = T)
    for (k in 1:length(eth)) {
      voter.file[paste("q", eth[k], sep = "_")] <- voter.file[paste("u", 
                                                                    eth[k], sep = "_")]/voter.file$u_tot
    }
  }
  if (missing(party) == F) {
    for (k in 1:length(eth)) {
      voter.file[paste("u", eth[k], sep = "_")] <- voter.file[paste("p", 
                                                                    eth[k], sep = "_")] * voter.file[paste("r", eth[k], 
                                                                                                           sep = "_")] * voter.file[paste("r_pid", eth[k], 
                                                                                                                                          sep = "_")]
    }
    voter.file$u_tot <- apply(voter.file[paste("u", eth, 
                                               sep = "_")], 1, sum, na.rm = T)
    for (k in 1:length(eth)) {
      voter.file[paste("q", eth[k], sep = "_")] <- voter.file[paste("u", 
                                                                    eth[k], sep = "_")]/voter.file$u_tot
    }
  }
  for (k in 1:length(eth)) {
    voter.file[paste("pred", eth[k], sep = ".")] <- voter.file[paste("q", 
                                                                     eth[k], sep = "_")]
  }
  pred <- paste("pred", eth, sep = ".")
  return(voter.file[c(vars.orig, pred)])
}


recode_race <- function(df){
  # recode the Race column in df (output from wru::predict_race())
  df %>%
    mutate(
      Race = fct_recode(
        Race,
        'Asian' = 'pred.asi',
        'Black' = 'pred.bla',
        'Hispanic' = 'pred.his',
        'Other' = 'pred.oth',
        'White' = 'pred.whi',
        'Other categories' = 'pred_sum_others'
      )
    )
}

recode_gender <- function(df){
  # recode the Race column in df (output from wru::predict_race())
  df %>%
    mutate(
      gender = fct_recode(
        gender, 
        'Female' = 'probability_female',
        'Male' = 'probability_male')
  )
}

gender_breakdown <- function(df, category = 'main', ...) {
  # plot stacked bargraphs for each gender, mean_prob by year
  my_plot <- df %>%
    recode_gender() %>%
    ggplot(aes(year, mean_prob, fill = fct_relevel(gender, c('Male', 'Female')))) +
    geom_bar(stat = 'identity', alpha = 0.9) +
    scale_fill_viridis_d(option = 'E', end = 0.8, direction = -1) +
    # scale_x_continuous(breaks = seq(start_year, end_year, 4)) +
    # coord_cartesian(xlim = c(start_year, end_year)) +
    theme(legend.position = 'None') +
    labs(x = NULL) +
    facet_wrap(vars(...), ncol = 1)      +
    scale_x_date(
      labels = scales::date_format("%Y"),
      expand = c(0, 0),
      limits = c(
        ymd(start_year - 1, truncated = 2L),
        ymd(end_year, truncated = 2L)
      )
    )
  
  if (category == 'main') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.2)
      ) +
      labs(y = 'Estimated composition')
  } else if (category == 'sub') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.5)
      ) +
      labs(y = NULL)
  }
  my_plot
}


region_breakdown <- function(df, category = 'main', ...) {
  # plot stacked bargraphs for each gender, mean_prob by year
  my_plot <- df %>%
    ggplot(aes(year, mean_prob, fill = fct_relevel(region, region_levels))) +
    geom_bar(stat = 'identity', alpha = 0.9) +
    # scale_fill_viridis_d(option = 'A') +
    paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto', direction = -1) +
    theme(legend.position = 'None') +
    labs(x = NULL) +
    facet_wrap(vars(...), ncol = 2) 
  
  if (category == 'main') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.2)
      ) +
      scale_x_date(
        labels = scales::date_format("%Y"),
        expand = c(0, 0),
        limits = c(
          ymd(start_year - 1, truncated = 2L),
          ymd(end_year, truncated = 2L)
        )
      ) +
      labs(y = 'Estimated composition')
  } else if (category == 'sub') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.5)
      ) +
      labs(y = NULL) +
      scale_x_date(
        labels = scales::date_format("'%y"),
        expand = c(0, 0),
        limits = c(
          ymd(start_year - 1, truncated = 2L),
          ymd(end_year, truncated = 2L)
        )
      )
  }
  my_plot
}



race_breakdown <- function(df, category = 'main', ...){
  # plot stacked bargraphs for each race, mean_prob by year
  my_plot <- df %>%
    ggplot(aes(year, mean_prob, fill = fct_relevel(Race, race_levels))) +
    geom_bar(stat = 'identity') +
    scale_fill_viridis_d(direction = -1) +
    # paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto_black') +
    # paletteer::scale_fill_paletteer_d('ggthemes::colorblind', direction = -1) +
    theme(legend.position = 'None') +
    labs(x = NULL) +
    facet_wrap(vars(...), ncol = 2)
  
  if (category == 'main') {
    my_plot <- my_plot +
      scale_x_date(
        labels = scales::date_format("%Y"),
        expand = c(0, 0),
        limits = c(ymd(start_year - 1, truncated = 2L), ymd(end_year, truncated = 2L))
      ) +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.2)
      ) +
      labs(y = 'Estimated composition')
  } else if (category == 'sub') {
    my_plot <- my_plot +
      scale_x_date(
        labels = scales::date_format("'%y"),
        expand = c(0, 0),
        limits = c(ymd(start_year - 1, truncated = 2L), ymd(end_year, truncated = 2L))
      ) +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.5)
      ) +
      labs(y = NULL)
  }
  
}

get_keynote_summary <- function(df){
  df %>% 
    filter(type != 'Pubmed authors') %>% 
    select(- c(probabilities, journal)) %>% 
    distinct()
}
