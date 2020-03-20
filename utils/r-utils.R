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

recode_region <- function(df){
  # recode the Race column in df (output from wru::predict_race())
  df %>%
    mutate(
      region = fct_recode(
        region, 
        'Celtic English' = 'CelticEnglish',
        'East Asian' = 'EastAsian',
        'South Asian' = 'SouthAsian',
        'Israeli' = 'Israel',
        'Arabic' = 'Muslim',
        'African' = 'Africa',
        'Other categories' = 'OtherCategories')
    )
}

recode_region_letter <- function(df){
  # recode the Race column in df (output from wru::predict_race())
  df %>%
    mutate(
      region = fct_recode(
        region, 
        'A' = 'CelticEnglish',
        'C' = 'EastAsian',
        'E' = 'SouthAsian',
        'G' = 'Israel',
        'H' = 'African',
        'B' = 'European',
        'D' = 'Hispanic',
        'F' = 'Muslim',
        'O' = 'OtherCategories')
    )
}

gender_breakdown <- function(df, category = 'main', ...) {
  # plot stacked bargraphs for each gender, mean_prob by year
  my_plot <- df %>%
    recode_gender() %>%
    ggplot(aes(year, mean_prob, fill = fct_relevel(gender, c('Male', 'Female')))) +
    geom_bar(stat = 'identity', alpha = 0.9) +
    scale_fill_viridis_d(drop = F, option = 'E', end = 0.8, direction = -1) +
    labs(x = NULL)
  
  if (category == 'main') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.2)
      ) +
      labs(y = 'Estimated composition') +
      facet_wrap(vars(...), nrow = 1, scales = 'free_x') +
      theme(legend.position = 'bottom',
            legend.key.height = unit(3, 'mm'),
            legend.key.width = unit(6, 'mm'),
            # legend.text = element_text(size = 8),
            legend.margin = margin(-0.2, 0, 0, 0, unit='cm')) +
      scale_x_date(
        labels = scales::date_format("%Y"),
        expand = c(0, 0)
      )
  } else if (category == 'sub') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.5)
      ) +
      labs(y = NULL) +
      facet_wrap(vars(...), nrow = 1) +
      scale_x_date(
        labels = scales::date_format("'%y"),
        expand = c(0, 0),
        limits = c(
          ymd(start_year - 1, truncated = 2L),
          ymd(end_year, truncated = 2L)
        )) +
      theme(legend.position = 'None')
  }
  my_plot
}

region_breakdown <- function(df, category = 'main', ...) {
  # plot stacked bargraphs for each region, mean_prob by year
  my_plot <- df %>%
    recode_region_letter() %>% 
    ggplot(aes(year, mean_prob, fill = fct_relevel(region, region_levels_let))) +
    geom_bar(stat = 'identity') +
    # paletteer::scale_fill_paletteer_d('colorblindr::OkabeIto', direction = -1) +
    # scale_fill_brewer(palette = 'Set3') +
    scale_fill_manual(
      drop = FALSE,
      values = c('#ffffb3', '#fccde5', '#b3de69', '#fdb462', '#80b1d3', '#8dd3c7', '#bebada', '#fb8072')) +
    theme(legend.position = 'None',
          panel.grid.minor = element_blank()) +
    labs(x = NULL) 
  
  if (category == 'main') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.2)
      ) +
      scale_x_date(
        labels = scales::date_format("%Y"),
        expand = c(0, 0)
      ) +
      labs(y = 'Estimated composition') +
      facet_wrap(vars(...), ncol = 2, scales = 'free_x') 
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
        expand = c(0, 0)
      ) +
      facet_wrap(vars(...), nrow = 1) 
  }
  my_plot
}

affiliation_breakdown <- function(df, category = 'main', ...) {
  # plot stacked bargraphs for each region, mean_prob by year
  my_plot <- df %>%
    mutate(region = fct_expand(region, region_levels) %>% fct_relevel(region_levels)) %>% 
    ggplot(aes(year, mean_prob, fill = region)) +
    geom_bar(stat = 'identity') +
    scale_fill_manual(
      drop = FALSE,
      values = c('#ffffb3', '#fccde5', '#b3de69', '#fdb462', '#80b1d3', '#8dd3c7', '#bebada', '#fb8072')) +
    theme(legend.position = 'None',
          panel.grid.minor = element_blank()) +
    labs(x = NULL) 
  
  if (category == 'main') {
    my_plot <- my_plot +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.2)
      ) +
      scale_x_date(
        labels = scales::date_format("%Y"),
        expand = c(0, 0)
      ) +
      labs(y = 'Estimated composition') +
      facet_wrap(vars(...), ncol = 2, scales = 'free_x') 
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
        expand = c(0, 0)
      ) +
      facet_wrap(vars(...), nrow = 1) 
  }
  my_plot
}



race_breakdown <- function(df, category = 'main', ...){
  # plot stacked bargraphs for each race, mean_prob by year
  my_plot <- df %>%
    ggplot(aes(year, mean_prob, fill = fct_relevel(Race, race_levels))) +
    geom_bar(stat = 'identity') +
    # scale_fill_viridis_d(direction = -1) +
    # scale_fill_brewer(palette = 'Set3') +
    scale_fill_manual(
      values = c('#ffffb3', '#b3de69','#fb8072', '#fdb462', '#80b1d3',  '#8dd3c7', '#bebada')) +
    theme(legend.position = 'None', 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank()) +
    labs(x = NULL) +
    facet_wrap(vars(...), ncol = 2)
  
  if (category == 'main') {
    my_plot <- my_plot +
      scale_x_date(
        labels = scales::date_format("%Y"),
        expand = c(0, 0),
        limits = c(ymd(start_year, truncated = 2L), ymd(end_year, truncated = 2L))
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
        limits = c(ymd(start_year, truncated = 2L), ymd(end_year, truncated = 2L))
      ) +
      scale_y_continuous(
        labels = scales::percent_format(),
        expand = c(0, 0),
        breaks = seq(0, 1, 0.5)
      ) +
      labs(y = NULL) +
      theme(legend.position = c(0.75, 0.25),
            legend.key.height = unit(4, 'mm'),
            legend.background = element_rect(fill = "transparent", colour = "transparent"))
  }
  
}

get_keynote_summary <- function(df){
  df %>% 
    filter(type != 'Pubmed authors') %>% 
    select(- c(probabilities, journal)) %>% 
    distinct()
}

loess_and_ci <- function(df){
  df %>% 
    ggplot(aes(group = type)) +
    geom_smooth(size = 0.2, data = . %>% filter(type == 'Pubmed authors'),
                aes(x = publication_date, y = probabilities, 
                    fill = type, 
                    color = type)) +
    geom_pointrange(data = . %>% get_keynote_summary(),
                    alpha = 0.75, size = 0.3,
                    aes(x = year, y = mean_prob, shape = type,
                        ymin = mean_prob - me_prob,
                        ymax = mean_prob + me_prob)) +
    scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format()) +
    scale_x_date(labels = scales::date_format("'%y"), breaks = '5 years',
                 limits = c(ymd(start_year, truncated = 2L),
                            ymd(end_year, truncated = 2L))) +
    scale_color_manual(values = '#3fa392') +
    scale_fill_manual(values = '#3fa392') +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = NULL, y = 'Estimated composition') +
    theme(panel.grid.minor = element_blank(),
          legend.margin = margin(-0.5, 0, 0, 0, unit='cm'))
}

my_riskratio <- function (x, y = NULL, conf.level = 0.95, rev = c("neither", 
                                                                  "rows", "columns", "both"), correction = FALSE, verbose = FALSE) 
  # with continuity correction:
  # https://stats.stackexchange.com/questions/21298/confidence-interval-around-the-ratio-of-two-proportions
  # http://www.zen103156.zen.co.uk/rr.pdf
  # https://stats.stackexchange.com/questions/3112/calculation-of-relative-risk-confidence-interval
  {
  if (is.matrix(x) && !is.null(y)) {
    stop("y argument should be NULL")
  }
  if (is.null(y)) {
    x <- epitable(x, rev = rev)
  }
  else {
    x <- epitable(x, y, rev = rev)
  }
  tmx <- table.margins(x)
  p.exposed <- sweep(tmx, 2, tmx["Total", ], "/")
  p.outcome <- sweep(tmx, 1, tmx[, "Total"], "/")
  Z <- qnorm(0.5 * (1 + conf.level))
  nr <- nrow(x)
  small <- matrix(NA, nr, 3)
  small[1, 1] <- 1
  for (i in 2:nr) {
    a0 <- x[1, 2]
    b0 <- x[1, 1]
    a1 <- x[i, 2]
    b1 <- x[i, 1]
    n1 <- a1 + b1
    n0 <- a0 + b0
    m0 <- b0 + b1
    m1 <- a0 + a1
    # for US:
    # nested_obs_exp %>% 
    #   filter(country_name == 'United States') %>% 
    #   ungroup() %>% 
    #   select(-country_name) %>% 
    #   unlist() %>%
    #   matrix(ncol = 2, byrow = TRUE) %>%
    #   my_riskratio(correction = TRUE)
    # 237.5/(8804.316/22714*394)
    # print('a1 should be 237.5, n1 should be 394')
    # print('a0 should be 8804.316, n0 should be 22714')
    # cat('a1 = ', a1)
    # cat('n1 = ', n1)
    # cat('a0 = ', a0)
    # cat('n0 = ', n0)
    est <- (a1/n1)/((a0)/(n0))
    logRR <- log(est)
    # choose delta = 0.1 for only a slight correction, avoid divide by 0
    SElogRR <- sqrt(1/(a1+0.1) - 1/(n1) + 1/(a0+0.1) - 1/(n0))
    ci <- exp(logRR + c(-1, 1) * Z * SElogRR)
    small[i, ] <- c(est, ci)
  }
  pv <- tab2by2.test(x, correction = correction)
  colnames(small) <- c("estimate", "lower", "upper")
  rownames(small) <- rownames(x)
  cn2 <- paste("risk ratio with", paste(100 * conf.level, "%", 
                                        sep = ""), "C.I.")
  names(dimnames(small)) <- c(names(dimnames(x))[1], cn2)
  rr <- list(x = x, data = tmx, p.exposed = p.exposed, p.outcome = p.outcome, 
             measure = small, conf.level = conf.level, p.value = pv$p.value, 
             correction = pv$correction)
  rrs <- list(data = tmx, measure = small, p.value = pv$p.value, 
              correction = pv$correction)
  attr(rr, "method") <- "small sample-adjusted UMLE & normal approx (Wald) CI"
  attr(rrs, "method") <- "small sample-adjusted UMLE & normal approx (Wald) CI"
  if (verbose == FALSE) {
    rrs
  }
  else rr
}

