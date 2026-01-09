# R/census_helpers.R
# Helper functions for reproducible, low-traffic Census + inflation workflows
# All functions use explicit namespace syntax (package::function)
#
# Required packages:
#   tidycensus, tigris, dplyr, purrr, tibble, stringr, readr,
#   digest, lubridate, quantmod, zoo, rlang, ggplot2, flextable
#
# Author: Dan
# -----------------------------------------------------------------------------

# =============================================================================
# CACHING UTILITIES
# =============================================================================

ds_cache_path <- function(cache_dir, key, ext = "rds") {

  base::dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)

  base::file.path(cache_dir, base::paste0(key, ".", ext))
}

ds_cache_key <- function(prefix, params) {
  digest::digest(
    base::list(prefix = prefix, params = params),
    algo = "xxhash64"
  )
}

ds_cache_read <- function(path) {
  if (base::file.exists(path)) base::readRDS(path) else NULL
}

ds_cache_write <- function(x, path) {

  base::dir.create(base::dirname(path), showWarnings = FALSE, recursive = TRUE)
  base::saveRDS(x, path)
  base::invisible(x)
}

# =============================================================================
# CENSUS API KEY
# =============================================================================

ds_set_census_key <- function(key = NULL) {


#' Set Census API key for tidycensus
#'
#' Checks environment variable CENSUS_API_KEY first, then falls back to
#' provided key parameter. Does NOT install to ~/.Renviron.

  if (base::is.null(key) || !base::nzchar(key)) {
    key <- base::Sys.getenv("CENSUS_API_KEY")
  }
  if (!base::nzchar(key)) {
    base::stop(
      "No Census API key found. ",
      "Set CENSUS_API_KEY in .Renviron or pass key parameter."
    )
  }
  tidycensus::census_api_key(key, install = FALSE, overwrite = TRUE)
  base::invisible(TRUE)
}

# =============================================================================
# STATE / PLACE NAME UTILITIES
# =============================================================================

ds_get_state_fips <- function(state_abbr) {

#' Get state FIPS code from abbreviation
#'
#' @param state_abbr Two-letter state abbreviation (e.g., "TX")
#' @return Two-digit FIPS code as character (e.g., "48")

  fips_tbl <- tigris::fips_codes
  match_row <- fips_tbl[fips_tbl$state == state_abbr, ]
  if (base::nrow(match_row) == 0L) {
    base::stop("Unknown state abbreviation: ", state_abbr)
  }
  unique_fips <- base::unique(match_row$state_code)
  if (base::length(unique_fips) != 1L) {
    base::stop("Ambiguous FIPS for state: ", state_abbr)
  }
  base::message("Using FIPS code '", unique_fips, "' for state '", state_abbr, "'")
  unique_fips
}

ds_get_state_name <- function(state_abbr) {

#' Get full state name from abbreviation
#'
#' @param state_abbr Two-letter state abbreviation (e.g., "TX")
#' @return Full state name (e.g., "Texas")

  fips_tbl <- tigris::fips_codes
  match_row <- fips_tbl[fips_tbl$state == state_abbr, ]
  if (base::nrow(match_row) == 0L) {
    base::warning("Unknown state abbreviation: ", state_abbr, "; using abbr as-is")
    return(state_abbr)
  }
  base::unique(match_row$state_name)[1L]
}

ds_place_name <- function(city, state_abbr) {

#' Build Census place name string
#'
#' tidycensus returns NAME like "Schertz city, Texas".
#' This builds that string from city name and state abbreviation.
#'
#' @param city City name (e.g., "Schertz")
#' @param state_abbr Two-letter state abbreviation (e.g., "TX")
#' @return Place name string (e.g., "Schertz city, Texas")

  state_full <- ds_get_state_name(state_abbr)
  base::paste0(city, " city, ", state_full)
}

# =============================================================================
# PEP POPULATION (2010-2024)
# =============================================================================

ds_get_pep_population_place <- function(
    city,
    state,
    years,
    cache_dir = "cache/census"
) {

#' Get PEP annual population estimates for a place
#'
#' Uses tidycensus::get_estimates() for all years:
#'   - 2010-2019: vintage = 2019 with time_series = TRUE
#'   - 2020-2024: vintage = 2024 (tidycensus reads Census flat files)
#'
#' @param city City name (e.g., "Schertz")
#' @param state Two-letter state abbreviation (e.g., "TX")
#' @param years Integer vector of years to retrieve
#' @param cache_dir Directory for caching results
#' @return Tibble with columns: source, series, year, estimate, moe

  years <- base::sort(base::unique(base::as.integer(years)))
  place_nm <- ds_place_name(city, state)

  # -------------------------------------------------------------------------
  # Part A: 2010-2019 via tidycensus time series (vintage 2019)
  # -------------------------------------------------------------------------
  years_2010s <- years[years >= 2010L & years <= 2019L]
  out_2010s <- tibble::tibble()

  if (base::length(years_2010s) > 0L) {
    key_a <- ds_cache_key("pep_2010s_v2019", base::list(city = city, state = state))
    path_a <- ds_cache_path(cache_dir, key_a)
    cached_a <- ds_cache_read(path_a)

    if (!base::is.null(cached_a)) {
      out_2010s <- cached_a
    } else {
      raw_ts <- tidycensus::get_estimates(
        geography   = "place",
        product     = "population",
        state       = state,
        vintage     = 2019L,
        time_series = TRUE
      )

      # Filter to target place
      place_data <- raw_ts[raw_ts$NAME == place_nm, ]
      if (base::nrow(place_data) == 0L) {
        base::warning(
          "No PEP 2010s data found for '", place_nm, "'. ",
          "Available places: ", base::paste(base::head(base::unique(raw_ts$NAME), 5), collapse = ", ")
        )
        out_2010s <- tibble::tibble()
      } else {
        out_2010s <- tibble::tibble(
          source   = "PEP",
          series   = "population",
          year     = 2009L + base::as.integer(place_data$DATE),
          estimate = base::as.numeric(place_data$value),
          moe      = NA_real_
        )
        out_2010s <- out_2010s[out_2010s$year %in% years_2010s, ]
        out_2010s <- out_2010s[base::order(out_2010s$year), ]
        ds_cache_write(out_2010s, path_a)
      }
    }
  }

  # -------------------------------------------------------------------------
  # Part B: 2020-2024 via tidycensus (vintage 2024)
  # tidycensus reads Census flat files directly for post-2020 data
  # -------------------------------------------------------------------------
  years_2020s <- years[years >= 2020L & years <= 2024L]
  out_2020s <- tibble::tibble()

  if (base::length(years_2020s) > 0L) {
    key_b <- ds_cache_key("pep_2020s_v2024", base::list(city = city, state = state))
    path_b <- ds_cache_path(cache_dir, key_b)
    cached_b <- ds_cache_read(path_b)

    if (!base::is.null(cached_b)) {
      out_2020s <- cached_b
    } else {
      # Get all available years from vintage 2024
      raw_2020s <- tidycensus::get_estimates(
        geography = "place",
        variables = "POPESTIMATE",
        state     = state,
        vintage   = 2024L
      )

      # Filter to target place
      place_data <- raw_2020s[raw_2020s$NAME == place_nm, ]
      if (base::nrow(place_data) == 0L) {
        # Try partial match on city name
        place_data <- raw_2020s[base::grepl(
          base::paste0("^", city, " "),
          raw_2020s$NAME,
          ignore.case = TRUE
        ), ]
      }

      if (base::nrow(place_data) == 0L) {
        base::warning(
          "No PEP 2020s data found for '", place_nm, "'. ",
          "Check city name spelling."
        )
        out_2020s <- tibble::tibble()
      } else {
        # For vintage 2024, the year column contains the estimate year
        out_2020s <- tibble::tibble(
          source   = "PEP",
          series   = "population",
          year     = base::as.integer(place_data$year),
          estimate = base::as.numeric(place_data$value),
          moe      = NA_real_
        )
        out_2020s <- out_2020s[out_2020s$year %in% years_2020s, ]
        out_2020s <- out_2020s[base::order(out_2020s$year), ]
        ds_cache_write(out_2020s, path_b)
      }
    }
  }

  # Combine and return
  result <- dplyr::bind_rows(out_2010s, out_2020s)
  result[base::order(result$year), ]
}

# =============================================================================
# ACS 5-YEAR DATA
# =============================================================================

ds_get_acs_place <- function(
    city,
    state,
    years,
    variables,
    survey = "acs5",
    cache_dir = "cache/census"
) {

#' Get ACS 5-year estimates for a place
#'
#' @param city City name
#' @param state Two-letter state abbreviation
#' @param years Integer vector of end-years for ACS 5-year estimates
#' @param variables Named or unnamed character vector of ACS variable codes
#' @param survey Survey type (default "acs5")
#' @param cache_dir Cache directory
#' @return Tibble with columns: source, series, year, estimate, moe

  place_nm <- ds_place_name(city, state)

  purrr::map_dfr(years, function(y) {
    key <- ds_cache_key(
      "acs_place",
      base::list(city = city, state = state, year = y, variables = variables, survey = survey)
    )
    path <- ds_cache_path(cache_dir, key)
    cached <- ds_cache_read(path)
    if (!base::is.null(cached)) return(cached)

    raw <- tidycensus::get_acs(
      geography   = "place",
      variables   = variables,
      year        = y,
      survey      = survey,
      state       = state,
      cache_table = TRUE
    )

    place_data <- raw[raw$NAME == place_nm, ]
    if (base::nrow(place_data) == 0L) {
      base::warning("No ACS data for '", place_nm, "' in year ", y)
      return(tibble::tibble())
    }

    out <- tibble::tibble(
      source   = base::paste0("ACS_", survey),
      series   = place_data$variable,
      year     = y,
      estimate = place_data$estimate,
      moe      = place_data$moe
    )

    ds_cache_write(out, path)
  })
}

# =============================================================================
# DECENNIAL CENSUS
# =============================================================================

ds_get_decennial_place <- function(
    city,
    state,
    year,
    variable,
    sumfile,
    cache_dir = "cache/census"
) {

#' Get Decennial Census data for a place
#'
#' @param city City name
#' @param state Two-letter state abbreviation
#' @param year Census year (2000, 2010, or 2020)
#' @param variable Variable code (e.g., "P001001" for 2010, "P1_001N" for 2020)
#' @param sumfile Summary file ("sf1" for 2000/2010, "pl" for 2020)
#' @param cache_dir Cache directory
#' @return Tibble with columns: source, series, year, estimate, moe

  place_nm <- ds_place_name(city, state)

  key <- ds_cache_key(
    "decennial_place",
    base::list(city = city, state = state, year = year, variable = variable, sumfile = sumfile)
  )
  path <- ds_cache_path(cache_dir, key)
  cached <- ds_cache_read(path)
  if (!base::is.null(cached)) return(cached)

  raw <- tidycensus::get_decennial(
    geography   = "place",
    variables   = variable,
    year        = year,
    sumfile     = sumfile,
    state       = state,
    cache_table = TRUE
  )

  place_data <- raw[raw$NAME == place_nm, ]
  if (base::nrow(place_data) == 0L) {
    base::warning("No Decennial data for '", place_nm, "' in year ", year)
    return(tibble::tibble())
  }

  out <- tibble::tibble(
    source   = base::paste0("Decennial_", year),
    series   = variable,
    year     = base::as.integer(year),
    estimate = base::as.numeric(place_data$value),
    moe      = NA_real_
  )

  ds_cache_write(out, path)
}

# =============================================================================
# CPI / INFLATION
# =============================================================================

ds_get_cpi_annual <- function(
    start_year,
    end_year,
    base_year = end_year,
    cache_dir = "cache/inflation"
) {

#' Get CPI-U annual averages and inflation factors
#'
#' Downloads CPIAUCSL from FRED via quantmod, computes annual averages,
#' and calculates conversion factors to base_year dollars.
#'
#' @param start_year First year
#' @param end_year Last year
#' @param base_year Year for constant dollars (default = end_year)
#' @param cache_dir Cache directory
#' @return Tibble with columns: year, cpi_annual_avg, base_year, factor_to_base

  key <- ds_cache_key(
    "cpi_annual",
    base::list(start_year = start_year, end_year = end_year, base_year = base_year)
  )
  path <- ds_cache_path(cache_dir, key)
  cached <- ds_cache_read(path)
  if (!base::is.null(cached)) return(cached)

  xt <- quantmod::getSymbols("CPIAUCSL", src = "FRED", auto.assign = FALSE)
  df <- tibble::tibble(
    date = base::as.Date(zoo::index(xt)),
    cpi  = base::as.numeric(xt[, 1L])
  )
  df$year <- lubridate::year(df$date)
  df <- df[df$year >= start_year & df$year <= end_year, ]

  annual <- dplyr::summarise(
    dplyr::group_by(df, year),
    cpi_annual_avg = base::mean(cpi, na.rm = TRUE),
    .groups = "drop"
  )

  base_cpi <- annual$cpi_annual_avg[annual$year == base_year]
  if (base::length(base_cpi) != 1L) {
    base::stop("Base year CPI not found for year ", base_year)
  }

  annual$base_year <- base_year
  annual$factor_to_base <- base_cpi / annual$cpi_annual_avg

  ds_cache_write(annual, path)
}

ds_adjust_to_base_dollars <- function(
    df,
    value_col = "estimate",
    year_col = "year",
    cpi_tbl,
    out_col = "estimate_real"
) {

#' Adjust dollar values to constant base-year dollars
#'
#' @param df Data frame with values to adjust
#' @param value_col Name of column containing nominal values
#' @param year_col Name of column containing years
#' @param cpi_tbl CPI table from ds_get_cpi_annual()
#' @param out_col Name of output column for real values
#' @return Data frame with added column for inflation-adjusted values

  cpi_slim <- cpi_tbl[, c("year", "factor_to_base")]
  base::names(cpi_slim)[1L] <- year_col

  merged <- dplyr::left_join(df, cpi_slim, by = year_col)
  merged[[out_col]] <- base::as.numeric(merged[[value_col]]) * merged$factor_to_base
  merged
}

# =============================================================================
# SPC / XmR CHARTING
# =============================================================================

ds_xmr_limits <- function(x) {

#' Calculate XmR (Individuals) control chart limits
#'
#' UCL/LCL = X̄ ± 2.66 × MR̄
#' (E(MR) = d2×σ with d2=1.128; 3σ → 3/d2 ≈ 2.66)
#'
#' @param x Numeric vector of individual values
#' @return List with xbar, ucl, lcl, mrbar

  x <- base::as.numeric(x)
  xbar <- base::mean(x, na.rm = TRUE)
  mr <- base::abs(base::diff(x))
  mrbar <- base::mean(mr, na.rm = TRUE)
  ucl <- xbar + 2.66 * mrbar
  lcl <- xbar - 2.66 * mrbar
  base::list(xbar = xbar, ucl = ucl, lcl = lcl, mrbar = mrbar)
}

ds_plot_xmr <- function(
    df,
    year_col = "year",
    value_col = "estimate",
    title = NULL
) {

#' Create XmR control chart
#'
#' @param df Data frame with time series data
#' @param year_col Name of year/time column
#' @param value_col Name of value column
#' @param title Chart title (optional)
#' @return ggplot2 object

  d <- df[base::order(df[[year_col]]), ]
  d$val <- base::as.numeric(d[[value_col]])

  lim <- ds_xmr_limits(d$val)

  ggplot2::ggplot(d, ggplot2::aes(x = .data[[year_col]], y = .data$val)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_hline(yintercept = lim$xbar, linetype = "dashed", color = "blue") +
    ggplot2::geom_hline(yintercept = lim$ucl, linetype = "dotted", color = "red") +
    ggplot2::geom_hline(yintercept = lim$lcl, linetype = "dotted", color = "red") +
    ggplot2::labs(
      title   = title %||% "XmR (Individuals) Chart",
      x       = "Year",
      y       = value_col,
      caption = "Limits: X\u0304 \u00b1 2.66\u00d7MR\u0304 (Individuals/Moving Range)"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

# =============================================================================
# FLEXTABLE STYLING
# =============================================================================

ds_style_flextable <- function(ft, title = NULL) {

#' Apply standard flextable styling
#'
#' Styling per Dan's preferences:
#'   - Title row: blue italic text, white background
#'   - Header row: pale green background
#'   - Auto-fit column widths
#'
#' @param ft A flextable object
#' @param title Optional title string to add as header
#' @return Styled flextable object

  if (!base::is.null(title)) {
    ft <- flextable::add_header_lines(ft, values = title)
    ft <- flextable::color(ft, i = 1, part = "header", color = "blue")
    ft <- flextable::italic(ft, i = 1, part = "header")
    ft <- flextable::align(ft, i = 1, part = "header", align = "left")
    ft <- flextable::fontsize(ft, i = 1, part = "header", size = 12)
    ft <- flextable::bg(ft, i = 1, part = "header", bg = "white")
    ft <- flextable::bg(ft, i = 2, part = "header", bg = "palegreen")
  } else {
    ft <- flextable::bg(ft, i = 1, part = "header", bg = "palegreen")
  }

  ft <- flextable::autofit(ft)
  ft
}
