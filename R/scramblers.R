scramble_lgcl <- function(v) {
  tab <- table(v, useNA = 'ifany')
  props <- proportions(tab)
  sample(as.logical(names(props)), size = length(v), prob = props, replace = TRUE)
}

scramble_int <- function(v) {
  if (inherits(v, 'factor')) { v <- as.integer(v); fact <- TRUE } else { fact <- FALSE }
  prop_na <- sum(is.na(v)) / length(v)
  samp_vec <- c(min(v[is.infinite(v) == F], na.rm = TRUE):max(v[is.infinite(v) == F], na.rm = TRUE), NA)
  samp_probs <- c(rep((1-prop_na)/(length(samp_vec)-1), times = length(samp_vec)-1), prop_na)
  scrambled_vals <- sample(samp_vec, size = length(v), prob = samp_probs, replace = TRUE)
  if (fact) { scrambled_vals <- factor(scrambled_vals, levels = unique(scrambled_vals)) }
  scrambled_vals
}

scramble_dbl <- function(v) {
  prop_na <- sum(is.na(v)) / length(v)
  deepest_decimal_place <- max(nchar(gsub('^\\d*\\.?', '', v, perl = TRUE)), na.rm = TRUE)
  scrambled_vals <- stats::runif(length(v), min(v[is.infinite(v) == F], na.rm = TRUE), max(v[is.infinite(v) == F], na.rm = TRUE))
  scrambled_vals[sample(c(TRUE, FALSE), size = length(scrambled_vals), prob = c(prop_na, 1-prop_na), replace = TRUE)] <- NA
  round(scrambled_vals, digits = deepest_decimal_place)
}

scramble_char <- function(v) {
  n_unique <- length(unique(v))
  tab <- table(v, useNA = 'ifany')
  props <- proportions(tab)
  is_na_present <- NA %in% names(props)
  med_nchar <- stats::median(nchar(v), na.rm = TRUE)
  sample_pool <- c(letters, LETTERS, 1:9)
  names(props) <- replicate(n_unique, paste0(sample(sample_pool, med_nchar, replace = TRUE), collapse = ''))
  # Note: It's possible to encounter a case where you try to generate N unique strings from a pool that
  # can't generate N unique strings; i.e., if choose(length(sample_pool), med_nchar) < n_unique
  # The while loop handles that case by extending duplicated names one random character at a time until all are unique
  while (any(duplicated(names(props)))) {
    names(props)[duplicated(names(props))] <- paste0(names(props)[duplicated(names(props))],
                                                     sample(sample_pool,
                                                            size = sum(duplicated(names(props))),
                                                            replace = TRUE))
  }
  # If NAs are present in the input, ensure those are sampled in the output with the right probability
  # (If NA is present in the input, its proportion is the last value in props)
  if (is_na_present) { names(props)[length(names(props))] <- NA }
  sample(names(props), size = length(v), prob = props, replace = TRUE)
}

scramble_unsure <- function(v) {
  if (interactive() == TRUE) {
    cat('\nPrincipled approach not defined for data type; returning random values from N(0, 1)\n')
  }
  round(stats::rnorm(length(v), mean = 0, sd = 1), digits = 2)
}
