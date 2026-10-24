## Set-up
# Rebuild the five-authority A/B/C analytic samples from BES wave files and
# the dated LA lookup. No prior RDS files, fitted models or Azure paths required.
# Open repo.Rproj first, then source("scripts/01_prepare_data.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(haven)
  library(basicspace)
})

bes_dir <- "data"
lookup_file <- file.path("data", "lookup", "la_mayoral_lookup.csv")
out_dir <- file.path("data", "derived")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (f in c(file.path(bes_dir, paste0("w", c(12, 21, 26), ".sav")), lookup_file)) {
  if (!file.exists(f)) stop("Required input not found: ", f)
}

## Variables retained for EDA and modelling
# Add BES names here to retain additional source variables in bes_mayors_long.rds.
# Required modelling fields are checked below; optional EDA variables use any_of().
vars_geo <- c("oslaua", "gor", "pcon", "pcon_code", "new_pcon", "new_pcon_code",
              "country")
vars_vote <- c("generalElectionVote", "localElectionVote", "localTurnoutRetro",
               "turnoutUKGeneral", "mayorTurnout", "londonTurnout",
               "partyId", "partyIdStrength", "partyIdSqueeze")
vars_like <- c("likeLab", "likeCon", "likeLD", "likeSNP", "likePC", "likeGrn",
               "likeCorbyn", "likeStarmer", "likeMay", "likeJohnson", "likeSunak",
               "likeDavey", "likeSturgeon", "likeYousaf", "likeFarage",
               "likeFarron", "likePrice", "likeTice", "likeIorwerth")
vars_values <- c(paste0("al", 1:5), paste0("lr", 1:5), "al_scale", "lr_scale")
vars_party_pos <- c("leftRight", "lrCon", "lrLab", "lrLD", "lrSNP", "lrPC",
                    "lrUKIP", "lrBrexit", "lrgreens", "immigSelf", "immigCon",
                    "immigLab", "immigLD", "immigSNP", "immigPC", "immigUKIP",
                    "immigBrexit", "immigGreen", "EUIntegrationSelf",
                    "EUIntegrationCon", "EUIntegrationLab", "EUIntegrationLD",
                    "EUIntegrationSNP", "EUIntegrationPC", "EUIntegrationUKIP",
                    "EUIntegrationBrexit", "EUIntegrationGreen", "immigEcon")
vars_issues <- c("mii", "mii_cat", "mii_cat_llm", "bestOnMII",
                 "ptvCon", "ptvLab", "ptvLD", "ptvSNP", "ptvPC",
                 "ptvUKIP", "ptvBrexitParty", "ptvGrn")

vars_profile <- c("gender", "age", "ageGroup", "p_education", "p_work_stat",
                  "p_ethnicity2", "p_ethnicity", "p_gross_household",
                  "p_gross_personal", "p_job_sector", "p_hh_children", "p_hh_size",
                  "p_socgrade", "p_education_age", "p_housing", "p_marital",
                  "p_religion", "p_disability", "p_country_birth", "p_edlevel",
                  "p_edlevelUni", "p_eurefvote", "p_past_vote_2015",
                  "p_past_vote_2017", "p_past_vote_2019", "p_past_vote_2024",
                  "p_turnout_2015", "p_turnout_2017", "p_turnout_2019",
                  "p_turnout_2024", "p_sexuality", "p_parent", "p_paper_read")
vars_mayor_2017 <- c("manchesterFirst", "manchesterSecond", "liverpoolFirst",
                     "liverpoolSecond", "teesFirst", "teesSecond", "wmidsFirst",
                     "wmidsSecond", "westFirst", "westSecond", "doncasterFirst",
                     "doncasterSecond", "tyneFirst", "tyneSecond")
vars_mayor_2021 <- c("londonFirstChoice", "londonSecondChoice",
                     "cambridgeFirstChoice", "cambridgeSecondChoice",
                     "gmFirstChoice", "gmSecondChoice",
                     "liverpoolRegionFirstChoice", "liverpoolRegionSecondChoice",
                     "teesFirstChoice", "teesSecondChoice",
                     "westMidFirstChoice", "westMidSecondChoice",
                     "westEngFirstChoice", "westEngSecondChoice",
                     "westYorkFirstChoice", "westYorkSecondChoice",
                     "northOfTyneFirstChoice", "northOfTyneSecondChoice",
                     "bristolFirstChoice", "bristolSecondChoice",
                     "doncasterFirstChoice", "doncasterSecondChoice", #city mayor
                     "liverpoolFirstChoice", "liverpoolSecondChoice",
                     "salfordFirstChoice", "salfordSecondChoice") # city mayor fyi
vars_mayor_2024 <- c("londonMayorVoteFPTP", "gmFPTP", "liverpoolRegionFPTP",
                     "teesFPTP", "westMidFPTP", "westYorkFPTP",
                     "northYorkFPTP", "eastMidFPTP", "northEastFPTP",
                     "southYorksFPTP")

# Economic circumstances and subjective class

economic_vars <- c(
  "econPersonalRetro",
  "econGenRetro",
  "econPersonalProsp",
  "subjClass",
  "subjClassSqueeze",
  "worryEconSecurity",
  "homeFinance"
)

vars_mayor <- c(vars_mayor_2017, vars_mayor_2021, vars_mayor_2024)
vars_all <- unique(c("wt", "wt_daily", "starttime", "endtime", vars_geo, economic_vars,
                     vars_vote, vars_like, vars_values, vars_party_pos,
                     vars_issues, vars_profile, vars_mayor))
## Historical ballots and source validation
# These are 14 documented five-authority election contests, not generic party votes.
# The known ballot coding Labour=2 is checked against original labelled fields.
ballots <- tibble::tribble(
  ~wave, ~ca, ~ballot,
  12L, "Greater Manchester", "manchesterFirst",
  12L, "Liverpool City Region", "liverpoolFirst",
  12L, "Tees Valley", "teesFirst",
  12L, "West Midlands", "wmidsFirst",
  21L, "Greater Manchester", "gmFirstChoice",
  21L, "Liverpool City Region", "liverpoolRegionFirstChoice",
  21L, "Tees Valley", "teesFirstChoice",
  21L, "West Midlands", "westMidFirstChoice",
  21L, "West Yorkshire", "westYorkFirstChoice",
  26L, "Greater Manchester", "gmFPTP",
  26L, "Liverpool City Region", "liverpoolRegionFPTP",
  26L, "Tees Valley", "teesFPTP",
  26L, "West Midlands", "westMidFPTP",
  26L, "West Yorkshire", "westYorkFPTP"
)
ca_levels <- c("Greater Manchester", "Liverpool City Region", "Tees Valley",
               "West Midlands", "West Yorkshire")

read_wave <- function(w) {
  x <- haven::read_sav(file.path(bes_dir, paste0("w", w, ".sav")))
  required <- unique(c("id", "gor", "oslaua", "generalElectionVote", "mii", "wt",
    if (w == 26L) "mii_cat_llm" else "mii_cat",
    if (w == 12L) "likeCorbyn" else "likeStarmer",
    ballots$ballot[ballots$wave == w], paste0("al", 1:5), paste0("lr", 1:5)))
  absent <- setdiff(required, names(x))
  if (length(absent)) stop("w", w, ".sav is missing: ", paste(absent, collapse = ", "))

  # Preserve ballot response labels BEFORE stripping SPSS metadata for row binding.
  audit <- bind_rows(lapply(which(ballots$wave == w), function(i) {
    candidate <- x[[ballots$ballot[i]]]
    labour_label <- unique(as.character(haven::as_factor(candidate))[as.numeric(candidate) == 2])
    tibble(wave = w, ca = ballots$ca[i], ballot = ballots$ballot[i],
           labour_code = 2L, labour_label = paste(labour_label, collapse = "; "))
  }))
  if (any(!grepl("Labour", audit$labour_label, ignore.case = TRUE))) {
    stop("Ballot code 2 must have a Labour label in every mapped contest: wave ", w)
  }

  cleaned <- x |>
    mutate(id = as.character(id), wave = as.integer(w),
           region = as.character(haven::as_factor(gor)),
           mii_status = as.character(haven::as_factor(mii))) |>
    dplyr::select(id, wave, region, mii_status, any_of(vars_all)) |>
    mutate(across(-c(id, wave, region, mii_status), haven::zap_labels))
  if (anyDuplicated(cleaned$id)) stop("Duplicate respondent IDs in wave ", w)
  attr(cleaned, "ballot_audit") <- audit
  cleaned
}

w12 <- read_wave(12L)
w21 <- read_wave(21L)
w26 <- read_wave(26L)


ballot_audit <- bind_rows(attr(w12, "ballot_audit"), attr(w21, "ballot_audit"),
                         attr(w26, "ballot_audit"))


bes_long <- bind_rows(w12, w21, w26)
bes_long <- bes_long |>
  mutate(oslaua_code = as.character(oslaua),
         national_code = as.numeric(generalElectionVote),
         wt_raw = as.numeric(wt),
         leader_raw = if_else(wave == 12L, as.numeric(likeCorbyn), as.numeric(likeStarmer)),
         leader_name = if_else(wave == 12L, "Corbyn", "Starmer"),
         mii_code = if_else(wave == 26L, as.numeric(mii_cat_llm), as.numeric(mii_cat)),
         class_raw = if ("p_socgrade" %in% names(bes_long)) as.numeric(p_socgrade) else NA_real_)



## Geographic lookup and model population
# Blank assignments mean the authority is not mapped for that year.
# The retained BES table includes England-wide responses for the scaling model;
# the A/B/C sample is restricted to the five study CAs and 14 actual ballots.
la_lookup <- readr::read_csv(lookup_file, col_types = cols(.default = col_character()))
lookup_columns <- c("oslaua_code", "la_name", paste0("mayoral_authority_", c(2017, 2021, 2024)))
if (!all(lookup_columns %in% names(la_lookup))) stop("LA lookup has missing columns")
if (anyDuplicated(la_lookup$oslaua_code)) stop("Duplicate LA codes in lookup")

bes_long <- bes_long |>
  left_join(la_lookup, by = "oslaua_code", relationship = "many-to-one") |>
  mutate(
    mayoral_authority = case_when(
      wave == 12L ~ mayoral_authority_2017,
      wave == 21L ~ mayoral_authority_2021,
      wave == 26L ~ mayoral_authority_2024
    ),
    source_ballot = case_when(
      wave == 12L & mayoral_authority == "Greater Manchester" ~ "manchesterFirst",
      wave == 12L & mayoral_authority == "Liverpool City Region" ~ "liverpoolFirst",
      wave == 12L & mayoral_authority == "Tees Valley" ~ "teesFirst",
      wave == 12L & mayoral_authority == "West Midlands" ~ "wmidsFirst",
      wave == 21L & mayoral_authority == "Greater Manchester" ~ "gmFirstChoice",
      wave == 21L & mayoral_authority == "Liverpool City Region" ~ "liverpoolRegionFirstChoice",
      wave == 21L & mayoral_authority == "Tees Valley" ~ "teesFirstChoice",
      wave == 21L & mayoral_authority == "West Midlands" ~ "westMidFirstChoice",
      wave == 21L & mayoral_authority == "West Yorkshire" ~ "westYorkFirstChoice",
      wave == 26L & mayoral_authority == "Greater Manchester" ~ "gmFPTP",
      wave == 26L & mayoral_authority == "Liverpool City Region" ~ "liverpoolRegionFPTP",
      wave == 26L & mayoral_authority == "Tees Valley" ~ "teesFPTP",
      wave == 26L & mayoral_authority == "West Midlands" ~ "westMidFPTP",
      wave == 26L & mayoral_authority == "West Yorkshire" ~ "westYorkFPTP",
      TRUE ~ NA_character_
    ),
    # Candidate codes are not converted to party labels.
    mayor_code = case_when(
      source_ballot == "manchesterFirst" ~ as.numeric(manchesterFirst),
      source_ballot == "liverpoolFirst" ~ as.numeric(liverpoolFirst),
      source_ballot == "teesFirst" ~ as.numeric(teesFirst),
      source_ballot == "wmidsFirst" ~ as.numeric(wmidsFirst),
      source_ballot == "gmFirstChoice" ~ as.numeric(gmFirstChoice),
      source_ballot == "liverpoolRegionFirstChoice" ~ as.numeric(liverpoolRegionFirstChoice),
      source_ballot == "teesFirstChoice" ~ as.numeric(teesFirstChoice),
      source_ballot == "westMidFirstChoice" ~ as.numeric(westMidFirstChoice),
      source_ballot == "westYorkFirstChoice" ~ as.numeric(westYorkFirstChoice),
      source_ballot == "gmFPTP" ~ as.numeric(gmFPTP),
      source_ballot == "liverpoolRegionFPTP" ~ as.numeric(liverpoolRegionFPTP),
      source_ballot == "teesFPTP" ~ as.numeric(teesFPTP),
      source_ballot == "westMidFPTP" ~ as.numeric(westMidFPTP),
      source_ballot == "westYorkFPTP" ~ as.numeric(westYorkFPTP),
      TRUE ~ NA_real_
    ),
    national_labour = case_when(
      national_code == 2 ~ 1L,
      national_code %in% c(1, 3, 4, 5, 6, 7, 9, 12) ~ 0L,
      TRUE ~ NA_integer_
    ),
    mayor_labour = case_when(
      is.na(mayor_code) | mayor_code %in% c(0, 99, 222, 9998, 9999) ~ NA_integer_,
      mayor_code == 2 ~ 1L,
      TRUE ~ 0L
    ),
    leader_rating = if_else(leader_raw >= 0 & leader_raw <= 10,
                            leader_raw, NA_real_),
    mii_group = case_when(
      is.na(mii_status) | mii_status != "Issue stated" ~ NA_character_,
      mii_code %in% c(1, 2, 10, 26:39, 43, 44, 49) ~ "Economic",
      mii_code %in% c(7:9, 11:15, 21:23, 40:42, 50) ~ "Cultural",
      mii_code == 48 ~ "Coronavirus",
      mii_code %in% c(3:6, 16:19, 24:25, 45, 47) ~ "Other",
      TRUE ~ NA_character_
    ),
    social_grade4 = case_when(
      class_raw %in% 1:2 ~ "AB", class_raw == 3 ~ "C1",
      class_raw == 4 ~ "C2", class_raw %in% 5:6 ~ "DE"
    )
  ) |>
  dplyr::select(-starts_with("mayoral_authority_"))

## Blackbox ideology measurement


## Estimate Blackbox


## Blackbox ideology measurement

# Estimate two-dimensional basic space separately for 2017, 2021 and 2024.
# All eligible England respondents are used, before selecting mayoral authorities.

attitude_items <- c(paste0("al", 1:5), paste0("lr", 1:5))
al_items <- paste0("al", 1:5)
lr_items <- paste0("lr", 1:5)

england_regions <- c(
  "North East", "North West", "Yorkshire and the Humber",
  "East Midlands", "West Midlands", "East", "East of England",
  "London", "South East", "South West"
)

england <- bes_long |>
  filter(
    wave %in% c(12L, 21L, 26L),
    region %in% england_regions
  ) |>
  mutate(
    across(
      all_of(attitude_items),
      ~ if_else(as.numeric(.x) %in% 1:5, as.numeric(.x), NA_real_)
    )
  ) |>
  drop_na(all_of(attitude_items)) |>
  arrange(wave, id)

england_waves <- split(england, england$wave)

stopifnot(
  identical(names(england_waves), c("12", "21", "26")),
  !anyDuplicated(england[c("id", "wave")])
)


## Estimate Blackbox

# Retain both the fitted objects and respondent coordinates.
# The original dimension ordering and signs are preserved.

blackbox_fits <- purrr::imap(england_waves, \(dat, w) {
  
  issues <- dat |>
    dplyr::select(all_of(attitude_items)) |>
    as.matrix()
  
  storage.mode(issues) <- "double"
  rownames(issues) <- dat$id
  
  if (nrow(issues) < 100L) {
    stop("Insufficient English respondents in wave ", w)
  }
  
  fit <- basicspace::blackbox(
    issues,
    missing = NULL,
    dims = 2,
    minscale = 10,
    verbose = FALSE
  )
  
  coords <- as.matrix(fit$individuals[[2]])
  one_dim <- as.matrix(fit$individuals[[1]])
  
  stopifnot(
    nrow(coords) == nrow(dat),
    ncol(coords) >= 2L,
    nrow(one_dim) == nrow(dat)
  )
  
  list(
    fit = fit,
    scores = tibble(
      id = dat$id,
      wave = as.integer(w),
      raw_c1 = as.numeric(coords[, 1]),
      raw_c2 = as.numeric(coords[, 2]),
      lral = as.numeric(one_dim[, 1]),
      lr_raw = if (w == "26") -coords[, 2] else coords[, 2],
      al_raw = if (w == "21") coords[, 1] else -coords[, 1]
    )
  )
})

scores <- purrr::map_dfr(blackbox_fits, "scores")

if (anyDuplicated(scores[c("id", "wave")])) {
  stop("Non-unique Blackbox respondent-wave scores")
}


## Inspect dimensional fit

# Extract the dimensional fit statistics reported by basicspace.
# Preserve the original column names to avoid changing their definitions.

dimensional_fit <- purrr::imap_dfr(
  blackbox_fits,
  \(x, w) {
    as_tibble(x$fit$fits, rownames = "dimension") |>
      mutate(wave = as.integer(w), .before = 1)
  }
)

# Inspect weights and fit for individual attitude items.
# These are from the final two-dimensional solution.

item_fit <- purrr::imap_dfr(
  blackbox_fits,
  \(x, w) {
    as_tibble(x$fit$stimuli[[2]], rownames = "item") |>
      mutate(wave = as.integer(w), .before = 1)
  }
)

print(dimensional_fit, n = Inf, width = Inf)
print(item_fit, n = Inf, width = Inf)



## Validate and orient Blackbox coordinates


# As Blackbox axes can reverse between estimations, this is a check
# Identify the substantive dimensions using the BES reference scales:
# raw_c2 = economic; raw_c1 = cultural.
# Orient each dimension positively towards its corresponding reference scale.

orientation_checks <- england |>
  inner_join(scores, by = c("id", "wave"), relationship = "one-to-one") |>
  group_by(wave) |>
  summarise(
    n = sum(complete.cases(raw_c1, raw_c2, lr_scale, al_scale)),
    economic_c1 = cor(raw_c1, lr_scale, use = "complete.obs"),
    economic_c2 = cor(raw_c2, lr_scale, use = "complete.obs"),
    cultural_c1 = cor(raw_c1, al_scale, use = "complete.obs"),
    cultural_c2 = cor(raw_c2, al_scale, use = "complete.obs"),
    .groups = "drop"
  ) |>
  mutate(
    economic_axis_ok = abs(economic_c2) > abs(economic_c1),
    cultural_axis_ok = abs(cultural_c1) > abs(cultural_c2),
    lr_sign = if_else(economic_c2 >= 0, 1, -1),
    al_sign = if_else(cultural_c1 >= 0, 1, -1),
    economic_reference = economic_c2 * lr_sign,
    cultural_reference = cultural_c1 * al_sign
  )

print(orientation_checks, n = Inf, width = Inf)

# Axis exchanges require inspection: do not silently swap dimensions.
if (
  any(!is.finite(orientation_checks$economic_reference)) ||
  any(!is.finite(orientation_checks$cultural_reference)) ||
  any(!orientation_checks$economic_axis_ok) ||
  any(!orientation_checks$cultural_axis_ok)
) {
  stop("Blackbox dimension ordering requires inspection")
}

scores <- scores |>
  dplyr::select(-lr_raw, -al_raw) |>
  left_join(
    dplyr::select(orientation_checks, wave, lr_sign, al_sign),
    by = "wave",
    relationship = "many-to-one"
  ) |>
  mutate(
    lr_raw = raw_c2 * lr_sign,
    al_raw = raw_c1 * al_sign
  ) |>
  dplyr::select(-lr_sign, -al_sign)

# Keep the fitted-score objects consistent with the corrected orientation.
blackbox_fits <- purrr::imap(
  blackbox_fits,
  \(x, w) {
    x$scores <- scores |>
      filter(wave == as.integer(w))
    x
  }
)


## Bootstrap standard errors

# Resample attitude items, separately within the AL and LR batteries.
# Estimate two-dimensional basic space for each item resample.
# Align bootstrap coordinates to the original solution using
# orthogonal Procrustes rotation, including sign and axis exchanges.
#
# Standard errors are respondent-level standard deviations across
# bootstrap estimates. These measure item-selection uncertainty.

B <- 200L
set.seed(20261019)

sample_battery <- function(items) {
  repeat {
    chosen <- sample(items, length(items), replace = TRUE)
    
    if (length(unique(chosen)) >= 3L) {
      return(chosen)
    }
  }
}

align_coordinates <- function(boot, reference) {
  
  valid <- rowSums(is.finite(boot)) == 2L &
    rowSums(is.finite(reference)) == 2L
  
  if (sum(valid) < 100L) {
    stop("Insufficient finite coordinates for Procrustes alignment")
  }
  
  reference_mean <- colMeans(reference[valid, , drop = FALSE])
  bootstrap_mean <- colMeans(boot[valid, , drop = FALSE])
  
  R <- sweep(
    reference[valid, , drop = FALSE],
    2, reference_mean, "-"
  )
  
  Z <- sweep(
    boot[valid, , drop = FALSE],
    2, bootstrap_mean, "-"
  )
  
  rotation <- svd(crossprod(Z, R))
  Q <- rotation$u %*% t(rotation$v)
  
  aligned <- matrix(
    NA_real_,
    nrow = nrow(boot),
    ncol = 2L
  )
  
  aligned[valid, ] <- sweep(
    Z %*% Q,
    2,
    reference_mean,
    "+"
  )
  
  aligned
}


bootstrap_wave <- function(dat, original, w) {
  
  reference <- original$scores
  
  stopifnot(
    identical(as.character(dat$id), as.character(reference$id))
  )
  
  issues <- dat |>
    dplyr::select(all_of(attitude_items)) |>
    as.matrix()
  
  storage.mode(issues) <- "double"
  rownames(issues) <- dat$id
  
  reference_coords <- reference |>
    dplyr::select(raw_c1, raw_c2) |>
    as.matrix()
  
  draws <- array(
    NA_real_,
    dim = c(nrow(dat), 2L, B)
  )
  
  successful <- 0L
  failed <- 0L
  
  # Resample the ten questions 200 times for each wave.
  # Unsuccessful Blackbox fits are retried with a fresh item sample.
  
  for (b in seq_len(B)) {
    
    aligned <- NULL
    
    for (attempt in seq_len(10L)) {
      
      selected <- c(
        sample_battery(al_items),
        sample_battery(lr_items)
      )
      
      X <- issues[
        , match(selected, colnames(issues)),
        drop = FALSE
      ]
      
      colnames(X) <- make.unique(selected)
      
      aligned <- tryCatch({
        
        fit <- basicspace::blackbox(
          X,
          missing = NULL,
          dims = 2,
          minscale = 10,
          verbose = FALSE
        )
        
        coords <- as.matrix(fit$individuals[[2]])
        
        if (
          nrow(coords) != nrow(reference_coords) ||
          ncol(coords) < 2L
        ) {
          stop("Unexpected bootstrap coordinate dimensions")
        }
        
        align_coordinates(
          coords[, 1:2, drop = FALSE],
          reference_coords
        )
        
      }, error = function(e) NULL)
      
      if (!is.null(aligned)) {
        break
      }
    }
    
    if (is.null(aligned)) {
      failed <- failed + 1L
    } else {
      draws[, , b] <- aligned
      successful <- successful + 1L
    }
  }
  
  if (successful < B) {
    stop(
      "Only ", successful, " of ", B,
      " Blackbox bootstrap replications succeeded in wave ", w
    )
  }
  
  # Orientate against checks
  
  lr_sign <- orientation_checks$lr_sign[
    match(as.integer(w), orientation_checks$wave)
  ]
  
  al_sign <- orientation_checks$al_sign[
    match(as.integer(w), orientation_checks$wave)
  ]
  
  
  lr_draws <- matrix(
    draws[, 2, ],
    nrow = nrow(dat),
    ncol = B
  ) * lr_sign
  
  al_draws <- matrix(
    draws[, 1, ],
    nrow = nrow(dat),
    ncol = B
  ) * al_sign
  
  # Standard errors and normal-approximation 95% intervals.
  # Missing original coordinates remain missing.
  
  individual <- tibble(
    id = dat$id,
    wave = as.integer(w),
    lr_point = reference$lr_raw,
    al_point = reference$al_raw,
    lr_boot_mean = rowMeans(lr_draws, na.rm = TRUE),
    al_boot_mean = rowMeans(al_draws, na.rm = TRUE),
    lr_boot_se = apply(lr_draws, 1, sd, na.rm = TRUE),
    al_boot_se = apply(al_draws, 1, sd, na.rm = TRUE)
  ) |>
    mutate(
      lr_lower = lr_point - 1.96 * lr_boot_se,
      lr_upper = lr_point + 1.96 * lr_boot_se,
      al_lower = al_point - 1.96 * al_boot_se,
      al_upper = al_point + 1.96 * al_boot_se
    )
  
  summary <- individual |>
    summarise(
      wave = as.integer(w),
      n_respondents = n(),
      requested = B,
      successful = successful,
      failed = failed,
      lr_median_boot_se = median(lr_boot_se, na.rm = TRUE),
      al_median_boot_se = median(al_boot_se, na.rm = TRUE),
      lr_p90_boot_se = unname(
        quantile(lr_boot_se, .90, na.rm = TRUE)
      ),
      al_p90_boot_se = unname(
        quantile(al_boot_se, .90, na.rm = TRUE)
      )
    )
  
  list(
    individual = individual,
    summary = summary
  )
}


bootstrap_results <- purrr::imap(
  england_waves,
  \(dat, w) bootstrap_wave(dat, blackbox_fits[[w]], w)
)

bootstrap_individual <- purrr::map_dfr(
  bootstrap_results,
  "individual"
)

bootstrap_summary <- purrr::map_dfr(
  bootstrap_results,
  "summary"
)

print(bootstrap_summary, n = Inf, width = Inf)


## Standardise coordinates

# Positive econ_z denotes economic left.
# Positive auth_z denotes cultural authoritarianism.
# Use England-wide moments, separately for each wave.

scaling <- scores |>
  group_by(wave) |>
  summarise(
    lr_mean = mean(lr_raw, na.rm = TRUE),
    lr_sd = sd(lr_raw, na.rm = TRUE),
    al_mean = mean(al_raw, na.rm = TRUE),
    al_sd = sd(al_raw, na.rm = TRUE),
    .groups = "drop"
  )

if (
  any(!is.finite(unlist(dplyr::select(scaling, -wave)))) ||
  any(scaling$lr_sd <= 0 | scaling$al_sd <= 0)
) {
  stop("Invalid Blackbox scaling")
}

scores <- scores |>
  left_join(scaling, by = "wave") |>
  mutate(
    econ_z = -(lr_raw - lr_mean) / lr_sd,
    auth_z = (al_raw - al_mean) / al_sd
  ) |>
  dplyr::select(
    id, wave, lral, lr_raw, al_raw, econ_z, auth_z
  )

bes_long <- bes_long |>
  left_join(
    scores,
    by = c("id", "wave"),
    relationship = "many-to-one"
  )


## Save measurement results

# Save diagnostics and uncertainty separately from the model inputs.
# The original Blackbox coordinates and Stan fits are not modified.

readr::write_csv(
  dimensional_fit,
  file.path(out_dir, "blackbox_dimensional_fit.csv")
)

readr::write_csv(
  item_fit,
  file.path(out_dir, "blackbox_item_fit.csv")
)

readr::write_csv(
  orientation_checks,
  file.path(out_dir, "blackbox_orientation_checks.csv")
)

readr::write_csv(
  bootstrap_summary,
  file.path(out_dir, "blackbox_bootstrap_summary.csv")
)

readr::write_csv(
  bootstrap_individual,
  file.path(out_dir, "blackbox_bootstrap_individual.csv")
)

saveRDS(
  purrr::map(blackbox_fits, "fit"),
  file.path(out_dir, "blackbox_original_fits.rds")
)

saveRDS(
  bootstrap_individual,
  file.path(out_dir, "blackbox_bootstrap_individual.rds")
)

print(scaling, n = Inf, width = Inf)


## NB: as the correlation changed i allowed time to be a fixed effect


## Model sample and predictors
# The full five-CA A sample uses valid paired ballots, two ideology scores,
# and a positive wave weight. B and C retain their distinct eligible samples.


raw <- bes_long |>
  filter(mayoral_authority %in% ca_levels, !is.na(source_ballot))
if (anyNA(raw$oslaua_code) || anyNA(raw$la_name)) {
  stop("Missing LA code or lookup match in the five-CA sample")
}
if (nrow(raw) == 0L) stop("The five-authority geographic join produced no cases")

paired <- raw |>
  filter(!is.na(national_labour), !is.na(mayor_labour),
         !is.na(econ_z), !is.na(auth_z), is.finite(wt_raw), wt_raw > 0) |>
  arrange(id, wave) |>
  group_by(id) |>
  mutate(econ_between = mean(econ_z), auth_between = mean(auth_z),
         econ_within = econ_z - econ_between,
         auth_within = auth_z - auth_between) |>
  ungroup() |>
  mutate(
    cross_signed = (econ_z + auth_z) / sqrt(2),
    cross_group = case_when(
      econ_z >= .5 & auth_z >= .5 ~ "Left-authoritarian",
      econ_z <= -.5 & auth_z <= -.5 ~ "Right-libertarian",
      TRUE ~ "Other"
    ),
    ca = factor(mayoral_authority, levels = ca_levels)
  )
if (nrow(paired) == 0L) stop("No paired analytic observations")

# Center once on unique respondent-waves, not duplicated contest rows.
curvature_centre <- mean(paired$cross_signed^2)
paired <- paired |>
  mutate(deviation_sq = cross_signed^2 - curvature_centre) |>
  group_by(wave) |>
  mutate(wt_model = wt_raw / mean(wt_raw)) |>
  ungroup()

model_a <- paired |>
  tidyr::pivot_longer(c(national_labour, mayor_labour),
    names_to = "contest", values_to = "labour") |>
  mutate(contest = factor(contest,
    levels = c("national_labour", "mayor_labour"),
    labels = c("National", "Mayoral")))
stopifnot(nrow(model_a) == 2L * nrow(paired),
          !anyNA(model_a$labour), all(model_a$labour %in% c(0L, 1L)),
          all(as.integer(table(paste(model_a$id, model_a$wave))) == 2L))



model_b <- model_a |> filter(!is.na(mii_group))
model_c <- model_a |> filter(!is.na(leader_rating))
set.seed(20261019)
ids <- sort(unique(as.character(model_b$id)))
if (length(ids) < 10) stop("Insufficient MII-observed respondents")
test_ids <- sample(ids, max(1L, floor(.2 * length(ids))))
model_b <- model_b |>
  mutate(partition = if_else(as.character(id) %in% test_ids, "test", "train"))




## Save and audit
# The EDA table retains wider source variables; the three model tables preserve
# all original model inputs. Only a compact summary is printed here.
funnel <- tibble(stage = c("Five-CA respondent-waves", "Valid paired ballots",
    "Model A with ideology and weight", "Model B MII-eligible", "Model C leader-eligible"),
    respondent_waves = c(nrow(raw),
      sum(!is.na(raw$national_labour) & !is.na(raw$mayor_labour)),
      nrow(paired), nrow(model_b)/2, nrow(model_c)/2))
readr::write_csv(funnel, file.path(out_dir, "sample_funnel.csv"))
readr::write_csv(ballot_audit, file.path(out_dir, "ballot_label_audit.csv"))
readr::write_csv(raw |> count(wave, mayoral_authority, source_ballot, mayor_code,
    mayor_labour, name = "n"), file.path(out_dir, "ballot_recoding_audit.csv"))
readr::write_csv(paired |> count(wave, ca, cross_group, name = "n"),
    file.path(out_dir, "cross_pressure_counts.csv"))
readr::write_csv(scaling, file.path(out_dir, "ideology_scaling.csv"))

readr::write_csv(
  orientation_checks,
  file.path(out_dir, "blackbox_orientation_checks.csv")
)


readr::write_csv(bes_long |> distinct(wave, oslaua_code, la_name, mayoral_authority),
    file.path(out_dir, "la_lookup_to_review.csv"))



saveRDS(bes_long, file.path(out_dir, "bes_mayors_long.rds"))
saveRDS(scores, file.path(out_dir, "blackbox_scores.rds"))
saveRDS(paired, file.path(out_dir, "respondent_wave_paired.rds"))
saveRDS(model_a, file.path(out_dir, "model_A_all.rds"))
saveRDS(model_b, file.path(out_dir, "model_B_mii_split.rds"))
saveRDS(model_c, file.path(out_dir, "model_C_leader.rds"))

print(funnel, n = Inf)
cat("Prepared data in", out_dir, "\n")
