## Set-up
# Generate posterior contrasts and respondent-grouped held-out comparisons.
# Works from saved model fits; NEVER refits Stan. Run after 02_fit_models.R.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(posterior)
  library(tibble)
})

out <- "results"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

## Protect existing results (this is a safety check which stops the ones I ran being over-riden)

existing <- file.path(out, c(
  "model_A_posterior_probabilities.csv",
  "model_A_ideological_contrasts.csv",
  "model_A_mayoral_minus_national.csv",
  "probabilities_corrected.csv",
  "mii_economic_vs_cultural_contrasts.csv",
  "C_leadership_probabilities.csv",
  "respondent_heldout_scores.csv",
  "heldout_comparison_summary.csv"
))

if (any(file.exists(existing))) {
  stop("Existing results found. No files have been overwritten.")
}







metadata <- readRDS(file.path("models", "model_metadata.rds"))
cells <- metadata$cells



original_a <- readRDS(file.path("data", "derived", "model_A_all.rds"))
model_c_data <- readRDS(file.path("data", "derived", "model_C_leader.rds"))
fits <- list(
  A = readRDS(file.path("models", "A_fit.rds")),
  A_match = readRDS(file.path("models", "A_match_fit.rds")),
  B = readRDS(file.path("models", "B_fit.rds")),
  C = readRDS(file.path("models", "C_fit.rds"))
)

# Always sample JOINT draws: posterior contrasts must subtract at each draw.
set.seed(20261024)
get_draws <- function(fit, extra = character(), n = 750L) {
  parameters <- c("alpha", "beta_e", "beta_a", "gamma_e", "gamma_a",
                  "delta", "sigma_person", extra)
  x <- posterior::as_draws_matrix(fit$draws(variables = parameters))
  x[sort(sample.int(nrow(x), min(n, nrow(x)))), , drop = FALSE]
}
draw_a <- get_draws(fits$A)
draw_b <- get_draws(fits$B, c("theta", "phi"))
draw_c <- get_draws(fits$C, "leader_beta")

# Thirty-one midpoint normal quantiles integrate a new respondent's random
# intercept instead of incorrectly fixing it at zero.
z <- qnorm((seq_len(31L) - 0.5) / 31L)

scenario_probability <- function(d, g, contest, e, a, model = "A",
                                 mii = "Economic", leader_z = 0) {
  cc <- match(contest, c("National", "Mayoral"))
  signed <- (e + a) / sqrt(2)
  cp2 <- signed^2 - metadata$curvature_centre
  eta <- d[, paste0("alpha[", g, "]")] +
    d[, paste0("beta_e[", g, "]")] * e +
    d[, paste0("beta_a[", g, "]")] * a +
    d[, paste0("delta[", cc, "]")] * cp2
  if (model == "B" && mii != "Economic") {
    k <- match(mii, metadata$mii_levels) - 1L
    if (is.na(k) || k < 1L) stop("Unknown MII category")
    eta <- eta + d[, paste0("theta[", cc, ",", k, "]")] +
      d[, paste0("phi[", cc, ",", k, "]")] * signed
  }
  if (model == "C") {
    eta <- eta + d[, paste0("leader_beta[", g, "]")] * leader_z
  }
  p <- numeric(nrow(d))
  for (zz in z) p <- p + plogis(eta + d[, "sigma_person"] * zz) / length(z)
  p
}

summarise_draws <- function(x) {
  c(mean = mean(x), lo = unname(quantile(x, .025)),
    hi = unname(quantile(x, .975)), p_gt_0 = mean(x > 0))
}

## Model A — ideology and mayoral-national contrasts
# Profiles are fixed standardised values, not observed subgroup averages.
profiles <- tibble::tribble(
  ~profile, ~econ, ~auth,
  "Centre", 0, 0,
  "Economic left (-1)", 1, 0,
  "Economic right (+1)", -1, 0,
  "Cultural libertarian (-1)", 0, -1,
  "Cultural authoritarian (+1)", 0, 1,
  "Left-authoritarian (+1,+1)", 1, 1,
  "Right-libertarian (-1,-1)", -1, -1
)
agrid <- tidyr::expand_grid(cell_index = cells$cell_index, profile = profiles$profile) |>
  left_join(cells, by = "cell_index") |>
  left_join(profiles, by = "profile") |>
  tidyr::separate(cell, into = c("ca", "wave", "contest"),
                   sep = " \\| ", remove = FALSE) |>
  mutate(wave = as.integer(wave),
         year = recode(wave, `12` = 2017L, `21` = 2021L, `26` = 2024L))

aprob <- vapply(seq_len(nrow(agrid)), function(i) {
  s <- agrid[i, ]
  scenario_probability(draw_a, s$cell_index, s$contest, s$econ, s$auth)
}, numeric(nrow(draw_a)))

aprob_summ <- t(apply(aprob, 2, summarise_draws))
a_summary <- bind_cols(agrid, as_tibble(aprob_summ))
readr::write_csv(a_summary, file.path(out, "model_A_posterior_probabilities.csv"))

contrast_specs <- list(
  economic_left_vs_right = c("Economic left (-1)", "Economic right (+1)"),
  cultural_authoritarian_vs_libertarian = c("Cultural authoritarian (+1)", "Cultural libertarian (-1)"),
  left_authoritarian_vs_right_libertarian = c("Left-authoritarian (+1,+1)", "Right-libertarian (-1,-1)")
)
contrasts <- bind_rows(lapply(cells$cell_index, function(g) {
  bind_rows(lapply(names(contrast_specs), function(nm) {
    pair <- contrast_specs[[nm]]
    first <- which(agrid$cell_index == g & agrid$profile == pair[1])
    second <- which(agrid$cell_index == g & agrid$profile == pair[2])
    as_tibble_row(as.list(c(cell_index = g, contrast = nm,
      summarise_draws(aprob[, first] - aprob[, second]))))
  }))
})) |>
  mutate(cell_index = as.integer(cell_index),
         across(c(mean, lo, hi, p_gt_0), as.numeric)) |>
  left_join(agrid |> distinct(cell_index, ca, year, contest), by = "cell_index")
readr::write_csv(contrasts, file.path(out, "model_A_ideological_contrasts.csv"))

gaps <- bind_rows(lapply(unique(paste(agrid$ca, agrid$year, agrid$profile)), function(key) {
  matching <- which(paste(agrid$ca, agrid$year, agrid$profile) == key)
  nat <- matching[agrid$contest[matching] == "National"]
  may <- matching[agrid$contest[matching] == "Mayoral"]
  if (length(nat) != 1L || length(may) != 1L) return(NULL)
  as_tibble_row(as.list(c(ca = agrid$ca[may], year = agrid$year[may],
    profile = agrid$profile[may], summarise_draws(aprob[, may] - aprob[, nat]))))
})) |>
  mutate(year = as.integer(year), across(c(mean, lo, hi, p_gt_0), as.numeric))
readr::write_csv(gaps, file.path(out, "model_A_mayoral_minus_national.csv"))

## Model B — MII economic versus cultural priority
# Economic is the reference; Cultural is the first category offset.
bgrid <- tidyr::expand_grid(cell_index = cells$cell_index,
  profile = c("Left-authoritarian", "Right-libertarian"),
  mii = c("Economic", "Cultural")) |>
  left_join(cells, by = "cell_index") |>
  tidyr::separate(cell, into = c("ca", "wave", "contest"),
                   sep = " \\| ", remove = FALSE) |>
  mutate(wave = as.integer(wave),
         year = recode(wave, `12` = 2017L, `21` = 2021L, `26` = 2024L),
         E = if_else(profile == "Left-authoritarian", 1, -1),
         A = if_else(profile == "Left-authoritarian", 1, -1))
bprob <- vapply(seq_len(nrow(bgrid)), function(i) {
  s <- bgrid[i, ]
  scenario_probability(draw_b, s$cell_index, s$contest, s$E, s$A,
                       model = "B", mii = s$mii)
}, numeric(nrow(draw_b)))
b_summ <- t(apply(bprob, 2, summarise_draws))
b_summary <- bind_cols(bgrid, as_tibble(b_summ)) |>
  transmute(year, ca, contest, profile, mii,
    mean_probability = mean, lower_95 = lo, upper_95 = hi, p_gt_0)
readr::write_csv(b_summary, file.path(out, "probabilities_corrected.csv"))

b_diffs <- bind_rows(lapply(unique(paste(bgrid$cell_index, bgrid$profile, sep = "|")), function(key) {
  ix <- which(paste(bgrid$cell_index, bgrid$profile, sep = "|") == key)
  econ <- ix[bgrid$mii[ix] == "Economic"]
  cultural <- ix[bgrid$mii[ix] == "Cultural"]
  s <- bgrid[econ, ]
  as_tibble_row(as.list(c(year = s$year, ca = s$ca, contest = s$contest,
    profile = s$profile, summarise_draws(bprob[, econ] - bprob[, cultural]))))
})) |>
  mutate(year = as.integer(year), across(c(mean, lo, hi, p_gt_0), as.numeric))
readr::write_csv(b_diffs, file.path(out, "mii_economic_vs_cultural_contrasts.csv"))

## Model C — national leader assessments
# As in the earlier results, compare observed ratings 2/10 and 8/10, not
# a claimed continuous descriptive relationship. Within-wave z scores use
# the same C-eligible respondent distribution as the fitted model.
leader_reference <- model_c_data |>
  # Keep both contest rows, matching the exact C fitting standardisation.
  group_by(wave) |>
  summarise(leader_mean = mean(leader_rating),
            leader_sd = sd(leader_rating), .groups = "drop")
cgrid <- tidyr::expand_grid(cell_index = cells$cell_index,
  profile = c("Neutral ideology", "Left-authoritarian", "Right-libertarian"),
  leader_rating = c(2, 8)) |>
  left_join(cells, by = "cell_index") |>
  tidyr::separate(cell, into = c("ca", "wave", "contest"),
                   sep = " \\| ", remove = FALSE) |>
  mutate(wave = as.integer(wave),
         year = recode(wave, `12` = 2017L, `21` = 2021L, `26` = 2024L),
         e = case_when(profile == "Left-authoritarian" ~ 1,
                       profile == "Right-libertarian" ~ -1, TRUE ~ 0),
         a = e) |>
  left_join(leader_reference, by = "wave") |>
  mutate(leader_z = (leader_rating - leader_mean) / leader_sd)
if (anyNA(cgrid$leader_z)) stop("Missing leader standardisation for C predictions")
cprob <- vapply(seq_len(nrow(cgrid)), function(i) {
  s <- cgrid[i, ]
  scenario_probability(draw_c, s$cell_index, s$contest, s$e, s$a,
                       model = "C", leader_z = s$leader_z)
}, numeric(nrow(draw_c)))
c_summ <- t(apply(cprob, 2, summarise_draws))
c_summary <- bind_cols(cgrid, as_tibble(c_summ)) |>
  transmute(ca, year, contest, profile, leader_rating,
    mean_pct = 100 * mean, lower_pct = 100 * lo, upper_pct = 100 * hi)
readr::write_csv(c_summary, file.path(out, "C_leadership_probabilities.csv"))



## Held-out A_match versus B


# Entire respondent IDs were held out

# intercept across that respondent's contest rows, for a joint log score.
# Brier is the sum of row-level squared prediction errors.
heldout <- metadata$test
heldout$cell_index <- match(paste(as.character(heldout$ca), heldout$wave,
                                  as.character(heldout$contest), sep = " | "), cells$cell)
heldout$contest_index <- match(as.character(heldout$contest), c("National", "Mayoral"))
heldout$mii_index <- match(as.character(heldout$mii_group), metadata$mii_levels)
if (anyNA(heldout[c("cell_index", "contest_index", "mii_index")])) {
  stop("Held-out cases not compatible with fitted cell and MII indexes")
}

logmeanexp <- function(x) {
  m <- max(x)
  m + log(mean(exp(x - m)))
}

score_heldout <- function(draws, model, scope) {
  subset <- heldout[scope, , drop = FALSE]
  persons <- split(seq_len(nrow(subset)), as.character(subset$id))
  bind_rows(lapply(persons, function(ix) {
    x <- subset[ix, , drop = FALSE]
    eta <- vapply(seq_len(nrow(x)), function(j) {
      g <- x$cell_index[j]
      cc <- x$contest_index[j]
      v <- draws[, paste0("alpha[", g, "]")] +
        draws[, paste0("beta_e[", g, "]")] * x$econ_between[j] +
        draws[, paste0("beta_a[", g, "]")] * x$auth_between[j] +
        draws[, paste0("gamma_e[", cc, "]")] * x$econ_within[j] +
        draws[, paste0("gamma_a[", cc, "]")] * x$auth_within[j] +
        draws[, paste0("delta[", cc, "]")] * x$deviation_sq[j]
      if (model == "B" && x$mii_index[j] > 1L) {
        k <- x$mii_index[j] - 1L
        v <- v + draws[, paste0("theta[", cc, ",", k, "]")] +
          draws[, paste0("phi[", cc, ",", k, "]")] * x$cross_signed[j]
      }
      v
    }, numeric(nrow(draws)))
    eta <- matrix(eta, nrow = nrow(draws), ncol = nrow(x))
    y <- matrix(rep(as.numeric(x$labour), each = nrow(draws)),
                nrow = nrow(draws))
    joint <- matrix(NA_real_, nrow(draws), length(z))
    pp <- matrix(0, nrow(draws), nrow(x))
    for (k in seq_along(z)) {
      p <- plogis(sweep(eta, 1, draws[, "sigma_person"] * z[k], "+"))
      joint[, k] <- rowSums(dbinom(y, 1, p, log = TRUE))
      pp <- pp + p / length(z)
    }
    tibble(id = as.character(x$id[1]), model = model,
           n_rows = nrow(x), joint_log_score = logmeanexp(apply(joint, 1, logmeanexp)),
           sum_brier = sum((as.numeric(x$labour) - colMeans(pp))^2))
  }))
}

draw_heldout_a <- get_draws(fits$A_match, n = 500L)
draw_heldout_b <- get_draws(fits$B, c("theta", "phi"), n = 500L)

scopes <- list(
  all = rep(TRUE, nrow(heldout)),
  cross_pressured = heldout$cross_group %in% c("Left-authoritarian", "Right-libertarian"),
  gm_2024 = heldout$wave == 26L & as.character(heldout$ca) == "Greater Manchester"
)
comparison <- bind_rows(lapply(names(scopes), function(nm) {
  idx <- scopes[[nm]]
  if (!any(idx)) return(NULL)
  a <- score_heldout(draw_heldout_a, "A_match", idx)
  b <- score_heldout(draw_heldout_b, "B", idx)
  a |> inner_join(b, by = "id", suffix = c("_A", "_B")) |>
    mutate(scope = nm,
           elpd_difference = joint_log_score_B - joint_log_score_A,
           brier_difference = sum_brier_B - sum_brier_A)
}))
if (nrow(comparison) == 0L) stop("No held-out results")
readr::write_csv(comparison, file.path(out, "respondent_heldout_scores.csv"))
heldout_summary <- comparison |>
  group_by(scope) |>
  summarise(respondents = n(),
    elpd_B_minus_A = sum(elpd_difference),
    se_elpd = sqrt(n()) * sd(elpd_difference),
    brier_B_minus_A = sum(brier_difference), .groups = "drop")
readr::write_csv(heldout_summary, file.path(out, "heldout_comparison_summary.csv"))

cat("\nModel results written to: ", out, "\n", sep = "")
print(heldout_summary, n = Inf)
