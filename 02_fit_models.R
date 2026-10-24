## Set-up

# Reproduce the four weighted Bayesian models from the prepared BES data.
# Run from the repository root, after 01_prepare_data.R.




library(dplyr)
library(cmdstanr)

prepared <- "data/derived"
model_dir <- "models"
stan_dir <- file.path(model_dir, "stan")

dir.create(stan_dir, recursive = TRUE, showWarnings = FALSE)

all_a <- readRDS(file.path(prepared, "model_A_all.rds"))
mii <- readRDS(file.path(prepared, "model_B_mii_split.rds"))
leader <- readRDS(file.path(prepared, "model_C_leader.rds"))


## Prepare model samples

# A uses all eligible observations.
# A_match and B use identical training observations.
# C uses respondents with available leadership ratings.

train <- mii |> filter(partition == "train")
test <- mii |> filter(partition == "test")

stopifnot(
  nrow(train) > 100,
  nrow(test) > 100,
  !any(as.character(train$id) %in% as.character(test$id)),
  all(c("mii_group", "cross_signed") %in% names(mii))
)

# Corbyn in 2017; Starmer in 2021 and 2024.
# Standardise ratings within each wave for the Model C sample.

leader <- leader |>
  group_by(wave) |>
  mutate(leader_z = (leader_rating - mean(leader_rating)) / sd(leader_rating)) |>
  ungroup()

if (anyNA(leader$leader_z)) stop("Invalid leadership ratings")

# Curvature is centred once in data preparation, using the full A sample.

curvature_offset <- unique(round(
  all_a$cross_signed^2 - all_a$deviation_sq, 9
))

stopifnot(length(curvature_offset) == 1L)


## Index respondents and electoral contests

# Each authority × wave × contest has its own intercept and
# between-person ideological gradients, without partial pooling.

all_a$cell <- interaction(
  as.character(all_a$ca),
  all_a$wave,
  as.character(all_a$contest),
  drop = TRUE,
  sep = " | "
)

cells <- tibble(
  cell_index = seq_along(levels(all_a$cell)),
  cell = levels(all_a$cell)
)

stopifnot(nrow(cells) == 28L)

encode <- function(x) {
  
  needed <- c(
    "id", "wave", "ca", "contest", "labour",
    "econ_between", "auth_between",
    "econ_within", "auth_within",
    "deviation_sq", "wt_model"
  )
  
  if (!all(needed %in% names(x))) stop("Missing model variables")
  if (anyNA(x[needed])) stop("Missing model observations")
  if (!all(x$labour %in% c(0, 1))) stop("Invalid vote coding")
  if (!all(is.finite(x$wt_model) & x$wt_model > 0)) stop("Invalid weights")
  
  cell <- paste(x$ca, x$wave, x$contest, sep = " | ")
  group <- match(cell, cells$cell)
  contest <- match(as.character(x$contest), c("National", "Mayoral"))
  
  if (anyNA(group) || anyNA(contest)) stop("Unrecognised electoral cell")
  
  if (!all(as.integer(table(paste(x$id, x$wave))) == 2L)) {
    stop("Each respondent-wave must contain both contests")
  }
  
  people <- sort(unique(as.character(x$id)))
  
  list(
    N = nrow(x),
    J = length(people),
    G = nrow(cells),
    y = as.integer(x$labour),
    person = match(as.character(x$id), people),
    grp = group,
    contest = contest,
    eb = as.numeric(x$econ_between),
    ab = as.numeric(x$auth_between),
    ew = as.numeric(x$econ_within),
    aw = as.numeric(x$auth_within),
    cp2 = as.numeric(x$deviation_sq),
    w = as.numeric(x$wt_model)
  )
}

a_data <- encode(all_a)
am_data <- encode(train)
b_data <- am_data
c_data <- encode(leader)

# Economic MII is the reference category in Model B.

mii_levels <- c("Economic", "Cultural", "Other", "Coronavirus")

b_data$mii <- match(as.character(train$mii_group), mii_levels)
b_data$cross <- as.numeric(train$cross_signed)

if (anyNA(b_data$mii) || anyNA(b_data$cross)) {
  stop("Invalid MII coding")
}

c_data$leader <- as.numeric(leader$leader_z)


## Stan specifications

# Shared Model A:
#   alpha         Authority × year × contest intercepts
#   beta_e/a      Between-person economic and cultural gradients by cell
#   gamma_e/a     Within-person ideological deviations by contest
#   delta         Curvature by contest
#   sigma_person  Respondent-level random intercept SD
#
# No partial pooling of authority-level ideological gradients.
# Survey weights enter through the Bernoulli log-likelihood.

stan_data <- '
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1> G;
  array[N] int<lower=0,upper=1> y;
  array[N] int<lower=1,upper=J> person;
  array[N] int<lower=1,upper=G> grp;
  array[N] int<lower=1,upper=2> contest;
  vector[N] eb;
  vector[N] ab;
  vector[N] ew;
  vector[N] aw;
  vector[N] cp2;
  vector<lower=0>[N] w;
'

stan_parameters <- '
  vector[G] alpha;
  vector[G] beta_e;
  vector[G] beta_a;
  vector[2] gamma_e;
  vector[2] gamma_a;
  vector[2] delta;
  vector[J] z_person;
  real<lower=0> sigma_person;
'

stan_priors <- '
  alpha ~ normal(0, 2);
  beta_e ~ normal(0, 1);
  beta_a ~ normal(0, 1);
  gamma_e ~ normal(0, 1);
  gamma_a ~ normal(0, 1);
  delta ~ normal(0, 0.7);
  z_person ~ std_normal();
  sigma_person ~ normal(0, 1);
'

stan_predictor <- '
  eta = alpha[grp]
      + beta_e[grp] .* eb
      + beta_a[grp] .* ab
      + gamma_e[contest] .* ew
      + gamma_a[contest] .* aw
      + delta[contest] .* cp2
      + u[person];
'

# B extends A with MII category effects and interactions with
# signed cross-pressure, separately for national and mayoral voting.
#
# C extends A with leadership-rating slopes for each electoral cell.
#
# A_match retains the exact A specification on B's training sample.



model_code <- function(model) {
  
  if (!model %in% c("A", "A_match", "B", "C")) {
    stop("Unknown model")
  }
  
  extra_data <- switch(
    model,
    B = '
      array[N] int<lower=1,upper=4> mii;
      vector[N] cross;
    ',
    C = 'vector[N] leader;',
    ''
  )
  
  extra_parameters <- switch(
    model,
    B = '
      matrix[2,3] theta;
      matrix[2,3] phi;
    ',
    C = 'vector[G] leader_beta;',
    ''
  )
  
  extra_priors <- switch(
    model,
    B = '
      to_vector(theta) ~ normal(0, 0.55);
      to_vector(phi) ~ normal(0, 0.45);
    ',
    C = 'leader_beta ~ normal(0, 0.7);',
    ''
  )
  
  extra_predictor <- switch(
    model,
    B = '
      for (n in 1:N) {
        if (mii[n] > 1) {
          eta[n] += theta[contest[n], mii[n]-1]
                  + phi[contest[n], mii[n]-1] * cross[n];
        }
      }
    ',
    C = 'eta += leader_beta[grp] .* leader;',
    ''
  )
  
  paste0(
    'data {\n', stan_data, extra_data, '}\n',
    'parameters {\n', stan_parameters, extra_parameters, '}\n',
    'transformed parameters {\n',
    '  vector[J] u = sigma_person * z_person;\n',
    '}\n',
    'model {\n',
    '  vector[N] eta;\n',
    '  vector[N] log_lik;\n',
    stan_priors,
    extra_priors,
    stan_predictor,
    extra_predictor,
    '  for (n in 1:N) {\n',
    '    log_lik[n] = bernoulli_logit_lpmf(y[n] | eta[n]);\n',
    '  }\n',
    '  target += dot_product(w, log_lik);\n',
    '}\n'
  )
}


## Fit models

# Four independent Stan fits using the original sampling settings.
# A_match and B share an identical training sample
# Held-out respondents are not used in either fit.

fit_inputs <- list(
  A = a_data,
  A_match = am_data,
  B = b_data,
  C = c_data
)

# Prevent accidental replacement of saved model fits.

fit_paths <- file.path(
  model_dir,
  paste0(names(fit_inputs), "_fit.rds")
)

if (any(file.exists(fit_paths))) {
  stop("Existing fitted models found in models/. Preserve them before refitting.")
}

cores <- parallel::detectCores(logical = FALSE)
if (is.na(cores) || cores < 1L) cores <- 2L

for (model in names(fit_inputs)) {
  
  stan_file <- file.path(stan_dir, paste0(model, ".stan"))
  writeLines(model_code(model), stan_file)
  
  compiled <- cmdstanr::cmdstan_model(stan_file)
  
  fit <- compiled$sample(
    data = fit_inputs[[model]],
    seed = 20261022,
    chains = 4,
    parallel_chains = min(4L, cores),
    iter_warmup = 900,
    iter_sampling = 1100,
    adapt_delta = 0.97,
    max_treedepth = 12,
    refresh = 100,
    output_dir = model_dir
  )
  
  fit$save_object(file.path(model_dir, paste0(model, "_fit.rds")))
  
  readr::write_csv(
    fit$summary(),
    file.path(model_dir, paste0(model, "_summary.csv"))
  )
  
  saveRDS(
    fit$diagnostic_summary(),
    file.path(model_dir, paste0(model, "_diagnostics.rds"))
  )
}


## Save model metadata

readr::write_csv(cells, file.path(model_dir, "cell_index.csv"))

saveRDS(
  list(
    test = test,
    cells = cells,
    mii_levels = mii_levels,
    curvature_centre = as.numeric(curvature_offset[1]),
    leader_scaling = "Within-wave standardisation among C-eligible observations"
  ),
  file.path(model_dir, "model_metadata.rds")
)

cat("Models A, A_match, B and C saved in models/\n")