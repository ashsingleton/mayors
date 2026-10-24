
## Set-up


library(tidyverse)

## Figure outputs

figures_dir <- file.path("manuscript", "figures")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)


original_dir <- "data/labour_abc_prepared"

a_run <- file.path(
  original_dir, "model_A_runs", "A_20261019_193521"
)

bc_run <- file.path(
  original_dir, "model_BC_runs", "BC_20261019_205527"
)

# Poppins (optional font)
if (requireNamespace("sysfonts", quietly = TRUE) &&
    requireNamespace("showtext", quietly = TRUE)) {
  try({
    sysfonts::font_add_google("Poppins")
    showtext::showtext_auto()
  }, silent = TRUE)
}


## Plot style

party_cols <- c(
  "Con" = "#2e368e",
  "conservative" = "#2e368e",
  "Conservative" = "#2e368e",
  "ukip" = "purple",
  "UKIP" = "purple",
  "United Kingdom Independence Party" = "purple",
  "Labour" = "#E4003B",
  "Lab" = "#E4003B",
  "labour" = "#E4003B",
  "lab" = "#E4003B",
  "LD" = "#ffbd31",
  "lib_dem" = "#ffbd31",
  "ld" = "#ffbd31",
  "liberal democrat" = "#ffbd31",
  "liberal_democrat" = "#ffbd31",
  "Liberal Democrat" = "#ffbd31",
  "Lib Dems" = "#ffbd31",
  "Lib Dem" = "#ffbd31",
  "lib dem" = "#ffbd31",
  "LibDem" = "#ffbd31",
  "Grn" = "#45CC2D",
  "green" = "#45CC2D",
  "Green" = "#45CC2D",
  "Greens" = "#45CC2D",
  "Ref" = "#a2dbe1",
  "Reform" = "#a2dbe1",
  "Reform UK" = "#a2dbe1",
  "ruk" = "#a2dbe1",
  "reform uk" = "#a2dbe1",
  "reform_uk" = "#a2dbe1",
  "SNP" = "#FFF95D",
  "snp" = "#FFF95D",
  "scottish national party" = "#FFF95D",
  "PC" = "#005B54",
  "plaid" = "#005B54",
  "Plaid" = "#005B54",
  "pc" = "#005B54",
  "plaid_cymru" = "#005B54",
  "nv" = "grey60",
  "non_voter" = "grey60",
  "other" = "grey70",
  "independent" = "grey50",
  "snp/plaid" = "darkgrey",
  "NA" = "darkgrey"
)

theme_df <- function(
    base_family = "Poppins",
    brand_blue = "#1d70b7",
    bg = "white",
    panel_bg = "white",
    grid_major = "#E7E3D8",
    grid_minor = "#F2EFE7",
    border = "#D8D0BF",
    strip_fill = "#F5F2E9",
    text_col = "#222222",
    muted_col = "#666666"
) {
  ggplot2::theme_minimal(base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = base_family, colour = text_col
      ),
      plot.background = ggplot2::element_rect(
        fill = bg, colour = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = panel_bg, colour = NA
      ),
      panel.grid.major = ggplot2::element_line(
        colour = grid_major, linewidth = 0.35
      ),
      panel.grid.minor = ggplot2::element_line(
        colour = grid_minor, linewidth = 0.2
      ),
      panel.border = ggplot2::element_rect(
        fill = NA, colour = border, linewidth = 0.6
      ),
      plot.title = ggplot2::element_text(
        face = "bold", hjust = 0, colour = text_col,
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = brand_blue, hjust = 0, lineheight = 1.15,
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption = ggplot2::element_text(
        colour = muted_col, hjust = 0, lineheight = 1.1,
        margin = ggplot2::margin(t = 10)
      ),
      plot.caption.position = "plot",
      axis.title = ggplot2::element_text(
        face = "bold", colour = brand_blue,
        margin = ggplot2::margin(t = 6, r = 6, b = 6, l = 6)
      ),
      axis.text = ggplot2::element_text(colour = "#333333"),
      axis.ticks = ggplot2::element_line(
        colour = border, linewidth = 0.35
      ),
      strip.background = ggplot2::element_rect(
        fill = strip_fill, colour = border, linewidth = 0.5
      ),
      strip.text = ggplot2::element_text(
        face = "bold", colour = text_col,
        margin = ggplot2::margin(t = 7, r = 7, b = 7, l = 7)
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = text_col),
      legend.key = ggplot2::element_rect(
        fill = bg, colour = NA
      ),
      legend.spacing.x = grid::unit(0.25, "cm"),
      plot.tag = ggplot2::element_text(
        family = base_family, face = "bold", hjust = 1,
        vjust = 1, colour = muted_col
      ),
      plot.tag.position = c(0.98, 0.98)
    )
}

scale_colour_party <- function(..., na.value = "grey80") {
  ggplot2::scale_colour_manual(
    values = party_cols, na.value = na.value, ...
  )
}

scale_fill_party <- function(..., na.value = "grey80") {
  ggplot2::scale_fill_manual(
    values = party_cols, na.value = na.value, ...
  )
}

contest_colours <- c(
  "National" = "#555555",
  "Mayoral" = party_cols[["Labour"]]
)


## Load original analysis data (optional for v quick EDA)

model_A <- readRDS(file.path(original_dir, "model_A_all.rds"))
model_B <- readRDS(file.path(original_dir, "model_B_mii_split.rds"))
model_C <- readRDS(file.path(original_dir, "model_C_leader.rds"))

# Load wider BES data when available, for EDA and supplementary regressions.

bes_file <- "data/bes_mayors_long.rds"

if (file.exists(bes_file)) {
  bes_long <- readRDS(bes_file)
}


## Load original posterior results


A_contrasts <- readr::read_csv(
  file.path(
    a_run, "results_export",
    "model_A_ideological_contrasts.csv"
  ),
  show_col_types = FALSE
)

A_proportions <- readr::read_csv(
  file.path(
    a_run, "results_export",
    "model_A_posterior_probabilities.csv"
  ),
  show_col_types = FALSE
)

B_proportions <- readr::read_csv(
  file.path(
    bc_run, "mii_posterior_contrasts",
    "corrected_results", "probabilities_corrected.csv"
  ),
  show_col_types = FALSE
)

C_proportions <- readr::read_csv(
  file.path(
    bc_run, "RQ4_leadership_class",
    "C_leadership_probabilities.csv"
  ),
  show_col_types = FALSE
)


## Results lookup

results_folders <- c(
  "results",
  file.path(original_dir, "model_A_runs"),
  file.path(original_dir, "model_BC_runs"),
  "data/labour_imputation_sensitivity"
)

results_folders <- results_folders[dir.exists(results_folders)]

results_lookup <- tibble(
  path = unlist(
    lapply(results_folders, \(folder) {
      list.files(
        folder,
        pattern = "\\.csv(\\.gz)?$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    }),
    use.names = FALSE
  )
) |>
  mutate(
    file = basename(path),
    source = case_when(
      grepl("labour_imputation_sensitivity", path) ~ "Imputation sensitivity",
      grepl("model_A_runs", path) ~ "Original Model A",
      grepl("model_BC_runs", path) ~ "Original Models B/C",
      TRUE ~ "Other saved results"
    )
  ) |>
  dplyr::select(source, file, path) |>
  arrange(source, file, path)


## Data lookup

# Inspect the available datasets without combining samples or altering them.

data_lookup <- bind_rows(
  tibble(
    dataset = "model_A",
    observations = nrow(model_A),
    respondents = n_distinct(model_A$id),
    variables = ncol(model_A)
  ),
  tibble(
    dataset = "model_B",
    observations = nrow(model_B),
    respondents = n_distinct(model_B$id),
    variables = ncol(model_B)
  ),
  tibble(
    dataset = "model_C",
    observations = nrow(model_C),
    respondents = n_distinct(model_C$id),
    variables = ncol(model_C)
  )
)

if (exists("bes_long")) {
  data_lookup <- bind_rows(
    tibble(
      dataset = "bes_long",
      observations = nrow(bes_long),
      respondents = n_distinct(bes_long$id),
      variables = ncol(bes_long)
    ),
    data_lookup
  )
}

print(data_lookup, n = Inf)
print(results_lookup |> count(source, name = "files"), n = Inf)



figure_theme <- theme(
  text = element_text(size = 24),
  
  plot.title = element_blank(),
  plot.subtitle = element_blank(),
  
  axis.title = element_text(size = 25, face = "bold"),
  axis.text = element_text(size = 22),
  axis.text.x = element_text(size = 22),
  axis.text.y = element_text(size = 22),
  
  strip.text = element_text(size = 24, face = "bold"),
  strip.text.x = element_text(size = 24, face = "bold"),
  strip.text.y.right = element_text(
    size = 23,
    face = "bold",
    angle = 270
  ),
  
  legend.text = element_text(size = 22),
  legend.key.width = grid::unit(1.1, "cm"),
  legend.spacing.x = grid::unit(0.5, "cm"),
  
  panel.spacing = grid::unit(0.35, "cm"),
  plot.margin = margin(12, 16, 12, 16)
)



p1 <- p1 + figure_theme
p2 <- p2 + figure_theme
p3 <- p3 + figure_theme
p4 <- p4 + figure_theme

p2_gm <- p2_gm + figure_theme
p3_gm <- p3_gm + figure_theme
p4_gm <- p4_gm + figure_theme

p_mayoral_crossparty <- p_mayoral_crossparty + figure_theme

p_gm_heat <- p_gm_heat + figure_theme
p_gm_heat_2024 <- p_gm_heat_2024 + figure_theme

p_rail <- p_rail + figure_theme
p_rail_change <- p_rail_change + figure_theme

p_class_gm <- p_class_gm + figure_theme


## Figure 1: Ideological gradients in Greater Manchester

p1 <- A_contrasts |>
  filter(
    ca == "Greater Manchester",
    contrast %in% c(
      "economic_left_vs_right",
      "cultural_authoritarian_vs_libertarian"
    )
  ) |>
  mutate(
    gradient = factor(
      contrast,
      levels = c("economic_left_vs_right", "cultural_authoritarian_vs_libertarian"),
      labels = c("Economic: left vs right", "Cultural: authoritarian vs libertarian")
    )
  ) |>
  ggplot(aes(x = as.integer(year), y = 100 * mean, colour = gradient, group = gradient)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.75) +
  geom_line(linewidth = 1.35) +
  geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi),
                width = 0.14, linewidth = 1.05) +
  geom_point(size = 4.2) +
  facet_wrap(~contest, nrow = 1) +
  scale_x_continuous(breaks = c(2017, 2021, 2024)) +
  scale_colour_manual(values = c(
    "Economic: left vs right" = "#00B6C5",
    "Cultural: authoritarian vs libertarian" = "#FA6866"
  )) +
  labs(
    title = "Greater Manchester: economic and cultural gradients",
    subtitle = "Model A | Posterior means and 95% credible intervals",
    x = "Election year", y = "Labour voting probability contrast (pp)"
  ) +
  theme_df() + figure_theme +
  theme(legend.position = "bottom", legend.box = "horizontal")


## Figure 2: Fixed cross-pressure profiles across authorities

# Keep the actual two profiles used in Model A: left-authoritarian and
# right-libertarian. Do not relabel the latter as right-authoritarian.
p2_data <- A_proportions |>
  filter(profile %in% c(
    "Left-authoritarian (+1,+1)",
    "Right-libertarian (-1,-1)"
  ))

p2 <- p2_data |>
  ggplot(aes(x = as.integer(year), y = mean, colour = contest, group = contest)) +
  geom_line(linewidth = 1.05) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.14, linewidth = 0.85) +
  geom_point(size = 3.3) +
  facet_grid(
    profile ~ ca,
    labeller = labeller(ca = c(
      "Greater Manchester" = "Greater\nManchester",
      "Liverpool City Region" = "Liverpool\nCity Region",
      "Tees Valley" = "Tees Valley",
      "West Midlands" = "West Midlands",
      "West Yorkshire" = "West Yorkshire"
    ), profile = c(
      "Left-authoritarian (+1,+1)" = "Left-authoritarian\n(+1 SD, +1 SD)",
      "Right-libertarian (-1,-1)" = "Right-libertarian\n(-1 SD, -1 SD)"
    ))
  ) +
  scale_x_continuous(breaks = c(2017, 2021, 2024)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_colour_manual(values = contest_colours) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Labour support for fixed ideological profiles",
    subtitle = "Model A | Population-integrated probabilities at ±1 SD; 95% credible intervals",
    x = "Election year", y = "Probability of Labour vote"
  ) +
  theme_df() + figure_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 17),
    strip.text.x = element_text(size = 20, face = "bold"),
    strip.text.y.right = element_text(size = 20, angle = 270, face = "bold")
  )

# Optional focused figure, readable at standard portrait-paper width.
p2_gm <- p2_data |>
  filter(ca == "Greater Manchester") |>
  ggplot(aes(x = as.integer(year), y = 100 * mean,
             colour = contest, group = contest)) +
  geom_line(linewidth = 1.3) +
  geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi),
                width = 0.15, linewidth = 0.95) +
  geom_point(size = 4.2) +
  facet_wrap(~profile, nrow = 1, labeller = labeller(profile = c(
    "Left-authoritarian (+1,+1)" = "Left-authoritarian (+1 SD)",
    "Right-libertarian (-1,-1)" = "Right-libertarian (-1 SD)"
  ))) +
  scale_x_continuous(breaks = c(2017, 2021, 2024)) +
  scale_colour_manual(values = contest_colours) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Greater Manchester: fixed ideological profiles",
    subtitle = "Model A | 95% credible intervals",
    x = "Election year", y = "Probability of Labour vote (%)"
  ) +
  theme_df() + figure_theme +
  theme(strip.text = element_text(size = 15))


## Figure 3: Most important issue, 2024

p3_data <- B_proportions |>
  filter(as.integer(year) == 2024L,
         mii %in% c("Economic", "Cultural")) |>
  mutate(mii = factor(mii, levels = c("Cultural", "Economic")))

p3 <- p3_data |>
  ggplot(aes(x = mii, y = 100 * mean_probability, colour = contest,
             group = contest)) +
  geom_errorbar(aes(ymin = 100 * lower_95, ymax = 100 * upper_95),
                position = position_dodge(width = 0.42), width = 0.16,
                linewidth = 0.95) +
  geom_point(position = position_dodge(width = 0.42), size = 3.7) +
  facet_grid(profile ~ ca, labeller = labeller(ca = c(
    "Greater Manchester" = "Greater\nManchester",
    "Liverpool City Region" = "Liverpool\nCity Region",
    "Tees Valley" = "Tees Valley",
    "West Midlands" = "West Midlands",
    "West Yorkshire" = "West Yorkshire"
  ), profile = c(
    "Left-authoritarian (+1,+1)" = "Left-authoritarian\n(+1 SD, +1 SD)",
    "Right-libertarian (-1,-1)" = "Right-libertarian\n(-1 SD, -1 SD)"
  ))) +
  scale_colour_manual(values = contest_colours) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Labour support by stated issue priority, 2024",
    subtitle = "Model B | Fixed ideological profiles; 95% credible intervals",
    x = "Most important issue", y = "Predicted Labour vote (%)"
  ) +
  theme_df() + figure_theme +
  theme(
    strip.text.x = element_text(size = 20, face = "bold"),
    strip.text.y.right = element_text(size = 20, angle = 270, face = "bold"),
    axis.text.x = element_text(size = 18)
  )

p3_gm <- p3_data |>
  filter(ca == "Greater Manchester") |>
  ggplot(aes(x = mii, y = 100 * mean_probability,
             colour = contest, group = contest)) +
  geom_errorbar(aes(ymin = 100 * lower_95, ymax = 100 * upper_95),
                position = position_dodge(width = 0.48), width = 0.16,
                linewidth = 1.0) +
  geom_point(position = position_dodge(width = 0.48), size = 4.3) +
  facet_wrap(~profile, nrow = 1, labeller = labeller(profile = c(
    "Left-authoritarian (+1,+1)" = "Left-authoritarian (+1 SD)",
    "Right-libertarian (-1,-1)" = "Right-libertarian (-1 SD)"
  ))) +
  scale_colour_manual(values = contest_colours) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Greater Manchester: issue priority and Labour support",
    subtitle = "Model B | 2024, 95% credible intervals",
    x = "Most important issue", y = "Predicted Labour vote (%)"
  ) +
  theme_df() + figure_theme


## Figure 4: Leader evaluations at 2/10 and 8/10

p4_data <- C_proportions |>
  filter(profile == "Neutral ideology")

p4 <- p4_data |>
  ggplot(aes(x = leader_rating, y = mean_pct, colour = contest,
             group = contest)) +
  geom_errorbar(aes(ymin = lower_pct, ymax = upper_pct),
                position = position_dodge(width = 0.42),
                width = 0.22, linewidth = 1.05) +
  geom_point(position = position_dodge(width = 0.42), size = 4.0) +
  facet_grid(ca ~ year, labeller = labeller(year = c(
    "2017" = "2017: Corbyn",
    "2021" = "2021: Starmer",
    "2024" = "2024: Starmer"
  ))) +
  scale_x_continuous(breaks = c(2, 8), limits = c(0, 10)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_colour_manual(values = contest_colours) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Labour voting and assessments of the national leader",
    subtitle = "Model C | Corbyn (2017), Starmer (2021 and 2024); 95% credible intervals",
    x = "Leader assessment (0–10)", y = "Predicted Labour vote (%)"
  ) +
  theme_df() + figure_theme +
  theme(
    strip.text.x = element_text(size = 16, face = "bold"),
    strip.text.y.right = element_text(size = 18, angle = 270, face = "bold"),
    panel.spacing = grid::unit(0.22, "cm")
  )


p4_gm <- p4_data |>
  filter(ca == "Greater Manchester") |>
  ggplot(aes(x = leader_rating, y = mean_pct, colour = contest,
             group = contest)) +
  geom_errorbar(aes(ymin = lower_pct, ymax = upper_pct),
                position = position_dodge(width = 0.42),
                width = 0.22, linewidth = 1.1) +
  geom_point(position = position_dodge(width = 0.42), size = 4.8) +
  facet_wrap(~year, nrow = 1, labeller = labeller(year = c(
    "2017" = "2017: Corbyn",
    "2021" = "2021: Starmer",
    "2024" = "2024: Starmer"
  ))) +
  scale_x_continuous(breaks = c(2, 8), limits = c(0, 10)) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  scale_colour_manual(values = contest_colours) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "Greater Manchester: Labour voting and leader assessments",
    subtitle = "Model C | Neutral ideological profile, 95% credible intervals",
    x = "Leader assessment (0–10)", y = "Predicted Labour vote (%)"
  ) +
  theme_df() + figure_theme


## Figure 5: Non-Labour parliamentary voters supporting Labour mayors, 2024

if (!exists("bes_long")) {
  stop("Figures 5–6 require data/bes_mayors_long.rds; run from the repo root.")
}

# National party labels come from BES Wave 26, as in the original script.
w26_parties <- haven::read_sav(
  "data/w26.sav", col_select = c("id", "generalElectionVote")
) |>
  transmute(
    id = as.character(id),
    national_party_raw = as.character(haven::as_factor(generalElectionVote))
  )

mayoral_data <- bes_long |>
  filter(
    wave == 26L,
    mayoral_authority %in% c(
      "Greater Manchester", "Liverpool City Region",
      "West Midlands", "West Yorkshire"
    ),
    !is.na(mayor_labour), is.finite(wt), wt > 0
  ) |>
  left_join(w26_parties, by = "id", relationship = "many-to-one") |>
  mutate(
    national_party = case_when(
      national_code == 2 ~ "Labour",
      str_detect(national_party_raw, regex("Conservative", ignore_case = TRUE)) ~ "Conservative",
      str_detect(national_party_raw, regex("Liberal Democrat", ignore_case = TRUE)) ~ "Lib Dem",
      str_detect(national_party_raw, regex("Green", ignore_case = TRUE)) ~ "Green",
      str_detect(national_party_raw, regex("Reform|Brexit Party", ignore_case = TRUE)) ~ "Reform",
      TRUE ~ NA_character_
    ),
    labour_mayor = as.integer(mayor_labour == 1),
    national_party = factor(
      national_party,
      levels = c("Labour", "Lib Dem", "Green", "Conservative", "Reform")
    ),
    mayoral_authority = factor(
      mayoral_authority,
      levels = c("Greater Manchester", "Liverpool City Region",
                 "West Midlands", "West Yorkshire")
    )
  ) |>
  filter(!is.na(national_party))


mayoral_model <- glm(
  labour_mayor ~ mayoral_authority * national_party,
  data = mayoral_data, family = quasibinomial(), weights = wt
)

mayoral_predictions <- mayoral_data |>
  distinct(mayoral_authority, national_party) |>
  arrange(mayoral_authority, national_party)

mayoral_pred_link <- predict(
  mayoral_model, newdata = mayoral_predictions,
  type = "link", se.fit = TRUE
)

mayoral_predictions <- mayoral_predictions |>
  mutate(
    probability = 100 * plogis(mayoral_pred_link$fit),
    lower = 100 * plogis(mayoral_pred_link$fit - 1.96 * mayoral_pred_link$se.fit),
    upper = 100 * plogis(mayoral_pred_link$fit + 1.96 * mayoral_pred_link$se.fit)
  )

p_mayoral_crossparty <- mayoral_predictions |>
  filter(national_party != "Labour") |>
  ggplot(aes(x = probability, y = national_party,
             colour = national_party)) +
  geom_segment(aes(x = lower, xend = upper, yend = national_party),
               linewidth = 3.1, alpha = 0.30, lineend = "round") +
  geom_point(size = 9) +
  facet_wrap(~mayoral_authority, ncol = 2) +
  scale_colour_manual(values = party_cols) +
  scale_x_continuous(breaks = seq(0, 100, 25),
                     labels = \(x) paste0(x, "%")) +
  coord_cartesian(xlim = c(0, 80)) +
  labs(
    title = "Labour mayoral support among non-Labour voters, 2024",
    subtitle = "By parliamentary party choice | Model estimates and approximate 95% confidence intervals",
    x = "Probability of Labour mayoral vote", y = NULL
  ) +
  theme_df() + figure_theme +
  theme(
    strip.text = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )


## Figure 6: GM Blackbox distributions, national and mayoral, 2017–2024


gm_heat <- bes_long |>
  filter(
    wave %in% c(12, 21, 26),
    mayoral_authority == "Greater Manchester",
    is.finite(econ_z), is.finite(auth_z), is.finite(wt), wt > 0
  ) |>
  mutate(
    year = factor(wave, levels = c(12, 21, 26),
                  labels = c("2017", "2021", "2024")),
    econ_plot = -econ_z
  ) |>
  dplyr::select(year, econ_plot, auth_z, wt, national_labour, mayor_labour) |>
  pivot_longer(cols = c(national_labour, mayor_labour),
               names_to = "contest", values_to = "labour_vote") |>
  filter(labour_vote == 1) |>
  mutate(
    contest = factor(
      contest, levels = c("national_labour", "mayor_labour"),
      labels = c("National", "Mayoral")
    )
  )



blackbox_bandwidth <- 0.28

gm_density <- gm_heat |>
  group_by(year, contest) |>
  nest() |>
  mutate(surface = map(data, \(d) {
    grid <- seq(-3, 3, length.out = 180)
    kx <- outer(grid, d$econ_plot, \(x, mu)
      dnorm((x - mu) / blackbox_bandwidth))
    ky <- outer(d$auth_z, grid, \(mu, y)
      dnorm((y - mu) / blackbox_bandwidth))
    density <- kx %*% sweep(ky, 1, d$wt, `*`)
    expand_grid(auth_z = grid, econ_plot = grid) |>
      mutate(density = as.vector(density) /
               (sum(d$wt) * 2 * pi * blackbox_bandwidth^2))
  })) |>
  dplyr::select(year, contest, surface) |>
  unnest(surface) |>
  ungroup() |>
  mutate(relative_density = density / max(density))

p_gm_heat <- gm_density |>
  ggplot(aes(econ_plot, auth_z, z = relative_density)) +
  geom_contour_filled(breaks = c(0.12, 0.3, 0.5, 0.7, 0.9, 1.001)) +
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.9,
             linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "grey25", linewidth = 0.9,
             linetype = "dashed") +
  facet_grid(contest ~ year) +
  coord_equal(xlim = c(-3, 3), ylim = c(-3, 3), expand = FALSE) +
  scale_fill_brewer(palette = "Reds", guide = "none") +
  scale_x_continuous(breaks = c(-2, 0, 2)) +
  scale_y_continuous(breaks = c(-2, 0, 2)) +
  labs(
    title = "Greater Manchester: ideological distribution of Labour voters",
    subtitle = "National and mayoral vote, 2017–2024 | Weighted relative kernel density",
    x = "Economic position (left to right)",
    y = "Cultural position (liberal to authoritarian)"
  ) +
  theme_df() + figure_theme +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(size = 17, face = "bold"),
    strip.text.y.right = element_text(size = 18, angle = 270),
    axis.text = element_text(size = 15),
    legend.position = "none",
    panel.spacing = grid::unit(0.25, "cm")
  )

p_gm_heat_2024 <- p_gm_heat %+% (gm_density |> filter(year == "2024")) +
  facet_wrap(~contest, nrow = 1) +
  labs(
    title = "Greater Manchester: ideological distribution of Labour voters, 2024",
    subtitle = "Weighted relative kernel density | Common national–mayoral scale"
  )





## Display-only updates: keep headings in the manuscript, not inside the images.
# These changes do not modify any fitted model or posterior result.
p1 <- p1 + labs(title = NULL, subtitle = NULL)
p2 <- p2 + labs(title = NULL, subtitle = NULL)
p2_gm <- p2_gm + labs(title = NULL, subtitle = NULL)
p3 <- p3 + labs(title = NULL, subtitle = NULL)
p3_gm <- p3_gm + labs(title = NULL, subtitle = NULL)
p4 <- p4 + labs(title = NULL, subtitle = NULL)
p4_gm <- p4_gm + labs(title = NULL, subtitle = NULL)
p_mayoral_crossparty <- p_mayoral_crossparty + labs(title = NULL, subtitle = NULL)
p_gm_heat <- p_gm_heat + labs(title = NULL, subtitle = NULL)
p_gm_heat_2024 <- p_gm_heat_2024 + labs(title = NULL, subtitle = NULL)

# The comprehensive 5-authority figures are for landscape pages/appendices.
# Make text big relative to the saved canvas (not just the pixel count).
p2 <- p2 + theme(
  axis.text = element_text(size = 20),
  axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
  axis.title = element_text(size = 22),
  strip.text.x = element_text(size = 21, face = "bold"),
  strip.text.y.right = element_text(size = 21, angle = 270, face = "bold"),
  legend.text = element_text(size = 21)
)
p3 <- p3 + theme(
  axis.text = element_text(size = 20),
  axis.title = element_text(size = 22),
  strip.text.x = element_text(size = 21, face = "bold"),
  strip.text.y.right = element_text(size = 21, angle = 270, face = "bold"),
  legend.text = element_text(size = 21)
)

## Figure 7: Rail renationalisation, 2017 and 2024 — England
# Original BES question: "Britain should renationalise the railways".
# 1 = strongly disagree ... 5 = strongly agree; 9999 = don't know.
# Outcome: agree or strongly agree (4/5), versus valid 1–3.
# This is a separate supplementary logistic model, NOT a Blackbox refit.
# The existing England-wide Blackbox ideological coordinates are predictors.

w12_options <- c("data/w12.sav", path.expand("~/data/w12.sav"),
                 "C:/R/datasets/bes_waves/w12.sav")
w26_options <- c("data/w26.sav", path.expand("~/data/w26.sav"),
                 "C:/R/datasets/bes_waves/w26.sav")
w12_path <- w12_options[file.exists(w12_options)][1]
w26_path <- w26_options[file.exists(w26_options)][1]
if (is.na(w12_path) || is.na(w26_path)) {
  stop("Rail figure requires w12.sav and w26.sav. Check data/, ~/data/ or C:/R/datasets/bes_waves/.")
}

# Print verified variable availability and coded labels before fitting.
rail_headers <- bind_rows(
  tibble(wave = 12L, variable = names(haven::read_sav(w12_path, n_max = 0))),
  tibble(wave = 26L, variable = names(haven::read_sav(w26_path, n_max = 0)))
)
rail_variable_check <- rail_headers |>
  filter(str_detect(variable, regex("renationalis|rail|pubPriv|socgrade", ignore_case = TRUE)))
print(rail_variable_check, n = Inf)
if (!all(c(12L, 26L) %in% (rail_variable_check |>
                             filter(variable == "renationaliseRail") |>
                             pull(wave)))) {
  stop("renationaliseRail absent from W12 or W26; check the printed variable names.")
}

rail_raw <- bind_rows(
  haven::read_sav(w12_path, col_select = c("id", "renationaliseRail")) |>
    transmute(id = as.character(id), wave = 12L,
              rail_response = as.numeric(renationaliseRail)),
  haven::read_sav(w26_path, col_select = c("id", "renationaliseRail")) |>
    transmute(id = as.character(id), wave = 26L,
              rail_response = as.numeric(renationaliseRail))
)
stopifnot(!anyDuplicated(rail_raw[c("id", "wave")]))
print(rail_raw |> count(wave, rail_response, name = "n"), n = Inf)

# The local-authority code prefix E identifies England. This retains
# respondents outside the five mayoral authorities for this question.
rail_data <- bes_long |>
  filter(wave %in% c(12L, 26L),
         str_starts(as.character(oslaua_code), "E"),
         is.finite(econ_z), is.finite(auth_z),
         is.finite(wt), wt > 0) |>
  transmute(id = as.character(id), wave = as.integer(wave),
            econ_z, auth_z, wt) |>
  left_join(rail_raw, by = c("id", "wave"), relationship = "many-to-one") |>
  mutate(
    rail_response = na_if(rail_response, 9999),
    rail_support = case_when(
      rail_response %in% 4:5 ~ 1L,
      rail_response %in% 1:3 ~ 0L,
      TRUE ~ NA_integer_
    ),
    year = factor(wave, levels = c(12L, 26L), labels = c("2017", "2024"))
  ) |>
  filter(!is.na(rail_support)) |>
  group_by(year) |>
  mutate(wt_model = wt / mean(wt)) |>
  ungroup()

rail_sample <- rail_data |>
  group_by(year) |>
  summarise(n = n(), weighted_support = weighted.mean(rail_support, wt_model),
            effective_n = sum(wt_model)^2 / sum(wt_model^2), .groups = "drop")
print(rail_sample, n = Inf)
if (n_distinct(rail_data$year) != 2L || nrow(rail_data) < 100L) {
  stop("Too few valid rail ownership observations across 2017 and 2024.")
}

# Interactions permit changing economic and cultural associations and their
# joint configuration. Four comparable +/-1 SD scores are evaluated.
rail_model <- glm(
  rail_support ~ year * econ_z * auth_z,
  data = rail_data,
  family = quasibinomial(),
  weights = wt_model
)

rail_vcov <- if (requireNamespace("sandwich", quietly = TRUE)) {
  sandwich::vcovCL(rail_model, cluster = rail_data$id)
} else {
  message("sandwich not installed: using model-based rather than respondent-clustered intervals")
  vcov(rail_model)
}

rail_grid <- expand_grid(
  profile = factor(
    c("Left-authoritarian", "Right-libertarian",
      "Left-libertarian", "Right-authoritarian"),
    levels = c("Left-authoritarian", "Right-libertarian",
               "Left-libertarian", "Right-authoritarian")
  ),
  year = factor(c("2017", "2024"), levels = c("2017", "2024"))
) |>
  mutate(
    econ_z = case_when(profile %in% c("Left-authoritarian", "Left-libertarian") ~ 1, TRUE ~ -1),
    auth_z = case_when(profile %in% c("Left-authoritarian", "Right-authoritarian") ~ 1, TRUE ~ -1)
  )

# Draws from the coefficient sampling distribution give intervals for
# both levels and their differences, sharing parameter uncertainty.
set.seed(20261023)
rail_coeff_draws <- MASS::mvrnorm(
  n = 4000, mu = coef(rail_model), Sigma = rail_vcov
)
rail_design <- model.matrix(delete.response(terms(rail_model)), data = rail_grid)
rail_probability_draws <- plogis(rail_design %*% t(rail_coeff_draws))
rail_predictions <- rail_grid |>
  mutate(
    probability = as.vector(plogis(rail_design %*% coef(rail_model))),
    lower = apply(rail_probability_draws, 1, quantile, probs = 0.025),
    upper = apply(rail_probability_draws, 1, quantile, probs = 0.975)
  )

rail_change_rows <- seq(1L, nrow(rail_grid), by = 2L)
rail_change_draws <- rail_probability_draws[rail_change_rows + 1L, , drop = FALSE] -
  rail_probability_draws[rail_change_rows, , drop = FALSE]
rail_changes <- rail_grid[rail_change_rows, ] |>
  mutate(
    change_pp = 100 * (rail_predictions$probability[rail_change_rows + 1L] -
                         rail_predictions$probability[rail_change_rows]),
    lower = 100 * apply(rail_change_draws, 1, quantile, probs = 0.025),
    upper = 100 * apply(rail_change_draws, 1, quantile, probs = 0.975)
  ) |>
  dplyr::select(profile, change_pp, lower, upper)

rail_colours <- c(
  "Left-authoritarian" = "#E4003B",
  "Right-libertarian" = "#2e368e",
  "Left-libertarian" = "#00A8A8",
  "Right-authoritarian" = "#8C669B"
)

# Main figure: the two cross-pressured poles central to the hypothesis.
p_rail <- rail_predictions |>
  filter(profile %in% c("Left-authoritarian", "Right-libertarian")) |>
  ggplot(aes(x = year, y = 100 * probability,
             colour = profile, group = profile)) +
  geom_line(linewidth = 1.5) +
  geom_errorbar(aes(ymin = 100 * lower, ymax = 100 * upper),
                width = 0.12, linewidth = 1.2) +
  geom_point(size = 5) +
  scale_colour_manual(values = rail_colours, breaks = c("Left-authoritarian", "Right-libertarian")) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20),
                     labels = scales::label_percent(scale = 1)) +
  labs(x = "Survey year", y = "Predicted support for rail renationalisation", colour = NULL) +
  theme_df() + figure_theme +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 21),
    legend.text = element_text(size = 19)
  )

# Supplemental figure: changes across four fixed ideological profiles.
p_rail_change <- rail_changes |>
  ggplot(aes(y = fct_rev(profile), x = change_pp, colour = profile)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_segment(aes(x = lower, xend = upper, yend = fct_rev(profile)),
               linewidth = 1.35) +
  geom_point(size = 5) +
  scale_colour_manual(values = rail_colours, guide = "none") +
  labs(x = "Change in predicted probability, 2024 minus 2017 (pp)", y = NULL) +
  theme_df() + figure_theme +
  theme(axis.text = element_text(size = 18), axis.title = element_text(size = 20))

readr::write_csv(rail_predictions,
                 file.path(figures_dir, "07_rail_ownership_modelled_probabilities.csv"))
readr::write_csv(rail_changes,
                 file.path(figures_dir, "07_rail_ownership_modelled_changes.csv"))

## Figure 8: Class — GM complete-case descriptive evidence
# Existing Appendix E includes these descriptive AB/C1/C2/DE results.
# This does not add social grade to Models A–C.
class_data <- model_A |>
  filter(ca == "Greater Manchester", !is.na(social_grade4),
         is.finite(wt_model), wt_model > 0) |>
  mutate(
    year = factor(wave, levels = c(12L, 21L, 26L),
                  labels = c("2017", "2021", "2024")),
    social_grade4 = factor(social_grade4, levels = c("AB", "C1", "C2", "DE"))
  ) |>
  group_by(year, social_grade4, contest) |>
  summarise(
    n = n(),
    effective_n = sum(wt_model)^2 / sum(wt_model^2),
    probability = weighted.mean(labour, wt_model),
    .groups = "drop"
  ) |>
  mutate(
    se = sqrt(probability * (1 - probability) / effective_n),
    lower = pmax(0, probability - 1.96 * se),
    upper = pmin(1, probability + 1.96 * se)
  )

p_class_gm <- class_data |>
  ggplot(aes(year, 100 * probability, colour = contest, group = contest)) +
  geom_line(linewidth = 1.1) +
  geom_errorbar(aes(ymin = 100 * lower, ymax = 100 * upper),
                width = 0.13, linewidth = 0.85) +
  geom_point(size = 4) +
  facet_wrap(~social_grade4, nrow = 2) +
  scale_colour_manual(values = contest_colours) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "Election year", y = "Weighted Labour vote share (%)") +
  theme_df() + figure_theme +
  theme(axis.text = element_text(size = 17), strip.text = element_text(size = 20))

readr::write_csv(class_data,
                 file.path(figures_dir, "08_gm_social_grade_shares.csv"))

ggsave(file.path(figures_dir, "01_gm_gradients.png"), p1,
       width = 11.2, height = 6.0, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "02_cross_pressure_profiles.png"), p2,
       width = 12.0, height = 8.0, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "03_mii_profiles.png"), p3,
       width = 12.0, height = 8.0, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "04_leadership.png"), p4,
       width = 13.2, height = 12.2, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "05_mayoral_crossparty_2024.png"), p_mayoral_crossparty,
       width = 11.6, height = 7.6, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "06_gm_labour_heatmap_2017_2024.png"), p_gm_heat,
       width = 11.5, height = 8.1, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "06_gm_labour_heatmap.png"), p_gm_heat_2024,
       width = 10.6, height = 5.8, dpi = 300, bg = "white")



ggsave(file.path(figures_dir, "02_cross_pressure_gm.png"), p2_gm,
       width = 10.8, height = 5.4, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "03_mii_gm_2024.png"), p3_gm,
       width = 10.6, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "04_leadership_gm.png"), p4_gm,
       width = 11.4, height = 5.6, dpi = 300, bg = "white")

print(mayoral_predictions, n = Inf)
print(p4_gm)
print(p_mayoral_crossparty)


ggsave(file.path(figures_dir, "07_rail_ownership_profiles_2017_2024.png"), p_rail,
       width = 9.0, height = 5.2, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "07_rail_ownership_change_pp.png"), p_rail_change,
       width = 9.4, height = 4.8, dpi = 300, bg = "white")
ggsave(file.path(figures_dir, "08_gm_class_descriptive.png"), p_class_gm,
       width = 9.0, height = 7.2, dpi = 300, bg = "white")

print(rail_predictions, n = Inf)
print(rail_changes, n = Inf)
print(class_data, n = Inf)
