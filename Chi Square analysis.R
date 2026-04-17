library(dplyr)
library(tidyr)
library(purrr)

df <- read.csv("myfile2.csv")   # or whatever your path is

# adjust these column names to match your file -----------------------
# Here I assume:
#   group column   = "Cohort"
#   total patients = "N"
#   cases          = "Count"
#   outcome label  = "OutcomeName"
# --------------------------------------------------------------------

chi_results <- df %>%
  # keep only concussion groups, not controls
  filter(Cohort %in% c("CX1 V12", "CX2 V12", "CX3 V12")) %>%
  mutate(
    conc_group = recode(Cohort,
                        "CX1 V12" = "1x",
                        "CX2 V12" = "2x",
                        "CX3 V12" = "3x"),
    No = N - Count
  ) %>%
  group_by(OutcomeName) %>%
  nest() %>%
  mutate(
    chisq = map(data, ~{
      mat <- as.matrix(.x[, c("Count", "No")])
      rownames(mat) <- .x$conc_group
      chisq.test(mat)
    }),
    chisq_stat = map_dbl(chisq, ~ .x$statistic),
    df        = map_dbl(chisq, ~ .x$parameter),
    p_value   = map_dbl(chisq, ~ .x$p.value)
  ) %>%
  select(OutcomeName, chisq_stat, df, p_value)

chi_results
