# Homework8

library(dplyr)
library(ggplot2)
library(broom)
library(forcats)
library(scales)
library(dplyr)
library(tidyr)


#use the model you actually fit
trad_data$pred_prob <- predict(model_logit1, type = "response")

1
A tidy helper for weighted/binomial CIs by group
rate_ci <- function(df, group_var) {
  df %>%
    group_by({{ group_var }}) %>%
    summarise(
      n = n(),
      rate = mean(he_more_than_3yrs_than_her, na.rm = TRUE),
      se = sqrt(rate * (1 - rate) / n),
      ci_low  = pmax(0, rate - 1.96 * se),
      ci_high = pmin(1, rate + 1.96 * se),
      .groups = "drop"
    )
}

coef_plot <- tidy(model_logit1, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%                      # drop intercept
  mutate(term = fct_reorder(term, estimate))             # order by OR

ggplot(coef_plot, aes(x = term, y = estimate)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15) +
  coord_flip() +
  scale_y_log10(labels = label_number(accuracy = 0.01)) +
  labs(title = "Odds ratios from logit: Pr(He ≥3 years older)",
       x = NULL, y = "Odds ratio (log scale)") +
  theme_minimal()



  
  2

ggplot(trad_data, aes(x = pred_prob, fill = factor(he_more_than_3yrs_than_her))) +
  geom_density(alpha = 0.5, color = "black") +
  scale_fill_manual(
    name = "Actual outcome",
    values = c("0" = "#F2A7A7", "1" = "#87C6C9"),
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(title = "where false positives and false negatives occur",
       x = "Predicted probability (logit)", y = "Density") +
  theme_minimal()


3
# Use your numeric encodings to bin into labels
lab_from_num <- function(x){
  dplyr::case_when(
    x >= 18 ~ "Advanced", x >= 16 ~ "Bachelor",
    x >= 14 ~ "Some college/Assoc.", x >= 12 ~ "HS",
    TRUE ~ "Less than HS" )
}

heat_df <- trad_data %>%
  mutate(w_edu = lab_from_num(educ_numeric),
         h_edu = lab_from_num(h_educ_numeric)) %>%
  filter(!is.na(w_edu), !is.na(h_edu)) %>%
  group_by(h_edu, w_edu) %>%
  summarise(rate = mean(he_more_than_3yrs_than_her, na.rm = TRUE),
            n = n(), .groups = "drop")

ggplot(heat_df, aes(x = w_edu, y = h_edu, fill = rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(rate, accuracy = 0.1))) +
  scale_fill_gradient(low = "#f0f9e8", high = "#0868ac", labels = percent) +
  labs(title = "Share with He ≥3 years older by couple education matrix",
       x = "Woman education", y = "Man education", fill = "Share") +
  theme_minimal()
