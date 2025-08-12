library(tidyverse)
raw_data <- read_csv(here::here("data/326_survey_test_results.csv")) 
view(raw_data)
## add my points to the plot.
## transform into z-scores
raw_data$Q4
question_bank <- raw_data |> slice(1) |> pivot_longer(starts_with("Q"),
                                     names_to = "question_number",
                                     values_to = "wording") |>
  select(question_number, wording) |>
  mutate(wording = str_remove(wording, "Rate how \"useful\" you believe completing the following learning outcome is. - ")) |>
  mutate(wording_short = case_when(str_detect(wording, "bias-variance") ~ "Bias-Variance Trade-off",
                                   str_detect(wording, "are between") ~ "Bayesian Conceptual",
                                   str_detect(wording, "Type I Error") ~ "Hypothesis Errors",
                                   str_detect(wording, "sampling distribution of a sample statistic") ~ "Samp. Dist. Concept",
                                   str_detect(wording, "interpreting a confidence interval") ~ "Conf. Int. Interpretation",
                                   str_detect(wording, "properties of a sampling distribution") ~ "Samp. Dist. Simulation",
                                   str_detect(wording, "maximum likelihood estimation") ~ "MLEs",
                                   str_detect(wording, "the bias of an estimator") ~ "MSE, Bias Calculations",
                                   str_detect(wording, "pivot method") ~ "Conf. Int Pivots",
                                   str_detect(wording, "bootstrapping") ~ "Bootstrap Conf. Intervals",
                                   str_detect(wording, "empirical conf") ~ "Simulating Conf. Intervals",
                                   str_detect(wording, "non-informative") ~ "Bayes Analysis",
                                   str_detect(wording, "permutation test") ~ "Permutation Test",
                                   str_detect(wording, "simple hypotheses") ~ "Neyman-Pearson",
                                   str_detect(wording, "empirical power") ~ "Empirical Power")) |>
  relocate(wording_short)


cleaned_full <- raw_data |>
  slice(4, 6:27) |> 
  mutate(instructor = if_else(row_number() == 1, true = "mh", false = "student")) |>
  mutate(across(starts_with("Q"), ~ as.numeric(.x))) |> 
  pivot_longer(starts_with("Q"), names_to = "Question_Number",
               values_to = "response") |>
  relocate(Question_Number, response) |>
  filter(Question_Number != "Q4" & Question_Number != "Q5" & Question_Number != "Q6") |>
  left_join(question_bank, join_by(Question_Number == question_number)) |>
  relocate(wording_short)

cleaned_data <- cleaned_full |> filter(instructor == "student")

cleaned_data_mh <- cleaned_full |> filter(instructor == "mh")

cleaned_data_Q2 <- cleaned_data |> filter(wording_short %in% c("Bias-Variance Trade-off",
                                                               "Hypothesis Errors",
                                                               "Conf. Int. Interpretation",
                                                               "Samp. Dist. Concept",
                                                               "Bayesian Conceptual")) |>
  mutate(wording_short = fct_reorder(wording_short, response, mean))

cleaned_data_Q2_mh <- cleaned_data_mh |> filter(wording_short %in% c("Bias-Variance Trade-off",
                                                               "Hypothesis Errors",
                                                               "Conf. Int. Interpretation",
                                                               "Samp. Dist. Concept",
                                                               "Bayesian Conceptual"))

cleaned_data_Q2 <- cleaned_data_Q2 |> group_by(IPAddress) |>
  mutate(mean_response = mean(response),
            sd_response = sd(response),
         z = (response - mean_response) / sd_response) |>
  print(n = Inf)

cleaned_data_Q2_mh <- cleaned_data_Q2_mh |> group_by(IPAddress) |>
  mutate(mean_response = mean(response),
         sd_response = sd(response),
         z = (response - mean_response) / sd_response) |>
  print(n = Inf)


summary_data_Q2 <- cleaned_data_Q2 |>
  group_by(wording_short) |>
  summarise(mean_response = mean(response),
            mean_z = mean(z))

ggplot(data = cleaned_data_Q2, aes(x = response, y = wording_short)) +
  geom_jitter(height = 0.10, width = 0) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  geom_point(data = summary_data_Q2, aes(x = mean_response), colour = "deeppink1",
             size = 2.5) ##+
 ## geom_point(data = cleaned_data_Q2_mh, shape = 8, colour = "purple")

ggsave("first_group_raw.pdf")
ggplot(data = cleaned_data_Q2, aes(x = z, y = wording_short)) +
  geom_jitter(height = 0.10, width = 0) +
  theme_minimal() +
  geom_point(data = summary_data_Q2, aes(x = mean_z), colour = "deeppink1",
             size = 2.5) ##+
  ##geom_point(data = cleaned_data_Q2_mh, shape = 8, colour = "purple")
ggsave("first_group_z.pdf")


cleaned_data_Q2 |> group_by(wording_short) |>
  summarise(n_resp = n(),
            mean_score = mean(response),
            median_score = median(response),
            sd_score = sd(response)) |>
  arrange(desc(mean_score))


cleaned_data_Q3 <- cleaned_data |> filter(!(wording_short %in% c("Bias-Variance Trade-off",
                                                               "Hypothesis Errors",
                                                               "Conf. Int. Interpretation",
                                                               "Samp. Dist. Concept",
                                                               "Bayesian Conceptual"))) |>
  mutate(wording_short = fct_reorder(wording_short, response, mean))

cleaned_data_Q3_mh <- cleaned_data_mh |> filter(!(wording_short %in% c("Bias-Variance Trade-off",
                                                                     "Hypothesis Errors",
                                                                     "Conf. Int. Interpretation",
                                                                     "Samp. Dist. Concept",
                                                                     "Bayesian Conceptual")))

cleaned_data_Q3 <- cleaned_data_Q3 |> group_by(IPAddress) |>
  mutate(mean_response = mean(response),
         sd_response = sd(response),
         z = (response - mean_response) / sd_response) |>
  print(n = Inf)

cleaned_data_Q3_mh <- cleaned_data_Q3_mh |> group_by(IPAddress) |>
  mutate(mean_response = mean(response),
         sd_response = sd(response),
         z = (response - mean_response) / sd_response) |>
  print(n = Inf)

summary_data_Q3 <- cleaned_data_Q3 |>
  group_by(wording_short) |>
  summarise(mean_response = mean(response),
            mean_z = mean(z))

ggplot(data = cleaned_data_Q3, aes(x = response, y = wording_short)) +
  geom_jitter(height = 0.2, width = 0.05) +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
  geom_point(data = summary_data_Q3, aes(x = mean_response), colour = "deeppink1",
             size = 2.5) ##+
 ## geom_point(data = cleaned_data_Q3_mh, shape = 8 , colour = "purple")
ggsave("second_group_raw.pdf")

ggplot(data = cleaned_data_Q3, aes(x = z, y = wording_short)) +
  geom_jitter(height = 0.2, width = 0.05) +
  theme_minimal() +
  geom_point(data = summary_data_Q3, aes(x = mean_z), colour = "deeppink1",
             size = 2.5) ##+
 ## geom_point(data = cleaned_data_Q3_mh, colour = "purple", shape = 8)
ggsave("second_group_z.pdf")

cleaned_data_Q3 |> group_by(wording_short) |>
  summarise(n_resp = n(),
            mean_score = mean(response),
            median_score = median(response),
            sd_score = sd(response)) |>
  arrange(desc(mean_score))
