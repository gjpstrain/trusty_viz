data <- read_csv("data/final_data.csv")

passed <- read_csv("data/passed_all_final.csv")

passed_yes <- passed %>%
  filter(passed == TRUE)

just_passed <- inner_join(data, passed_yes, by = "participant")

trust_data <- just_passed %>%
  group_by(participant) %>%
  mutate(ID = cur_group_id())

trust_data$participant <- NULL

trust_data <- trust_data %>%
  rename(participant = ID)

write_csv(trust_data, "data/trust_data.csv")

# This script is generic.