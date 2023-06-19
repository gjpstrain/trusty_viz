library(tidyverse)

raw <- read_csv("data/full_data.csv")

just_columns <- raw %>%
  select(c("participant", "unique_item_no", "textbox_number_component.text")) %>%
  filter(unique_item_no %in% c(181,182,183,184,185,186)) %>%
  mutate_all(~replace(., is.na(.), 50))

# The slider is at zero by default, so replacing NULL entries with 0 allows
# us to account for the fact that some participants may have simply left the
# slider on 0.

my_id <- unique(just_columns$participant)

just_columns$answer = NULL

if (just_columns$unique_item_no == 184){
  just_columns$answer = 0
}

new_df <- just_columns %>%
  mutate(answer = case_when(
    unique_item_no == 181 ~ 100,
    unique_item_no == 182 ~ 100,
    unique_item_no == 183 ~ 100,
    unique_item_no == 184 ~ 0,
    unique_item_no == 185 ~ 0,
    unique_item_no == 186 ~ 0
  )) %>%
  mutate(correct = case_when(
    unique_item_no < 184 ~ textbox_number_component.text == answer,
    unique_item_no > 183 ~ textbox_number_component.text == answer
  )) %>%
  group_by(participant) %>%
  summarise(total_correct = sum(correct)) %>%
  arrange(-total_correct)

new_df$passed <- new_df$total_correct > 3

write_csv(new_df, "data/passed_all_final.csv")

# The data frame new_df has two columns - participant id, and total attention check questions correct.







