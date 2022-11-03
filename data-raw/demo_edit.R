## code to prepare `demo_edit` dataset goes here

#library(charlatan)
demo_edit <- data.frame(
  "name" = ch_name(n = 20),
  "job" = ch_job(n = 20),
  "credit card provider" = as.factor(ch_credit_card_provider(n = 20)),
  "credit card security code" = as.numeric(ch_credit_card_security_code(n = 20)),
  "date obtained" = sample(seq(as.Date('2015/01/01'), as.Date('2022/01/01'), by = "year"), 20, replace = TRUE),
  "contactless card" = sample(c(TRUE, FALSE), 20, replace = TRUE)
)
demo_edit <- janitor::clean_names(demo_edit)

usethis::use_data(demo_edit, overwrite = TRUE)
