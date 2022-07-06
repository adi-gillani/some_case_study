library(dplyr)
library(readxl)

#importing data
sales_data <- read_excel("C:/Users/Adnan/Downloads/business_analyst_case/sales_data_excel.xlsx",
                         col_types = c("text", "numeric", "text", "text", "numeric", "date", 
                                       "numeric","date", "text", "numeric", "date",
                                       "text", "numeric"))

#calculating datediff which is basically the lag between two consecutive orders
model_data <- sales_data  %>% 
  arrange(company, fixed_status_change_date) %>%
  group_by(company) %>% 
  summarise(status_change_date = fixed_status_change_date,
            opportunity_value = opportunity_value,
            old_status_label = old_status_label,
            new_status_label = new_status_label,
            lead_created_date = fixed_lead_created_date,
            current_lead_status = current_lead_status,
            lead_created_date = fixed_lead_created_date,
            billing_period_unit = billing_period_unit,
            billing_period = billing_period,
            date_difference = fixed_status_change_date - lag(fixed_status_change_date)) %>%
  mutate(contact_type = ifelse(is.na(date_difference), "First Contact", "Not First Contact")) %>%
  mutate(contact_rank = rank(status_change_date, ties.method = 'first'))

#converting date difference into days
model_data$date_difference <-(model_data$date_difference/86400)
model_data$date_difference <- sub('sec', '', model_data$date_difference)

#identifying the final status change object
model_data <- model_data %>% mutate(last_status = ifelse(contact_rank == max(contact_rank), old_status_label, "NA"))

#identifying re-engagement object
model_data <- model_data %>% 
  mutate(reengage_contact = ifelse(contact_rank != max(contact_rank) & old_status_label == "Opp - Lost", "Contact Re-Engaged", "NA"))

#finding object to derive the time it takes to make first contact
model_data <- model_data %>% 
  group_by(company) %>%
  mutate(mr_sd_qualified = ifelse(old_status_label %in% c('MR - Qualification', 'SD - Engaged','SD - Interested') &
                                    new_status_label == 'AE - Qualified',
                                  1, 0))

#tagging marketing qualified leads
model_data <- model_data %>%
  group_by(company) %>%
  mutate(market_qualified = ifelse(mr_sd_qualified %in% 1, 1,NA))

#calculating sales cycle for closed deals
model_data <- model_data %>%
  mutate(sales_cycle = ifelse(current_lead_status == 'Paying',
                              max(status_change_date) - lead_created_date,
                              0))
#calculating date difference for one contact clients
model_data <- model_data %>%
  group_by(company, contact_rank) %>%
  mutate(date_diff_one_contact = ifelse(contact_rank <= 1, status_change_date - lead_created_date, 0))

#calculating disqualified leads
model_data <- model_data %>%
  mutate(disqualified_lead = ifelse(last_status %in% c('MR - Qualification', 'SD - Engaged', 'SD - Interested') &
                                      current_lead_status == 'Opp - Lost', 1, 0))

#calculating time to first contact
model_data <- model_data %>%
  mutate(time_to_first_contact = ifelse(contact_type == 'First Contact',
                                        status_change_date - lead_created_date, 0))

#calculating quote to close ratio
model_data <- model_data %>%
  mutate(quote_to_close = ifelse(last_status %in% c('AE - Negotiation') &
                                   current_lead_status %in% c('Confirmed', 'Paying'), 1,0))

#calculating stage time
#model_data <- model_data %>%
#  mutate(stage_time = ifelse(as.character(date_difference) == "", date_diff_one_contact, date_difference))

#exporting excel
writexl::write_xlsx(model_data, "model_data.xlsx")
