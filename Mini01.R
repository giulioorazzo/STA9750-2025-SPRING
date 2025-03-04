if(!file.exists("data/mp01/nyc_payroll_export.csv")){
  dir.create("data/mp01", showWarnings=FALSE, recursive=TRUE)
  
  ENDPOINT <- "https://data.cityofnewyork.us/resource/k397-673e.json"
  
  if(!require("httr2")) install.packages("httr2")
  library(httr2)
  
  if(!require("jsonlite")) install.packages("jsonlite")
  library(jsonlite)
  
  if(!require("dplyr")) install.packages("dplyr")
  library(dplyr)
  
  if(!require("readr")) install.packages("readr")
  library(readr)
  
  BATCH_SIZE <- 50000
  OFFSET     <- 0
  END_OF_EXPORT <- FALSE
  ALL_DATA <- list()
  
  while(!END_OF_EXPORT){
    cat("Requesting items", OFFSET, "to", BATCH_SIZE + OFFSET, "\n")
    
    req <- request(ENDPOINT) |>
      req_url_query(`$limit`  = BATCH_SIZE, 
                    `$offset` = OFFSET)
    
    resp <- req_perform(req)
    
    batch_data <- fromJSON(resp_body_string(resp))
    
    ALL_DATA <- c(ALL_DATA, list(batch_data))
    
    if(NROW(batch_data) != BATCH_SIZE){
      END_OF_EXPORT <- TRUE
      
      cat("End of Data Export Reached\n")
    } else {
      OFFSET <- OFFSET + BATCH_SIZE
    }
  }
  
  ALL_DATA <- bind_rows(ALL_DATA)
  
  cat("Data export complete:", NROW(ALL_DATA), "rows and", NCOL(ALL_DATA), "columns.")
  
  write_csv(ALL_DATA, "data/mp01/nyc_payroll_export.csv")
}
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(scales)
})

data <- read_csv("C:/Users/orazz/OneDrive/Documents/STA9750-2025-SPRING/data/mp01/nyc_payroll_export.csv")

data <- data |>
  mutate(agency_name = str_to_title(agency_name),
    last_name = str_to_title(last_name),
    first_name = str_to_title(first_name),
    work_location_borough = str_to_title(work_location_borough),
    title_description = str_to_title(title_description),
    leave_status_as_of_june_30 = str_to_title(leave_status_as_of_june_30)
  )
#create unique key identifier
data<- data|>
  mutate(primary_key =paste(first_name,
                            coalesce(mid_init, ""),
                            last_name)
         )

data <- data|>
  mutate( total_pay =case_when(
    pay_basis == "per Hour" ~ (regular_hours * base_salary ) + (ot_hours *(base_salary * 1.5)),
    pay_basis == "per Day" ~ (base_salary *(regular_hours / 7.5)) + (ot_hours * (base_salary/7.5)*1.5),
    pay_basis == "per Annum" ~ (base_salary) + (((base_salary )/ 1950) * ot_hours),
    pay_basis == "Prorated Annual" ~ regular_gross_paid,
    TRUE ~ NA_real_
  ))
mayor_table <- data |> filter( first_name == "Eric" , last_name == "Adams", mid_init == "L")|>
  select("First Name" = first_name, 
         "Last Name"= last_name, 
         "Middle Initial" = mid_init, 
         "Fiscal Year" = fiscal_year,
         "Position" = title_description , 
         "Agency" = agency_name, 
         "Salary" = total_pay
         ) |>
  arrange(`Fiscal Year`)

# Group by Fiscal Year and calculate the average salary if there are multiple positions in the same year
mayor_table_avg <- mayor_table |>
  group_by(`Fiscal Year`) |>
  summarize(
    avg_salary = if(n() > 1) mean(Salary, na.rm = TRUE) else first(Salary),  # Average salary if multiple positions in a year, else keep the single salary
    .groups = "drop"  # Remove grouping after summarization
  )

# Merge the average salary back to the original table (to keep the positions as well)
mayor_table <- mayor_table |>
  left_join(mayor_table_avg, by = "Fiscal Year") |>
  select(-Salary) |>
  rename(Salary = avg_salary)
# Reorder columns to have 'Fiscal Year' first
mayor_table <- mayor_table |> 
  select(`Fiscal Year`, everything())
# Format Salary as dollar values
mayor_table$Salary <- dollar(mayor_table$Salary)

#View(mayor_table) 
  
#data|> filter(first_name =="Eric" , mid_init =="L", last_name =="Adams")|>View()

# View(filter(data , first_name == "Eric" , last_name == "Adams", mid_init == "L"))
# Check what kind of data for different pay_basis
#data |> filter(pay_basis == "per Hour")|>
 # select(pay_basis, regular_hours, base_salary, regular_gross_paid, ot_hours, total_ot_paid, total_other_pay , total_pay)|>
  #View()


# checking to see how many regular_gross_paid are less than 0
#data |> filter(regular_gross_paid < 0)|>
  #summarize(count = n())
#data |> filter( pay_basis == "Prorated Annual" ) |>summarize(count = n())

#Assuming no legitimate hourly rate is > $1000/hr
#filter(pay_basis == "per Hour", base_salary > 1000) |>
  #select(title_description, base_salary, regular_hours, ot_hours, total_pay) |>
  #distinct()
# switch Custodian Engineer from hourly to salary
data<- data|>
  mutate(pay_basis = if_else(title_description == "Custodian Engineer" & pay_basis == "per Hour", "per Annum", pay_basis))

# Creating hourly rate
data<- data|>
  mutate(hourly_rate = case_when(
    pay_basis == "per Hour" ~ base_salary,
    pay_basis == "per Day" ~ base_salary / 7.5,
    pay_basis == "per Annum" ~ base_salary / 2000,
    pay_basis == "Prorated Annual" ~ base_salary / 2000,
    TRUE ~ NA_real_))
# selecting highest paid title by hourly rate
highest_paid_title <- data|>
  mutate(hourly_rate = case_when(
    pay_basis == "per Hour" ~ base_salary,
    pay_basis == "per Day" ~ base_salary / 7.5,
    pay_basis == "per Annum" ~ base_salary / 2000,
    pay_basis == "Prorated Annual" ~ base_salary / 2000,
    TRUE ~ NA_real_))|>
  group_by(title_description)|>
  summarize(avg_hourly_rate = mean(hourly_rate , na.rm = TRUE) , pay_basis , base_salary , regular_hours, regular_gross_paid)|>
  slice_max(order_by = avg_hourly_rate , n =1)|> View()


print(highest_paid_title)
highest_salary<- data|>
  group_by(title_description)|>
  summarize(avg_tot_pay = mean(total_pay))|>
  slice_max(order_by = avg_tot_pay , n=1)

data|> filter(title_description == "Chief Actuary")|>
  summarize(base_salary , total_pay , regular_hours, regular_gross_paid , pay_basis)|>
  slice_max( order_by = total_pay)|> View()

highest_paid_employee <-
  data |> group_by(fiscal_year)|>
  slice_max(total_pay , n= 1)|>
  select(fiscal_year , title_description, agency_name , first_name, mid_init, last_name, total_pay)|>
  ungroup()|>
  slice_max(total_pay , n=1)|> View()

overtime<- data|>
  group_by()|>
  summarize(tot_ot = sum(ot_hours , na.rm = TRUE))

mayor_table <- data |> filter( first_name == "Eric" , last_name == "Adams", mid_init == "L")|>
  select("First Name" = first_name, 
         "Last Name"= last_name, 
         "Middle Initial" = mid_init, 
         "Fiscal Year" = fiscal_year,
         "Position" = title_description , 
         "Agency" = agency_name, 
         "Salary" = total_pay
  ) |>
  arrange(`Fiscal Year`)

# Group by Fiscal Year and calculate the average salary if there are multiple positions in the same year
mayor_table_avg <- mayor_table |>
  group_by(`Fiscal Year`) |>
  summarize(
    avg_salary = if(n() > 1) mean(Salary, na.rm = TRUE) else first(Salary),
    .groups = "drop"
  )

# Merge the average salary back to the original table (to keep the positions as well)
mayor_table <- mayor_table |>
  left_join(mayor_table_avg, by = "Fiscal Year") |>
  select(-Salary) |>
  rename(Salary = avg_salary)
# Reorder columns to have 'Fiscal Year' first
mayor_table_updated <- mayor_table |> 
  select(`Fiscal Year`, everything())|>
  distinct(`Fiscal Year`, .keep_all = TRUE)
# Format Salary as dollar values
mayor_table_updated$Salary <- dollar(mayor_table_updated$Salary)

overpaid_employees <- data |>
  left_join(mayor_table_updated, by = c("fiscal_year" = "Fiscal Year")) |>
  filter(!is.na(Salary), total_pay > Salary) |>
  mutate(
    total_pay = as.numeric(total_pay),
    Salary = as.numeric(Salary),
    savings = total_pay - Salary  # Calculate savings here
  ) |>
  select(fiscal_year, first_name, last_name, title_description, agency_name, total_pay, Salary, savings)

# Calculate total potential savings
total_savings <- overpaid_employees |>
  summarize(total_savings = sum(savings, na.rm = TRUE))
total_savings$total_savings <- as.numeric(total_savings$total_savings)
total_savings$total_savings <- dollar(total_savings$total_savings)

# Identify affected agencies and calculate savings
affected_agencies <- overpaid_employees |>
  group_by(agency_name, title_description) |>
  summarize(
    total_affected = n(), 
    total_savings = sum(savings, na.rm = TRUE)
  ) |>
  arrange(desc(total_savings))
affected_agencies$total_savings <- dollar(affected_agencies$total_savings)

mayor_salary <- mayor_table|>
  select(`Fiscal Year` , Salary)|>
  rename('fiscal_year' = `Fiscal Year`)
overpaid_employees <- data |>
  left_join(mayor_salary , by = fiscal_year )
overpaid_employees <- overpaid_employees |>
  mutate(
    savings = total_pay - Salary  
  )

savings_summary <- overpaid_employees |>
  summarize(
    min_savings = min(savings, na.rm = TRUE), 
    max_savings = max(savings, na.rm = TRUE),
    mean_savings = mean(savings, na.rm = TRUE),
    sum_savings = sum(savings, na.rm = TRUE)
  )

zero_or_negative_savings <- overpaid_employees |>
  filter(savings <= 0)

missing_or_zero_salary <- overpaid_employees |>
  filter(is.na(Salary) | Salary == 0 | total_pay == 0)

print(missing_or_zero_salary) 

overpaid_employees_clean <- overpaid_employees |>
  filter(savings > 0)

total_savings <- overpaid_employees_clean |>
  summarize(total_savings = sum(savings, na.rm = TRUE))

total_savings$total_savings <- as.numeric(total_savings$total_savings)

affected_agencies <- overpaid_employees_clean |>
  group_by(agency_name, title_description) |>
  summarize(
    total_affected = n(),               
    total_savings = sum(savings, na.rm = TRUE)  
  ) |>
  arrange(desc(total_savings))             
affected_agencies$total_savings <- as.numeric(affected_agencies$total_savings)




mayor_salary <- mayor_table|>
  select(`Fiscal Year` , Salary)|>
  rename(fiscal_year = `Fiscal Year`)
overpaid_employees <- data |>
  left_join(mayor_salary , by = fiscal_year )
overpaid_employees <- overpaid_employees |>
  mutate(
    savings = total_pay - Salary  
  )

savings_summary <- overpaid_employees |>
  summarize(
    min_savings = min(savings, na.rm = TRUE), 
    max_savings = max(savings, na.rm = TRUE),
    mean_savings = mean(savings, na.rm = TRUE),
    sum_savings = sum(savings, na.rm = TRUE)
  )

zero_or_negative_savings <- overpaid_employees |>
  filter(savings <= 0)

missing_or_zero_salary <- overpaid_employees |>
  filter(is.na(Salary) | Salary == 0 | total_pay == 0)

print(missing_or_zero_salary) 

overpaid_employees_clean <- overpaid_employees |>
  filter(savings > 0)

total_savings <- overpaid_employees_clean |>
  summarize(total_savings = sum(savings, na.rm = TRUE))

total_savings$total_savings <- as.numeric(total_savings$total_savings)

affected_agencies <- overpaid_employees_clean |>
  group_by(agency_name, title_description) |>
  summarize(
    total_affected = n(),               
    total_savings = sum(savings, na.rm = TRUE)  
  ) |>
  arrange(desc(total_savings))             
affected_agencies$total_savings <- as.numeric(affected_agencies$total_savings)


ot_by_title <- data|>
  group_by(agency_name , title_description)|>
  summarize(total_ot_hours = sum(ot_hours , na.rm = TRUE),.groups = 'drop')

full_time_equivalent  <- ot_by_title|>
  mutate(full_time_equivalents = as.numeric(total_ot_hours) / 2000)
ot_by_title <- ot_by_title |>
  left_join(data|>
              group_by(agency_name , title_description)|>
              summarize( hourly_rate = mean(hourly_rate ), .groups = 'drop'),
            by = c("agency_name" , "title_description"))
ot_by_title <- ot_by_title |>
  filter(!is.na(hourly_rate))
ot_by_title <- ot_by_title|>
  mutate(savings = total_ot_hours * hourly_rate * 1.5 - total_ot_hours * hourly_rate)
      
savings_by_agency <- ot_by_title |>
  group_by(agency_name) |>
  summarize(
    tot_savings = sum(savings, na.rm = TRUE),
    tot_full_time_equivalent = sum(full_time_equivalent$full_time_equivalents, na.rm = TRUE))|>
  arrange(desc(tot_savings))

top_agencies <- savings_by_agency|>
  top_n(5, tot_savings)

top_job_titles <- ot_by_title|>
  top_n(5, savings)



data_transformed <- data|>
  group_by(agency_name, fiscal_year) |>
  summarize(
    total_employees = n(),
    avg_payroll = mean(total_pay, na.rm = TRUE),
    positions_to_cut = floor(total_employees * 0.05),
    payroll_savings = positions_to_cut * avg_payroll
  ) |>
  ungroup()
top_agencies_reduction <- data_transformed |>
  group_by(agency_name) |>
  summarize(
    total_positions_to_cut = sum(positions_to_cut, na.rm = TRUE),
    total_payroll_savings = sum(payroll_savings, na.rm = TRUE),
    avg_payroll_savings = mean(payroll_savings, na.rm = TRUE)
  ) |>
  arrange(desc(total_payroll_savings)) %>%
  slice_head(n = 10)

annual_savings <- data_transformed|>
  group_by(fiscal_year)|>
  summarize(total_savings = sum(payroll_savings, na.rm = TRUE))
annual_savings$total_savings  <- dollar(annual_savings$total_savings)
