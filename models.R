# ---
# title: "cpstn"
# format: html
# ---
#   
#   
# # Load packages ------------------------------------------------------------
# 
# # Import data --------------------------------------------------------------
# 
# # Data cleaning ------------------------------------------------------------
# 
# # Summary statistics -------------------------------------------------------
# 
# # Data visualization -------------------------------------------------------
# 
# # Model estimation ---------------------------------------------------------
# 
# # Export tables and figures ------------------------------------------------
#   # Loading 
#   
# #| message: false
# #| warning: false


# Load Packages -----------------------------------------------------------

library(readr)
library(tidyverse)
library(skimr)
library(DT)
library(stargazer)
library(broom)
library(sf)
library(tigris)
library(ggplot2)
library(margins)
library(yardstick)
library(WVPlots)
library(pROC)
library(glmnet)
library(gamlr)
library(Matrix)


# Import Data -------------------------------------------------------------

char_vars <- c("CensusTract", "County", "State")

# binary flags (ONLY actual 0/1 variables)
binary_vars <- c(
  "Urban", "GroupQuartersFlag", "LILATracts_1And10",
  "LILATracts_halfAnd10", "LILATracts_1And20",
  "LILATracts_Vehicle", "HUNVFlag", "LowIncomeTracts",
  "LA1and10", "LAhalfand10", "LA1and20",
  "LATracts_half", "LATracts1", "LATracts10",
  "LATracts20", "LATractsVehicle_20"
)

fara2019 <- read_csv("Food Access Research Atlas.csv") |>
  filter(State == "New York") |>
  rename(POP2010 = Pop2010) |> 
  mutate(Year = 2019,
         County = sub(" County$", "", County))

fara2019_clean <- fara2019 %>%
  mutate(across(all_of(char_vars), as.character)) %>%
  mutate(across(all_of(binary_vars), ~ factor(., levels = c(0,1)))) %>%
  mutate(across(
    .cols = setdiff(names(.), c(char_vars, binary_vars)),
    ~ as.numeric(.)
  )) 


fara2015 <- read_csv("FoodAccessResearchAtlasData2015.csv") |>
  filter(State == "New York") |> 
  mutate(Year = 2015)

fara2015_clean <- fara2015 %>%
  mutate(across(all_of(char_vars), as.character)) %>%
  mutate(across(all_of(binary_vars), ~ factor(., levels = c(0,1)))) %>%
  mutate(across(
    .cols = setdiff(names(.), c(char_vars, binary_vars)),
    ~ as.numeric(.)
  ))


fara_panel <- bind_rows(fara2015_clean, fara2019_clean)

fara_panel <- fara_panel |> 
  mutate(TractLOWI = TractLOWI / POP2010,
         TractKids = TractKids / POP2010,
         TractSeniors = TractSeniors / POP2010,
         TractWhite = TractWhite / POP2010,
         TractBlack = TractBlack / POP2010,
         TractAsian = TractAsian / POP2010,
         TractNHOPI = TractNHOPI / POP2010,
         TractAIAN = TractAIAN / POP2010,
         TractOMultir = TractOMultir / POP2010,
         TractHispanic = TractHispanic /POP2010,,
         TractSNAP = TractSNAP / OHU2010
  )



fara2015_sum <- fara2015_clean |> 
  skim()

fara2019_sum <- fara2019_clean |> 
  skim()

fara2019_sum_high_prop_NAs <- fara2019_sum |> 
  filter(complete_rate < .7) |> 
  select(1:4)


fara_panel_no_high_NA <- fara_panel |> 
  select(-fara2019_sum_high_prop_NAs$skim_variable) |> 
  select(-State) |> 
  drop_na() |> 
  mutate(CensusTract = factor(CensusTract),
         County = factor(County),
         Year = factor(Year)) 

fara_panel_no_high_NA_sum <- fara_panel_no_high_NA |> 
  skim()

write_csv(fara_panel_no_high_NA, "cleaned_fara_data.csv")


# Data Prep ---------------------------------------------------------------

X <- fara_panel_no_high_NA |> 
  select(-LAhalfand10, -LA1and10, -LA1and20, 
         -LATracts_half, -LATracts1, -LATracts10, -LATracts20,
         -GroupQuartersFlag, -TractHUNV,
         -starts_with("LI"), -starts_with("LATracts"))

X_matrix <- model.matrix(
  ~ Urban * Year * ( . ) + CensusTract, 
  data=X)[,-1] |> 
  Matrix(sparse = TRUE)

# -LATracts_half, -LATracts1, -LATracts10, -LATracts20,
# -LATracts_half, -LATracts1, -LATracts10
fara_panel_no_high_NA <- fara_panel_no_high_NA |> 
  mutate(LAhalfand10 = as.integer(LAhalfand10) - 1,
         LA1and10 = as.integer(LA1and10) - 1
         )

y_halfand10 <- as.integer(fara_panel_no_high_NA$LAhalfand10)
y_1and10 <- as.integer(fara_panel_no_high_NA$LA1and10)

rm(fara_panel,
   fara2015, fara2015_clean, fara2015_sum,
   fara2019, fara2019_clean, fara2019_sum,
   fara2019_sum_high_prop_NAs,
   fara_panel_no_high_NA_sum
)

# Var Definitions Table ---------------------------------------------------------------
key_var_def <- read.csv("data-dict.csv") |> 
  filter(Field %in% c("LAhalfand10", 
                    "LA1and10",
                    "Urban",
                    "LowIncomeTracts",
                    "HUNVFlag",
                    "PovertyRate",
                    "TractSNAP",
                    "Year",
                    "CensusTract"
                    )) |> 
  arrange(Field) |> 
  rename("Key Variables" = Field,
         "Full Name" = LongName)

saveRDS(key_var_def, "key_var_def.rds")

# Summary Stats Table ---------------------------------------------------------------
skim(fara_panel_no_high_NA)

# Map ---------------------------------------------------------------

fara_map_data <- fara_panel_no_high_NA |> 
  mutate(GEOID10 = as.character(CensusTract),
         LAhalfand10 = as.factor(LAhalfand10),
         LA1and10 = as.factor(LA1and10)
         )

ny_tracts <- tracts(state = "NY", year = 2010) 

fara_map<- left_join(ny_tracts, fara_map_data, by = "GEOID10")

fara_map_LAhalfand10 <-
  ggplot(filter(fara_map, !is.na(Year))) +
  geom_sf(aes(fill = factor(LAhalfand10))) +
  facet_wrap(~Year) +
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "red"),
    na.value = "transparent"
  ) +
  labs(title = "Low Access Tracts at the 1/2 and 10 mile Threshold",
       fill = "LAhalfand10") +
  theme_minimal()

fara_map_LA1and10 <-
ggplot(filter(fara_map, !is.na(Year))) +
  geom_sf(aes(fill = factor(LA1and10))) +
  facet_wrap(~Year) +
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "red"),
    na.value = "transparent"
  ) +
  labs(title = "Low Access Tracts at the 1 and 10 mile Threshold",
       fill = "LA1and10") +
  theme_minimal()

saveRDS(fara_map_LAhalfand10, "fara_map_LAhalfand10.rds")
saveRDS(fara_map_LA1and10, "fara_map_LA1and10.rds")



# Lasso Linear Probability Models -----------------------------------------

set.seed(320)
# Lasso Models
model_halfand10 <- cv.glmnet(
  x         = X_matrix,
  y         = y_halfand10,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)

model_1and10 <- cv.glmnet(
  x         = X_matrix,
  y         = y_1and10,   
  alpha     = 1,
  # family = "binomial", 
  # intercept = FALSE 
)


beta_1se_halfand10 <- coef(model_halfand10, s = "lambda.1se")
beta_min_halfand10 <- coef(model_halfand10, s = "lambda.min")
beta_1se_1and10 <- coef(model_1and10, s = "lambda.1se")
beta_min_1and10 <- coef(model_1and10, s = "lambda.min")

betas <- data.frame(
  term = rownames(beta_1se_halfand10),
  
  beta_1se_halfand10 = as.numeric(beta_1se_halfand10),
  beta_min_halfand10 = as.numeric(beta_min_halfand10),
  
  beta_1se_1and10 = as.numeric(beta_1se_1and10),
  beta_min_1and10 = as.numeric(beta_min_1and10)
)

betas_nz <- betas |>
  filter(beta_1se_halfand10 != 0 | beta_min_halfand10 != 0 |
           beta_1se_1and10 != 0 | beta_min_1and10 != 0 
  ) |>
  arrange(abs(beta_1se_1and10), abs(beta_min_1and10))




# OLS with Selected Variables ---------------------------------------------

clean_term_names <- function(terms) {
  terms %>%
    str_replace_all("Urban0", "Urban") %>%
    str_replace_all("Urban1", "Urban") %>%
    str_replace_all("Year2015", "Year") %>%
    str_replace_all("Year2019", "Year") %>%
    str_replace_all("HUNVFlag0", "HUNVFlag") %>%
    str_replace_all("HUNVFlag1", "HUNVFlag") %>%
    str_replace_all("LowIncomeTracts0", "LowIncomeTracts") %>%
    str_replace_all("LowIncomeTracts1", "LowIncomeTracts") %>%
    str_replace_all("GroupQuartersFlag0", "GroupQuartersFlag") %>%
    str_replace_all("GroupQuartersFlag1", "GroupQuartersFlag") %>%
    unique()
}

get_clean_terms <- function(beta_vec) {
  term_df <- tibble(
    term = rownames(beta_vec),
    beta = as.numeric(beta_vec)
  ) %>%
    filter(beta != 0)
  
  selected_terms <- term_df %>%
    pull(term) %>%
    clean_term_names()
  
  # Keep only interpretable 2-way interactions
  twoway_terms_clean <- selected_terms[
    str_count(selected_terms, ":") == 1 &
      !str_detect(selected_terms, "CensusTract|County")
  ]
  
  # Main effects directly selected by lasso
  main_terms_clean <- selected_terms[
    !str_detect(selected_terms, ":") &
      selected_terms != "(Intercept)" &
      !str_detect(selected_terms, "^CensusTract|^County")
  ]
  
  # Add hierarchical parent terms from selected 2-way interactions
  parent_terms <- twoway_terms_clean %>%
    str_split(":", simplify = TRUE) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    unlist(use.names = FALSE) %>%
    unique()
  
  parent_terms <- parent_terms[
    parent_terms != "" &
      parent_terms != "(Intercept)" &
      !str_detect(parent_terms, "^CensusTract|^County")
  ]
  
  main_terms_clean <- unique(c(main_terms_clean, parent_terms))
  
  list(
    main_terms_clean = main_terms_clean,
    twoway_terms_clean = twoway_terms_clean
  )
}

terms_halfand10   <- get_clean_terms(beta_1se_halfand10)
terms_1and10      <- get_clean_terms(beta_1se_1and10)







make_formula <- function(y_name, main_terms, twoway_terms) {
  rhs_terms <- unique(c(main_terms, twoway_terms))
  as.formula(
    paste(y_name, "~", paste(rhs_terms, collapse = " + "))
  )
}

form_halfand10 <- make_formula(
  "LAhalfand10",
  terms_halfand10$main_terms_clean,
  terms_halfand10$twoway_terms_clean
)

form_1and10 <- make_formula(
  "LA1and10",
  terms_1and10$main_terms_clean,
  terms_1and10$twoway_terms_clean
)


fara_panel_no_high_NA <- fara_panel_no_high_NA |>
  mutate(Urban = as.numeric(as.character(Urban)),
         CensusTract = factor(CensusTract),
         Year = factor(Year))

# Remove Year first
form_halfand10_no_year <- update(form_halfand10, . ~ . - Year)
form_1and10_no_year    <- update(form_1and10, . ~ . - Year)

# Extract RHS terms from both formulas
x_halfand10 <- attr(terms(form_halfand10_no_year), "term.labels")
x_1and10    <- attr(terms(form_1and10_no_year), "term.labels")

# Union of all RHS terms
x_all <- union(x_halfand10, x_1and10)

# Make common RHS
rhs_all <- paste(x_all, collapse = " + ")

# Keep each formula's original dependent variable
y_halfand10 <- as.character(form_halfand10_no_year)[2]
y_1and10    <- as.character(form_1and10_no_year)[2]

# New formulas with same RHS
form_halfand10_common <- as.formula(
  paste(y_halfand10, "~", rhs_all)
)

form_1and10_common <- as.formula(
  paste(y_1and10, "~", rhs_all)
)


# install.packages("fixest")
library(fixest)

# POLS
mod_halfand10_POLS <- feols(
  fml = form_halfand10_common,
  data = fara_panel_no_high_NA
)

mod_1and10_POLS <- feols(
  fml = form_1and10_common,
  data = fara_panel_no_high_NA
)

# CensusTract FE
mod_halfand10_FE_CensusTract <- feols(
  fml = form_halfand10_common,
  fixef = c("CensusTract"),
  data = fara_panel_no_high_NA
)

mod_1and10_FE_CensusTract <- feols(
  fml = form_1and10_common,
  fixef = c("CensusTract"),
  data = fara_panel_no_high_NA
)


# Year FE
mod_halfand10_FE_Year <- feols(
  fml = form_halfand10_common,
  fixef = c("Year"),
  data = fara_panel_no_high_NA
)

mod_1and10_FE_Year <- feols(
  fml = form_1and10_common,
  fixef = c("Year"),
  data = fara_panel_no_high_NA
)

# Two-way FE
mod_halfand10_FE_CensusTract_Year <- feols(
  fml = form_halfand10_common,
  fixef = c("CensusTract", "Year"),
  data = fara_panel_no_high_NA
)

mod_1and10_FE_CensusTract_Year <- feols(
  fml = form_1and10_common,
  fixef = c("CensusTract", "Year"),
  data = fara_panel_no_high_NA
)

# Save Models

saveRDS(mod_halfand10_POLS, "mod_halfand10_POLS.rds")
saveRDS(mod_1and10_POLS, "mod_1and10_POLS.rds")
saveRDS(mod_halfand10_FE_CensusTract, "mod_halfand10_FE_CensusTract.rds")
saveRDS(mod_1and10_FE_CensusTract, "mod_1and10_FE_CensusTract.rds")
saveRDS(mod_halfand10_FE_Year, "mod_halfand10_FE_Year.rds")
saveRDS(mod_1and10_FE_Year, "mod_1and10_FE_Year.rds")
saveRDS(mod_halfand10_FE_CensusTract_Year, "mod_halfand10_FE_CensusTract_Year.rds")
saveRDS(mod_1and10_FE_CensusTract_Year, "mod_1and10_FE_CensusTract_Year.rds")



# Model Tables

library(modelsummary)
modelsummary(
  list(
    "Half & 10:\n POLS" = mod_halfand10_POLS,
    "Half & 10:\n CensusTract FE" = mod_halfand10_FE_CensusTract,
    "Half & 10:\n Year FE" = mod_halfand10_FE_Year,
    "Half & 10:\n Two-way FE" = mod_halfand10_FE_CensusTract_Year
  ),
  stars = TRUE,
  coef_omit = "Intercept|TractAsian|TractHispanic|TractWhite"
)

modelsummary(
  list(
    "1 & 10:\n POLS" = mod_1and10_POLS,
    "1 & 10:\n CensusTract FE" = mod_1and10_FE_CensusTract,
    "1 & 10:\n Year FE" = mod_1and10_FE_Year,
    "1 & 10:\n Two-way FE" = mod_1and10_FE_CensusTract_Year
  ),
  stars = TRUE,
  coef_omit = "InterceptIntercept|TractAsian|TractHispanic|TractWhite"
)

modelsummary(
  list(
    "Half & 10:\n POLS" = mod_halfand10_POLS,
    "1 & 10:\n POLS" = mod_1and10_POLS,
    
    "Half & 10:\n CensusTract FE" = mod_halfand10_FE_CensusTract,
    "1 & 10:\n CensusTract FE" = mod_1and10_FE_CensusTract,
    
    "Half & 10:\n Year FE" = mod_halfand10_FE_Year,
    "1 & 10:\n Year FE" = mod_1and10_FE_Year,
    
    "Half & 10:\n Two-way FE" = mod_halfand10_FE_CensusTract_Year,
    "1 & 10:\n Two-way FE" = mod_1and10_FE_CensusTract_Year
  ),
  stars = TRUE,
  coef_omit = "InterceptIntercept|TractAsian|TractHispanic|TractWhite|TractAIAN|TractKids|TractSeniors|POP2010|TractOMultir"
)
