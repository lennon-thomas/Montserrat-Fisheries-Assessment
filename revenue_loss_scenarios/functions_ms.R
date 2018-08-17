#####################################
## Quick MS projection
## August 13, 2018
#####################################


## forward projection model
forward_projection <- function(scenario, input_df, projection_time) {
  
  ################################################
  ## filter for stock
  inputs <- input_df %>%
    filter(stockid == scenario$stock)
  
  ## set parameters
  BMSY <- inputs$Bmsy
  MSY <- inputs$MSY
  K <- inputs$K
  g <- inputs$g
  FMSY <- inputs$g
  f <- inputs$f
  
  ## set f and b according to policy
  f0 <- ifelse(scenario$f_policy == "BAU", f0, f0 * 0.7)
  F0 <- FMSY * f0
  b0 <- b0
  B0 <- BMSY * b0
  
  
  ## initialize vectors
  bb <- vector(length = projection_time)
  BB <- vector(length = projection_time)
  HH <- vector(length = projection_time)
  pi <- vector(length = projection_time)
  pipv <- vector(length = projection_time)
  
  ## Starting bb and BB
  bb[1] <- b0
  BB[1] <- B0
  
  ## projection functions
  for (t in seq(1, projection_time, by = 1)) {
    
    HH[t] <- min(F0 * BB[t], BB[t])
    if(t < projection_time) {BB[t + 1] = (BB[t] + (phi + 1) / phi * g * BB[t] * (1 - (BB[t] / K) ^ phi)) - HH[t]}
    if(t < projection_time) {bb[t + 1] = BB[t + 1] / BMSY}
    pi[t] = p * HH[tt] - c * FF[t] ^ beta
    pipv[t] = (delta ^ t) * pi[t]

  }
  
  time_vec <- seq(1, projection_time, by = 1)
  
  projection_outputs <- data.frame(stockid = scenario$stock,
                                   policy = scenario$f_policy,
                                   time = time_vec,
                                   b = bb,
                                   biomass = BB,
                                   harvest = HH,
                                   profit = pi,
                                   disc_profit = pipv)
  
  return(projection_outputs)
  
}













# Map over all mpa_scenarios
fisheries_projections <-  purrr::map(mpa_scenarios, function(mpa_scenario){
  temp_df <- unlumped_fisheries_status_filtered %>%
    # filter(!Country %in% c("High Seas Tuna and Billfish","Multinational")) %>%
    filter(Country %in% c("Montserrat")) %>%
    # Set their fraction of stock in eez to 1
    mutate(fraction_stock = 1) %>%
    # Add high seas and multinational stocks
    bind_rows(high_seas_status) %>%
    # Add in MPA coverage for each EEZ
    left_join(mpa_coverage_by_eez %>% dplyr::select(iso3,eez_area_km2,current_mpa_coverage),by="iso3") %>%
    mutate(eez_area_km2 = ifelse(is.na(eez_area_km2),0,eez_area_km2),
           current_mpa_coverage = ifelse(is.na(current_mpa_coverage),0,current_mpa_coverage)) %>%
    # For high seas stocks, MPA only covers area proportional to MPA size and fraction of stock in EEZ
    mutate(new_mpa_coverage = ifelse(mpa_scenario == 0 | current_mpa_coverage > mpa_scenario,
                                     0,
                                     (mpa_scenario - current_mpa_coverage) * eez_area_km2),
           # Calculate FvFmsy based on current MPA coverage and mpa scenario
           FvFmsy = ifelse(mpa_scenario == 0 | current_mpa_coverage > mpa_scenario,
                           FvFmsy,
                           pmax(FvFmsy * (1 - (mpa_scenario - current_mpa_coverage) * fraction_stock),0)),
           # Rename a few columns to get to work with GUM package
           catch = Catch,
           b_to_k_ratio = BtoKRatio,
           year = Year,
           # Add unique identifier that includes country
           IdOrig = paste0(IdOrig,"_",iso3)) %>%
    as.data.frame()
  
  projections <- temp_df %>%
    # Run projections using GUM package
    RunProjection(BaselineYear = 2012,
                  NumCPUs = 2,
                  ProjectionTime = 50,
                  Discount = 0.05,
                  Policies = c("StatusQuoFForever")) %>%
    # Grab data from projections
    .$DataPlus %>%
    # Add MPA scenario to dataframe
    mutate(Policy = ifelse(mpa_scenario == 0,"Business-as-usual",
                           paste0("MPA Scenario: ",mpa_scenario * 100,"%")),
           Profits = Price * MSY * FvFmsy * BvBmsy - MarginalCost * (FvFmsy * g) ^ beta)
  # Print alert
  print(paste0("MPA Scenario: ",mpa_scenario * 100,"%"))
  
  projections
  
}) %>%
  bind_rows()
# Cache slimmed down version
fisheries_projections %>%
  dplyr::select(Policy,Country,iso3,IdOrig,CommName,SciName,Year,BvBmsy,FvFmsy,Biomass,Catch,Profits) %>%
  write_csv("results/ms_fisheries_projections.csv")


