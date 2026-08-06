# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm trial, 4 states, 6 sites, mechanical/manual weed control tools (incl. control) in maize. 
# Main table (d3b) = weed density (r3b) + averaged work-capacity/fuel (r4b, no Rep). 
# Yield sub-samples (r1b, unspecified sub-sample area) kept as a separate long table. Spanish terms translated
# (Primera/Segunda -> first/second pass; Criollo -> landrace).

## ISSUES
# land_prep_method (AC/LC/RQ) inferred.
# Tierra Blanca has 5+ same-named candidates in Chiapas - best guess used; Guelache; other sites geocoded/searched.
# El_Armadillo's 12 Ctrl rows lack weed_density (site-specific gap).
# sample_weight not converted to yield - its sub-sample area doesn't match r4b's Area; 
# most likely that the sample weight is from different sections of the plot with no area
# weeding_cost = fuel + labor only, from r2b's fixed assumptions (fuel price, wage, workday) - excludes tool purchase/repair costs. r2b mixes
# mach_cat, weeding_pass, weed_density_pre, work_area, work_time,
# fuel_consumption, sample_weight: no terminag equivalent, suggested new terms.

carob_script <- function(path) {

"
Mechanical weed control trials in Mexico
 
On-farm trials across 4 Mexican states (Chiapas, Oaxaca, Quintana Roo,
Yucatan) and 6 sites, evaluating small-scale farm equipment (hoe, machete,
plow, cultivator, minitiller, rototiller, roto-weeder, trimmer, sprayer,
and an untreated control) for mechanical weed control in maize. For each
tool, the trials measured: weed density before and after each weeding
intervention (plants/m2), maize grain yield sub-samples and moisture, and
the tool's work capacity (area covered, time, and fuel consumption per
treatment). A separate cost-model reference table estimates the annual
ownership, maintenance, and operating cost of each tool.
"

# on-farm trial, 10 mechanical/manual weed control treatments (incl. untreated control), 4 states, 6 sites

  uri <- "doi:10.71682/10549432"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
  meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "weeding_method",
		response_vars = "weed_density; weeding_cost",
		notes = NA,
		carob_contributor = "Stella Muthoni",
		carob_date = "2026-07-24",
		carob_completion = 60,
		carob_effort = 4
  )
  
  f1 <- ff[basename(ff) == "Maize_Grain_Yield_VF.xlsx"]
  f2 <- ff[basename(ff) == "Weed_Control_Costs_VF.xlsx"]
  f3 <- ff[basename(ff) == "Weed_Density_VF.xlsx"]
  f4 <- ff[basename(ff) == "Work_Capacity_and_Fuel_Consumption_VF.xlsx"]
  
  r1a <- carobiner::read.excel(f1, sheet="Variable description")
  r1b <- carobiner::read.excel(f1, sheet="Data")                  # Maize yield sub-samples
  r2a <- carobiner::read.excel(f2, sheet="Metadata")
  r2b <- carobiner::read.excel(f2, sheet="Data", na="NA")                  # Weed control cost-model assumptions
  r3a <- carobiner::read.excel(f3, sheet="Variable description")
  r3b <- carobiner::read.excel(f3, sheet="Data", na="NA")                  # Weed density (base table)
  r4a <- carobiner::read.excel(f4, sheet="Variable description")
  r4b <- carobiner::read.excel(f4, sheet="Data", na="NA")                  # Work capacity/fuel
  ## r2 (cost-model reference) is not merged into d3b/d1b - see ISSUES
  
  d3b <- data.frame(
    country = "Mexico",
    adm1 = r3b$State,
    location = gsub("_", " ", r3b$Site),
    
    # Assumed decoding of land preparation
    # AC = Agricultura de Conservacion (conservation agriculture) -> approximated as "reduced tillage"
    # LC = Labranza Convencional (conventional tillage) -> "conventional"
    # RQ = Roza y Quema (slash and burn)
	
    land_prep_method = ifelse(r3b$Soil_preparation == "AC", "reduced tillage",
                       ifelse(r3b$Soil_preparation == "LC", "conventional", 
                       ifelse(r3b$Soil_preparation == "RQ", "slash and burn", NA))),
    
    variety = r3b$Maize_variety,
    variety_type = ifelse(grepl("Criollo", r3b$Maize_variety), "landrace", "improved"),
    row_spacing = r3b$Row_spacing * 100,   # m -> cm
    
    weeding_method = ifelse(r3b$Treatment == "Ctrl", "none", tolower(r3b$Treatment)),
    weeding_times = as.integer(r3b$N_interv_perf),
    weeding_done = r3b$N_interv_perf > 0,
	# suggested field - which weeding pass
    weeding_pass = ifelse(r3b$Intervention == "Primera", "first",
                   ifelse(r3b$Intervention == "Segunda", "second", r3b$Intervention)), 
    rep = as.integer(r3b$Rep)
  )

  d3b$record_id <- 1:nrow(d3b)
  #Manually searched for El Armadillo and Tierra Blanca as geo-code was off.
  geo <- data.frame(
    location = c("El Armadillo", "Tierra Blanca", "Guelache", "Puerto Arturo", "Tixmehuac", "Tlanichico"),
    longitude = c(-93.3867, -92.1759, -96.777457, -89.0664, -89.0850, -96.8064),
    latitude = c(16.7013, 15.3940, 17.214845, 19.6595, 20.2483, 16.9717)
  )
  d3b <- merge(d3b, geo, by = "location", all.x = TRUE)
  d3b$geo_from_source <- FALSE
  
  
  # long format: one row per before/after reading, not two side-by-side columns
  d3b_before <- d3b[, c("record_id", "weeding_pass")]
  d3b_before$period <- ifelse(is.na(d3b$weeding_pass), "before weeding", paste("before", d3b$weeding_pass, "weeding"))
  d3b_before$weed_density <- r3b$Density_Ini * 10000
  
  d3b_after <- d3b[, c("record_id", "weeding_pass")]
  d3b_after$period <- ifelse(is.na(d3b$weeding_pass), "after weeding", paste("after", d3b$weeding_pass, "weeding"))
  d3b_after$weed_density <- r3b$Density_fin * 10000
  
  d_long <- rbind(d3b_before, d3b_after)
  d_long$weeding_pass <- NULL 
  d_long <- d_long[!is.na(d_long$weed_density), ]
  
  ### r4b has multiple timing readings per key, no Rep to link - averaged per State+Site+Treatment+Intervention
  d4b <- aggregate(Area ~ State + Site + Treatment + Intervention, data = r4b, FUN = mean)
  d4b_time <- aggregate(Time ~ State + Site + Treatment + Intervention, data = r4b, FUN = function(x) (mean(as.numeric(x), na.rm = TRUE)))
  d4b_fuel <- aggregate(Fuel_consumption ~ State + Site + Treatment + Intervention, data = r4b, FUN = function(x) (mean(as.numeric(x), na.rm = TRUE)))
  
  d4b <- merge(d4b, d4b_time, by = c("State","Site","Treatment","Intervention"), all.x = TRUE)
  d4b <- merge(d4b, d4b_fuel, by = c("State","Site","Treatment","Intervention"), all.x = TRUE)

  # go from experimental area to ha
  to_ha = 10000 / d4b$Area
  
  d4b <- data.frame(
    adm1 = d4b$State,
    location = gsub("_", " ", d4b$Site),
    weeding_method = ifelse(d4b$Treatment == "Ctrl", "none", d4b$Treatment),
    weeding_pass = ifelse(d4b$Intervention == 1, "first",
                   ifelse(d4b$Intervention == 2, "second", as.character(d4b$Intervention))),
	plot_area = d4b$Area, 
	# suggested field - hours
	weeding_time = to_ha * (d4b$Time / 60), 
	# suggested field - L 
    weeding_fuel = to_ha * (d4b$Fuel_consumption / 1000)
  )

  ### weeding_cost: fuel cost (motorized tools only) + labor cost (all tools), using fixed assumptions from r2b's "1. 
  ### This is a partial operating cost (fuel + labor only) - does NOT include tool purchase/depreciation/repair costs.
  fuel_price_usd_per_L <- 1.18
  hourly_wage_usd <- 13.33 / 8
  
  d4b$weeding_fuel_cost <- d4b$weeding_fuel * fuel_price_usd_per_L
  d4b$weeding_labor_cost <- d4b$weeding_time * hourly_wage_usd
  d4b$weeding_cost <- ifelse(is.na(d4b$weeding_fuel_cost), 0, d4b$weeding_fuel_cost) + d4b$weeding_labor_cost
  d4b$currency <- "USD"

  d <- merge(d3b, d4b, by = c("adm1","location","weeding_method","weeding_pass"), all.x = TRUE)
  
  d$trial_id <- r3b$Site
  d$on_farm <- TRUE
  d$is_survey <- FALSE
  d$crop <- "maize"
  d$irrigated <- NA
  d$planting_date <- NA
  d$harvest_date <- NA
  d$N_fertilizer <- NA
  d$P_fertilizer <- NA
  d$K_fertilizer <- NA
  d$yield <- NA
  d$yield_part <- NA
  d$yield_moisture <- NA
  d$yield_isfresh <- NA
  d$weeding_pass <- NULL 
  
  ### Yield sub-samples (r1b).
  #d1b <- data.frame(
  #  trial_id = r1b$Site,
  #  rep = as.integer(r1b$Rep),
  #  weeding_method = ifelse(r1b$Treatment == "Ctrl", "none", r1b$Treatment),
    
  #  sample_weight = r1b$Sample_weight,   # kg, sub-sample from an unspecified area
  #  yield_moisture = r1b$Moisture_per
  #)
  
  carobiner::write_files(path, meta, d, long=d_long)
}
