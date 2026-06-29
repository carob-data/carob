# R script for "carob"
# license: GPL (>=3)


## rejected for having no location data (country only)

## ISSUES
#1. outliers in fertilizer amount and rainfall amount

## also see https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UIWQQH

carob_script <- function(path) {

"
Kenya Rural Household Panel Survey - Household and maize data 2010 & 2013

Data from two CIMMYT and KALRO household surveys representative of six maize production areas or agroecological zones in Kenya. The surveys were conducted in 2010 and 2013 collected data on farmer demographics, adoption of improved technologies and practices, marketing, access to agricultural information, and farmer adaptation to climate change.
"

	uri <- "hdl:11529/10548820"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIMMYT",
		publication = NA,
		project = NA,
		design = "unitOfAnalysis: Households",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-06-29",
		carob_completion = 90,	
		carob_effort = 6
	)
	
	f1 <- ff[basename(ff) == "Kenya Rural Household Panel Survey - Household and maize data 2010 & 2013 blurred.xlsx"]
	r <- carobiner::read.excel(f1, sheet="Data")
	
	
	d <- data.frame(
	  country="Kenya",
	  hhid= as.character(r$hhid_2013),#using 2013 hhid since it has no NAs 
	  date=as.character(r$year),
	  hh_income=r$total_income,
	  sex=r$sex,
	  age=r$age_range,
	  education=r$educ_hoh_range,#number of years attended in school
	  hh_size=r$hh_size,
	  crop="maize",
	  yield_part="grain",
	  yield_moisture=as.numeric(NA),
	  yield_isfresh=TRUE,
	  planting_date=as.character(NA),
	  cropland_used=r$maize_area_ha,
	  fertilizer_amount=r$fert_qua_kh,
	  yield=r$maize_qua_kh,
	  tmin=r$minimum_2m_air_temperature_year,
	  tmax=r$maximum_2m_air_temperature_year,
	  prec=r$total_precipitation_year,
	  agro_eco_zone=r$aez,
	  currency="KES"
	)

	r$dismarket_km[r$dismarket_km == "NA"] <- NA
	d$market_distance <- as.numeric(r$dismarket_km)

	d$fertilizer_used[r$fert_use=="0"] <- FALSE
	d$certified_seed_used[r$hybrid=="0"] <- FALSE
	d$geo_from_source=FALSE
	d$is_survey<- TRUE
	d$irrigated <- FALSE
	d$on_farm <- TRUE
	d$sex <- ifelse(d$sex == 1, "male", "female")
	                	
	#obtaining a midpoint for age
	x <- strsplit(d$age, " to ")
	
	d$age <- sapply(x, function(z) {
	  mean(as.numeric(z))
	})
	
	
	d$trial_id <- paste(d$hhid,d$date,sep = "_")
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
	
 } 
