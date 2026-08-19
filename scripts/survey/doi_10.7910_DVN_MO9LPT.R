# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Gender-Responsive Participatory Variety Selection in Kenya: Implications for Common Bean (Phaseolus vulgaris L.) Breeding in Kenya

The data collection aimed to investigate gender-responsive participatory variety selection (PVS) for common bean breeding in Kenya. It was conducted across Embu and Nakuru Counties in 2019 to understand gender differences in varietal and trait preferences among farmers. The study focused on preferences for biofortified released and local bean varieties, covering major topics like yield, maturity, pest resistance, marketability, and cooking time. The Principal Investigators sought to identify gender-specific adoption barriers, potential trade-offs, and socio-economic influences on varietal preferences​

 Methodology:The study used a modified gender-responsive PVS tool integrating socio-demographic data with gender gap questions. Data was collected from 93 farmers through systematic random sampling. On-farm trials and demonstrations were conducted to facilitate farmer engagement, and participants ranked their preferred bean varieties. Descriptive statistical analysis was used to highlight gendered differences in preferences, examining factors such as land ownership, age, and education.
"

	uri <- "doi:10.7910/DVN/MO9LPT"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_date = "2026-08-19",
		carob_completion = 80,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "Combined Dataset.dta"]
	f2 <- ff[basename(ff) == "02b. Nakuru.csv"]
	f3 <- ff[basename(ff) == "02c. Embu.csv"]
	#f4 <- ff[basename(ff) == "01a. Codebook - Combined Data Dictionary.xlsx"]
	#f5 <- ff[basename(ff) == "01b. Data Dictionary - Nakuru.xlsx"]
	#f6 <- ff[basename(ff) == "01c. Codebook - Embu.xlsx"]

	r1 <- carobiner::read.dta(f1) # include r2 and r3
	r2 <- read.csv(f2)
	r3 <- read.csv(f3)
	#r4 <- carobiner::read.excel(f4)
	#r5 <- carobiner::read.excel(f4)
	#r6 <- carobiner::read.excel(f5)

  
#### Process
	
	d1 <- data.frame(
	  hhid = as.character(r1$id),
	  adm1 = r1$county,
	  country = "Kenya",
	  crop = "common bean",
	  sex = r1$sex,
	  age = r1$age,
	  hh_head = r1$hh_head,
	  education = r1$educlevel,
	  civil_status = r1$marital,
	  farmland = r1$land,
	  cropland = r1$land_cropped,
	  cropland_used = r1$land_beans,
	  land_ownedby = r1$ownership,
	  seed_source = r1$bean_source,
	  #r1$Who_buys,
	  variety1 =  r1$variety1,
	  variety2 = r1$variety2,
	  variety3 = r1$variety3
	)

	d <- reshape(d1, varying = c("variety1", "variety2", "variety3"), v.names = "variety", direction = "long")
	d$id <- d$time <- NULL
	d <- unique(d[!is.na(d$variety),])
	### Adding geo coordinate 
	
	geo <- data.frame(
	  adm1 = c("Embu","Nakuru"),
	  longitude = c(37.6259, 36.0777),
	  latitude = c(-0.5922, -0.4573),
	  geo_from_source = FALSE,
	  geo_uncertainty = c(60722, 91977),
	  geo_source = "GADM 4.1, adm1"
	  
	)
	
	d <- merge(d, geo, by= "adm1", all.x = TRUE)
	
	d$is_survey <- TRUE
	d$on_farm <- FALSE
	d$yield <- NA
	d$yield_moisture <- NA
	d$yield_part <- "none"
	d$irrigated <- NA
	d$planting_date <- NA_character_
	d$harvest_date <- NA_character_
	d$yield_isfresh <- TRUE
	
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}


