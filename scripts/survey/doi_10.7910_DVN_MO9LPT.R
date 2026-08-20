# R script for "carob"
# license: GPL (>=3)

## ISSUES
## adding variety_pref == variety preference

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
		carob_completion = 90,	
		carob_effort = 3
	)
	

	f1 <- ff[basename(ff) == "Combined Dataset.dta"]
	f2 <- ff[basename(ff) == "02b. Nakuru.csv"]
	f3 <- ff[basename(ff) == "02c. Embu.csv"]
	#f4 <- ff[basename(ff) == "01a. Codebook - Combined Data Dictionary.xlsx"]
	#f5 <- ff[basename(ff) == "01b. Data Dictionary - Nakuru.xlsx"]
	#f6 <- ff[basename(ff) == "01c. Codebook - Embu.xlsx"]

	r1 <- carobiner::read.dta(f1) # include r2 and r3
	r2 <- read.csv(f2, na= "")
	r3 <- read.csv(f3, na = "")
	#r4 <- carobiner::read.excel(f4)
	#r5 <- carobiner::read.excel(f5)
	#r6 <- carobiner::read.excel(f6)

  
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

	### rejected beans varieties
	rejected <- paste(
	  unique(c(r2$Rejected_variety1, r2$Rejected_variety2)),
	  collapse = "|"
	)
	
	d2 <- data.frame(
	 hhid = as.character(41:93),
	 adm1 = "Nakuru",
	 variety_trait1 = ifelse(!grepl(rejected, r2$variety1), r2$Reason1, 
	                  ifelse(grepl(rejected, r2$variety1) & grepl(rejected, r2$Reason_rej1), r2$Reason_rej1, r2$Reason_rej2)) ,
	 variety_trait2 =  ifelse(!grepl(rejected, r2$variety2), r2$Reason1, 
	                          ifelse(grepl(rejected, r2$variety2) & grepl(rejected, r2$Reason_rej1), r2$Reason_rej1, r2$Reason_rej2)) ,
	 variety_trait3 =  ifelse(!grepl(rejected, r2$variety3), r2$Reason1, 
	                          ifelse(grepl(rejected, r2$variety3) & grepl(rejected, r2$Reason_rej1), r2$Reason_rej1, r2$Reason_rej2)) ,
	 variety_accepted1 = !grepl(rejected, r2$variety1),
	 variety_accepted2 = !grepl(rejected, r2$variety2),
	 variety_accepted3 = !grepl(rejected, r2$variety3)
	)
	
	## best beans varieties
	accepted <- paste(
	  unique(c(r3$F1_1, r3$F1_2, r3$F1_3)),
	  collapse = "|"
	)
	
	d3 <- data.frame(
	  adm1 = "Embu",
	  hhid = r3$id,
	  hh_size = r3$hhsize,
	  #variety_trait1 = r3$missingtraits1_sn1, ## 
	  #variety_trait2 = r3$missingtraits2_sn1,
	  #variety_trait3 = r3$missingtraits3_sn1,
	  variety_accepted1 = grepl(accepted, r3$variety1),
	  variety_accepted2 = grepl(accepted, r3$variety2),
	  variety_accepted3 = grepl(accepted, r3$variety3)
	  
	  
	)
	
	dd <- carobiner::bindr(d2, d3)
	### merge
	
	d <- merge(d1, dd, by= c("hhid", "adm1"), all.x  = TRUE)
	
	d <- reshape(d, varying = list(c("variety1", "variety2", "variety3"), c("variety_trait1", "variety_trait2", "variety_trait3"), c("variety_accepted1", "variety_accepted2", "variety_accepted3")), v.names = c("variety", "variety_traits", "variety_pref"), direction = "long")
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


