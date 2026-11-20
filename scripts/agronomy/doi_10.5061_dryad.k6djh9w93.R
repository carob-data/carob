# R script for "carob"
# license: GPL (>=3)

## ISSUES
# yield was given from 6 samples selected per plot but the plot size was not given resulting in NA's from yield.
# NA from intercrops arising after binding d1 and d2
# germination rate had out of bounds values
# 1 record with harvest_date within 45 days of planting_date 
# The harvest date for one record was recorded as 137 days from 1900, resulting in a date warning
# Basal NPS  (19-38-7) rate for maize was not reported
# For common bean: no fertilizer information given
# The dataset contains 5 ADM4 locations (kebeles/villages) within three ADM3 locations. However, coordinates are available only at ADM3 level, resulting in duplicate coordinates for villages within the same district. This results in warning: "duplicate coordinates: fewer coordinates than locations".

carob_script <- function(path) {

"
Role of sustainable agricultural intensification in food and nutrition security of smallholder subsistence farmers: Evidence from cereal legume intercropping in Eastern Ethiopia

This data set is a biological data generated through field experimentation on cereal legume intercropping. In general, the contents of the data set are type of treatment (experimental materials), site of experiment, biomass and grain yields, etc. There is no legal and ethical issues related to this data set and its process of generation. The data can be re-used with proper acknowledgement of the authors. 
"

	uri <- "doi:10.5061/dryad.k6djh9w93"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=NA,
		data_organization = "HARAU",
		publication = NA,
		project = "InnoVAfrica",
		design = "RCBD",
		data_type = "on-farm experiment",
		treatment_vars = "variety;intercrops",
		response_vars = "yield", 
		carob_contributor = "Premrose Masunungure",
		carob_date = "2026-09-18",
		carob_completion = 90,	
		carob_effort = 14
	)
	

	f1 <- ff[basename(ff) == "Field_Experiment_data.xlsx"]
	f2 <- ff[basename(ff) == "README.txt"]

	r1a <- carobiner::read.excel(f1, sheet="Maize")
	r1b <- carobiner::read.excel(f1, sheet="Common bean")
	
	r1a$`Grain moisture content (%)`[r1a$`Grain moisture content (%)` == "null"] <- NA

	d1 <- data.frame(
	  adm3 = r1a$District,
	  adm4 = r1a$`Vilage/Kebele`,
	  hhid = r1a$`Farmer/field`,
	  treatment = r1a$Treatment,
	  variety = r1a$`Variety 1 and 2`,
	  intercrops = ifelse(r1a$`Growing system`=="Sole","none","maize_common bean"),
	  row_spacing = as.numeric(r1a$`Row spacing (cm)`),
	  plant_spacing = as.numeric(r1a$`B/n plant spacing (cm)`),
	  land_prep_method = r1a$`Tillage method (no tillage, conventional plough, manual hoe tillage)`,
	  planting_date = as.character(r1a$`Date of sowing`),
	  harvest_date = as.character(r1a$`Date of harvesting`),
	  fertilizer_date = as.character(as.Date(r1a$`Date of basal application (year-month-date)`)),
	  germination_rate = as.numeric(r1a$`Estimated germination (%)`),
	  yield_moisture = as.numeric(r1a$`Grain moisture content (%)`),
	  crop = "maize"
	)
	
	d1$land_prep_method[d1$land_prep_method == "Manual hoe tillage"] <- "hoeing"
	
	###weeding has two dates so I tried to combine them to standardize to weding_dates
	w1 <- as.character(as.Date(r1a$`Date of first weeding (yr-month-date)`))
	w2 <- as.character(as.Date(r1a$`Date of second weeding`))
	w <- paste0(w1, ";", w2)
	w <- gsub("^NA;|;NA$", "", w)
	d1$weeding_dates <- w
	d1$weeding_dates[d1$weeding_dates == "NA"] <- NA
	
	r1b$`Moisture of Beans grain (%)`[r1b$`Moisture of Beans grain (%)` == "null"] <- NA
	
	d2 <- data.frame(
	  adm3 = r1b$District,
	  adm4 = r1b$Kebele,
	  hhid = r1b$Farmer,
	  treatment = as.character(r1b$Treatment),
	  variety = r1b$`Variety 1 and 2`,
	  planting_date = r1b$`Date of sowing`,
	  yield_moisture = as.numeric(r1b$`Moisture of Beans grain (%)`),
	  maturity_date = r1b$`Date 90% maturity`,
	  harvest_date = r1b$`Date of harvesting`,
	  intercrops = "maize_common bean",
	  crop = "common bean"
	)

	d2$planting_date[d2$planting_date == "null"] <- NA
	d2$maturity_date[d2$maturity_date == "null"] <- NA
	d2$harvest_date[d2$harvest_date == "null"] <- NA
	
	d2$planting_date <- as.character(as.Date(as.numeric(d2$planting_date),origin = "1900-01-01"))
	d2$maturity_date <- as.character(as.Date(as.numeric(d2$maturity_date),origin = "1900-01-01"))
	d2$harvest_date <- as.character(as.Date(as.numeric(d2$harvest_date),origin = "1900-01-01"))
	
	d <- carobiner::bindr(d1, d2)

	d$adm3 <- gsub("HU|Hu","Haro Maya",d$adm3)
	location <- data.frame(
	  adm3 = c("Kombolcha", "Meta", "Haro Maya"),
	  latitude = c(9.5059, 9.3549, 9.3425),
	  longitude = c(42.1788, 41.5954, 41.9971),
	  geo_uncertainty = c(18345, 25393, 24619 ),
	  geo_source = c("GADM 4.1, adm3", "GADM 4.1, adm3", "GADM 4.1, adm3")
	)
    
  d <- merge(d, location, by = "adm3", all.x = TRUE)
  d$geo_from_source <- FALSE
	
	d$trial_id <- "1"
	
	d$country <- "Ethiopia"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- NA

	maize <- d$crop == "maize"
	d$N_fertilizer <- ifelse(maize, 46, NA)
	d$P_fertilizer <- d$K_fertilizer <- d$S_fertilizer <- NA
	d$fertilizer_type <- ifelse(maize, "NPS; urea", NA)
 
  d$inoculated <- NA
  d$inoculant <- NA
   
	d$yield <- NA
	d$yield_part <- "grain"

	carobiner::write_files(path, meta, d)
}
