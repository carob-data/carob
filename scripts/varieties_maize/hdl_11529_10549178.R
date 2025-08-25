# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. Crop management practices are also a treatment variable in this experiment 
#2. Since date is only a year, i cant properly format it to become date as it requires a month and day

carob_script <- function(path) {



"Replication Data for: Understanding the interactions of genotype with environment and management (G×E×M) to maize productivity in conservation agriculture systems of Malawi
  
The database consists of data collected over seven seasons to evaluate maize productivity among smallholder farmers in Malawi, focusing on the performance of various maize genotypes under distinct management practices.
The key objectives of the study included assessing the interactions between genotype (G), environment (E), and management (M) to optimize maize production amid climatic variability and soil fertility challenges."


## Identifiers
	uri <- "hdl:11529/10549178"
	group <- "varieties_maize"

## Download data 
	ff  <- carobiner::get_data(uri, path, group)

## metadata 
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "Machinga Agriculture Development Division;TLC;CIMMYT",
		publication = "10.1371_journal.pone.0298009",
		project = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety_code;location",
		response_vars = "yield", 
		completion = 0,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-08-20",
		notes = NA,
		design = "Split plot + RCB"
	)
	
## read data 

	f <- ff[basename(ff) == "Malawi Maize Performance.xlsx"]
	r <- read_excel(f, sheet ="Data")

## select the variables of interest and assign them to the correct name
	d <- data.frame(
		country = r$Country,
		harvest_date = as.Date(r$`Harvest Year`),
		planting_date = as.Date(r$`Harvest Year`-1),
		adm2= r$District,
		adm3= r$Village,
		rep = as.integer(r$Rep),
		crop_rotation = r$`Growth strategy`,
		treatment = r$Tmnt.,
		variety = r$Variety,
		crop= tolower(r$`Crop grown`),
		plant_density= r$`Plant population`,
		dmy_stems= r$`Stalk yield (kg/ha)`,
		yield= r$`Grain yield (kg/ha)`,
		yield_part= "grain"
		)
	
	d$adm2 <- gsub("Nkotakhota","Nkhotakota",d$adm2)
	pub_data <- data.frame(
	  adm2 = c("Dowa", "Nkhotakota", "Nkhotakota", "Nkhotakota", "Salima",
	           "Balaka", "Balaka", "Balaka", "Machinga", "Zomba"),
	  adm4 = c("Chipeni", "Linga", "Mwansambo", "Zidyana", "Chinguluwe",
	           "Herbert", "Lemu", "Malula", "Matandika", "Songani"),
	  latitude = c(-13.76, -12.75, -13.29, -13.23, -13.69,
	               -14.88, -14.80, -14.96, -15.18, -15.34),
	  longitude = c(34.05, 34.20, 34.13, 34.26, 34.24,
	                35.05, 35.02, 34.99, 35.28, 35.39),
	  elevation = c(1166, 491, 632, 535, 657,
	                635, 720, 605, 688, 788),
	  temp = c(21.5, 26.9, 26.5, 26.9, 26.6,
	           23.4, 23.4, 23.4, 23.4, 23.5),
	  soil_texture = tolower(c("Sandy loam", "Sandy loam", "Sandy clay loam",
	                   "Sandy clay loam", "Sandy clay loam",
	                   "Sandy loam", "Sandy loam", "Loamy sand",
	                   "Sandy loam", "Clay loam"))
	  
	)

	d <- merge(d, pub_data, by = c("adm2"), all.x = TRUE)
	d$crop_rotation<- ifelse(d$crop_rotation=="sole crop","maize","maize_legume")
	d$trial_id <- paste(d$adm4, as.character(d$planting_date), sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$geo_from_source <- TRUE
  d$P_fertilizer <- 21/2.29
  d$K_fertilizer <- 0
  d$N_fertilizer <- 69
  d$S_fertilizer <- 4
  d$lime <- as.numeric(NA)
	d$yield_part <- "tubers"
	d$yield_moisture <- 12.5
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}
