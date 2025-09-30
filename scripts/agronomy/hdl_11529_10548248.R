# R script for "carob"
# license: GPL (>=3)

## ISSUES

carob_script <- function(path) {


"Long-term tillage and residue management experiment in San Luis Potosí, Mexico
  
An experiment initiated in 1996 in the highlands of the state of San Luis Potosí, Mexico, evaluated different tillage methods and levels of soil cover under permanent raised beds for their effects on yield, profitability, and soil quality in an irrigated, summer maize- winter oats rotation. The experiment was located at the Experimental Station “San Luis” of the Instituto Nacional de Investigaciones Forestales, Agricolas y Pecuarias (INIFAP) in Soledad de Graciano Sánchez, San Luis Potosí, Mexico (22° 13' 35.30 N 100 50 56.67W, 1,838 m above sea level). The experiment evaluated four tillage methods; permanent, raised beds and the following three conventional methods: 1) Inversion tillage with a disc plow and disk harrowing to a depth of 30 cm (P+D), 2) disk harrowing to a depth of 30 cm (D), and 3) non-inversion tillage with a “multiplow” and disk harrowing to a depth of 30 cm (M+D). In all three conventional tillage treatments, 1.65 m wide beds were shaped after tillage. In treatments P+D, D, and M+D, soil preparation took place at the beginning of each maize and oats cycle. Four levels of soil cover were tested on the raised beds: zero (PB0%), one third (PB33%), two thirds (PB66%) and full soil cover (PB100%). The amount of residue to cover one-third, two-thirds and all of the soil surface, corresponds to 1.3, 2.6, and 4.0 t ha-1, respectively, and it was applied once a year after maize harvest. All residue was removed after harvest and the amount of residue for each treatment was returned every year. The permanent raised beds (PB) were created in 1996 and are 1.65 m wide. Furrows in the raised beds have been reshaped every five years. Plots were 8.25 m wide and 30.0 m long. Treatments were evaluated in a randomized complete block design with two replications. The data set contains daily weather data for the weather station closest to the experimental site for 1995-2018 (precipitation, minimum and maximum temperature), yield data (grain yield for maize and biomass yield for oats), maize height data for three seasons and soil quality data (texture, saturation point, pH, electric conductivity, macronutrients, micronutrients, penetration resistance, time to pond, bulk density, mean weight diameter after dry and wet sieving). (2019-08-04)"

	uri <- "hdl:11529/10548248"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIMMYT;INIFAP",
		publication = "doi:10.3390/agronomy9120845",
		project = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method",
		response_vars = "yield", 
		completion = 100,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2025-09-30",
		notes = NA,
		design = "Randomized Complete Block Design")
	
	f <- ff[basename(ff) == "DAT-PUB-Soledad.xlsx"]
  r1 <- carobiner::read.excel(f, sheet="Maize yield")
  r2 <- carobiner::read.excel(f, sheet="Oats yield")
  s_chem <- carobiner::read.excel(f, sheet="Chemical soil quality")
  s_phys <- carobiner::read.excel(f, sheet="Physical soil quality")

	maize <- data.frame(
	  crop="maize",
	  planting_date=as.character(r1$Year),
	  treatment=r1$Treatment,
	  rep=as.integer(r1$Repetition),
	  yield=r1$Yield,
	  variety_code=r1$Hybrid)
	
	oats <- data.frame(
	  crop="oats",
	  planting_date=as.character(r2$Year),
	  treatment=r2$Treatment,
	  rep=as.integer(r2$Repetition),
	  yield=r2$Yield,
	  variety_code=NA)

	d <- rbind(maize,oats)
	
	soil_phys <- data.frame(
	  treatment=s_phys$Treatment,
	  rep=s_phys$Repetition,
	  soil_bd=(s_phys$`Bulk Density 0-5 cm (g/cm3)`+s_phys$`Bulk Density 5-15 cm (g/cm3)`),
	  previous_crop_residue_perc=s_phys$`Residue cover (%)`)

	d <- merge(d, soil_phys, by = c("treatment", "rep"), all.x = TRUE)
	
	soil_chem <- data.frame(
	  treatment=s_chem$Treatment,
	  depth_top=0,
	  depth_bottom=s_chem$Depth,
	  rep=s_chem$Repetition,
	  soil_sand=as.numeric(s_chem$`Sand (%)`),
	  soil_clay=as.numeric(s_chem$`Clay (%)`),
	  soil_silt=as.numeric(s_chem$`Silt (%)`),
	  soil_pH=as.numeric(s_chem$`pH(1:2 Water)`),
	  soil_SOM=as.numeric(s_chem$`Organic matter (%)`),
	  soil_Ca=s_chem$`Calcium (ppm)`,
	  soil_Mg=s_chem$`Magnesium (ppm)`,
	  soil_Na=s_chem$`Sodium (ppm)`,
	  soil_Fe=s_chem$`Iron (ppm)`,
	  soil_Zn=s_chem$`Zinc (ppm)`,
	  soil_Mn=s_chem$`Manganese (ppm)`,
	  soil_Cu=s_chem$`Copper (ppm)`,
	  soil_B=s_chem$`Boron (ppm)`,
	  soil_S=s_chem$`Sulphur (ppm)`,
	  soil_EC=s_chem$`Electric conductivity (ds/m)`)
	
	d <- merge(d, soil_chem, by = c("treatment", "rep"), all.x = TRUE)
	d$land_prep_method <- d$treatment
	d$land_prep_method <- gsub("^M\\+D$", "reduced tillage", d$land_prep_method)
	d$land_prep_method <- gsub("^P\\+D$", "conventional",   d$land_prep_method)	
	d$land_prep_method<- gsub("PB0%|PB100%|PB33%|PB66%","wide permanent beds",d$land_prep_method)
	d$land_prep_method<- gsub("^D$","minimum tillage",d$land_prep_method)
	d$soil_P <- ifelse(!is.na(s_chem$`Phosphorous Bray (ppm)`), s_chem$`Phosphorous Bray (ppm)`, s_chem$`Phosphorous Olsen (ppm)`)
	d$location <- "San Luis Experiment Station"
	d$country<- "Mexico"
	d$longitude <- -100.849075
	d$latitude <- 22.226472
	d$elevation <- 1838
	d$trial_id <- as.character(paste0(d$location,"_",d$planting_date))
	d$crop_rotation <- "maize;oats"
	d$soil_texture <- "sandy clay loam"
	d$season <- "dry"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- TRUE
	d$geo_from_source <- TRUE
  d$P_fertilizer <- 140
  d$K_fertilizer <- 0
  d$N_fertilizer <- 280
  d$S_fertilizer <- d$lime <- as.numeric(NA)
	d$yield_part <- "grain"
	d$yield_moisture <- ifelse(d$crop =="maize",14,70)

	carobiner::write_files(path, meta, d)
}

