# R script for "carob"
# license: GPL (>=3)

## ISSUES
## the amount of rainfall treatment is missing.

carob_script <- function(path) {

"
Data for 'Intra-seasonal rainfall patterns and extremes drive maize productivity and nitrogen use in sub-humid Zimbabwe'

These are the raw data of the paper 'Intra-seasonal rainfall patterns and extremes drive maize productivity and nitrogen use in sub-humid Zimbabwe' authored by Abderrahim Bouhenache, Gwenaëlle Lashermes, Hugues Clivot, Sylvie Recous, Regis Chikowo, Armwell Shumba, Hope Mazungunye, Gonzague Alavoine, Olivier Delfosse, Gatien N Falconnier, François Affholder, Marc Corbeels, Rémi Cardinael
"

	uri <- "doi:10.18167/DVN1/GKONUQ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD; INRAE; URCA; UZIM",
		publication = "doi:10.1016/j.fcr.2025.110126",
		project = NA,
		carob_date = "2025-10-14",
		design = NA,
		data_type ="experiment",
		treatment_vars = "N_fertilizer;mulch",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f <- ff[basename(ff) == "Bouhenache_et_al_2025_FCR_Dataverse.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="Exp_years_climate_dataset")[-1,]
	r2 <- carobiner::read.excel(f, sheet="TAGB_N_data", na= "NA")[-1,]
	r3 <- carobiner::read.excel(f, sheet="Grain_yield_data", na= "NA")[-1,]


	### process weather data 
	
	d1 <- data.frame(
		longitude =31.008 ,
		latitude = -17.703 ,
		location = "Agro-industrial Park, university of Zimbabwe",
		geo_from_source = TRUE,
		country = "Zimbabwe",
		date = paste(r1$year, r1$Nmonth, r1$NdayM, sep = "-"),
		prec = as.numeric(r1$rain),
		temp = as.numeric(r1$tmean),
		tmin = as.numeric(r1$tmin),
		tmax = as.numeric(r1$tmax),
		srad = as.numeric(r1$srad),
		wspd = as.numeric(r1$wind),
		rhum = as.numeric(r1$rhum)
	)
	
	### process treatment and yield data
	
	d2 <- data.frame(
	   harvest_date = as.character(as.Date(as.numeric(r2$Date), origin = "1899-12-31")),
	   planting_date = substr(r2$Crop_season, 1, 4),
	   treatment = r2$Treatment,
	   rep = as.integer(gsub("B", "", r2$Replicate)),
	   mulch = as.numeric(gsub("M", "", r2$Mulch))*1000,
	   N_fertilizer =as.numeric(gsub("N", "", r2$Nitrogen)),
	   dmy_leaves = as.numeric(r2$Leaves_DM_t_ha)*1000,
	   dmy_stems = as.numeric(r2$Stem_DM_t_ha)*1000,
	   dmy_total = as.numeric(r2$AGB_DM_t_ha)*1000,
	   harvest_index = as.numeric(r2$Harvest_index),
	   leaf_N = as.numeric(r2$Tot_N_Leaves)*10,
	   #stem_N = r2$Tot_N_Stem,
	   grain_N = as.numeric(r2$Tot_N_Grains)*10,
	   residue_N = as.numeric(r2$Tot_N_AGB)
	   
	)
	
	 
	
	d3 <- data.frame(
	   harvest_date = as.character(as.Date(as.numeric(r3$Date), origin = "1899-12-31")),
	   planting_date = substr(r3$Crop_season, 1, 4),
	   treatment = r3$Treatment,
	   rep = as.integer(gsub("B", "", r3$Replicate)),
	   mulch = as.numeric(gsub("M", "", r3$Mulch))*1000,
	   N_fertilizer = as.numeric(gsub("N", "", r3$Nitrogen)),
	   seed_weight = as.numeric(r3$DW_1000grains_g),
	   yield = as.numeric(r3$G_yield_t_DM_ha)*1000,
	   trial_id = ifelse(grepl("2022", r3$Crop_season), "1", "2"), 
	   on_farm = FALSE, 
	   is_survey = FALSE, 
	   crop = "maize", 
	   irrigated = FALSE, 
	   ## from publication
	   P_fertilizer = 15, 
	   K_fertilizer = 30, 
	   fertilizer_type ="SSP;KCl",  
	   yield_moisture = 12.5 ,
	   row_spacing = 90,
	   plant_spacing = 25,
	   longitude =31.008 ,
	   latitude = -17.703 ,
	   location = "Agro-industrial Park, university of Zimbabwe",
	   country = "Zimbabwe",
	   geo_from_source = TRUE,
	   yield_part = "grain"
	)
	
d <- merge(d3, d2, by= intersect(names(d2), names(d3)), all = TRUE)

d <- d[!is.na(d$yield),]

### soil from publication
d$soil_clay <- 24.53
d$soil_silt <- 42.9
d$soil_sand <- 32.7
d$soil_texture <- "loam" 
d$soil_pH <- 6.6
d$soil_CEC <-  12.18
d$soil_SOC <- 0.913 #%
d$soil_N_total <- 770
d$soil_bd <-  1.255 


carobiner::write_files(path, meta, d, wth = d1)
}


