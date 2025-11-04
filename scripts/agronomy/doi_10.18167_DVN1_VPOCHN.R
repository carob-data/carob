# R script for "carob"

carob_script <- function(path) {

"These are the raw data of the paper: 'Mulch application as the overarching factor explaining increase in soil organic carbon stocks under conservation agriculture in two 8-year-old experiments in Zimbabwe.' authored by Armwell Shumba, Regis Chikowo, Christian Thierfelder, Marc Corbeels, Johan Six, Rémi Cardinael and submitted for publication in a peer-reviewed journal."

#treatments: "NTMR" "NTR"  "CTR"  "NTM"  "CT"   "NT"  
#NT = no till, CT = conventional till
#M = +mulch
#R = +residue

	uri <- "doi:10.18167/DVN1/VPOCHN"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)
 
	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=2,
		project = NA,
        publication = "doi:10.1016/j.agee.2022.108207",
		data_organization = "CIRAD; UZIM; CIMMYT; ETH",
		data_type = "experiment", 
		carob_contributor = "Hope Mazungunye",
		carob_date = "2024-02-15",
		modified_by = "Eduardo Garcia Bendito",
		last_modified = "2024-03-07",
		response_vars = "yield",
		treatment_vars = "land_prep_method;mulch"
	)
  
	f <- ff[basename(ff) == "Shumba_et_al_Raw_data_SOC_paper_vf.xlsx"]
	r <- carobiner::read.excel(f, sheet = "Seasonal_OC_inputs")
	
## process file(s)
	d <- data.frame(
		on_farm = FALSE, 
		is_survey = FALSE, 
		country = "Zimbabwe", 
		location = ifelse(r$Site == "DTC", "Domboshava Training Centre", "University of Zimbabwe Farm"),
        longitude = ifelse(r$Site == "DTC", 31.125, 31.013), 
		latitude = ifelse(r$Site == "DTC", -17.588, -17.706),
		geo_from_source = FALSE,
        treatment = r$Treatment, 
		rep = as.integer(r$Rep), 
		crop = tolower(r$Crop),
		mulch = r$Mulch_kg_ha1,
		previous_crop_residue_weight = r$C_input_mz_stubble_kg_ha1 + r$C_input_ABG_cwp_kg_ha1,
		yield = r$`Grain (kg/ha)`
#		crop_rotation = "maize", 
#		previous_crop = "maize"
	)

  # Assign trial_id
	d$trial_id <- paste(r$Year...2, r$Site)
  
  # Add planting and harvest dates from publication
	pyr <- gsub("\\/.*","", r$Year...2)
	d$planting_date <- as.character(paste0(pyr, "-11"))
	d$planting_date[pyr == "2019" & d$location == "DTC"] <- "2019-11-21"
	d$planting_date[pyr == "2019" & d$location == "UZF"] <- "2019-11-19"
	d$planting_date[pyr == "2020"] <- "2020-11-25"
 
	hyr <- as.character(as.integer(pyr) + 1)
	d$harvest_date <- as.character(paste0(hyr, "-04"))
	d$harvest_date[hyr == "2020" & d$location == "DTC"] <- "2020-04-15"
	d$harvest_date[hyr == "2020" & d$location == "UZF"] <- "2020-04-14"
	d$harvest_date[hyr == "2021" & d$location == "DTC"] <- "2021-04-25"
	d$harvest_date[hyr == "2021" & d$location == "UZF"] <- "2021-04-15"
  
  # Biomass variables
	d$dmy_roots <- r$Root_biomass_kg_ha1
	d$dmy_total <- r$Root_biomass_kg_ha1 + r$Total_AGB_kg_ha1
  
  # Yield variables
	d$yield_part <- "grain"
	d$fwy_residue <- r$`Veg_Biomass (kg/ha)`
	# Adding fresh weight
	d$yield[d$crop == "cowpea"] <- d$yield[d$crop == "cowpea"] * 1.125 
	d$yield[d$crop == "maize"] <- d$yield[d$crop == "maize"] * 1.13
  
  # Fertilizer variables
	d$fertilizer_type <- "NPK; AN"
  # 3 equal amoun splits according to publication
	d$N_fertilizer[d$crop == "maize"] <- 11.6 + (23.1 * 2)
	d$N_fertilizer[d$crop == "cowpea"] <- 11.6
	d$P_fertilizer <- 10.6
	d$K_fertilizer <- 9.6
	d$N_splits[d$crop == "maize"] <- 3L
	d$N_splits[d$crop == "cowpea"] <- 1L
  
  # Spacing
	d$plant_spacing <- 25
	d$row_spacing[d$crop == "maize"] <- 90
	d$plant_density[d$crop == "maize"] <- 44444
	d$row_spacing[d$crop == "cowpea"] <- 45
	d$plant_density[d$crop == "cowpea"] <- 88888
  
  # Land preparation
	d$land_prep_method <- "conventional"
	d$land_prep_method[grep("NT", d$treatment)] <- "none"
  
	d$irrigated <- FALSE
	
# all scripts must end like this
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}
