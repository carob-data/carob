# R script for "carob"

carob_script <- function(path) {

	uri <- "doi:10.5281/zenodo.11122388"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=4, minor=NA,
		data_organization = "UNL;OAF",
		publication="doi:10.1038/s41467-024-48859-0",
		project=NA,
		data_type= "survey",
		response_vars = "none",
		treatment_vars = "none",
		carob_contributor= "Robert Hijmans",
		carob_date="2024-06-23",
		carob_effort = NA
	)

	r <- read.csv(ff[basename(ff)=="One_Acre_Fund_MEL_maize_survey_data_2016-2022.csv"], na.strings = c("", "NA"))

	d <- data.frame(
		crop="maize",
		yield_part= "grain",
		variety_type = ifelse(r$hybrid, "hybrid", "not hybrid"),
		country = carobiner::fix_name(r$country, "title"),
		season=gsub(" season", "", r$season),
		planting_date=r$plant_date,
		harvest_date=r$harvest_date,
		plant_density = r$pl_m2 * 10000,
		row_spacing = r$row_spacing,
		yield=r$yield_kg_ha,
		N_fertilizer = r$N_kg_ha,
		P_fertilizer = r$P_kg_ha,
		K_fertilizer = r$K_kg_ha,
		lime = r$lime_kg_ha,
		fertilization_method = c("not in hole", "in hole")[r$fert_in_hole + 1],
		latitude = r$lat,
		longitude = r$lon,
		geo_from_source = TRUE,
		variety_release_year = r$hyb_yor,
		weeding_done = r$weeding, 
		insecticide_used = r$pesticide,
		striga_damage = r$striga,
		OM_used = r$compost | r$manure,
		OM_amount = r$comp_kg_ha,
		on_farm= TRUE,
		irrigated = NA,
		is_survey = TRUE
	)
	d$trial_id <- as.character(as.integer(as.factor(paste(d$longitude, d$latitude))))
	
	d$season_constraint <- apply(cbind(
				c("not disease", "disease")[r$disease+1], 
				c("not pest", "pest")[r$pest+1], 
				c("not water_excess", "water_excess")[r$water_excess+1]), 
			1, \(i) paste(i, collapse=";")
		)
	
	d$variety_traits <- apply(cbind(
				paste(c("not", ""), "maize lethal necrosis resistant")[r$hyb_tol_mln+1], 
				paste(c("not", ""), "maize streak virus resistant")[r$hyb_tol_msv+1], 
				paste(c("not", ""), "gray leaf spot resistant")[r$hyb_tol_gls+1], 
				paste(c("not", ""), "northern corn leaf blight resistant")[r$hyb_tol_nclb+1], 
				paste(c("not", ""), "rust resistant")[r$hyb_tol_rust+1], 
				paste(c("not", ""), "ear rot resistant")[r$hyb_tol_ear_rot+1]), 
			1, \(i) paste(i, collapse=";")
		) |> trimws()
	d$variety_traits[d$variety_traits == "NA;NA;NA;NA;NA;NA"] <- NA

	d$yield_moisture <- NA_real_
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d) 
}
