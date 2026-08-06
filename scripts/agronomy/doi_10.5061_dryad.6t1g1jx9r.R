# R script for "carob"
# license: GPL (>=3)

## ISSUES

## Core Ideas
# ∙ The legacy of alfalfa monoculture and polyculture stands on subsequent corn was evaluated.
# ∙ Corn grain yield and grain quality improved following alfalfa monocultures and polycultures compared to annual monocultures.
# ∙ Prior alfalfa density, soil organic matter, and soil minerals drove differences in corn grain yield and quality.
# ∙ Inclusion of an annual crop within a stand of alfalfa improved subsequent corn production.
 
#### Treatment 

# - Alfalfa treatment: previously grown either with wheat (mixed cropping system) or without wheat (alfalfa monocrop).
# - Annual crop: wheat crop previously grown as a monocrop in the plot


carob_script <- function(path) {

"
Data from: Legacy effects of alfalfa monocultures or annual crop/alfalfa mixtures on subsequent corn yield and quality

Interseeding annual crops into existing alfalfa (Medicago sativaL.) stands is gaining interest, and one reason may be that alfalfa lowers nitrogen requirements for subsequent crops. However, little is known about the legacy impact of this practice on subsequent corn (Zea mays L.) production. An experiment involving interseeding annual cool‐season crops into alfalfa was conducted between 2017 and 2021, which serendipitously allowed us to evaluate the legacy impact of this practice on subsequent corn grain production. This follow‐up study compared corn grain yield and quality of corn planted subsequently on positive control plots (alfalfa monoculture), negative control plots (annual crop monoculture), and experimental treatment polyculture plots (annual crops planted into alfalfa). We found that corn yield was lower following annual monocultures compared to corn following alfalfa monoculture and polyculture plots. The treatments did not have a significant effect on grain protein or starch percentage, but grain oil percentage was higher following polyculture compared to annual monoculture. Corn grain zinc concentration was positively associated with previous alfalfa density and corn ear leaf chlorophyll concentration. These findings indicate that alfalfa monoculture and alfalfa‐annual crop polycultures can have different positive legacy effects on corn yield, near‐surface soil attributes, and grain quality. Future research aimed at evaluating the legacy of crop/alfalfa mixtures on subsequent corn crops in the northern Great Plains in multiple locations over several years are needed.
"

	uri <- "doi:10.5061/dryad.6t1g1jx9r"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=NA,
		data_organization = "USDA-ARS",
		publication = "doi:10.1002/agg2.70114",
		project = NA,
		carob_date = "2026-07-30",
		design = NA,
		data_type = "on-station experiment",
		treatment_vars = "previous_crop",
		response_vars = "yield;SPAD", 
		notes = NA,
		carob_contributor = "Cedric Ngakou",
		carob_completion = 80,
		carob_effort = 2
	)
	

	f1 <- ff[basename(ff) == "AGE_Whippo_et_al_2025_Data.xlsx"]
	#f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, sheet="Soil Analysis", fix_names = TRUE)
	r2 <- carobiner::read.excel(f1, sheet="Alfalfa Density")
	r3 <- carobiner::read.excel(f1, sheet="Relative Chlorophyll")
	r4 <- carobiner::read.excel(f1, sheet="Grain")

### process
	### soil data
	d1 <- data.frame(
		plot_id = as.character(r1$plot),
		#rep = as.integer(r1$block),
		#previous_crop = tolower(r1$X2021_crop),
		#crop = gsub("Corn", "maize", r1$X2022_crop),
		#treatment = r1$treatment,
		soil_SOM = r1$organic_matter_loi_percent,
		soil_pH = r1$soil_ph,
		soil_N = r1$nitrate_n_ppm_n,
		soil_S = r1$sulfate_s_ppm_s,
		soil_K = r1$potassium_ppm_k,
		soil_P = r1$mehlich_p_iii_ppm_p,
		soil_P_method = "mehlich",
		soil_Zn = r1$zinc_ppm_zn,
		soil_Ca = r1$calcium_ppm_ca,
		soil_Mg = r1$magnesium_ppm_mg,
		soil_Fe = r1$iron_ppm_fe,
		soil_Na = r1$sodium_ppm_na,
		soil_Mn = r1$mangansese_ppm_mn,
		soil_CEC = r1$cec_sum_of_cations_me_100g,
		soil_texture = "sandy loam"
	)

### not sure how to capture this. 
### it is the density of one intercrop in the previous year
#	d2 <- data.frame(
#		plot_id = as.character(r2$plot),
#		prev_crop_plant_density = r2$`alfalfa_density_plants_m^-2`*10000 # /ha
#	)


	d3 <- data.frame(
		plot_id = as.character(r3$plot),
		date = as.character(r3$date),
		SPAD = r3$relative_chlorophyll
	)
	
	# multiple measurments on same date
	d3 <- aggregate(d3["SPAD"], d3[, c("plot_id", "date")], mean)
	

	d4 <- data.frame(
	  rep = as.integer(r4$rep),
	  plot_id = as.character(r4$plot),
	  #pass = r4$`plot_combine pass`,
	  
	  ### previous crop may be this + alfalfa
	  ### see "treatment"
	  previous_crop = tolower(r4$`2021_Crop`),
	  crop = gsub("Corn", "maize",  r4$`2022_Crop`),
	  treatment = r4$treatment,
	  yield = r4$yield_kg_ha,
	  grain_protein = as.numeric(r4$dry_basis_protein_percent),
	  grain_P = r4$P_mg_m,
	  grain_S = r4$S_mg_g,
	  grain_K = r4$K_mg_g,
	  grain_Mg = r4$Mg_mg_g,
	  grain_Fe = r4$Fe_ug_g/1000,
	  grain_Zn = r4$Zn_ug_g/1000
	)
	d4$crop_rotation <- apply(r4[, grep("crop$", tolower(names(r4)), ignore.case=TRUE)], 1, function(x) paste(x, collapse=";	"))
	
	# aggregate over two combine passes
	d4 <- aggregate(. ~ rep + plot_id + previous_crop + crop + treatment + crop_rotation, d4, mean)
	d4$record_id <- 1:nrow(d4)

	### merge d1 and d4
	d <- merge(d1, d4, by="plot_id")
	
	## merge d and d2
	## d <- merge(d, d2, by= "plot_id", all.x = TRUE)
	
	### d3 clearly is a long variable
	d3 <- merge(d3, d4[, c("record_id", "plot_id")], by= "plot_id")
	
	
	d$previous_crop <- gsub("alfalfa", "lucerne", d$previous_crop)
	d$previous_crop <- gsub("spring wheat", "wheat", d$previous_crop)
	d$previous_crop <- ifelse(grepl("Annual \\+ Alfalfa", d$treatment), "wheat;lucerne", d$previous_crop)
	d$crop_rotation <-  gsub("Alfalfa", "lucerne", d$crop_rotation)
	d$crop_rotation <-  gsub("Corn", "maize", d$crop_rotation)
	d$crop_rotation <-  gsub("Field Pea", "pea", d$crop_rotation)
	d$crop_rotation <-  gsub("Canola", "rapeseed", d$crop_rotation)
	d$crop_rotation <-  gsub("Spring Wheat|Winter Wheat", "wheat", d$crop_rotation)
	#d$intercropped_prevcrop <- d$previous_crop=="wheat;lucerne"
	#d$intercrop_prevcrop_type <- "mixed"  
		
	d$K_fertilizer <- d$N_fertilizer <- d$P_fertilizer <-  as.numeric(NA)

	d$is_survey <- FALSE
	d$on_farm <- FALSE
	d$trial_id <- "1"
	d$yield_moisture <- 13 
	d$yield_part <- "grain" 
	d$country <- "United States"  
	d$geo_from_source <- TRUE # from publication
	d$location <- "Mandan"
	d$latitude <- 46.80806  
	d$longitude <- - 100.9156 
	d$irrigated <- NA 
	d$planting_date <- "2022"
	d$harvest_date <- NA
	d$yield_isfresh <- TRUE

	carobiner::write_files(path, meta, d, long=d3)
}


