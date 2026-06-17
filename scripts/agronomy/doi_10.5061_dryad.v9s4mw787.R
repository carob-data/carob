# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Data from: Impacts of rotation, tillage, cover cropping, and drainage on soil health in soybean-based cropping systems: Evidence from 4–50-year trials across the US

Recent studies highlight conservation management practices as an effective strategy to enhance soil health. However, results vary, particularly regarding which soil health parameters respond most sensitively to these practices. More studies covering a wide range of soil types and climatic conditions are needed to assist farmers in making management decisions on production practices related to soil health. In this study, we collected soil samples (0-15 cm) from 21 (4–50 years) soybean [Glycine max (L.) Merr.]-based cropping systems trials across the United States (US) to assess the impact of management practices on soil health indicators. Soil indicators included wet aggregate stability (WAS), permanganate oxidizable carbon (POXC), organic matter loss-on-ignition (OM-LOI), mineralizable carbon (Min-C), water extractable organic carbon (WEOC), total organic carbon (TOC), soil extractable protein (ACE-N), total nitrogen (TN), pH, soil test phosphorus (STP), and soil test potassium (STK). Our objectives were: (i) to assess the effects of crop rotation, tillage, cover cropping, and artificial drainage on soil health; (ii) to inform soybean farmers about the management practices that are associated with improvements on soil health; and (iii) to develop and share a unique and open soil health dataset with the research community for future global meta-studies. To assess the effects of management practices on soil health indicators, both meta-analysis approach and linear mixed-effect models were used. Two-crop rotations were associated with greater STP values compared to a single-crop. The inclusion of cover crops was associated with greater Min-C and WEOC compared to no cover crops. No-tillage showed more acidic pH than conventional tillage. The remaining soil health indicators tested did not change in response to the management practices assessed. There were no statistically significant differences in observed soil tests between tile-drained and undrained treatments. Overall results suggest that cover crops can play an important role in building soil health in soybean-based cropping systems. Our open-access dataset provides a valuable resource for future research and meta-studies, ultimately contributing to the development of more effective management strategies for promoting more sustainable soybean cropping systems.
"
  
  uri <- "doi:10.5061/dryad.v9s4mw787"
  group <- "agronomy"
  ff  <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=NA,
		data_organization = "UAKS; NDSU; UKY; NCSU;UAR; OHSU; MSU; IASU; CLU; UMINN",  
		publication = "doi:10.1016/j.agee.2025.109950",
		project = NA,
		carob_effort = NA,
		carob_date = "2026-05-12",
		design = NA,
		data_type = "experiment",
		treatment_vars = "land_prep_method;crop_rotation;cover_crop_used",
		response_vars = "soil_pH;soil_N_total;soil_SOC;soil_C_total;soil_P;soil_K;soil_protein", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "SeveroSilvaEtal_EGEE_Dryad.xlsx"]
	f2 <- ff[basename(ff) == "README.md"]

	r1 <- carobiner::read.excel(f1, sheet="readme")
	r2 <- carobiner::read.excel(f1, sheet="meta")
	
	### process
	
	d <- data.frame(
	  trial_id = paste(r2$study_name, r2$site_number, sep="-"),
	  location = r2$location,
	  rep = as.integer(r2$rep),
	  date = as.character(r2$year_stablished),
	  crop_rotation = tolower(r2$rotation_crop),
	  land_prep_method = tolower(r2$tillage_factor),
	  treatment = paste(r2$meta_mgm,r2$cover_crop_species, sep = "-"),
	  #drainage = r2$drainage,
	  cover_crop_used = grepl("yes", r2$cover_crop),
	  cover_crop = tolower(r2$cover_crop_species), 
	  OM_used = !grepl("no",r2$manure),
	  fertilizer_used = grepl("yes", r2$fertilizer),
	  soil_type = r2$soil_order,
	  soil_texture = trimws(r2$texture_class),
	  soil_pH = r2$pH,
	  soil_P = r2$STP,
	  soil_K = r2$STK,
	  soil_SOC = r2$TOC,
	  soil_C_total = r2$TC,
	  soil_N_total = r2$TN*10000, ## ppm
	  soil_protein = r2$`ACE-N`,
	  
	  on_farm = TRUE, 
	  is_survey = FALSE, 
	  crop = as.character(NA), 
	  yield_part = "none", 
	  yield = as.numeric(NA), 
	  yield_moisture = as.numeric(NA), 
	  planting_date = as.character(NA),
	  irrigated = NA,
	  yield_isfresh = NA
	)

	###Fixing Crop rotation 
	
	P <- carobiner::fix_name(d$crop_rotation)
	P <- gsub("continuous soybean", "soybean;soybean", P)
	P <- gsub("^corn-soybean$", "maize;soybean", P)
	P <- gsub("wheat-double crop soybean", "wheat;soybean;soybean", P)
	P <- gsub("grain sorghum-soybean", "sorghum;soybean", P)
	P <- gsub("continuous corn", "maize;maize", P)
	P <- gsub("corn-wheat-double crop soybean", "maize;wheat;soybean;soybean", P)
	P <- gsub("grain sorghum-wheat-double crop soybean", "sorghum;wheat;soybean;soybean", P)
	P <- gsub("soybean-wheat-double crop grain sorghum", "soybean;wheat;sorghum;sorghum", P)
	P <- gsub("rotated corn-soybean", "maize;soybean", P)
	P <- gsub("corn-corn-soybean-soybean", "maize;maize;soybean;soybean", P)
	P <- gsub("corn-soybean-wheat", "maize;soybean;wheat", P)
	P <- gsub("corn-wheat-Soybean", "maize;wheat;soybean", P)
	P <- gsub("rotated soybean-corn", "soybean;maize", P)
	P <- gsub("1st yr wheat-double crop soybean after cotton-peanut-corn rotations", "wheat;soybean;soybean;cotton;peanut;maize", P)
	P <- gsub("soybean-wheat-corn", "soybean;wheat;maize", P)
	P <- gsub("soybean-wheat", "soybean;wheat", P)
	P <- gsub("1st year of corn after 5 yr of soybean", "maize;soybean;soybean;soybean;soybean;soybean", P)
	P <- gsub("5th year of soybean after 5 yr of corn", "soybean;soybean;soybean;soybean;soybean;maize;maize;maize;maize;maize", P)
	P <- gsub("rotated corn-soybean", "maize;soybean", P)
	P <- gsub("5th year of corn after 5 yr of soybean", "maize;maize;maize;maize;maize;soybean;soybean;soybean;soybean;soybean", P)
	P <- gsub("1st year of soybean after 5 yr of corn", "soybean;maize;maize;maize;maize;maize", P)
	P <- gsub("^soybean-corn$", "soybean;maize", P)
	P <- gsub("continous wheat", "wheat;wheat", P)
	P <- gsub("csilws", "unknown", P)
	P <- gsub("corn-soybean-corn-oat-alfalfa", "maize;soybean;maize;oats;lucerne", P)
	P <- gsub("corn-wheat-soybean", "maize;wheat;soybean", P)
	P <- gsub("grain sorghum-wheat", "sorghum;wheat", P)
	P <- gsub("1st yr ", "", P)
	P <- gsub(" after cotton-peanut-corn rotations", ";cotton;groundnut;maize", P)
	P <- gsub("corn-wheat", "maize;wheat", P)
	P <- gsub("corn-soybean-oat-alfalfa", "maize;soybean;oats;lucerne", P)
	d$crop_rotation <- P
	
	#### Fixing crop_cover
	
	P <- carobiner::fix_name(d$cover_crop)
	P <- gsub("^cereal rye$", "rye", P)
	P <- gsub("^black oats$", "black oats", P)
	P <- gsub("austrian winter pea", "pea", P)
	#P <- gsub("hairy vetch", "hairy vetch", P)
	P <- gsub("mix 1 \\(cereal rye, crimson clover, seven-top turnip\\)", "rye;crimson clover;turnip", P)
	P <- gsub("mix 2 \\(black oats and austrian winter peas\\)", "black oats;pea", P)
	P <- gsub("no", "none", P)
	P <- gsub("^winter cereal$", "cereal", P)
	P <- gsub("winter legume", "legume", P)
	P <- gsub("winter cereal before soybean", "cereal", P)
	P <- gsub("medium red clover", "red clover", P)
	#P <- gsub("red clover", "red clover", P)
	P <- gsub("mix \\(winter wheat, cereal rye, and camelina\\)", "wheat;rye;camelina", P)
	P <- gsub("mix 2 \\(black oats and peas\\)", "black oats;pea", P)
	d$cover_crop <- P
	
	
	### Fixing soil texture 
	
	d$soil_texture <- ifelse(grepl("silt loam", d$soil_texture), "silty loam", d$soil_texture)
	d$soil_texture <- ifelse(grepl("sandy loam", d$soil_texture), "sandy loam", d$soil_texture)
	d$soil_texture <- ifelse(grepl("silty clay loam", d$soil_texture), "silty clay loam", d$soil_texture)
	
  ### Fixing land_prep_method
  
  P <- carobiner::fix_name(d$land_prep_method)
  P <- gsub("no-tillage", "none", P)
  P <- gsub("moldboard plow", "ploughing", P)
  P <- gsub("conventional tillage", "conventional", P)
  P <- gsub("chisel plow", "ripping", P)
  d$land_prep_method <- P
	
	### Adding longitude and latitude coordinate
  
  geo <- data.frame(
    location = c("AR-Kibler", "AR-Rohwer", "AR-Colt", "AR-Marianna", "IA-Sutherland", "IA-Ames", "IA-Crawfordsville", "KY-Lexington", "MI-Hickory Corners", "MN-Wells", "NC-Lewiston", "NC-Rocky Mountain", "ND-Gardner", "ND-Fargo", "OH-Custar", "WI-Arlington", "WI-Lancaster"),
    longitude = c(-94.232, -91.2752, -90.8110, -90.7709, -95.4968,  -93.6324, -91.5403, -84.5099, -85.3758, -93.7244, -77.1764, -77.7658, -96.9675, -96.7994, -83.8441, -89.3804, -90.7080),
    latitude = c(35.4307, 33.7627, 35.1316, 34.774, 42.9740, 42.0294, 41.2172, 38.0419, 42.4417, 43.7456, 36.1237, 35.9728, 47.1473, 46.8816, 41.2848, 43.3379, 42.8476),
    country = "United States",
    geo_from_source = FALSE
  )
  
  d <- merge(d, geo, by = "location", all.x = TRUE)
  
  d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
  
  ### 
  d <- unique(d)
  
	carobiner::write_files(path, meta, d)
}


