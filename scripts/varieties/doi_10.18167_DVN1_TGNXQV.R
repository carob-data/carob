# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Phenotypic dataset for evaluation of sorghum varieties in Nicaragua (Cirad-Ciat project 2002-2008)

Phenotypic dataset of all the evaluation trials of diverse sorghum varieties carried out in Nicaragua from 2002 to 2008 in the framework of the joint Cirad-Ciat project. This dataset include trials managed on-station and on-farm trials
"

	uri <- "doi:10.18167/DVN1/TGNXQV"
	group <- "varieties"
	ff  <- carobiner::get_data(uri, path, group)
	
	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1017/S001447970999041X",
		project = "Cirad-Ciat project 2002-2008",
		carob_date = "2026-04-23",
		design = "unitOfAnalysis" ,
		data_type = "experiment",
		treatment_vars = "variety;intercrops",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "phenotypic_evaluation_of_grain_and_dual-purpose_sorghum_varieties_in_Nicaragua_PPB_project_2002-2008_V2_mars2026.csv"]
	f2 <- ff[basename(ff) == "readme_information on data collection.txt"]

	r1 <- read.csv(f1, sep = ";", na= "")
	
	d1 <- data.frame(
	   trial_id = trimws(r1$trial_code),
	   adm1 = carobiner::fix_name(r1$location..secteur_commune., "title"),
	   location = carobiner::fix_name(r1$location2..comarca., "title"),
	   rain = r1$rainfall_amount.season..mm.,
	   latitude = r1$latitude,
	   elevation = r1$altitude..m.,
	   soil_texture = ifelse(grepl("clay \\(se encharca=", r1$soil_type), "clay",
	                  ifelse(grepl("sandy", r1$soil_type), "sand", r1$soil_type)) ,
	   intercrops = ifelse(grepl("asocio con maiz", r1$cropping_system), "maize", "none") ,
	   rep = r1$rep,
	   plot_id = as.character(r1$plot_number),
	   variety = trimws(r1$genotype_name),
	   treatment =ifelse(is.na(r1$trial_treatment), "treatment", r1$trial_treatment) ,
	   planting_date = r1$sowing_date,
	   harvest_date = r1$harvest_date,
	   #planting_method = r1$sowing_mode,
	   flowering_date = as.character(as.Date(r1$flowering_date, "%d/%m/%y")),
	   flowering_days = r1$days_to_flowering,
	   #disease = "leaf diseases",
	   disease_severity = as.character(r1$leaf_diseases_severity...1.9.scale.),
	   plant_height = r1$plant_height..cm.,
	   bird_damage = gsub("?", "", r1$Birds_damage....),
	   #midge_damage = r1$midge_damage....,
	   #midge_severity = r1$midge_severity..1.9.scale.,
	   plot_area = r1$harvested_area..m..,
	   yield_moisture = r1$grain_moisture_content_harvest.time....,
	   yield = r1$grain_yield_14...kg.ha.,
	   seed_weight = r1$X1000_kernel_weight..g.,
	   fwy_residue = r1$Fresh_Stover._yield..t.ha.*1000,
	   country = "Nicaragua",
	   on_farm = TRUE, 
	   is_survey = FALSE,
	   crop = "sorghum", 
	   yield_part = "grain",
	   irrigated = NA, 
	   geo_from_source = FALSE
	)

	### Fixing dates
	d1$planting_date <- gsub("mai", "05", d1$planting_date)
	d1$planting_date <- gsub("juin", "06", d1$planting_date)
	d1$planting_date <- gsub("août", "08", d1$planting_date)
	d1$planting_date <- gsub("sept\\.", "09", d1$planting_date)
	d1$planting_date <- gsub("-", "/", d1$planting_date)
	d1$planting_date <- gsub("/03$", "/2003", d1$planting_date)
	d1$planting_date <- gsub("/04$", "/2004", d1$planting_date)	
	d1$planting_date <- as.character(as.Date(d1$planting_date, "%d/%m/%Y"))

	for (y in 2:9) {
		d1$harvest_date <- gsub(paste0("/0", y, "$"), paste0("/200", y), d1$harvest_date)	
	}
	d1$harvest_date <- as.character(as.Date(d1$harvest_date, "%d/%m/%Y"))
	
	### Adding longitude and latitude	
	geo <- data.frame(
	   location = c("La_ceiba", "Aguas_calientes", "Sol_seco", "El_rosario", "Dulce Nombre De Jesus", "La_esperanza", "El_mamel", "Musuli", "San_ramon", "La_concepcion", "La_grama", "Los_laureles", "Cayanlipe", "Quebrada_grande", "El_jobo", "Santa_isabel", "Quilan", "Jamaili", "Cofradia", "Lalamia", "La_carreta", "Jabillito", "La_unión", "Sontole", "Caldera", "El_socorro", "Chilca2", "Mango_solo", "El_ Jobo"),
	   longitude = c(-86.81547, -86.63071, -86.692, -86.5434, -86.3454, -86.4031, -86.444, -86.4070, -85.8406, -86.1885, -86.3969, -86.20414, -86.9170, -86.5069, -86.0706, -86.140, -86.446, -86.477, -86.124, -86.4806, -86.274, -86.13436, -86.3153, -86.7284, -86.448, -86.4553, -86.4153, -86.5138, -86.501),
	   lat = c(12.40011, 13.4691, 13.6345, 13.361, 11.8589, 13.2496, 13.577, 13.4406, 12.923, 11.940, 12.122, 12.1304, 12.9010, 12.9207, 13.03017, 13.1324, 13.5679, 13.4215, 12.1201, 13.379, 12.137, 11.6701, 12.111, 12.2508, 12.7285, 12.6192, 13.555, 13.5444, 13.532)
	)
	
	d <- merge(d1, geo, by="location", all.x = TRUE)
	#d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$latitude <- d$lat
	d$lat <- NULL
	geo1 <- data.frame(
	   adm1 = c("Ceo_posoltega", "Hornito_totogalpa", "Cuje_totogalpa", "Pueblo-Nuevo", "Condega", "Santo-Domingo_totogalpa", "Unile_somoto", "Cnia_managua", "La Manzana_san Lucas", "Villa-Nueva", "Ciudad Dario", "Cayantu_totogalpa"),
	   lon = c(-86.9820, -86.40851, -86.4935, -86.48004, -86.3957,  -86.4932, -86.5855, -86.1481, -86.644, -86.818, -86.12607, -86.4933),
	   lat = c(12.5445, 13.5676, 13.5640, 13.3804, 13.3650, 13.531, 13.4827, 12.1320, 13.4248, 12.965, 12.7318, 13.5641)
	)
	
	d <- merge(d, geo1, by="adm1", all.x = TRUE)
	
	d$latitude[is.na(d$latitude)] <- d$lat[is.na(d$latitude)]
	d$longitude[is.na(d$longitude)] <- d$lon[is.na(d$longitude)]
	d$lon <- d$lat <- NULL
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	### 
	d <- d[!is.na(d$yield),]

	
	carobiner::write_files(path, meta, d)
}


