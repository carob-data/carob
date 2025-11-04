# R script for "carob"


carob_script <- function(path) {

"The dataset contains experimental data from the San Juan Cotzocon research platform in Oaxaca, Mexico. The database contains data on maize yield, management, profitability and phenology in relation to 8 evaluated treatments. The treatments differed in tillage (disk harrow, zero tillage or permanent raised beds), residue management (removing all or leaving all), fertilization (conventional and soil analysis based) and crop rotation (monoculture maize or rotation with legume)"

	uri <- "hdl:11529/10548976"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		#data_citation="Fonteyne, Simon; Guera, Ouorou Ganni Mariel; Villa Alcántara, Jonatan; Núñez Peñaloza, Omar; Verhulst, Nele, 2023. Maize yield and profitability in a 5 year conservation agriculture experiment in Papaloapan, Oaxaca. https://hdl.handle.net/11529/10548976, CIMMYT Research Data & Software Repository Network, V1",
		data_organization = "CIMMYT",
		publication= NA,
		project=NA,
		data_type= "experiment",
		treatment_vars="land_prep_method;variety" , 
		response_vars="yield",
		carob_contributor= "Shumirai Manzvera",
		carob_date="2024-03-14",
		modified_by= "Cedric Ngakou",
		last_modified= "2024-09-26"
		
	)
	
	f <- ff[basename(ff) == "DAT-SJCotzoconExperiments.xlsx"]
	r <- carobiner::read.excel(f, sheet="San Juan Cotzocon_OAX")

	d <- data.frame(
	   crop="maize",
	   adm1=r$Estado, 
		latitude=r$Latitud, 
		longitude=r$Longitud, 
      elevation=r$Altitud, 
		treatment=r$Name_tr, 
		rep=as.integer(r$Num_Rep), 
		crop_rotation="maize; velvet bean; kidney bean",
	   variety=r$Variety, 
		seed_density=r$Seed_dens,
	   K_fertilizer=r$Fert_K,
		P_fertilizer=r$Fert_P,
		N_fertilizer=r$Fert_N,
		row_spacing=r$Row_dist*100, #cm
	   land_prep_method=r$Till, 
		plant_height=r$Height,
		planting_date=as.character(r$Sowing_date),
		emergence_date=as.character(r$Emergence_date),
	   harvest_date=as.character(r$Harvest_date),
		yield=r$Yield_moist,
		trial_id="1", 
		yield_part="grain" 
		)
 
	
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$country <- "Mexico"
   d$geo_from_source <- TRUE
   
   d$land_prep_method <- gsub("Cero Labranza", "none", d$land_prep_method)
   d$land_prep_method <- gsub("Labranza Convencional", "conventional", d$land_prep_method)
   d$land_prep_method <- gsub("Camas Permanentes Angostas", "permanent beds", d$land_prep_method)
   
	d$yield_moisture <- as.numeric(NA) #needs to be checked

   carobiner::write_files(meta, d, path=path)
}

