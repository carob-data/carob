# R script for "carob"


carob_script <- function(path) {

"Increasing organic matter/carbon contents of soils is one option from a basket of strategies being proposed to offset climate change inducing greenhouse gas (GHG) emissions, under the auspices of the Paris-COP 4 per mille initiative. One of the complimentary practices to sequester carbon in soils on decadal timescales is amending it with biochar, a carbon rich byproduct of biomass gasification. In sub-Saharan Africa (SSA) there is widespread and close interplay of agrarian based economies and the use of biomass for fuel, which makes that the co-benefits of biochar production for agriculture and energy supply are explicitly different from the rest of the world. To date, the quantities of residues available from staple crops for biochar production, and their potential for carbon sequestration in farming systems of SSA have not been comprehensively investigated.

Herein we assessed the productivity and usage of biomass waste from: maize, sorghum, rice, millet and groundnut crops; specifically quantifying straw, shanks, chaff and shells, based on measurements from multiple farmer fields and census/surveys in eastern Uganda"

	uri <- "doi:10.25502/EERP-3F45/D"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)
  
	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		project=NA, 
		publication= NA, 
		data_organization = "IITA;BOKU;MAK",
		carob_contributor="Cedric Ngakou", 
		carob_date="2023-11-21", 
		data_type="crop-cuts",
		response_vars = "none",
		treatment_vars = "none"
	)

	## process file(s) 
	r <- read.csv(ff[basename(ff)=="Groundnut_biomass_sampling.csv"])
	d <- r[,c("Season","Quadrant_ID","Density_plant_m_2","Yield_grain_dry_ton_ha_1","Yield_straw_dry_ton_ha_1")]
	colnames(d)<- c("season","trial_id","plant_density","yield","dmy_residue")
	
	d$dmy_storage <- d$yield * 1000 # in kg/ha 
	
	d$yield <- d$yield * 1000 * 1.18 #fresh weight in kg/ha 
	d$dmy_residue <- d$dmy_residue * 1000 
	
	d$plant_density <- d$plant_density * 10000 # in plant/ha
	
	#add columns
	d$crop <- "groundnut"
	d$country <- "Uganda"

	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$inoculated <- FALSE
	d$yield_part <- "grain" 
	## add long and lat
	
	## add long and lat from metadata
	d$longitude <- 32.29028   
	d$latitude <- 1.37333
	d$geo_from_source <- FALSE

	d$planting_date <- c("2016-02", "2017-02")[d$season]
	d$harvest_date <- c("2016-06", "2017-06")[d$season]	
	#d$season <- c("Feb-June 2016", "Feb-June 2017")[d$season]
	d$season <- NULL
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
 
	carobiner::write_files(meta, d, path=path)
}


