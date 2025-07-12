# R script for "carob"


carob_script <- function(path) {
  
"Crop cut survey in 2015 conducted by EIAR and CIMMYT. Replicated crop cuts of 16m2 in farmers fields along with additional data on nutrient use and variety, and soil sample. (2015)"
  
	uri <- "hdl:11529/10548215"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=1,
		project="TAMASA",
		publication="doi:10.5897/AJAR2019.14338",
		data_organization = "CIMMYT;EIAR",
		data_type="crop-cuts",
		response_vars = "none",
		treatment_vars = "none", 
		carob_contributor="Samar Attaher",
		carob_date="2024-06-07"
	)

	f <- ff[basename(ff) == "ET_Baseline_EIAR_2015.xls"] 	
	r <- carobiner::read.excel(f, sheet = "Revised_data")

	r[r == "n/a"] <- NA
	r[r == "."] <- NA


	d <- data.frame(
		country = "Ethiopia",
		latitude=r$Latitude,
		longitude=r$Longitude,
		geo_from_source = TRUE,
		elevation=r$Altitude,
		crop = "maize",
		yield_part = "grain",
		farmer_gender=r$Gender,
		variety_type=r$Type.of.variety,
		variety=r$Name.of.variety,
		plot_area= 16, #16 m2
#		fertilizer_used=r$Fertilizeruse,
		OM_used=as.logical(r$Fertilizer.type.organic),
#		fertilizer_inorganic_used=r$Fertilizer.type.inorganic,
		fertilizer_amount = as.numeric(r$amount.of.Inorganic.fertilizer),
		OM_amount= as.numeric(r$Org_fert_qty),
		soil_SOC = r$Carbon....,
		soil_pH  =r$pH,
		soil_Al =r$Al..mg.kg.,
		soil_Ca =r$Ca...mg.kg., 
		soil_EC =r$EC.S..dS.m.,
		soil_S =r$S...mg.kg.,
		soil_Mn=r$Mn...mg.kg.,
		soil_P_total =r$P...mg.kg.,
		soil_Zn=r$Zn...mg.kg., 
		soil_K=r$K...mg.kg., 
		soil_Mg=r$Mg...mg.kg., 
		soil_Na=r$Na...mg.kg., 
		soil_Fe=r$Fe...mg.kg., 
		soil_B=r$Boron...mg.kg.,
		soil_N = as.numeric(r$Nitrogen....) * 10000 # from % to [mg/kg]
	)

#	d$adm1<- ifelse(d$latitude>10 & d$latitude<12, "Amhara",
#			ifelse(d$latitude==9.696433576, "Beneshangul Gumu",
#		   ifelse(d$latitude>7.3 & d$latitude<10, "Oromia", "SNNPR")))

	d$variety_type <- gsub("Improved_variety", "improved", d$variety_type)
	d$variety_type <- gsub("local_variety", "local", d$variety_type)

	d$planting_date <- as.character(NA)
	d$trial_id <- as.character(NA)

	yield <- r[, grep(".Grain.yield.kg.ha", colnames(r))]
	d$yield <- rowMeans(sapply(yield, as.numeric), na.rm=TRUE)
	d <- d[!is.na(d$yield), ]

	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$irrigated <- as.logical(NA)
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)

	
	carobiner::write_files (path, meta, d) 
}

