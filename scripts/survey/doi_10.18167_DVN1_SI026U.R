# R script for "carob"
# license: GPL (>=3)

## ISSUES
# The planthealth_V2 and dataABC_V2.xlsx files have not been processed because
# the information in the planthealth_V2 file is already included in the data_field_clean file
#  and the dataABC_V2 file does not contain relevant information for carob.

carob_script <- function(path) {

"
Data of on-farm assessment of cropping practices in a 'one health' perspective, Winter wheat, France

Data measured in 44 farms covering a range of cropping practices, soil and production parameters under contrasted types of crop management: conventional and conservation agriculture. Eighty-six winter wheat fields located in Northwestern France were monitored for two growing seasons (2021/2022 and 2022/2023). The dataset encompasses data about cropping practices (tillage, soil cover, rotation, pesticides use, nutrition), soils (chemical, biological and physical parameters, including texture), and grain production (nutritional, technological and sanitary indicators).
"

	uri <- "doi:10.18167/DVN1/SI026U"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0,
		data_organization = "CIRAD; PUADV; ENSAT",
		publication = NA,
		project = NA,
		carob_date = "2025-11-17",
		carob_effort = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		carob_contributor = "Cedric Ngakou",
		carob_completion = 75,	
		notes = NA
	)
	

	
	f1 <- ff[basename(ff) == "data_field_clean_V2.xlsx"]
	#f2 <- ff[basename(ff) == "dataABC_V2.xlsx"]
	#f3 <- ff[basename(ff) == "planthealth_V2.xlsx"]
	#f4 <- ff[basename(ff) == "clean_metadata_V2.xlsx"]
	
	r1 <- carobiner::read.excel(f1, na="NA")
	#r2 <- carobiner::read.excel(f2)
	#r3 <- carobiner::read.excel(f3)
	#r4 <- carobiner::read.excel(f4)

	d1 <- data.frame(
		country = "France",
		soil_texture = gsub("_", " ", tolower(r1$Texture_USDA)), #  
		crop = "wheat",
		land_prep_method = ifelse(grepl("CONV", r1$Type), "conventional", "minimum tillage"),
		planting_date = as.character(as.Date(r1$date_seeding, origin= "1899-12-31")),
		harvest_date = as.character(as.Date(r1$date_harvest, origin= "1899-12-31")),
		harvest_days = r1$d.harv,
		#field_size = r1$FieldSize,
		soil_clay = r1$Clay,
		soil_silt = r1$Silt,
		soil_sand = r1$Sand,
		#r1$RestitRes_n,
		#r1$RestitRes_rot,
		P_fertilizer = r1$minP2O5_n/2.29,
		K_fertilizer = r1$minK2O_n/1.2051,
		N_organic = r1$orgN_n,
		P_organic = r1$orgP2O5_n/2.29,
		K_organic = r1$orgK2O_n/1.2051,
		N_fertilizer = r1$farmerYield_n*100/r1$minN_eff_n,
		previous_crop = tolower(r1$Prec_crop_n),
		herbicide_used = ifelse(r1$nbHerbi_n >0, TRUE, FALSE),
		fungicide_used = ifelse(r1$nbFungi_n >0, TRUE, FALSE),
		insecticide_used = ifelse(r1$nbIns_n >0, TRUE, FALSE),
		variety = r1$Wheatvar,
		yield = r1$ExpYield_n*100,
		seed_weight = r1$TKW,
		soil_pH = r1$pH,
		soil_P = r1$s.AP,
		soil_CEC = r1$CEC,
		soil_K = r1$s.AK2O/1.2051,
		soil_Mg = r1$s.AMg,
		soil_Zn = r1$s.AZn,
		soil_Mn = r1$s.AMn,
		soil_Cu = r1$s.ACu,
		soil_Fe = r1$s.AFe,
		soil_B = r1$s.AB,
		soil_Mo = r1$s.AMo,
		soil_Mn_total = r1$s.TMn,
		soil_Cu_total = r1$s.TCu,
		soil_Zn_total = r1$s.TZn,
		soil_Mo_total = r1$s.TMo,
		soil_B_total = r1$s.TB,
		soil_S_total = r1$s.TS,
		soil_K_total = r1$s.TK2O/1.2051,
		soil_Mg_total = r1$s.TMg,
		soil_Fe_total = r1$s.TFe,
		soil_SOM = r1$OM,
		soil_SOC = r1$OC,
		soil_N_total = r1$s.TN,
		soil_MBC = r1$TMC, # soil microbial carbon
		plant_height = r1$Height,
		grain_N = r1$g.TN*10, ## from % to mg/g
		grain_protein = r1$Prot,
		grain_Fe = r1$g.Fe/1000,# mg/kg to mg/g
		grain_K = r1$g.K/100, ## mg/100g to mg/g
		grain_Mn = r1$g.Mn/1000, 
		grain_P = r1$g.P/100, # mg/100g to mg/g
		grain_Zn = r1$g.Zn/1000,
		GHG_emission = r1$GHGtotEmiss,
		pest_number = as.integer(r1$fqPests), 
		pest_severity = as.character(r1$intPests),
		pest_species = "beetles and slug",
		
		trial_id = r1$Id, 
		on_farm = TRUE, 
		is_survey = FALSE, 
		yield_part = "grain", 
		yield_moisture = as.numeric(NA), 
		yield_isfresh = TRUE,
		irrigated = NA, 
		geo_from_source = TRUE		
	)
	d1$record_id <- 1:nrow(d1)

	loc <- data.frame(do.call(rbind, strsplit(r1$Location, " ")))
	names(loc) <- c("ID", "quad")
	loc$record_id <- d1$record_id
	loc$longitude <- r1$GPS_X
	loc$latitude <- r1$GPS_Y
	adm <- data.frame(
		code <- c("16", "17", "37", "49", "79", "86"),
		adm2 <- c("Charente", "Charente-Maritime", "Indre-et-Loire", "Maine-et-Loire", "Deux-Sèvres", "Vienne")
	)
	loc$adm2 <- adm$adm2[match(loc$ID, adm$code)]
	u <- aggregate(loc[, c("longitude", "latitude")], loc[, c("ID", "quad")], mean, na.rm=TRUE)
	names(u)[3:4] <- c("lon", "lat")
	loc <- merge(loc, u, by=c("ID", "quad"))
	i <- is.na(loc$latitude)
	loc$latitude[i] <- loc$lat[i]
	loc$longitude[i] <- loc$lon[i]
	loc <- loc[order(loc$record_id), ]
	d1 <- cbind(d1, loc[, c("adm2", "longitude", "latitude")])
	
	### plant health (use long format)
	dis <- data.frame(record_id=d1$record_id, disease = "septoria", disease_severity = r1$intSept)
	dis <- rbind(dis, data.frame(record_id=d1$record_id, disease = "powdery mildew", disease_severity = r1$intMil))
	dis <- rbind(dis, data.frame(record_id=d1$record_id, disease = "rust", disease_severity = r1$intRust))
	dis$severity_scale="0-1"
	### drop rows with NA in yield 
#	d1 <- d1[!is.na(d1$yield), ] 
	
	### Fixing soil type
	d1$soil_texture <- gsub( "silt loam", "silty loam", d1$soil_texture)
	
	### Fixing previous crop 
	
	P <- carobiner::fix_name(d1$previous_crop)
	P <- gsub("grain maize|seed maize|silage maize", "maize", P)
	P <- gsub("grain sorghum", "sorghum", P)
	P <- gsub("meslin dominated by legumes", "unknown", P)
	P <- gsub("oilseed flax", "flax", P)
	P <- gsub("winter barley", "barley", P)
	P <- gsub("winter wheat", "wheat", P)
	P <- gsub("alfalfa", "lucerne", P)
	P <- gsub("rapeseed \\+ legumes", "rapeseed", P)
	d1$previous_crop <- P
	
	carobiner::write_files(path, meta, d1, long=dis)
}

