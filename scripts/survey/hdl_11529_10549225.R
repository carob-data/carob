# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. NA values in yield produced from NA values in production and area
#2. added a new variable

carob_script <- function(path) {

"
Maize Producers (Households Participated) in MCM Activities in Nepal

The dataset used in this study was collected as part of the evaluation of the Maize Commercialization Model (MCM) in Nepal, implemented under the Nepal Seed and Fertilizer (NSAF) project led by CIMMYT and funded by USAID. The dataset comprises survey responses from 493 smallholder households across five districts—Banke, Bardiya, Kailali (MCM intervention areas), and Dang, Kanchanpur (non-MCM comparison areas). The survey utilized a multistage random sampling approach, ensuring adequate representation of both male- and female-headed households (MHH and FHH). Key variables include demographic characteristics, agricultural practices, access to resources, maize productivity, and market participation. The dataset enables gender-disaggregated analysis of the impact of bundled agricultural interventions on commercialization outcomes. Data collection was conducted through structured household interviews, ensuring consistency and reliability. The dataset is anonymized to protect respondent confidentiality and is available in CSV and STATA formats for further analysis.
"

	uri <- "hdl:11529/10549225"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "NSFP",
		design = "unitOfAnalysis: Household; targetSampleSize: list(targetSampleActualSize = list(typeName = targetSampleActualSize, multiple = FALSE, typeClass = primitive, value = 493)); collectionMode: Face to face interviews; researchInstrument: Survey questionnaire",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none", 
		notes = NA,
		carob_contributor = "Blessing Dzuda",
		carob_date = "2026-07-02",
		carob_completion = 90,	
		carob_effort = 16
	)
	

	f1 <- ff[basename(ff) == "MCM_Nepal_2022_NoPII.xlsx"]

	r <- carobiner::read.excel(f1, sheet="Main Sheet")
	
	d <- data.frame(
	  date=as.character(r$Date),
	  country="Nepal",
	  hhid=as.character(r$HH_ID...731),
	  adm2=r$District,
	  adm4=as.character(r$Ward),
	  latitude=r$GPS_Lat,
	  longitude=r$GPS_Long,
	  elevation=r$GPS_Alt,
	  age=as.numeric(r$Resp_Age),
	  sex=tolower(r$HHH_Sex),
	  education=as.character(r$HHH_Educn),#years of formal education
	  hh_adult_men=r$HH_Male_Num...34,
	  hh_adult_women=r$HH_Female_Num,
	  hh_size=r$HH_TotalMemb_Num,
	  hh_elders=r$HH_65Above_Num,
	  market_distance=r$distance_market_km,
	  hh_income_source=r$HH_Income_Src,
	  hh_income=r$AgriIncome_Total...78,
	  currency="NPR",
	  cattle=r$Livstck_Num_Cattle,
	  buffalo=r$Livstck_Num_Buffalo,
	  sheep=r$Livstck_Num_Sheep,
	  goat=r$Livstck_Num_Goat,
	  pig=r$Livstck_Num_Pig,
	  chicken=r$Livstck_Num_Poultry,
	  duck=r$Livstck_Num_Duck,
	  land_irrigated=r$LowLand_Irrg_Area+r$Upland_Irrg_Area,
	  cropland_owned=r$LowLand_NonIrrg_Area+r$Upland_NonIrrg_Area,
	  cropland_rentedin=r$AgriLand_Rent_Area,
	  crop="maize",
	  variety=r$Maize_MainVar_2020,
	  seed_rate=r$Maize_Seed_Amnt,
	  seed_cost=r$Maize_Seed_Cost,
	  fertilizer_type=r$Apply_ChemFert,
	  N_fertilizer=(r$Urea_Total_Qty * 0.46) + (r$DAP_Total_Qty * 0.18),
	  P_fertilizer=r$DAP_Total_Qty*0.2007,
	  K_fertilizer=r$MOP_Total_Qty * 0.4981,
	  OM_amount=r$FertApp_FYM_Kg,
	  farm_labour_hired=r$FemaleLab_Hired_Days+r$MaleLab_Hired_Days,
	  previous_crop=tolower(r$Prev_Crop)
	  )
	
	r$area <- r$MainVar_Area*0.03386 #converting Nepal kattha to hactares 
	d$yield <- r$Prod_MainVar_Kg/r$area #calculating yield from production
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$geo_from_source <- TRUE
	d$planting_date <- as.character(NA)
	d$OM_type <- NA
	d[!is.na(d$OM_amount), "OM_type"] <- "farmyard manure"
	d[!is.na(d$land_irrigated), "irrigated"] <- TRUE
	
	#cleaning fertilizer columns
	fert_lookup <- c(
	  DAP = "DAP",
	  Urea = "urea",
	  MOP = "KCl",
	  Otherfertilizers = "unknown")
	
	clean_fert <- function(x) {
	  if (is.na(x)) return(NA_character_)
	  if (x == "None") return("none")
	  
	  # collapse the two-word "Other ferilizers" into one token before splitting
	  x2 <- gsub("Other ferilizers", "Otherfertilizers", x, fixed = TRUE)
	  
	  toks <- strsplit(x2, "\\s+")[[1]]
	  mapped <- ifelse(toks %in% names(fert_lookup), fert_lookup[toks], "unknown")# "other" was not specified
	  mapped <- unique(mapped)  
	  
	  paste(mapped, collapse = ";")}
	
	d$fertilizer_type <- sapply(d$fertilizer_type, clean_fert)
	
	#clening previous crop
	crop_lookup <- c(rapeseed = "rapeseed", lentil = "lentil", 
	                 potato = "potato", wheat = "wheat")
	
	canon_order <- c("rapeseed", "lentil", "potato", "wheat")
	
	clean_crop <- function(x) {
	  if (is.na(x)) return(NA_character_)
	  if (x == "none") return("none")
	  
	  x2 <- gsub("rape-\\s*seed", "rapeseed", x)
	  x2 <- trimws(x2)
	  
	  toks <- strsplit(x2, "\\s+")[[1]]
	  toks <- toks[toks != ""]
	  
	  mapped <- ifelse(toks %in% names(crop_lookup), crop_lookup[toks], toks)
	  mapped <- mapped[mapped != ""]
	  mapped <- unique(mapped)
	  mapped <- mapped[order(match(mapped, canon_order))]
	  
	  paste(mapped, collapse = ";")
	}
	
	d$previous_crop <- sapply(d$previous_crop, clean_crop)

	#cleaning livestock namess
	d$row_id <- seq_len(nrow(d))
	
	animal_cols <- c("cattle", "buffalo", "sheep", "goat", "pig", "chicken", "duck")
	
	d <- reshape(d,
	                  varying = animal_cols,
	                  v.names = "heads",
	                  timevar = "animal",
	                  times = animal_cols,
	                  idvar = "row_id",
	                  direction = "long")
	
	rownames(d) <- NULL
	d$row_id <- NULL  
	
	d$trial_id <- paste(d$hhid,d$adm2,sep = "_")
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"

	carobiner::write_files(path, meta, d)
}
