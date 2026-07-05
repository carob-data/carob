# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. NA values in yield produced from NA values in production and area
#2. added a new variable

carob_script <- function(path) {

"
Maize Producers (Households Participated) in MCM Activities in Nepal

The dataset used in this study was collected as part of the evaluation of the Maize Commercialization Model (MCM) in Nepal, implemented under the Nepal Seed and Fertilizer (NSAF) project led by CIMMYT and funded by USAID. The dataset comprises survey responses from 493 smallholder households across five districts—Banke, Bardiya, Kailali (MCM intervention areas), and Dang, Kanchanpur (non-MCM comparison areas). The survey utilized a multistage random sampling approach, ensuring adequate representation of both male- and female-headed households (MHH and FHH). Key variables include demographic characteristics, agricultural practices, access to resources, maize productivity, and market participation. The dataset enables gender-disaggregated analysis of the impact of bundled agricultural interventions on commercialization outcomes. Data collection was conducted through structured household interviews, ensuring consistency and reliability. The dataset is anonymized to protect respondent confidentiality and is available in CSV and STATA formats for further analysis.

design: unitOfAnalysis: Household; targetSampleSize: list(targetSampleActualSize = list(typeName = targetSampleActualSize, multiple = FALSE, typeClass = primitive, value = 493)); collectionMode: Face to face interviews; researchInstrument: Survey questionnaire
"

	uri <- "hdl:11529/10549225"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "NSFP",
		design = NA,
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
	#converting Nepal kattha to hactares 
	r$area <- r$MainVar_Area * 0.03386 
	
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
	
	d$yield <- r$Prod_MainVar_Kg/r$area #calculating yield from production
	d$yield_moisture <- as.numeric(NA)
	d$yield_isfresh <- NA
	d$geo_from_source <- TRUE
	d$planting_date <- as.character(NA)
	d$OM_type <- NA
	d[!is.na(d$OM_amount), "OM_type"] <- "farmyard manure"
	d[!is.na(d$land_irrigated), "irrigated"] <- TRUE
	
	d$fertilizer_type <- gsub("Other ferilizers", "unknown", d$fertilizer_type)
	d$fertilizer_type <- gsub("MOP", "KCl", d$fertilizer_type)
	d$fertilizer_type <- gsub("Urea", "urea", d$fertilizer_type)
	d$fertilizer_type <- gsub("None", "none", d$fertilizer_type)
	d$fertilizer_type <- gsub(" ", ";", d$fertilizer_type)
	
	d$previous_crop <- gsub(" ", ";", gsub("- ", "", d$previous_crop))


	animal_cols <- c("cattle", "buffalo", "sheep", "goat", "pig", "chicken", "duck")	
	x <- reshape(d[, c("hhid", animal_cols)], varying = animal_cols, v.names = "heads", 
		timevar = "animal", times = animal_cols, idvar = "row_id", direction = "long")

	rownames(x) <- NULL
	x$row_id <- NULL  
	d[, animal_cols] <- NULL
	
	
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$yield_part <- "grain"

	carobiner::write_files(path, meta, d, long=x)
}
