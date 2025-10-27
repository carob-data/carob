# R script for "carob"
# license: GPL (>=3)

## ISSUES
## The units of some soil variables are not clear.
## The NPK values used are ambiguous. (https://agronomy.unl.edu/ofra/ )


# UNBL: University of Nebraska–Lincoln
#  INRA: Institut National de la Recherche Agronomique du Niger
# INER: Institut D'Economie Rurale
# MARI: Mlingano Agricultural Research Institute, Tanga, Tanzania
# AHBU : Ahmadu Bello University
# LUANR: Lilongwe university of Agriculture and Natural Resources




carob_script <- function(path) {

"
Data from: Diagnosis of crop secondary and micro-nutrient deficiencies in sub-Saharan Africa

Crop production in sub-Saharan Africa has numerous biotic and abiotic constraints, including nutrient deficiencies. Information on crop response to macronutrients is relatively abundant compared with secondary and micronutrients (SMN). Data from 1339 trial replicates of 280 field trials conducted from 2013 to 2016 in 11 countries were analyzed for the diagnosis of SMN deficiencies. The diagnostic data included relative yield response (RYR) and soil and foliar test results. The RYR to application of a combination of Mg, S, Zn, and B (Mg-S-Zn-B) relative to a comparable N-P-K treatment was a >5% increase for 35% of the legume blocks and 60% of the non-legume blocks. The frequencies of soil test Zn, Cu, and B being below their critical level were 28, 2 and 10% for eastern and southern Africa, respectively, and 55, 58 and 89% for western Africa, while low levels for other SMN were less frequent. The frequency of foliar results indicating low availability were 58% for Zn, 16% for S and less for other SMN. The r2 values for relationships between soil test, foliar test and RYR results were <0.035 with little complementarity except for soil test Zn and B with cassava (Manihot esculenta L. Crantz) RYR in Ghana, and foliar Zn with cereal RYR in Uganda. Positive RYR is powerful diagnostic information and indicative of good profit potential for well-targeted and well-specified SMN application. Geo-referenced RYR, soil analysis and foliar analysis results for diagnosis of SMN deficiencies in 11 countries of sub-Saharan Africa were generally not complementary.
"

	uri <- "doi:10.5061/dryad.5t065bb"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=NA,
		data_organization = "UNBL; NARL; INRA; RAB; INER; MARI; AHBU; CSIR; KALRO;CARS; LUANR; ZARI;IITA; INERA",
		publication = "doi:10.1007/s10705-018-09968-7",
		project = NA,
		carob_date = "2025-10-27",
		design = NA,
		data_type = "experiment",
		treatment_vars = "N_fertilizer; Mg_fertilizer; S_fertilizer; Zn_fertilizer; B_fertilizer",
		response_vars = "yield", 
		carob_contributor = "Cedric Ngakou",
		completion = 50,	
		notes = "Some files contain useful information but there are not suitable for this Carob version."
	)
	

	f <- ff[basename(ff) == "Tropical Africa Crop Nutrient Diagnosis data set Jan 2019.xlsx"]

	r1 <- carobiner::read.excel(f, sheet="Soil test data by 1347 blocks", na= "NA" )
	#r2 <- carobiner::read.excel(f, sheet="Foliar concentration data 646bl")
	#r3 <- carobiner::read.excel(f, sheet="Foliar critical values")
	#r4 <- carobiner::read.excel(f, sheet="Foliar conc location groups")
	#r5 <- carobiner::read.excel(f, sheet="Soil test location groups")
	#r6 <- carobiner::read.excel(f, sheet="MgSZnB non-legumes location")
	#r7 <- carobiner::read.excel(f, sheet="MgSZnB legumes location groups")

### process 
	
	d1 <- data.frame(
		country = r1$Country,
		trial_id = r1$Code,
		crop = tolower(r1$Crop),
		crop_type = r1$Type,
		#adm1 = r2$Region,
		latitude = r1$Latitude,
		longitude = r1$Longitude,
		elevation = r1$Elevation,
		yield_treatment = r1$MeanYld0Fert,
		yield_control = r1$`Zero yield`,
		soil_pH = r1$pH,
		soil_Al = as.numeric(gsub("762.3\\+AB19109", 762.3, r1$AL)),
		soil_B = r1$B,
		soil_Cu = r1$CU,
		soil_Fe = r1$FE,
		soil_Mn = r1$MN,
		soil_P = r1$m3P,
		soil_S = r1$S,
		soil_Zn = r1$Zn,
		#r2$PSI,
		soil_Na = as.numeric(r1$ExNa)*10*23,
		soil_Ca = r1$ExCa*10*40/2,
		soil_Mg = r1$ExMg*10*24/2,
		soil_K = r1$ExK*10*39,### from meq/100g to ppm
		#soil_EC = r1$ECd,
		soil_N_total = r1$TotN,
		soil_C_total = r1$TotCarbon/10000,
		soil_clay = r1$Clay/10,
		soil_silt = r1$Silt/10,
		soil_sand = r1$Sand/10,
		on_farm = TRUE, 
		is_survey = FALSE, 
		yield_part = "none", 
		yield_moisture = as.numeric(NA), 
		irrigated = NA, 
		geo_from_source= TRUE
		
		
	)

 d1 <- reshape(d1, varying = c("yield_treatment", "yield_control"), 
               v.names = "yield",
               times = c("MgSZnB + NPK", "NPK"),
               timevar = c("treatment"),
               direction = "long")	
	
row.names(d1) <- NULL

d1$yield <- d1$yield*1000 ## from Mg/ha to  kg/ha

### from publication 

 d1$Mg_fertilizer <- ifelse(grepl("MgSZnB \\+ NPK", d1$treatment), 10, 0)
 d1$S_fertilizer <- ifelse(grepl("MgSZnB \\+ NPK", d1$treatment), 15, 0)
 d1$Zn_fertilizer <- ifelse(grepl("MgSZnB \\+ NPK", d1$treatment), 2.5, 0)
 d1$B_fertilizer <- ifelse(grepl("MgSZnB \\+ NPK", d1$treatment), 0.5, 0)
 
 ### Adding NPK fertilizer from publication
 d1$P_fertilizer <- 15
 d1$K_fertilizer <- 20
 
 ### Fertilizer recommendation in Rwanda from https://agronomy.unl.edu/ofra/ as mention in publication
 
 d1$N_fertilizer <- ifelse(grepl("maize|wheat", d1$crop) & grepl("Rwanda", d1$country) , 41, 
                    ifelse(grepl("bean", d1$crop) & grepl("Rwanda", d1$country), 18, 
                    ifelse(grepl("rice", d1$crop) & grepl("Rwanda", d1$country),  80, 0)))
 
 ### Fertilizer recommendation in Zambia
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Zambia", d1$country) , 112, 
                    ifelse(grepl("bean", d1$crop) & grepl("Zambia", d1$country), 30, d1$N_fertilizer))
 
 
 ### Fertilizer recommendation in Malawi
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Malawi", d1$country) , 92, 
                    ifelse(grepl("soybean", d1$crop) & grepl("Malawi", d1$country), 50, d1$N_fertilizer))
 
 
 ### Fertilizer recommendation in Tanzania
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Tanzania", d1$country) , 112, 
                    ifelse(grepl("bean|sorghum", d1$crop) & grepl("Tanzania", d1$country), 30, 
                    ifelse(grepl("wheat", d1$crop) & grepl("Tanzania", d1$country),40, d1$N_fertilizer)))
 
 ### Fertilizer recommendation in Kenya
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Kenya", d1$country) , 75, 
                    ifelse(grepl("sorghum", d1$crop) & grepl("Kenya", d1$country), 50, 
                    ifelse(grepl("cassava", d1$crop) & grepl("Kenya", d1$country), 100, d1$N_fertilizer)))
 
 
 ## Fertilizer recommendation in Uganda
 
 d1$N_fertilizer <- ifelse(grepl("finger millet", d1$crop) & grepl("Uganda", d1$country) , 40, d1$N_fertilizer)
 
 
 ### Fertilizer recommendation in Ghana
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Ghana", d1$country) , 90, 
                    ifelse(grepl("sorghum|cassava", d1$crop) & grepl("Ghana", d1$country), 60, 
                    ifelse(grepl("cowpea", d1$crop) & grepl("Ghana", d1$country), 20, d1$N_fertilizer)))
 
 
 ### Fertilizer recommendation in Nigeria
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Nigeria", d1$country) , 120, 
                    ifelse(grepl("sorghum", d1$crop) & grepl("Nigeria", d1$country), 64, 
                    ifelse(grepl("soybean", d1$crop) & grepl("Nigeria", d1$country), 20, d1$N_fertilizer)))
 
 ### Fertilizer recommendation in Burkina Faso
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Burkina Faso", d1$country) , 67, 
                    ifelse(grepl("sorghum|pearl millet", d1$crop) & grepl("Burkina Faso", d1$country), 37, 
                    ifelse(grepl("rice", d1$crop) & grepl("Burkina Faso", d1$country), 74, 
                    ifelse(grepl("cowpea", d1$crop) & grepl("Burkina Faso", d1$country), 14, d1$N_fertilizer))))
 
 
 
 ### Fertilizer recommendation in Mali
 
 d1$N_fertilizer <- ifelse(grepl("maize", d1$crop) & grepl("Mali", d1$country) , 84, 
                    ifelse(grepl("sorghum|pearl millet", d1$crop) & grepl("Mali", d1$country), 32, 
                    ifelse(grepl("rice", d1$crop) & grepl("Mali", d1$country), 80, d1$N_fertilizer)))
 
 ### Fertilizer recommendation in Niger
 
 d1$N_fertilizer <- ifelse(grepl("maize|sorghum|pearl millet|cowpea", d1$crop) & grepl("Niger", d1$country) , 46, d1$N_fertilizer) 
                    
 #### For All legume crop, No N fertilizer were apply except bean 
 
 d1$N_fertilizer <- ifelse(grepl("Pulse", d1$crop_type), 0, 
                   ifelse(grepl("Pigeonpea", d1$crop), 0, d1$N_fertilizer))
 
 
 d1$crop_type <- d1$id <- NULL
 ### fixing crop 
 d1$crop <- gsub("^bean$", "common bean", d1$crop)
 d1$crop <- gsub("rice ul", "rice", d1$crop)
 d1$crop <- gsub("pigeonpea", "pigeon pea", d1$crop)

 ### Fixing planting date 
 
 d1$planting_date <- ifelse(grepl("13", d1$trial_id), "2013",
                     ifelse(grepl("14", d1$trial_id), "2014",
                     ifelse(grepl("15", d1$trial_id), "2015", "2016"))) 
 
 d1$rep <- suppressWarnings(as.integer(gsub("_|a|c|r|u|F|f|o|V|M|C|b|e|A|T|i|S", "", substr(d1$trial_id, nchar(d1$trial_id)-1, nchar(d1$trial_id)))))
 
 
 ### Fixing long and lat 
 i <- grepl("Ghana|Mali|Burkina Faso", d1$country)
 d1$longitude[i] <- -abs(d1$longitude[i])
 
 ### drop rows with NA in yield 
 
 d <- d1[!is.na(d1$yield),]
 # remove duplicated records
 d <- unique(d)
 
 ### We need to  investigate why some values are greater than 100
 
 d$soil_clay[ d$soil_clay > 100] <- NA
 d$soil_sand[ d$soil_sand > 100] <- NA
 d$soil_silt[ d$soil_silt > 100] <- NA
 
 
carobiner::write_files(path, meta, d)

}


