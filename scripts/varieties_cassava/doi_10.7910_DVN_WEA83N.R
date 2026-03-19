# R script for "carob"
# license: GPL (>=3)

## ISSUES


carob_script <- function(path) {

"
Cassava Diseases Evaluation - Edaphoclimatic Zone 2: Lowland Tropics; Acid Soils; Medium to High Precipitation

CIAT systematically evaluates all germplasm accessions in diverse sites in Colombia, to obtain a broad characterization of adaptation to different soils and climates, resistance to many pests and diseases, and quality traits. The information is used primarily to select parents which can potentially contribute characters requested by national programs. Based on these evaluations, some generalized conclusions can be drawn regarding the genetic potential of cassava landrace varieties.

Within the cultivated species Manihot esculenta a wide range of diversity exists for nearly all traits so far studied, including morphological, agronomic, resistance, and quality traits. Variation in physiological traits has not been widely studied, but there are indications of high variability for sensitivity to temperature and photoperiod, photosynthetic rate, and stomatal sensitivity to air humidity.

Yield potential of the majority of accessions is low and is manifested particularly as a low harvest index. Most landrace cultivars of Latin American origin have evolved with multiple resistance to the local pests and diseases. However, these resistance levels are generally low, since pest control in traditional cultivation systems is accomplished not only by varietal resistance, but also by isolation in space, intercropping, burning and other cultural practices.

The overall conclusion of the evaluations is that few landrace varieties combine all the traits required for more intensified (but low input) cultural systems. Therefore, genetic recombination will play the major role in the future for producing acceptable genotypes. A significant number of landrace varieties have favorable characteristics that have not yet been exploited in breeding programs.

The principal diseases attacking the crop are:

SUPER - Superelongation disease (SED; Sphaceloma manihoticola)   
BACTE - Bacterial blight, Cassava bacterial blight (Xanthomonas axonopodis pv. manihotis).  
Cassava brown streak disease (CBSD; an ipomovirus)  
CUESA - Cassava frogskin disease (CFSD; Candidatus phytoplasma, Cfdp of the 16SrIII-L and rpIII-H subgroups)  
ANTRA - Antracnosis de la yuca – Glomerella manihotis Chev., Collerortichum manihotis Henn, Cassava anthracnose.  
DIPLO, DPLD – Diploidia manihotis, Necrosamiento del tallo, Dry rot of stem and root.  
OIDIM – Oidium Manihotis, Ceniza de la yuca Cassava ash.  
PHOMA – Phoma sp, Mancha de anillos circulares, Concentric ring leaf spot.  
CERCV - Cercospora Vicosae, Añublo pardo fungoso, Diffuse leaf spot.  
CERCH - Cercospora Henningsi, Brown leaf spot.  
CERCC - Cercospora Caribea.  
CERCO - Complex of several Cercospores.  
MOSAI - Common Mosaic, Cassava mosaic disease (CMD; begomovirus complex).
"

	uri <- "doi:10.7910/DVN/WEA83N"
	group <- "varieties_cassava"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIAT",
		publication = "handle:10568/56417",
		project = NA,
		carob_date = "2026-03-13",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity", 
		carob_contributor = "Cedric Ngakou",
		completion = 100,	
		notes = NA
	)
	

	f1 <- ff[basename(ff) == "2.Cassava-diseases-amount.xls"]
	f2 <- ff[basename(ff) == "3.Cassava-diseases-dictionary.xls"]
	f3 <- ff[basename(ff) == "4.01.All-Type-Trials_Locations 2.xlsx"]
	f4 <- ff[basename(ff) == "4.02.All-Type-Trials_Data 2.xlsx"]
	f5 <- ff[basename(ff) == "5.01.Clonal-Evaluation_Locations 2.xlsx"]
	f6 <- ff[basename(ff) == "5.02.Clonal-Evaluation_Data 2.xlsx"]
	f7 <- ff[basename(ff) == "6.01.Preliminary-Yield_Trials_Locations 2.xlsx"]
	f8 <- ff[basename(ff) == "6.02.Preliminary-Yield_Trials_Data 2.xlsx"]
	f9 <- ff[basename(ff) == "7.01.Advanced-Yield_Trials_Locations 2.xlsx"]
	f10 <- ff[basename(ff) == "7.02.Advanced-Yield_Trials_Data 2.xlsx"]
	f11 <- ff[basename(ff) == "9.01.Regional_Trials_Locations 2.xlsx"]
	f12 <- ff[basename(ff) == "9.02.Regional_Trials_Data 2.xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5 <- carobiner::read.excel(f5)
	r6 <- carobiner::read.excel(f6)
	r7 <- carobiner::read.excel(f7)
	r8 <- carobiner::read.excel(f8)
	r9 <- carobiner::read.excel(f9)
	r10 <- carobiner::read.excel(f10)
	r11 <- carobiner::read.excel(f11)
	r12 <- carobiner::read.excel(f12)


### process
	
	d1a <- data.frame(
		trial_id = r3$TRIAL,
		location = r3$LOCATION_NAME,
		longitude = r3$LONGITUD,
		latitude = r3$LATITUDE,
		elevation = r3$ALTITUDE,
		planting_date = r3$PLANT_DATE,
		harvest_date = r3$HARVEST_DATE
	)
	
	d1b <- data.frame(
	  trial_id = r4$TRIAL,
	  disease = r4$Diseases,
	  variety = r4$DESIGNATION,
	  rep = r4$REPETITION,
	  disease_severity = r4$SEVERITY,
	  severity_scale = "1-5"
	)
### merge d1a and d1b
	
	d1 <- merge(unique(d1a), d1b, by = "trial_id", all = TRUE)
	
		
###
	d2a <- data.frame(
	  trial_id = r5$TRIAL,
	  location = r5$LOCATION_NAME,
	  longitude = r5$LONGITUD,
	  latitude = r5$LATITUDE,
	  elevation = r5$ALTITUDE,
	  planting_date = r5$PLANT_DATE,
	  harvest_date = r5$HARVEST_DATE
	)
	
	d2b <- data.frame(
	  trial_id = r6$TRIAL,
	  # note the mistake in variable names in the original data
	  disease = r6$DESIGNATION,
	  variety = r6$Diseases, 
	  rep = r6$REPETITION,
	  disease_severity = r6$SEVERITY,
	  severity_scale = "1-5"
	)
	
	d2 <- merge(d2a, d2b, by = "trial_id", all = TRUE)
	
	###	
	d3a <- data.frame(
	  trial_id = r7$TRIAL,
	  location = r7$LOCATION_NAME,
	  longitude = r7$LONGITUD,
	  latitude = r7$LATITUDE,
	  elevation = r7$ALTITUDE,
	  planting_date = r7$PLANT_DATE,
	  harvest_date = r7$HARVEST_DATE
	)
	
	d3b <- data.frame(
	  trial_id = r8$TRIAL,
	  disease = r8$Diseases,
	  variety = r8$DESIGNATION,
	  rep = r8$REPETITION,
	  disease_severity = r8$SEVERITY,
	  severity_scale = "1-5"
	)

	d3 <- merge(d3a, d3b, by = "trial_id", all = TRUE)	

	#####	
	d4a <- data.frame(
	  trial_id = r9$TRIAL,
	  location = r9$LOCATION_NAME,
	  longitude = r9$LONGITUD,
	  latitude = r9$LATITUDE,
	  elevation = r9$ALTITUDE,
	  planting_date = r9$PLANT_DATE,
	  harvest_date = r9$HARVEST_DATE
	)
	
	d4b <- data.frame(
	  trial_id = r10$TRIAL,
	  disease = r10$Diseases,
	  variety = r10$DESIGNATION,
	  rep = r10$REPETITION,
	  disease_severity = r10$SEVERITY,
	  severity_scale = "1-5"
	)
	
	d4 <- merge(d4a, d4b, by = "trial_id", all = TRUE)
	
	#####	
	d5a <- data.frame(
	  trial_id = r11$TRIAL,
	  location = r11$LOCATION_NAME,
	  longitude = r11$LONGITUD,
	  latitude = r11$LATITUDE,
	  elevation = r11$ALTITUDE,
	  planting_date = r11$PLANT_DATE,
	  harvest_date = r11$HARVEST_DATE
	)
	
	d5b <- data.frame(
	  trial_id = r12$TRIAL,
	  disease = r12$Diseases,
	  variety = r12$DESIGNATION,
	  rep = r12$REPETITION,
	  disease_severity = r12$SEVERITY,
	  severity_scale = "1-5"
	)
	
d5 <- merge(d5a, d5b, by = "trial_id", all = TRUE)

### combine d1, d2, d3, d4 and d5
d <- carobiner::bindr(d1, d2, d3, d4, d5)
#### remove duplicate records
d <- unique(d)

### Fixing Lon and lat coordinate 
d$longitude <- -(as.numeric(substr(d$longitude, 1, 2)) + as.numeric(substr(d$longitude, 4, 5))/60)
d$latitude <- as.numeric(substr(d$latitude, 1, 2)) + as.numeric(substr(d$latitude, 4, 5))/60

## Fixing date 

# convert month names to numeric once
pd <- carobiner::eng_months_to_nr(d$planting_date)
hd <- carobiner::eng_months_to_nr(d$harvest_date)
pd <- ifelse(nchar(pd) < 8, paste0(substr(pd,1,3),"0",substr(pd,4,8)), pd)
hd <- ifelse(nchar(hd) < 8, paste0(substr(hd,1,3),"0",substr(hd,4,8)), hd)

# convert to Date
d$planting_date <- as.character(as.Date(pd, "%d-%m-%y"))
d$harvest_date  <- as.character(as.Date(hd, "%d-%m-%y"))


## data type
d$rep <- as.integer(d$rep) 
d$disease_severity <- as.character(d$disease_severity)


d$country <- "Colombia"
d$on_farm <- NA
d$crop <- "cassava"
d$is_survey <- FALSE
d$geo_from_source <- TRUE
d$yield_moisture <- as.numeric(NA) 
d$yield_part <- "none"
d$yield_isfresh <- TRUE
d$irrigated <- NA
d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)


dis <- data.frame(matrix(c(
	"ANTRA", "cassava anthracnose", "Glomerella manihotis;Collerortichum manihotis",
	"BACTE", "cassava bacterial blight", "Xanthomonas axonopodis pv. manihotis",
	"SUPER", "superelongation disease", "Sphaceloma manihoticola", 
	"CERCC", "cassava brown leaf spot", "Cercospora caribea",
	"CERCH", "cassava brown leaf spot", "Cercospora henningsi",
	"CERCV", "diffuse leaf spot", "Cercospora vicosae",
	"CUESA", "cassava frogskin disease", "Candidatus phytoplasma",
	"ENFER", NA, NA), # probably "enfermedad"; perhaps an unknown disease?
	ncol=3, byrow=TRUE))
	
colnames(dis) <- c("code", "disease", "pathogen")
i <- match(d$disease, dis$code)
d$disease <- dis$disease[i]
d$pathogen <- dis$pathogen[i]
d <- d[!is.na(d$disease), ]

carobiner::write_files(path, meta, d)

}


