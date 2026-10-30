# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Sensory attributes and consumer acceptance of sweetpotato and potato genotypes in East Africa

Sensory attributes and consumer acceptance of sweetpotato and potato genotypes in East Africa
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/IU4LC1"
	group <- "varieties_other"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = NA,
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "01_Sweetpotato_Kamwenge_data (Dataset_for_Sensory_attributes_and_consumer_accept...).xlsx"]
	f2 <- ff[basename(ff) == "02_Sweetpotato_Lira_data (Dataset_for_Sensory_attributes_and_consumer_accept...).xlsx"]
	f3 <- ff[basename(ff) == "03_Potato_Rakai_data (Dataset_for_Sensory_attributes_and_consumer_accept...).xlsx"]
	f4 <- ff[basename(ff) == "04_Potato_Kabale_data (Dataset_for_Sensory_attributes_and_consumer_accept...).xlsx"]
	f5 <- ff[basename(ff) == "05_Data_dictionary_sweetpotato (Dataset_for_Sensory_attributes_and_consumer_accept...).xlsx"]
	f6 <- ff[basename(ff) == "06_Data_dictionary_potato (Dataset_for_Sensory_attributes_and_consumer_accept...).xlsx"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- carobiner::read.excel(f3)
	r4 <- carobiner::read.excel(f4)
	r5a <- carobiner::read.excel(f5, sheet="Dictionary_kamwenge")
	r5b <- carobiner::read.excel(f5, sheet="Dictionary_Lira")
	r6 <- carobiner::read.excel(f6)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		location = r1[["Location/ Name of Village/Town/City...2"]],
		country = r1[["Country of residence"]],
		soil_texture = tolower(r1[["537 - Non uniform texture"]])
	)
##r1: "Consumers", "Location/ Name of Village/Town/City...3", "Gender", "Nationality", "Ethnicity", "Age", "Age range", "Occupation", "Education", "Marital status", "Wealth Status", "Consumption frequency", "Plain (Snack)", "Katogo (Added with beans/groundnuts and ingredients – spices etc)", "With other sauce served separately (greens, beans, groundnuts)", "Mashed (Mugoyo)", "First form", "Second form", "Third form", "Breakfast", "Lunch", "In between meals", "Dinner", "First occasion/period", "Second occasion/period", "Third occasion/period", "Kamwenge- Kiribwa mukwe (537)", "Color- Kiribwa mukwe (537)", "Sweetness- Kiribwa mukwe (537)", "Firmness- Kiribwa mukwe (537)", "Mealiness- Kiribwa mukwe (537)", "537 -Attractive", "537 - Bad after taste", "537 - Blackish", "537 - Dry", "537 - Fibrous", "537 - Firm", "537 - Good sweetpotato smell", "537 - Hard", "537 - Mealy", "537 - No smell", "537 - Non homogeneous colour", "537 - Not sweet enough", "537 - Orange", "537 - Smooth", "537 - Sticky between finger", "537 - Sweet", "537 - Tasteless", "537 - Thick", "537 - Too soft", "537 - Vitamins", "537 - Watery", "537 - White", "537 - Yellow", "Kamwenge- Naspot 8 (246)", "Color- Naspot 8 (246)", "Sweetness- Naspot 8 (246)", "Firmness- Naspot 8 (246)", "Mealiness- Naspot 8 (246)", "246 -Attractive", "246 - Bad after taste", "246 - Blackish", "246 - Dry", "246 - Fibrous", "246 - Firm", "246 - Good sweetpotato smell", "246 - Hard", "246 - Mealy", "246 - No smell", "246 - Non homogeneous colour", "246 - Non uniform texture", "246 - Not sweet enough", "246 - Orange", "246 - Smooth", "246 - Sticky between finger", "246 - Sweet", "246 - Tasteless", "246 - Thick", "246 - Too soft", "246 - Vitamins", "246 - Watery", "246 - White", "246 - Yellow", "Kamwenge- Nderera baana (387)", "Color- Nderera baana (387)", "Sweetness- Nderera baana (387)", "Firmness- Nderera baana (387)", "Mealiness- Nderera baana (387)", "387 -Attractive", "387 - Bad after taste", "387 - Blackish", "387 - Dry", "387 - Fibrous", "387 - Firm", "387 - Good sweetpotato smell", "387 - Hard", "387 - Mealy", "387 - No smell", "387 - Non homogeneous colour", "387 - Non uniform texture", "387 - Not sweet enough", "387 - Orange", "387 - Smooth", "387 - Sticky between finger", "387 - Sweet", "387 - Tasteless", "387 - Thick", "387 - Too soft", "387 - Vitamins", "387 - Watery", "387 - White", "387 - Yellow", "Kamwenge- Otandibata (912)", "Color- Otandibata (912)", "Sweetness- Otandibata (912)", "Firmness- Otandibata (912)", "Mealiness- Otandibata (912)", "912 -Attractive", "912 - Bad after taste", "912 - Blackish", "912 - Dry", "912 - Fibrous", "912 - Firm", "912 - Good sweetpotato smell", "912 - Hard", "912 - Mealy", "912 - No smell", "912 - Non homogeneous colour", "912 - Non uniform texture", "912 - Not sweet enough", "912 - Orange", "912 - Smooth", "912 - Sticky between finger", "912 - Sweet", "912 - Tasteless", "912 - Thick", "912 - Too soft", "912 - Vitamins", "912 - Watery", "912 - White", "912 - Yellow", "Looks as usual gari", "...146", "...147", "Most dislike gari", "Why?...149", "Most like gari", "Why?...151", "Comment"


	d2 <- data.frame(
		location = r2[["Location/ Name of Village/Town/City...2"]],
		country = r2[["Country of residence"]],
		soil_texture = tolower(r2[["760 - Non uniform texture"]])
	)
##r2: "Consumers", "Location/ Name of Village/Town/City...3", "Gender", "Nationality", "Ethnicity", "Age", "Age range", "Occupation", "Marital status", "Wealth Status", "Consumption frequency", "Plain (Snack)", "Katogo (Added with beans/groundnuts and ingredients – spices etc)", "With other sauce served separately (greens, beans, groundnuts)", "Mashed (Mugoyo)", "First form", "Second form", "Third form", "Fourth form", "Breakfast", "Lunch", "In between meals", "Dinner", "First occasion/period", "Second occasion/period", "Third occasion/period", "Arakaraka (760) - Overall liking", "Color- Araka Araka (760)", "Sweetness- Araka Araka (760)", "Firmness- Araka Araka (760)", "Mealiness- Araka Araka (760)", "760 -Attractive", "760 - Bad after taste", "760 -Blackish", "760 - Dry", "760 - Fibrous", "760 - Firm", "760 - Good sweetpotato smell", "760 - Hard", "760 - Mealy", "760 - No smell", "760 - Non homogeneous colour", "760 - Not sweet enough", "760 - Orange", "760 - Smooth", "760 - Sticky between finger", "760 - Sweet", "760 - Tasteless", "760 - Thick", "760 - Too soft", "760 - Vitamins", "760 - Watery", "760 - White", "760 - Yellow", "NASPOT 8 (699)-Overall liking", "Colour- NASPOT 8 (699)", "Sweetness- NASPOT 8 (699)", "Firmness- NASPOT 8 (699)", "Mealiness- NASPOT 8 (699)", "699 - Attractive", "699 - Bad after taste", "699 - Blackish", "699 - Dry", "699 - Fibrous", "699 - Firm", "699 - Good sweetpotato smell", "699 - Hard", "699 - Mealy", "699 - No smell", "699 - Non homogeneous colour", "699 - Non uniform texture", "699 - Not sweet enough", "699 - Orange", "699 - Smooth", "699 - Sticky between finger", "699 - Sweet", "699 - Tasteless", "699 - Thick", "699 - Too soft", "699 - Vitamins", "699 - Watery", "699 - White", "699 - Yellow", "Okonynedo (545) -Overall liking", "Colour- Okonynedo (545)", "Sweetness- Okonynedo (545)", "Firmness- Okonynedo (545)", "Mealiness- Okonynedo (545)", "545 - Attractive", "545 - Bad after taste", "545 - Blackish", "545 - Dry", "545 - Fibrous", "545 -Firm", "545 - Good sweetpotato smell", "545 - Hard", "545 - Mealy", "545 - No smell", "545 - Non homogeneous colour", "545 - Non uniform texture", "545 -Not sweet enough", "545 - Orange", "545 - Smooth", "545 - Sticky between finger", "545 - Sweet", "545 - Tasteless", "545 - Thick", "545 - Too soft", "545 - Vitamins", "545 - Watery", "545 - White", "545 - Yellow", "Otada (178)-Overall likng", "Colour- Otada (178)", "Sweetness- Otada (178)", "Firmness- Otada (178)", "Mealiness- Otada (178)", "178 - Attractive", "178 - Bad after taste", "178 - Blackish", "178 - Dry", "178 - Fibrous", "178 - Firm", "178 - Good sweetpotato smell", "178 - Hard", "178 - Mealy", "178 - No smell", "178 - Non homogeneous colour", "178 - Non uniform texture", "178 -Not sweet enough", "178 -Orange", "178 - Smooth", "178 - Sticky between finger", "178 - Sweet", "178 - Tasteless", "178 - Thick", "178 - Too soft", "178 - Vitamins", "178 - Watery", "178 - White", "178 - Yellow"


	d3 <- data.frame(
		location = r3[["Location/ Name of Village/Town/City...4"]],
		country = r3[["P3. Country of residence"]],
		year = r3[["P5. Age (years) / age range"]]
	)
##r3: "Respondent ID", "Interview date (DD-MM-YYYY)", "Start time (HH:MM:SS)", "Location/ Name of Village/Town/City...5", "Class", "P1. Respodent's gender", "P2. Nationality", "P2. Nationality (OTHER)", "P3. Country of residence (OTHER)", "P4. Ethnic group", "P4. Ethnic group (OTHER)", "Age group", "P6. Occupation (formal or informal)", "P7. Marital status", "P8. Wealth status", "P2.1 How often do you consume steamed/boiled Potato", "P2.2 How do you consume steamed/boiled Potato - FIRST", "P2.2 How do you consume steamed/boiled Potato - SECOND", "P2.2 How do you consume steamed/boiled Potato - THIRD", "P2.2 How do you consume steamed/boiled Potato - FOURTH", "P2.3 At what occasion do you consume the most frequent form - 1ST", "P2.3 At what occasion do you consume the most frequent form - 2ND", "P2.3 At what occasion do you consume the most frequent form - 3RD", "P2.3 At what occasion do you consume the most frequent form - 4TH", "P3.1 Please select a list of available Potato products to be tested", "P61. Which Potato looks more like the one you usually consume", "P6.2A. Which Potato do you dislike the most", "P6.2B Why do you dislike this Potato", "P6.3A Which Potato do you like the most", "P6.3B Why do you like this Potato", "Enumerator Comment", "End Time"


	d4 <- data.frame(
		location = r4[["Location/ Name of Village/Town/City...4"]],
		country = r4[["P3. Country of residence"]],
		year = r4[["P5. Age (years) / age range"]]
	)
##r4: "Respondent ID", "Interview date (DD-MM-YYYY)", "Start time (HH:MM:SS)", "Location/ Name of Village/Town/City...5", "Class", "P1. Respodent's gender", "P2. Nationality", "P2. Nationality (OTHER)", "P3. Country of residence (OTHER)", "P4. Ethnic group", "P4. Ethnic group (OTHER)", "Age range", "P6. Occupation (formal or informal)", "P7. Marital status", "P8. Wealth status", "P2.1 How often do you consume steamed/boiled Potato", "P2.2 How do you consume steamed/boiled Potato - FIRST", "P2.2 How do you consume steamed/boiled Potato - SECOND", "P2.2 How do you consume steamed/boiled Potato - THIRD", "P2.2 How do you consume steamed/boiled Potato - FOURTH", "P2.3 At what occasion do you consume the most frequent form - 1ST", "P2.3 At what occasion do you consume the most frequent form - 2ND", "P2.3 At what occasion do you consume the most frequent form - 3RD", "P2.3 At what occasion do you consume the most frequent form - 4TH", "P3.1 Please select a list of available Potato products to be tested", "P61. Which Potato looks more like the one you usually consume", "P6.2A. Which Potato do you dislike the most", "P6.2B Why do you dislike this Potato", "P6.3A Which Potato do you like the most", "P6.3B Why do you like this Potato", "Enumerator Comment", "End Time"


	d5a <- data.frame()
##r5a: "Sheet", "Variable_name", "Categories"


	d5b <- data.frame()
##r5b: "Sheet", "Variable_name", "Categories"


	d6 <- data.frame()
##r6: "Sheet", "Variable_name", "Categories", "...4"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

