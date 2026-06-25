# R script for "carob"

## ISSUES
# data has multiple columns for previous crops dating back to 5 seasons
# only the year prior to current was extracted. The other could be added like this 

carob_script <- function(path) {

"This study contains yield data of the two crops from tow representative sections of a field on plots 10m by 10m. Data taken on each of the plots included agronomic practices undertaken including plant spacing, pest and disease control, organic/inorganic fertilizer applications and field history according to a protocol implemented in AfSIS. Each field was geo-referenced"

	uri <- "doi:10.7910/DVN/X6QS6M"
	group <- "survey"

	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=0, 
		data_organization = "CIAT;SARI", 
		publication = NA, 
		project = NA, 
		data_type = "survey", 
		treatment_vars = "none", 
		response_vars = "none", 
		carob_contributor = "Mitchelle Njukuya", 
		carob_date = "2024-07-18",
		carob_effort = NA
	)
	
	f <- ff[basename(ff) == "001_agronomic-surveybabatiyr2013.csv"]
	r <- read.csv(f)
		
	d <- data.frame(
		country="Tanzania", 
		location=r$Village, 
		site=r$Sub_Village, 
		crop=tolower(r$TCrop), 
		variety = r$VarietyName,
		variety_type=r$VarietyName, 
		weeding_done=TRUE, 
		weeding_times=r$WeedingFreq, 
		weeding_implement=tolower(r$WeedMethod), 
		weed_severity=as.character(r$WeedRating), 
		weed_species=tolower(r$MainWeed), 
		intercrops=tolower(r$CropsInter),
		herbicide_used = r$Herbicides=="Yes",
		OM_used = r$ManType=="FYM",
		OM_type = ifelse(r$ManType=="FYM", "farmyard manure", "none"),
		OM_amount = r$ManQty,
		previous_crop = tolower(r$S1crops),
		ear_rot = r$PercRot,
		plant_density = r$Density,
		fwy_residue = r$StoverYld,
		yield = r$GrainYld * 1000,
		landscape_position = r$Position,
		house_distance = r$Dist, 
		trial_id = as.character(r$Code),
		planting_date = as.character(as.Date("2012-11-01") + r$PlantingDelay),
		fertilizer_used = r$S1Fert == 1
	)

	d$weed_species[d$weed_species == ""] <- NA
	d$weeding_implement[d$weeding_implement == "hand hoe"] <- "hoe"
	
	d$on_farm <- FALSE
	d$is_survey <- TRUE
	d$irrigated <- FALSE
	d$yield_part <- "grain"

	sort_with_semicol <- function(x) {
		s <- sapply(strsplit(x, ";"), \(i) paste0(trimws(sort(i)), collapse=";"))
		s[s==""] <- NA
		s
	}
	
	d$previous_crop <- gsub(", |,", ";", d$previous_crop)
	d$previous_crop <- gsub("bean|beans", "common bean", d$previous_crop)
	d$previous_crop <- gsub("mize;|mai;", "maize;", d$previous_crop)
	d$previous_crop <- gsub("irish potato", "potato", d$previous_crop)
	d$previous_crop <- gsub("0", "none", d$previous_crop) # or NA?
	d$previous_crop <- gsub("pigeonpea", "pigeon pea", d$previous_crop)
 	d$previous_crop <- sort_with_semicol(d$previous_crop)
	
	d$intercrops <- gsub("beans", "common bean", d$intercrops)
	d$intercrops <- gsub("pigeonpea|peigeon pea", "pigeon pea", d$intercrops)
	d$intercrops <- gsub("irish potato", "potato", d$intercrops)
	d$intercrops <- gsub(", |,| and ", ";", d$intercrops)
	d$intercrops <- gsub("nil", "none", d$intercrops) # or NA?
	d$intercrops <- gsub("pigeon pea sunflower", "pigeon pea;sunflower", d$intercrops)
	d$intercrops <- sort_with_semicol(d$intercrops)

	# source?
	geodata <-data.frame(
		site = c("Bashnet", "Long", "Endaw", "Sabilo", "Dactara B", "Haysam B", "Shamna", "Getalongo",
				"Gidmu B", "Bermi B", "Munmunang", "Bermi A", "Maganjwa", "Dabil", "Maganjwa B", 
	             "Maganjwa A", "Backchan", "Hayeda", "Semark B", "Magwanjwa B", 
	             "Loto A", "Dactara A", "Magwanjwa", "Lomuhong Pri School", "Daktara B"), 
		longitude = c(35.4272, 35.4, 35.468, 35.4766, 35.4735, 35.4735, 35.4735, 35.4735, 
					35.4735, 35.5182, 35.4735, 35.5182, 35.4754, 35.4921, 35.4754, 35.4754, 35.4735, 
					34.99, 35.4735, 35.4735, 35.4933, 35.4735, 35.4735, 35.4735, 35.4735), 
		latitude = c(-4.2298, -4.2, -4.2061, -4.3458, -4.2402, -4.2402, -4.2402, -4.2402, 
					-4.2402, -4.2332, -4.2402, -4.2332, -4.2735, -4.2448, -4.2735, -4.2735, -4.2402, 
					-4.2683, -4.2402, -4.2402, -4.2454, -4.2402, -4.2402, -4.2402, -4.2402),
		geo_from_source = FALSE
	)
	d <- merge(d, geodata, by="site", all.x = TRUE)
	
	d$N_fertilizer <- d$P_fertilizer <- d$K_fertilizer <- as.numeric(NA)
	
	carobiner::write_files(path, meta, d)
}


