# R script for "carob"

carob_script <- function(path) {

"The dataset is meant for developing fertilizer management decision support tool for an effective crop-nutrient management. The dataset is developed on the basis of landscape targeting on-farm trials on crop-nutrient response and crop yield gap assessment across the Africa Rising target districts and other scaling up locations in the Ethiopian highlands."

	uri <- "doi:10.7910/DVN/ZXH0R8"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group, recursive=TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		publication="doi:10.1017/S1742170519000504",
		data_organization = "ICRISAT; ARARI; GIZ; ILRI",
		carob_contributor="Siyabusa Mkuhlani; Eduardo Garcia Bendito",
		carob_date="2022-02-16",
		data_type="on-farm experiment",
		project="Africa Rising",	   
		response_vars = "yield",
		treatment_vars = "N_fertilizer;P_fertilizer;K_fertilizer;Zn_fertilizer;S_fertilizer"
 	)

	get_rep <- function(d) {
		drep <- rep(NA, nrow(d))
		drep[1] <- 1
		for (r in 2:nrow(d)) { 
			if (d$Treatment[r] == d$Treatment[r-1]) {
			# If the treatment is the same name, it is a replicate...
				drep[r] <- drep[r-1] + 1 
			} else {
			# a new treatment, therefore assign the 1st replicate...
				drep[r] <- 1 
			}
		}
		as.integer(drep)
	}

	get_df <- function(d, trialname) {
		x <- data.frame(
			country = d$Country,
			adm1= d$Region.state,
			adm3 = d$LGA.District,
			planting_date = as.character(as.Date(d$Planting.date)),
			harvest_date = as.character(as.Date(d$Harvest.date)),
			on_farm = TRUE,
			is_survey = FALSE,
			treatment = d$Treatment,
			crop = tolower(d$Crop),
			variety = d$Variety,
			previous_crop = tolower(d$Crop.system),
			rep = get_rep(d),
			soil_type = d$Soil.type,
			#There were two N_splits: Basal (50%) and top dressing (50%)
			N_splits = 2L,
			# Yield data is reported as dry weight 
			yield = d$Yield..kg.ha. / 0.87, 
			 # Residue data is measured as dry weight
			fwy_residue = d$Stover.yield..kg.ha,
			
			fertilizer_type = 
				ifelse(d$Treatment == "0.33NP/farmer practice" | d$Treatment == "NP", "urea;DAP",
				ifelse(d$Treatment == "NPK", "urea;DAP;KNO",
				ifelse(d$Treatment == "NPKS", "urea;DAP;SOP", "urea;DAP;SOP;ZnSO4")))
		)
		# these are not present in all files
		x$N_fertilizer <- d$N.fertilizer.amount..kg.ha.
		x$P_fertilizer <- d$P.fertilizer.amount..kg.ha.
		x$K_fertilizer <- d$K.fertilizer.amount..kg.ha.
		x$Zn_fertilizer <- d$Zn.fertilizer.amount..kg.ha.
		x$S_fertilizer <- d$S.fertilizer.amount..kg.ha.
		x$emergence_days <- d$Days.to.emergency
		x$heading_days <- d$Days.to.heading
		x$flowering_days <- d$Days.to.flowering
		x$maturity_days <- d$Days.to.maturity		
		x$trial_id <- paste0(trialname, x$location)
		if (is.null(d$village.Kebele)) {
			x$location <- d$village
		} else {
			x$location <- d$village.Kebele
		}

	# x$biomass <- d$Biomass..kg.ha.  # Biomass is collected as fresh biomass
	###x$observation_date <- d$N.topdressing.date # Date of the application of the 2nd split
		x
	}

	read.file <- function(f) {
		d <- carobiner::read.excel(f)
		# standardize names 
		d <- data.frame(d)
		# order to determine the rep
		d[order(d$Treatment), ]
	}


## process 001_2014-2015_Wheat_ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "001_2014-2015_Wheat_ICRISAT-AR_ETH.xlsx"] 
	d <- read.file(f)
	d2 <- get_df(d, '2014-2015_Wheat_')

	# different names than in the other files
	d2$N_fertilizer <- d$N.fertilizer.amount...19
	d2$P_fertilizer <- ifelse(is.na(d$P.fertilizer.amount), 0, d$P.fertilizer.amount) 
	d2$K_fertilizer <- ifelse(is.na(d$K.fertilizer.amount..K2O.kg.ha..), 0, d$K.fertilizer.amount..K2O.kg.ha..)
	d2$Zn_fertilizer <- ifelse(is.na(d$Zn.fertilizer.amount..Zn.kg.ha..), 0, d$Zn.fertilizer.amount..Zn.kg.ha..)
	d2$S_fertilizer <- ifelse(is.na(d$S.fertilizer.amount..S.kg.ha..), 0, d$S.fertilizer.amount..S.kg.ha..)
	## remove bizarre high values
	d2$fwy_residue[d2$fwy_residue > 25000] <- NA
	
## process 002_2016_Wheat_ ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "002_2016_Wheat_ ICRISAT-AR_ETH.xlsx"]
	d <- read.file(f)
	d3 <- get_df(d, "2016_Wheat_")
	## weird (including many negative) values
	d3$fwy_residue <- NA
	
## process 003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "003_2017_Sorghum+Tef_ ICRISAT-AR_ETH.xlsx"]
	d <- read.file(f)
	d4 <- get_df(d, "2017_Sorghum-Tef_")
	d4$planting_date[is.na(d4$planting_date)] <- "2017"
	
## process 004_2019_Wheat_ ICRISAT-AR_ETH.xlsx
	f <- ff[basename(ff) == "004_2019_Wheat_ ICRISAT-AR_ETH.xlsx"]
	d <- read.file(f)
	d5 <- get_df(d, "2019_Wheat_")	
	d5$planting_date[is.na(d5$planting_date)] <- "2019"
	## many values that are much lower than yield
	d5$fwy_residue <- NA
	
## Append the data.frames
	d <- carobiner::bindr(d2, d3, d4, d5)
	
	
	d$yield_part <- "grain"

	d$crop <- gsub("tef", "teff", d$crop)
	d$previous_crop <- gsub("tef", "teff", d$previous_crop)
	d$previous_crop <- gsub("fieldpea", "pea", d$previous_crop)
	d$previous_crop <- gsub(",", ";", d$previous_crop)
	d$previous_crop <- gsub("fababean", "faba bean", d$previous_crop)
	
	d$P_fertilizer[is.na(d$P_fertilizer)] <- 0
	d$K_fertilizer[is.na(d$K_fertilizer)] <- 0
	d$Zn_fertilizer[is.na(d$Zn_fertilizer)] <- 0
	d$S_fertilizer[is.na(d$S_fertilizer)] <- 0

	d$location[d$location == "Goshebado"] <- "Goshe Bado"
	geo <- data.frame(
		location  = c("Goshe Bado", "Gudo Beret", "Hayk", "Kabe", "Lemo", "Selka", "Sinana", "Sirinka", "Tsibet"), 
		longitude = c(39.446, 39.682, 39.684, 39.46, 37.851, 40.289, 40.215, 39.607, 39.482), 
		latitude  = c(9.74, 9.799, 11.316, 10.826, 5.436, 6.857, 7.068, 11.748, 12.86),
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by="location", all.x=TRUE)

## RH becaus of all the error in residue yield it may not be reliable at all
## so I remove it from all records
	d$fwy_residue <- NULL

	d$irrigated <- NA
	
	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

