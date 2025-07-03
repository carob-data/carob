# R script for "carob"

## ISSUES
#1. Unspecified districts and subdisticts --> EGB: added some spatial information (2024-02-28)
# 1.1. One of the districts and sub-districts is not reported in the paper, where there are only 4 sites. Information on farmers not available.
# 1.2 The metadata (MetaSheet.csv) has the emergence date is not in %m/%d/%y but instead in %d/%m/%y.


carob_script <- function(path) {

"Working with 64 farmers in eight production environments, we examined yield response to three genotypes, BG25 and BG27 (with salinity - and heat - tolerant traits) and BG21 (local check), across a gradient of sowing dates, grouped as 'early' (sown before 15 December) and 'late' (after 15 December), under 0, 100 and 133 and 0, 67 and 100 kg N ha-1 for early- and late-sowing groups, respectively. 

Across environments and genotypes, yield ranged from 2.11 to 4.77 t ha-1(mean: 3.9 t ha-1) under early-sowing, and from 0.83 to 4.27 t ha-1(mean: 2.74 t ha-1) under late-sowing. Wheat performance varied with environment (1.68 - 4.77 t ha-1 at 100 kg N ha-1across sowing groups); the lowest yields found where early sowing was delayed and soil salinity levels were elevated. Small but significant (P less than 0.001) yield differences (0.22 t ha-1) were found between 100 and 133 kg N ha-1 for the early-sowing group, though no difference was found between 67and 100 kg N ha-1 for late-sowing. Combining early- and late-sowing groups, significant environment x N rate and sowing-group x N rate interactions (both P less than 0.001) for 100 kg N ha-1 indicated the importance of site-and time-specific N management in these stress-prone environments.

Considering all cultivars and environments, ECa at sowing, flowering and grain filling negatively correlated with yield (r = - 0.50, - 0.59 and - 0.54, all P less than 0.001). Correlations with ground water depth at flowering and grain filling were negative and significant, but less pronounced in the context of farmer-managed irrigation scheduling. Despite putative stress-tolerance traits in two of the three entries, no genotypic yield differences were found under early-sowing, though s mall differences (less than 0.19 t ha-1) were observed with late sowing. Agronomic fertilizer-N efficiency (AE-N) was consistently higher for 100 than 133 and 67 than 100 kg N ha-1 for early- and late-sowing. The marginal economic value of N application followed similar trends, indicating that rates of at most 100 and 67 kg N ha-1 are favorable for sowing before or after December 15th.]"

	uri <- "hdl:11529/11084"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		project = "CSISA",
		publication = "doi:10.1016/j.fcr.2014.09.019",
		data_organization = "CIMMYT; UMELB",
		data_type = "experiment", 
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2023-12-18",
		treatment_vars = "planting_date;variety;N_fertilizer",
		response_vars="yield",
		modified_by = "Eduardo Garcia Bendito",
		last_modified = "2024-02-28"
	)
	
	f <- ff[basename(ff) == "WheatGenoTrial.csv"]
	r <- read.csv(f, fileEncoding = "latin1")
	
	d <- data.frame(
		season = r$SEASON,
		N_fertilizer = r$Nitrogen_.kg.ha.,
		variety = r$GENOTYPE,
		on_farm = TRUE,
		irrigated = TRUE,
		irrigation_number = 1,
		irrigation_amount = 50,
		country = "Bangladesh",
		adm1 = r$DISTRICT_NAME,
		adm2 = r$UPAZILLA_NAME,
		crop = "wheat",
		yield_part = "grain",
		row_spacing = 20,
		is_survey=FALSE
	)
	
	d$trial_id <- as.character(as.integer(as.factor(paste(r$UPAZILLA_NAME, r$FARMER))))
	dos <- ifelse(r$DATE_OF_SOWING == ".", NA, r$DATE_OF_SOWING)
	d$planting_date <- as.character(as.Date(dos, "%m/%d/%y"))
	har <- ifelse(r$Harvest_Date_.Month.Day.Year == ".", NA, r$Harvest_Date_.Month.Day.Year)
	d$harvest_date <- as.character(as.Date(har, "%d/%m/%y"))
	emer <- ifelse(r$DATE_OF_80._EMERGENCE == ".", NA, r$DATE_OF_80._EMERGENCE)
	d$emergence_date <- as.character(as.Date(emer, "%d/%m/%y"))
	d$flowering_date <- as.character(as.Date(r$FLOWERING_DATE, "%d/%m/%y"))
	d$maturity_date <- as.character(as.Date(r$X.90._Maturity_Date_.Month.Day.Year., "%m/%d/%y"))

	w1 <- as.character(as.Date(r$WEEDING_DATE_1, "%d/%m/%y"))
	w2 <- as.character(as.Date(r$WEEDING_DATE_2, "%d/%m/%y"))
	w <- paste0(w1, ";", w2)
	d$weeding_dates <- gsub(";NA", "", w)
	d$weeding_dates[d$weeding_dates=="NA" ] <- NA

	ird <- as.matrix(r[, paste0("IRRIGATION_DATE_", 1:4)])
	ird[ird=="."] <- NA
	ird[ird=="00/01/00"] <- NA
	ird <- matrix(as.character(as.Date(as.vector(ird), "%d/%m/%y")), nrow=nrow(ird))
	d$irrigation_dates <- apply(ird, 1, \(i) paste(na.omit(i), collapse="; "))
	d$irrigation_dates[d$irrigation_dates == ""] <- NA
	d$irrigation_number <- as.integer(rowSums(!is.na(ird)))

	d$fertilizer_type <- ifelse(!is.na(d$N_fertilizer), "urea;DAP;TSP;KCl;borax", "TSP;KCl;borax;gypsum")
	d$N_splits <- as.integer(NA)
	d$N_splits[d$N_fertilizer < 100] <- as.integer(2)
	d$K_fertilizer <- 50
	d$P_fertilizer <- 24
	d$B_fertilizer <- 1
	d$S_fertilizer <- 100
	d$gypsum <- d$S_fertilizer/(0.19) # Amount of gypsum as per values_fertilizer_type.csv
	d$weeding_times <- as.integer(2) # As per publication
	
	#fixing data types to extract soil_EC
	samples <- paste0("SOIL_EC_.DS.M._._SAMPLE_", 1:8)
	s <- as.matrix(r[, samples])
	s[s=="."] <- NA
	s <- gsub(",|\\.\\.", ".", s)
	s <- gsub("^\\.|\\.$", "", s)
	s <- matrix(as.numeric(s), nrow=nrow(s))
	d$soil_EC <- rowMeans(s, na.rm=TRUE)
	
  ### Yield #####
	dot_numeric <- function(x) {
		x[x=="."] <- NA
		x[x=="#VALUE!"] <- NA
		as.numeric(x)
	}

	d$seed_weight <- dot_numeric(r$X100_grain_weight_after_oven_drying_.g.) * 10
	d$dmy_residue <- dot_numeric(r$Straw_yield_moisture_adjusted_.T.ha.) * 1000
	d$yield <- dot_numeric(r$Grain_yield_moisture_adjusted_.T.ha.) * 1000

	# EGB:
	# compare average grain weight to table 7 of doi:10.1016/j.fcr.2014.09.019
	# aggregate(d$seed_weight, c("UPAZILLA_NAME", "YEAR"), mean, na.rm=TRUE) 

	
	# # EGB:
	# # Based on above table, subdistrict 1 = Dumuria; subdistrict 2 = Fultala; subdistrict 3 = Kaligonj; subdistrict 4 = Sadar
	d$adm1 <- NA
	d$adm1[r$DISTRICT_NAME == "DISTRICT 1"] <- "Kuhlna"
	d$adm1[r$DISTRICT_NAME == "DISTRICT 2"] <- "Satkhira"
	d$adm2 <- NA
	d$adm2[r$UPAZILLA_NAME == "SUBDISTRICT 1"] <- "Dumuria"
	d$adm2[r$UPAZILLA_NAME == "SUBDISTRICT 2"] <- "Fultala"
	d$adm2[r$UPAZILLA_NAME == "SUBDISTRICT 3"] <- "Kaligonj"
	d$adm2[r$UPAZILLA_NAME == "SUBDISTRICT 4"] <- "Sadar"
##	d$adm2[r$UPAZILLA_NAME == "SUBDISTRICT 5"] <- 
	

# https://en.wikipedia.org/wiki/Phultala_Upazila
# https://en.wikipedia.org/wiki/Dumuria_Upazila
# https://en.wikipedia.org/wiki/Satkhira_Sadar_Upazila
# https://en.wikipedia.org/wiki/Kaliganj_Upazila,_Satkhira	
	geo <- data.frame(
		adm2 = c("Fultala", "Dumuria", "Sadar", "Kaligonj"),
		longitude = c(89.458, 89.425, 89.075, 89.0417),
		latitude = c(22.975, 22.808, 22.717, 22.45),
		geo_from_source = FALSE
	)

	d <- merge(d, geo, by="adm2", all.x=TRUE)

	d$season <- "rabi"
	d <- d[!is.na(d$planting_date), ]

	carobiner::write_files(meta, d, path=path)
}
