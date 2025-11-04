# R script for "carob"

carob_script <- function(path) {

"Human Zn and Fe deficiencies can be reduced through agronomic biofortification, but information on factors influencing maize grain-Zn and -Fe levels remains scant. This analysis: (1) Establishes the global distribution of Zn and Fe concentrations in maize grain; (2) Assesses different agronomic practices’ contributions to increasing maize-grain Zn and Fe levels; and (3) Identifies key biophysical factors to guide agronomic biofortification. Using 1,332 data points in 102 published papers from 24 countries, we estimated a 24% probability of grain-Zn concentrations exceeding the benchmark target of 38 mg kg−1."

	uri <- "doi:10.7910/DVN/SOAWL6"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		project=NA,
		publication= NA,
		data_organization = "CIAT",
		data_type="compilation",
		response_vars= "yield",
		treatment_vars="none",
		carob_contributor="Fredy Chimire",
		carob_date="2024-02-24"
	)

	f <- ff[basename(ff) == "02a. Maize data for meta_analysis_updated.txt"]
	r <- read.table(f, header = TRUE, sep = "\t", quote = "", fill = TRUE, na.strings = c("", "NA"))

# Avail_P, Avail_P_Mehlich.3, AveP.Znratio, AveP_method, Olsen.P.avail, OLsenP.class, 
# Fe.application..Soil..Foliar., Fe.Zn.ratio, Fe_applied..Yes..No..,
# Innoculation, Inoculation, MGF_1_Description_Trt,
# Parent.material, pH_CaCl2, PH_Method, pHClass, Phenols.ug.g._Trt, Phyticphosphorus.mg.g._Trt, 
# SOCclass, Treat_code, Treatment.corrected,
# X.If.yes..form.of.Zn.application..Soil..Foliar..,
# Zn_applied_.Kg.ha., Zn_method


	d <- data.frame(
		country=r$Country,
		planting_date=r$Yr_experiment,
		soil_pH_CaCl2=r$pH_CaCl2,
		location=r$TrialSite,
		N_fertilizer=r$N_Applied_Trt,
		K_fertilizer=r$K_Applied_Trt,
		P_fertilizer=r$P_Applied_Trt,
		Zn_fertilizer=r$Zn_Applied_Trt,
		Fe_fertilizer=r$Fe_Applied_Trt,

		OM_type = tolower(r$Organic_matter_type),
		OM_amount = r$FYM_Trt,
		OM_used = !is.na(r$FYM_Trt) | (tolower(r$Organic_matter_added) == "yes"),

		irrigated= tolower(r$Irrigation_Trt) == "yes",
		yield = as.numeric(r$GrainYld_Trt) * 1000,
		soil_clay=as.numeric(r$Clay),
		soil_sand=as.numeric(r$Sand),
		soil_silt=as.numeric(r$Silt),
		soil_SOC=r$SOC....,
		soil_SOM=r$SOM,
		soil_pH=r$pH,
		soil_K = r$Soil_K,
		soil_Fe = r$Soil_Fe,
		soil_Zn = r$Soil_Zn,
		soil_texture = gsub("_", " ", tolower(r$Texture)),
		soil_type = r$Soil.type_New,
		rain = r$SeasonalRainfall,
		grain_Zn = r$GrainZn_Trt,
		grain_Fe = r$GrainFe.mgkg.1._Trt,
		grain_N = r$GrainN_Trt,
		grain_protein = r$Proteins_Trt,
		
		yield_cnt = r$GrainYld_Cnt*1000, 
		N_fertilizer_cnt = r$N_Applied_Cnt,
		P_fertilizer_cnt = r$P_Applied_Cnt, 
		K_fertilizer_cnt = r$K_Applied_Cnt, 
		Fe_fertilizer_cnt = r$Fe_Applied_Cnt, 
		Zn_fertilizer_cnt = r$Zn_Applied_Cnt, 
		grain_Zn_cnt = r$GrainZn_Cnt, 
		grain_Fe_cnt = r$GrainFe.mgkg.1._Cnt,
		grain_P_cnt = r$InorgP_Cnt
	)

	d$grain_P = rowMeans(r[, c("InorgP_Trt", "InorgP_Trt.1")], na.rm=TRUE)

	d$trial_id <- as.character(as.integer(as.factor(paste(d$country, d$location))))
	d$land_prep_method <- c("conventional", "none", "reduced tillage", "unknown")[r$Tillage]
	d$soil_texture[d$soil_texture == "silty"] <- "silt"
	d$soil_type[d$soil_type=="20.5"] <- NA

	ref <- iconv(r$Study, "UTF-8", "UTF-8")
	d$reference <- trimws(gsub("\"", "", ref))

	d$location <- iconv(d$location, "UTF-8", "UTF-8")
	d$location <- trimws(gsub("\"", "", d$location))

	d$country[grep("\\+|Mn", d$country)] <- NA
	d$country <- gsub("USA", "United States", d$country)

	get_geo <- function(x) {
		x <- iconv(x, "latin1", "UTF-8")
		x <- gsub('\"', "", x)
		y1 <- stringr::str_split_fixed(x, "°|\\?", 2)
		y2 <- stringr::str_split_fixed(y1[,2], "'|\u0092|\u0091|\\?", 2)
		southwest <- grepl("S|W", y2[,2], ignore.case=TRUE) | grepl("S|W", y2[,1], ignore.case=TRUE)
		y2[,1] <- gsub("N|S|E|W", "", y2[,1])
		y2[,2] <- gsub("''|N|S|E|W|\\?", "", y2[,2])
		minutes <- as.numeric(y2[,1]) / 60
		seconds <- as.numeric(y2[,2]) / 3600
		out <- rowSums(cbind(as.numeric(y1[,1]), minutes, seconds), na.rm=TRUE)
		out[southwest] <- -out[southwest]
		round(out, 3)
	}
	d$longitude <- get_geo(r$Longitude)
	d$latitude <- get_geo(r$Latitude)
	d$geo_from_source <- TRUE

	d$crop <- "maize"
	d$yield_part <- "grain"
	d$planting_date <- gsub("[^0-9]", "", d$planting_date)
	d$planting_date[nchar(d$planting_date) != 4] <- NA
	d$planting_date <- d$planting_date

	d$OM_amount[d$OM_amount == "Yes"] <- NA
	d$OM_amount <- as.numeric(d$OM_amount)
	d$OM_type <- carobiner::replace_values(d$OM_type, 
				c("fym", "organic fertiliser"), 
				c("farmyard manure", "unknown"))
	
	d <- d[!is.na(d$country), ]
	d <- d[!is.na(d$yield), ]

	i <- d$latitude > 70
	swp <- d$latitude[i]
	d$latitude[i] <- d$longitude[i]
	d$longitude[i] <- swp

	i <- d$longitude > 0 & d$country == "United States"
	d$longitude[i] <- -d$longitude[i]

	i <- d$latitude > 0 & d$country == "Zimbabwe"
	d$latitude[i] <- -d$latitude[i]

	i <- d$latitude == 0 & d$longitude == 0
	d$latitude[i] <- d$longitude[i] <- NA

	i <- d$latitude == -17.723 & d$country == "Kenya"
	d$latitude[i] <- d$longitude[i] <- NA

	d$latitude[d$location == "Ismailia Governorate"] <- 30.695
	d$latitude[d$location == "Nyankpala"] <- 9.421
	d$latitude[d$location == "Palampur"] <- 32.1
	d$country[d$location %in% c("Gabura", "Tulatuli")] <- "Bangladesh"

	dd <- d[!is.na(d$yield_cnt), ] 
	cnms <- grep("_cnt$", names(d), value=TRUE)
	nms <- gsub("_cnt$", "", cnms)
	dd <- dd[, !(names(dd) %in% nms)]
	names(dd) <- gsub("_cnt$", "", names(dd))
	d <- d[, !(names(d) %in% cnms)]

	d <- rbind(d, dd)

	d$N_fertilizer[tolower(d$N_fertilizer) == "yes"] <- NA
	d$N_fertilizer <- as.numeric(d$N_fertilizer)

	d$P_fertilizer[tolower(d$P_fertilizer) %in% c("yes", "ssp")] <- NA
	d$P_fertilizer <- as.numeric(d$P_fertilizer)

	d$K_fertilizer[tolower(d$K_fertilizer) == "yes"] <- NA
	i <- d$K_fertilizer == "Yes (Foliar at 0.1%)"
	d$K_fertilizer[i] <- NA ## for now
	d$K_fertilizer <- as.numeric(d$K_fertilizer)

	d$Zn_fertilizer[tolower(d$Zn_fertilizer) == "yes"] <- NA
	d$Zn_fertilizer <- as.numeric(d$Zn_fertilizer)

	d$Fe_fertilizer[tolower(d$Fe_fertilizer) == "yes"] <- NA
	d$Fe_fertilizer <- as.numeric(d$Fe_fertilizer)
	
	
	d$on_farm <- NA
	d$is_survey <- FALSE

	d$yield_moisture <- as.numeric(NA) #needs to be checked

	carobiner::write_files(meta, d, path=path)
}

