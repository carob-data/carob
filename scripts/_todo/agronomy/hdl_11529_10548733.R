# R script for "carob"
# license: GPL (>=3)

## NOTES
# Literature compilation of soil and crop management studies (Jat et al. 2022, Crop and Environment). The Original sheet is one row per paired observation.
# Grain yield is not in the workbook. Methane, nitrous oxide, carbon dioxide, and global-warming columns are not stored: Carob has no variables for them.
# Country, adm1, and coordinates are inferred from the research station names where missing or unparseable in the source sheet (geo_from_source = FALSE for imputed coordinates).
# Coordinates are degree strings in the sheet. Values that are not degrees, minutes, and seconds, and minutes or seconds of 60 or more, are replaced by station coordinates where known.
# "Convetional tillage" is stored as conventional land preparation. Castor is stored as castor bean.
# Where the management cell is empty, the broad class is used as the treatment. FYM is not a fertilizer-type name, so those rows have no fertilizer type.
# Multi-crop labels and the codes M-W-MB, M-CP-Ses, M-Mu-MB, and M-M-Ses are not decoded as a crop.

## ISSUES
# Grain yield is not in the workbook.
# Exact duplicate rows in the Original sheet are removed with unique(d).

## NOT PROCESSED
# References, replication, rainfall, and the gas-flux, GWP, and GHGI columns.
# The PDF review.
# Paired, Final, and summary sheets. They repeat the Original sheet.


carob_script <- function(path) {

"
Carbon sequestration potential, challenges, and strategies towards climate action in smallholder agricultural systems of South Asia

South Asia is a global hotspot for climate change with enormous pressure on land and water resources for feeding the burgeoning population. The agricultural production systems are highly vulnerable in the region and is primarily dominated by small and marginal farmers with intensive farming practices that had favored the loss of C from soil. This review discusses the potential of soil and crop management practices such as minimum/reduced/ no-tillage, use of organic manure, balanced and integrated plant nutrient application, precision land levelling, precision.
"

	uri <- "hdl:11529/10548733"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = "doi:10.1016/j.crope.2022.03.005",
		project = NA,
		design = NA,
		data_type = "compilation",
		treatment_vars = "treatment",
		response_vars = "none",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 30,
		carob_effort = 0.4
	)

	f <- ff[basename(ff) == "Carbon sequestration data.xlsx"]
	r <- carobiner::read.excel(f, sheet="Original", na=c("", "NA"))
	r <- r[!is.na(r$Crop), ]

	crop_raw <- trimws(as.character(r$Crop))
	crop <- rep(NA_character_, length(crop_raw))
	crop[grepl("^Rice", crop_raw)] <- "rice"
	crop[crop_raw == "Wheat"] <- "wheat"
	crop[crop_raw == "Maize"] <- "maize"
	crop[crop_raw == "Cowpea"] <- "cowpea"
	crop[crop_raw == "Soybean"] <- "soybean"
	crop[crop_raw == "Pigeon pea"] <- "pigeon pea"
	crop[crop_raw == "Castor"] <- "castor bean"
	crop[crop_raw == "Rice-fellow"] <- "rice"

	rotation <- rep(NA_character_, length(crop_raw))
	rotation[crop_raw == "Rice-wheat"] <- "rice;wheat"
	rotation[crop_raw == "Maize-wheat"] <- "maize;wheat"
	rotation[crop_raw == "Rice-fellow"] <- "rice"

	loc <- trimws(gsub("[\r\n]", "", as.character(r$`Location(Regional/Global)`)))
	country <- rep(NA_character_, length(loc))
	country[grepl("India", loc)] <- "India"
	adm1 <- rep(NA_character_, length(loc))
	adm1[grepl("Haryana", loc)] <- "Haryana"
	adm1[grepl("Odisha", loc)] <- "Odisha"

	practice <- trimws(as.character(r$`Management practices`))
	practice[practice %in% c("", "NA")] <- NA_character_
	cls <- trimws(as.character(r$`Broad classes`))
	cls[cls %in% c("", "NA")] <- NA_character_
	cls[cls == "Cotnrol"] <- "Control"
	cls[cls == "Fertlizer"] <- "Fertilizer"
	practice[is.na(practice)] <- cls[is.na(practice)]
	low <- tolower(practice)
	land <- rep(NA_character_, length(low))
	land[grepl("convetional tillage|conventional tillage", low)] <- "conventional"
	land[grepl("minimum tillage", low)] <- "minimum tillage"
	land[grepl("reduced tillage|reduced till", low)] <- "reduced tillage"
	land[grepl("no-till|no till|zero till", low)] <- "none"

	n_fert <- as.numeric(r$`Fertilizer N (kg/ha)`)
	fert_type <- rep(NA_character_, length(n_fert))
	fert_type[!is.na(n_fert) & n_fert == 0] <- "none"

	dms <- function(x, limit) {
		x <- gsub("[\r\n]", "", as.character(x))
		sign <- rep(1, length(x))
		sign[grepl("S|W", x)] <- -1
		x <- gsub("◦|°|o", " ", x)
		x <- gsub("′|’|ʹ|'", " ", x)
		x <- gsub("″|ʹʹ|\"", " ", x)
		x <- gsub("[NSEW,]", " ", x)
		x <- trimws(gsub(" +", " ", x))
		parts <- strsplit(x, " ")
		out <- vapply(parts, function(z) {
			z <- z[nzchar(z)]
			if (length(z) < 1 || length(z) > 3) return(NA_real_)
			kept <- gsub("[^0-9.]", "", z)
			if (any(kept != z | !grepl("[0-9]", z) | grepl("\\..*\\.", z))) return(NA_real_)
			nums <- as.numeric(z)
			if (any(is.na(nums))) return(NA_real_)
			if (length(nums) > 1 && (nums[2] >= 60 || (length(nums) == 3 && nums[3] >= 60))) return(NA_real_)
			val <- nums[1]
			if (length(nums) > 1) val <- val + nums[2] / 60
			if (length(nums) == 3) val <- val + nums[3] / 3600
			if (val > limit) return(NA_real_)
			val
		}, numeric(1))
		out * sign
	}

	per <- as.character(r$`Period of observations`)
	has_year <- grepl("[0-9]{4}", per)
	year <- rep(NA_character_, length(per))
	year[has_year] <- sub("^.*?([0-9]{4}).*$", "\\1", per[has_year])

	irr <- rep(NA, length(crop_raw))
	irr[grepl("Irrigated", crop_raw)] <- TRUE
	irr[grepl("Rainfed", crop_raw)] <- FALSE

	texture <- tolower(trimws(as.character(r$`Soil texture`)))
	texture[texture %in% c("", "na")] <- NA_character_
	texture[texture == "silt loam"] <- "silty loam"
	texture[texture == "silt clay loam"] <- "silty clay loam"

	lat <- dms(r$Lat, 90)
	lon <- dms(r$Long, 180)
	geo_src <- !is.na(lat) & !is.na(lon)
	# Station-level country, adm1, and fallback coordinates for missing/unparseable entries.
	st <- data.frame(
		pat = c("cuttack|nrri|crri", "aduthurai|thanjavur|trri", "new delhi|iari", "ludhiana|pau",
			"bhopal|iiss", "karnal|cssri", "gazipur|brri", "mymensingh|bau", "pantnagar",
			"modipuram|meerut", "varanasi", "patna", "hyderabad|icrisat", "hisar",
			"kalyani|mohanpur", "raipur", "faisalabad", "bhairahawa|rampur|chitwan",
			"pusa|samastipur", "bhubaneswar", "jabalpur", "kanpur", "ranchi", "barrackpore",
			"almora", "jorhat", "coimbatore", "bangalore|bengaluru", "anand", "parbhani",
			"akola", "solapur", "udaipur", "bikaner", "dharwad", "jodhpur", "srinagar",
			"palampur", "cooch|pundibari", "sabour|bhagalpur", "bangladesh", "pakistan", "nepal"),
		cty = c("India", "India", "India", "India", "India", "India", "Bangladesh", "Bangladesh",
			"India", "India", "India", "India", "India", "India", "India", "India", "Pakistan",
			"Nepal", "India", "India", "India", "India", "India", "India", "India", "India",
			"India", "India", "India", "India", "India", "India", "India", "India", "India",
			"India", "India", "India", "India", "India", "Bangladesh", "Pakistan", "Nepal"),
		a1 = c("Odisha", "Tamil Nadu", "Delhi", "Punjab", "Madhya Pradesh", "Haryana", "Dhaka",
			"Mymensingh", "Uttarakhand", "Uttar Pradesh", "Uttar Pradesh", "Bihar", "Telangana",
			"Haryana", "West Bengal", "Chhattisgarh", "Punjab", "Lumbini", "Bihar", "Odisha",
			"Madhya Pradesh", "Uttar Pradesh", "Jharkhand", "West Bengal", "Uttarakhand", "Assam",
			"Tamil Nadu", "Karnataka", "Gujarat", "Maharashtra", "Maharashtra", "Maharashtra",
			"Rajasthan", "Rajasthan", "Karnataka", "Rajasthan", "Jammu and Kashmir",
			"Himachal Pradesh", "West Bengal", "Bihar", NA_character_, NA_character_, NA_character_),
		la = c(20.450, 11.000, 28.637, 30.905, 23.300, 29.705, 23.990, 24.715, 29.022,
			29.069, 25.268, 25.594, 17.511, 29.149, 22.975, 21.251, 31.450, 27.500,
			25.983, 20.296, 23.182, 26.450, 23.344, 22.767, 29.597, 26.757, 11.017,
			13.078, 22.565, 19.261, 20.700, 17.659, 24.585, 28.023, 15.459, 26.239,
			34.084, 32.111, 26.404, 25.261, 23.810, 30.375, 27.717),
		lo = c(85.934, 79.500, 77.157, 75.786, 77.400, 76.956, 90.409, 90.430, 79.490,
			77.706, 82.991, 85.138, 78.275, 75.722, 88.434, 81.630, 73.135, 83.450,
			85.673, 85.825, 79.986, 80.332, 85.310, 88.370, 79.659, 94.203, 76.956,
			77.579, 72.929, 76.775, 77.008, 75.906, 73.713, 73.312, 75.008, 73.024,
			74.797, 76.536, 89.386, 87.045, 90.413, 69.345, 85.324)
	)
	for (i in seq_len(nrow(st))) {
		hit <- grepl(st$pat[i], loc, ignore.case=TRUE)
		if (!any(hit)) next
		country[hit & is.na(country)] <- st$cty[i]
		adm1[hit & is.na(adm1)] <- st$a1[i]
		miss_geo <- hit & (is.na(lat) | is.na(lon))
		lat[miss_geo] <- st$la[i]
		lon[miss_geo] <- st$lo[i]
	}

	d <- data.frame(
		trial_id = as.character(r$`Study number`),
		plot_id = as.character(r$Sl.No),
		country = country,
		adm1 = adm1,
		location = loc,
		latitude = lat,
		longitude = lon,
		geo_from_source = geo_src,
		crop = crop,
		crop_rotation = rotation,
		treatment = practice,
		land_prep_method = land,
		N_fertilizer = n_fert,
		fertilizer_type = fert_type,
		soil_texture = texture,
		planting_date = year,
		irrigated = irr,
		on_farm = FALSE,
		is_survey = FALSE
	)
	d <- unique(d)

	carobiner::write_files(path, meta, d)
}
