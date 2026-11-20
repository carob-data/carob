# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer-wise soil health data from 13 districts of Andhra Pradesh, India.
# OC is organic carbon in % -> soil_SOC. EC in dS/m (= mS/cm) -> soil_EC.
# Available and exchangeable nutrients are taken as mg/kg.
# Extent (AC) is in acres. field_size is that value times 0.404686 (ha).
# Crop before: local (Telugu) names and spelling variants mapped to terminag crop names;
#   multiple crops separated by "; ". Names with no terminag match are kept as they are.
# Farmer Name and Fathers Name are not kept.

## ISSUES
# Units are not stated in the column names; assumed from ICRISAT soil-health convention.
# out of bounds (left as is, to be checked): soil_EC up to 27.5, soil_P up to 362, soil_Ca up to 13304, soil_S up to 2540.
# Some coordinates are not on land (in the sea); left as is.
# Some "Crop before" values look like soil descriptions (red, sandy, light black) or are unclear (maagu, mc).


carob_script <- function(path) {

"
AP Primary Sector farmer-wise soil health data of 13 districts of Andhra Pradesh, India.
"

	uri <- "doi:10.21421/D2/K3BPKW"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=3,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "none",
		notes = NA,
		carob_date = "2026-11-06",
		carob_contributor = "DMAS, ICRISAT",
		carob_completion = 80,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "AP Primary Sector Farmerwise Soil Health Data of 13 districts.xls"]
	r <- carobiner::read.excel(f, sheet="Farmer wise data", na=c("", "NA", "-"))
	names(r) <- trimws(gsub("\\s+", " ", names(r)))

	d <- data.frame(
		country = "India",
		adm1 = "Andhra Pradesh",
		adm2 = trimws(r$District),
		adm3 = trimws(r$Mandal),
		location = trimws(r$Village),
		latitude = as.numeric(r$Latitude),
		longitude = as.numeric(r$Longitude),
		geo_from_source = TRUE,
		hhid = trimws(as.character(r[["Farmer No"]])),
		field_id = trimws(as.character(r[["Survey No."]])),
		# nutrient_type_ = trimws(r[["Macro/ Micro nutrient"]]),
		soil_type = trimws(r[["Soil type"]]),
		field_size = as.numeric(gsub("AREA-", "", r[["Extent (AC)"]])) * 0.404686,
		soil_pH = as.numeric(r$pH),
		soil_EC = as.numeric(r$EC),
		soil_SOC = as.numeric(r$OC),
		soil_P = as.numeric(r[["Avail-P"]]),
		soil_K = as.numeric(r[["Exch-K"]]),
		soil_Ca = as.numeric(r[["Avail-Ca"]]),
		soil_Mg = as.numeric(r[["Avail-Mg"]]),
		soil_S = as.numeric(r[["Avail-S"]]),
		soil_Zn = as.numeric(r[["Avail-Zn"]]),
		soil_B = as.numeric(r[["Avail-B"]]),
		soil_Fe = as.numeric(r[["Avail-Fe"]]),
		soil_Cu = as.numeric(r[["Avail-Cu"]]),
		soil_Mn = as.numeric(r[["Avail-Mn"]]),
		on_farm = TRUE,
		is_survey = TRUE
	)

	# previous crop: remove sample labels, turn intercrop notes and separators into ","
	pc <- tolower(trimws(r[["Crop before"]]))
	pc <- gsub("\\(sample-[0-9]+\\)", "", pc)
	pc <- gsub("\\(inter ?cr?op|[()]| and ", ",", pc)

	crops <- c(
		"anaar"="pomegranate", "bajra"="pearl millet", "beans"="common bean",
		"beera"="luffa", "ridge guard"="luffa", "benda"="okra",
		"bengalgram"="chickpea", "senaga"="chickpea", "erra senaga"="chickpea",
		"blackgram"="black gram", "horsegram" = "horse gram",  "menumu"="black gram", "minumu"="black gram",
		"brinjal"="eggplant", "cabage"="cabbage", "donda" = "ivy gourd", "maagu" = "managu", 
		"cane"="sugarcane", "sugar cane"="sugarcane", "suger cane"="sugarcane", "sugercane"="sugarcane",
		"cashew nut"="cashew", "cashewnut"="cashew", "cashew raina"="cashew",
		"castor"="castor bean", "clastor"="castor bean", "castral"="castor bean",
		"chill"="chili pepper", "chilli"="chili pepper", "chillies"="chili pepper",
		"chilly"="chili pepper", "mirchi"="chili pepper", "merappa"="chili pepper",
		"cocnut"="coconut", "coriandam"="coriander", "dhanyalu"="coriander",
		"cottan"="cotton", "cow pea"="cowpea",
		"g.nut"="groundnut", "g.n"="groundnut", "grounat"="groundnut", "groundnat"="groundnut",
		"ground nat"="groundnut", "ground nut"="groundnut","veru senaga"="groundnut",
		"green gram"="mung bean", "g.gram"="mung bean", "peasara"="mung bean", "pesara"="mung bean",
		"jonna"="sorghum", "jonnalu"="sorghum", "jowar"="sorghum", "erra jonna"="sorghum",
		"pacha jonna"="sorghum", "mahendra jonna"="sorghum",
		"korra"="foxtail millet", "mazi"="maize", "horti"="vegetable",
		"mesta"="kenaf", "mulbarry"="mulberry", "mulberrry"="mulberry",
		"onian"="onion", "oniyan"="onion", "paddy"="rice", "paady"="rice", "vari"="rice",
		"paddy maize"="rice; maize", "paddy sugercane"="rice; sugarcane",
		"pacha gaddi"="forage crop", "fodder crops"="forage crop",
		"pigeonpea"="pigeon pea", "red gram"="pigeon pea", "chilli papaya"="papaya","redgram"="pigeon pea",
		"pogaku"="tobacco", "potatao"="potato", "ragi"="finger millet",
		"rajma"="kidney bean", "seasamum"="sesame", "sesamum"="sesame", "sesumum"="sesame",
		"sweetcorn"="sweet corn", "tamato"="tomato","niger"="niger",
		"topioca"="cassava", "topica"="cassava", "turmaric"="turmeric","vegetable"="vegetable",
		"vegitable"="vegetable", "eucaliptus"="eucalyptus", "red sandal" = "sandalwood", "sandal" = "sandalwood",
		 "mc" = NA, "red" = NA, "sandy" = NA
	)

	fix_crop <- function(x) {
		if (is.na(x)) return(NA)
		x <- trimws(unlist(strsplit(x, "[,/+;]")))
		x <- x[x != ""]
		i <- x %in% names(crops)
		x[i] <- crops[x[i]]
		if (length(x) == 0) return(NA)
		paste(unique(x), collapse="; ")
	}
	d$previous_crop <- sapply(pc, fix_crop, USE.NAMES=FALSE)

	carobiner::write_files(path, meta, d)
}

