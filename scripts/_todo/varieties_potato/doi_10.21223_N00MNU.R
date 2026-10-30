# R script for "carob"
# license: GPL (>=3)

## NOTES
# Open-field late-blight trial of the HERLB population at Oxapampa, Peru, rainy season 2022. Row-column design, four plants per plot, no replication. Elevation 1810 m is from the description.
# Coordinates and the soil test are the Soil_Data sheet (one sample for the field). Soil pH is a 1:1 suspension. Sand, silt, clay, organic matter and CaCO3 are percent. P and K are ppm. Exchangeable cations are meq/100g. Texture "Fr" is stored as loam (franco).
# LB1 to LB8 are percent leaf area. AUDPC is divided by 100 so it is on a fraction scale. rAUDPC is already a fraction in the file.
# Tuber weights have no unit and the plot area is not given, so they are not converted to kg/ha.

## ISSUES
# No planting or harvest date, and no fertilizer or irrigation rate.
# Weather_data has Date, Time and GMT that do not form an obvious clock time (Time 0.5 with GMT AM), so the series was not written.
# write_files: missing carob_contributor, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variables: ttwp_, mtwp_.

## NOT PROCESSED
# 03 layout and 06 layout files: plot arrangement, not measurements.
# Fieldbook NTP, NPE, NPH, NMTP, NNoMTP, NMTPL, ATMW, TNTP, TNTPL, NoMTWP, TTWPL: counts and per-plant weights. Plot tuber weights are kept as ttwp_ and mtwp_.
# 04_Weather_data: see ISSUES.
# Soil Requester, Operator, Date, total cations, total bases, exchangeable sodium percentage: administration or sums.

## NEW VARIABLES
# ttwp_: total tuber weight per plot. Unit is not stated.
# mtwp_: marketable tuber weight per plot. Unit is not stated.


carob_script <- function(path) {

"
Dataset for: Preliminary trial for late blight assessment in HERLB population (First trial_Oxapampa)

'An open-field trial was conducted in Oxapampa, Peru, at an elevation of 1810 meters above sea level during the rainy season of 2022 to assess the Late Blight (LB) response in the HERLB population. This population originated from crosses between advanced clones, referred to as LBHT, and HER clones (4x hybrids obtained from pre-breeding). The evaluation encompassed 2996 genotypes, organized in a row-column statistical design with four plants per plot and no replication. The varieties included in the trial had a known response to late infection, with Kory, Amarilis, and Yungay representing high resistance, moderate resistance, and susceptibility to LB, respectively. At 40 days after planting (DAP), a meticulous recording of the percentage of leaf area affected by late blight infection per plot was undertaken throughout the season. Subsequently, the area under the disease progress curve (AUDPC) was calculated, offering a comprehensive measure of the impact of late blight infection.'
"

	uri <- "doi:10.21223/N00MNU"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = "row-column",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "AUDPC;rAUDPC",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "01_Fieldbook_HERLB_PER_2022_LateBlight_Oxa.xlsx"]
	r <- carobiner::read.excel(f)
	ml <- carobiner::read.excel(ff[basename(ff) == "02_Material_List_HERLB_PER_2022_LateBlight_Oxa.xlsx"])
	s <- carobiner::read.excel(ff[basename(ff) == "05_Soil_Data-HERLB_PER_2022_LateBlight_Oxa.xlsx"])
	sval <- function(nm) {
		hit <- s$Data[trimws(as.character(s$Abbreviture)) == nm]
		if (length(hit) == 0) return(NA_character_)
		trimws(as.character(hit[1]))
	}

	id <- seq_len(nrow(r))
	num <- function(x) {
		x <- trimws(as.character(x))
		x[!nzchar(x) | x %in% c("NA", "NULL")] <- NA_character_
		as.numeric(x)
	}
	d <- data.frame(
		record_id = id,
		plot_id = as.character(r$plot),
		variety = trimws(as.character(r$cip_number)),
		treatment = trimws(as.character(r$cip_number)),
		AUDPC = num(r$AUDPC) / 100,
		rAUDPC = num(r$rAUDPC),
		ttwp_ = num(r$TTWP),
		mtwp_ = num(r$MTWP),
		stringsAsFactors = FALSE
	)

	ctrl <- tolower(trimws(as.character(ml$Is_control)))
	alias <- data.frame(
		variety = trimws(as.character(ml$CIPNumber)),
		variety_alt = trimws(as.character(ml$Accesion_Name)),
		variety_type = ifelse(ctrl == "yes", "check", NA_character_),
		stringsAsFactors = FALSE
	)
	alias$variety_alt[!nzchar(alias$variety_alt)] <- NA_character_
	alias <- alias[!duplicated(alias$variety), ]
	d <- merge(d, alias, by="variety", all.x=TRUE)

	d$crop <- "potato"
	d$country <- "Peru"
	d$location <- "Oxapampa"
	d$elevation <- 1810
	d$latitude <- as.numeric(sval("Latitude"))
	d$longitude <- as.numeric(sval("Longitude"))
	d$geo_from_source <- TRUE
	d$trial_id <- "HERLB_Oxapampa_2022"
	d$on_farm <- NA
	d$is_survey <- FALSE
	d$stress <- "disease"
	d$pathogen <- "Phytophthora infestans"
	d$soil_sand <- as.numeric(sval("Sand"))
	d$soil_silt <- as.numeric(sval("Silt"))
	d$soil_clay <- as.numeric(sval("Clay"))
	d$soil_texture <- "loam"
	d$soil_BS <- as.numeric(sval("Base saturation"))
	d$soil_CaCO3 <- as.numeric(sval("CaCO3")) * 10
	d$soil_EC <- as.numeric(sval("Soil Electrical conductivity"))
	d$soil_pH <- as.numeric(sval("Soil PH"))
	d$soil_P <- as.numeric(sval("Phosphorus"))
	d$soil_SOM <- as.numeric(sval("Organic matter"))
	d$soil_K <- as.numeric(sval("Potassium"))
	d$soil_Ca_exch <- as.numeric(sval("Exchangeable Calcium"))
	d$soil_Mg_exch <- as.numeric(sval("Exchangeable Magnesium"))
	d$soil_Na_exch <- as.numeric(sval("Exchangeable Sodium"))
	d$soil_K_exch <- as.numeric(sval("Exchangeable Potassium"))
	d$soil_ex_acidity <- as.numeric(sval("Aluminium + hidrogenum"))

	pieces <- lapply(1:8, function(i) {
		data.frame(
			record_id = id,
			order = i,
			disease_severity = as.character(r[[paste0("LB", i)]]),
			stringsAsFactors = FALSE
		)
	})
	dlong <- do.call(rbind, pieces)
	dlong <- dlong[!is.na(dlong$disease_severity) & nzchar(dlong$disease_severity), ]
	dlong$disease <- "potato late blight"
	dlong$severity_scale <- "0-100"

	carobiner::write_files(path, meta, d, long=dlong)
}
