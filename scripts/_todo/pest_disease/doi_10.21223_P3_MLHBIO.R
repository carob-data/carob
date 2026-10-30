# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field survey of leafminer flies and parasitoids along the Peruvian coast, winter 2003 and autumn 2004.
# The citation in the dataset metadata is doi:10.1603/EN10170.
# Latitude and longitude are degrees-minutes-seconds without a hemisphere. The survey is on the Peruvian coast, so latitude is south and longitude is west.
# Foliar damage uses the four-level scale named in the column: 1 no damage, 2 low, 3 middle, 4 high.
# Crop names were aligned to the vocabulary where the file uses another name for the same crop: alfalfa to lucerne, soja to soybean, green pea to pea, cowpea bean to cowpea, sweet pepper to bell pepper, muskmelon to melon.
# Tmax and Tmin are the values recorded on the survey row, not a daily weather series.

## ISSUES
# There is no yield, fertilizer, or planting date. This is an occurrence survey.
# There is no applied treatment. crop is recorded in treatment_vars because that field has to name a column that varies.
# Several crop names are not in the vocabulary and are kept as recorded.
# Leafminer fly is not in the pathogen vocabulary, so it is not stored as pathogen. The taxa are in the long table.
# season_ is the cropping season in the file (winter, autumn). It is not one of the wet/dry season terms.
# Some DMS points are flagged as not on land, and some rows have no coordinates.
# Some rows have no crop name.
# write_files: missing variables: carob_contributor, trial_id, yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. metadata: missing values in treatment variable crop. unknown variable: season_ (season?). unknown variables: damage_class_, leafminer_adults_, parasitoid_adults_, pupae_, larvae_per_leaflet_, taxon_, insect_count_. invalid terms: crop: bean, scallion, wild watermelon, yellow peruvian chili. coordinates not on land: Peru. NA detected: latitude, longitude. time/depth: no time/depth variables in long records?

## NEW VARIABLES
# damage_class_: foliar-damage band from the percent classes in the column header (low 1-25%, middle 26-50%, high >50%).
# season_: winter or autumn, as recorded.
# leafminer_adults_: count of leafminer fly adults.
# parasitoid_adults_: count of parasitoid adults.
# pupae_: pupae without emerging adults.
# larvae_per_leaflet_: larvae per leaflet.
# taxon_: insect taxon in the count table.
# insect_count_: count of that taxon in the sample.

## NOT PROCESSED
# 1.1_dictionary.xls: column definitions.
# Crop (Family plant): plant family, redundant with crop once the crop is named.
# Total (LMF + Par + Pupae): sum of the adult, parasitoid, and pupae columns.


carob_script <- function(path) {

"
Replication data for: Leafminer fly (Diptera: Agromyzidae) Occurrence, Distribution and Parasitoid Associations in Field and Vegetable Crops along the Peruvian Coast

Surveys to study the occurrence and distribution of leafminer fly and their associated parasitoids were conducted in the winter, from July to August 2003, and autumn cropping season, from April to May 2004, in ten agricultural production regions along the Peruvian coast from Tumbes (0º01´11' S) in the north of Peru to Tacna (18°21'05' S) in the south in a total of 29 provinces. In the different regions the production of field and vegetable crops as host plants of leafminer flies are not equally important, hence the sample size varied in each region. The survey did not take into account the different plant-protection measures and pesticide applications by farmers and therefore does not reflect undisturbed herbivore-parasitoid relationships. On average, 30 leafminer fly-infested leaves were randomly collected from each field sampled, covering a total of 459 fields, with 243 and 216 fields in the winter and autumn cropping season, respectively. During the daily surveys, the leaf samples collected were immediately stored in paper bags and afterwards transferred onto damp paper towels into plastic containers (15x25x10 cm) to avoid drying of the leaves. Leaf layers were separated with dry paper towels and all towels were changed at 2-day intervals for the following 6 days. Boxes were closed with a lid containing muslin windows (20x12 cm) for ventilation and kept at temperatures of 20ºC. After 7 to 10 days pupae were collected from the boxes and stored in plastic Petri dishes until adults of either leafminer flies or endoparasitoids hatched. Leaves were returned to the container to identify the ectoparastoids. Specimens were identified using conventional taxonomic keys and reference collections. Infested fields were registered based on visual assessment of presence or ausence of leafminer foliar damage. In each field, the infestation intensity was estimated by using a rating scale of four levels of leaf damage: no infestation (0%), low (1-25%), middle (26-50%) and high (> 50%) leaf infestation. These parameters were calculated in 30 plants randomly selected per field.
"

	uri <- "doi:10.21223/P3/MLHBIO"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIP",
		publication = "doi:10.1603/EN10170",
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "crop",
		response_vars = "disease_severity",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.55
	)

	f <- ff[basename(ff) == "subchapter 1.1_Survey dataset.xls"]
	r <- carobiner::read.excel(f, na="NA")
	names(r) <- trimws(names(r))

	dms <- function(x, neg) {
		s <- gsub("[[:space:]]+", "", trimws(as.character(x)))
		s[!nzchar(s)] <- NA_character_
		out <- rep(NA_real_, length(s))
		# One longitude is stored as 77°44"54" (a quote where the minutes mark should be).
		ok <- !is.na(s) & grepl("^[0-9]+°[0-9]+['\"][0-9]+\"?$", s)
		if (any(!is.na(s) & !ok)) stop("unparsed coordinate")
		parts <- strsplit(s[ok], "[°'\"]")
		mat <- do.call(rbind, parts)
		deg <- as.numeric(mat[, 1]) + as.numeric(mat[, 2]) / 60 + as.numeric(mat[, 3]) / 3600
		if (neg) deg <- -deg
		out[ok] <- deg
		out
	}

	crop <- tolower(trimws(as.character(r[["Crop (common name)"]])))
	crop[crop == "alfalfa"] <- "lucerne"
	crop[crop == "soja"] <- "soybean"
	crop[crop == "green pea"] <- "pea"
	crop[crop == "cowpea bean"] <- "cowpea"
	crop[crop == "sweet pepper"] <- "bell pepper"
	crop[crop == "muskmelon"] <- "melon"
	crop[crop == "faba bean"] <- "faba bean"

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		country = "Peru",
		adm1 = carobiner::fix_name(r$Region, "title"),
		latitude = dms(r[["Latitude (DMS)"]], TRUE),
		longitude = dms(r[["Longitude (DMS)"]], TRUE),
		elevation = as.numeric(r[["Altitude (m)"]]),
		season_ = tolower(trimws(as.character(r$Season))),
		crop = crop,
		treatment = crop,
		tmin = as.numeric(r$Tmin),
		tmax = as.numeric(r$Tmax),
		damage_class_ = trimws(as.character(r[["Foliar damage (low = 1-25%; middle=26-50%; high>50%)"]])),
		disease_severity = as.character(r[["Foliar damage (no damage=1;low = 2; middle=3; high=4)"]]),
		severity_scale = "1=no damage;2=low (1-25%);3=middle (26-50%);4=high (>50%)",
		leafminer_adults_ = as.numeric(r[["leafminer fly adults"]]),
		parasitoid_adults_ = as.numeric(r[["Parasitoid adults"]]),
		pupae_ = as.numeric(r[["Pupae (without emerging adults)"]]),
		larvae_per_leaflet_ = as.numeric(r[["Larva/ leaflet"]]),
		stress = "pests",
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)
	d$damage_class_[!nzchar(d$damage_class_)] <- NA_character_
	d$disease_severity[!nzchar(d$disease_severity)] <- NA_character_
	d$season_[!nzchar(d$season_)] <- NA_character_
	d$crop[!nzchar(d$crop)] <- NA_character_
	d$treatment <- d$crop
	d$geo_from_source <- !is.na(d$latitude) & !is.na(d$longitude)

	known <- c(
		"Region", "Latitude (DMS)", "Longitude (DMS)", "Altitude (m)", "Season", "Tmax", "Tmin",
		"Crop (common name)", "Crop (Family plant)",
		"Foliar damage (low = 1-25%; middle=26-50%; high>50%)",
		"Foliar damage (no damage=1;low = 2; middle=3; high=4)",
		"leafminer fly adults", "Parasitoid adults", "Pupae (without emerging adults)",
		"Total (LMF + Par + Pupae)", "Larva/ leaflet"
	)
	taxa <- setdiff(names(r), known)
	pieces <- vector("list", length(taxa))
	for (i in seq_along(taxa)) {
		pieces[[i]] <- data.frame(
			record_id = d$record_id,
			order = i,
			taxon_ = taxa[i],
			insect_count_ = as.numeric(r[[taxa[i]]])
		)
	}
	dlong <- do.call(rbind, pieces)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
