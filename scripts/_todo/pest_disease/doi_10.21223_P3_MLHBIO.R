# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Two DMS longitudes have minutes or seconds >= 60: Piura "80°80'99\"" (5 rows; parses to -81.361 in the Pacific Ocean, corrected to -80.8099 assuming decimal degrees) and Moquegua/Tacna "70°49'97\"" (4 rows; corrected to -70.8328 assuming decimal minutes 70°49.97'), with geo_from_source = FALSE on those 9 rows.
# In 15 rows leafminer_adults_ exceeds Liriomyza sativae + L. huidobrensis, and in 17 rows parasitoid_adults_ differs from the sum of individual parasitoid taxa (likely unidentified specimens or raw transcription discrepancies); both totals and per-taxon counts are retained.
# Tmin and Tmax are site/season temperatures attached to each survey row rather than a daily weather series.
# Crop names were standardized to terminag terms following Mujica & Kroschel (2011): bean -> common bean (Phaseolus vulgaris), wild watermelon -> watermelon (Citrullus lanatus), yellow peruvian chili -> chili pepper (Capsicum baccatum), scallion -> green onions, alfalfa -> lucerne, soja -> soybean, green pea -> pea, cowpea bean -> cowpea, sweet pepper -> bell pepper.

## NEW
# season: winter, autumn (Peruvian coastal cropping seasons; not in values_season)
# leafminer_adults_: count of leafminer fly adults emerging from the ~30-leaflet field sample (count)
# parasitoid_adults_: count of parasitoid adults emerging from the sample (count)
# pupae_: count of pupae without emerging adults (count)
# larvae_per_leaflet_: mean number of leafminer larvae per leaflet (count/leaflet)

## NOTES
# Field survey of leafminer flies (Liriomyza spp.) and parasitoids across 459 fields in 10 coastal regions of Peru during winter (Jul-Aug 2003, 243 fields) and autumn (Apr-May 2004, 216 fields); Mujica & Kroschel (2011), doi:10.1603/EN10170.
# Trailing Excel summary/blank rows (rows 460-462) were dropped. Foliar damage is stored in pest_severity (1-4 scale) and per-taxon counts for 2 leafminer and 61 parasitoid taxa in long (insect_species, insect_abundance).
# DMS coordinates lack hemisphere indicators; all sites are on the Peruvian coast (south latitude, west longitude).

## NOT PROCESSED
# 1.1_dictionary.xls: variable codebook.
# Crop (Family plant): plant family, redundant with crop.
# Foliar damage (low = 1-25%; middle=26-50%; high>50%): 1-to-1 text label for the 1-4 pest_severity code.
# Total (LMF + Par + Pupae): sum of leafminer_adults_, parasitoid_adults_, and pupae_.

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
		project = "Control of the Leafminer, Liriomyza huidobrensis in Potatoes through IPM",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "pest_severity;insect_abundance",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "subchapter 1.1_Survey dataset.xls"]
	r <- carobiner::read.excel(f, na="NA")
	names(r) <- trimws(names(r))
	# Drop trailing Excel summary/blank rows (rows 460-462)
	r <- r[!is.na(r$Region), ]

	dms <- function(x, neg) {
		s <- gsub("[[:space:]]+", "", trimws(as.character(x)))
		parts <- strsplit(s, "[°'\"]")
		mat <- do.call(rbind, parts)
		deg <- as.numeric(mat[, 1]) + as.numeric(mat[, 2]) / 60 + as.numeric(mat[, 3]) / 3600
		if (neg) deg <- -deg
		deg
	}

	lon_raw <- gsub("[[:space:]]+", "", trimws(as.character(r[["Longitude (DMS)"]])))
	lon <- dms(lon_raw, TRUE)
	bad_piura <- lon_raw == "80°80'99\""
	bad_moq <- lon_raw == "70°49'97\""
	lon[bad_piura] <- -80.8099
	lon[bad_moq] <- -(70 + 49.97 / 60)

	crop <- tolower(trimws(as.character(r[["Crop (common name)"]])))
	crop <- carobiner::replace_values(crop,
		c("alfalfa", "bean", "cowpea bean", "green pea", "scallion", "soja",
		  "sweet pepper", "wild watermelon", "yellow peruvian chili"),
		c("lucerne", "common bean", "cowpea", "pea", "green onions", "soybean",
		  "bell pepper", "watermelon", "chili pepper")
	)

	season <- tolower(trimws(as.character(r$Season)))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = as.character(seq_len(nrow(r))),
		country = "Peru",
		adm1 = carobiner::fix_name(r$Region, "title"),
		latitude = round(dms(r[["Latitude (DMS)"]], TRUE), 5),
		longitude = round(lon, 5),
		elevation = as.numeric(r[["Altitude (m)"]]),
		geo_from_source = !(bad_piura | bad_moq),
		date = ifelse(season == "winter", "2003", "2004"),
		season = season,
		crop = crop,
		tmin = as.numeric(r$Tmin),
		tmax = as.numeric(r$Tmax),
		pest_severity = as.character(as.integer(r[["Foliar damage (no damage=1;low = 2; middle=3; high=4)"]])),
		severity_scale = "1=no damage (0%);2=low (1-25%);3=middle (26-50%);4=high (>50%)",
		leafminer_adults_ = as.integer(r[["leafminer fly adults"]]),
		parasitoid_adults_ = as.integer(r[["Parasitoid adults"]]),
		pupae_ = as.integer(r[["Pupae (without emerging adults)"]]),
		larvae_per_leaflet_ = as.numeric(r[["Larva/ leaflet"]]),
		stress = "pests",
		on_farm = TRUE,
		is_survey = TRUE
	)

	known <- c(
		"Region", "Latitude (DMS)", "Longitude (DMS)", "Altitude (m)", "Season", "Tmax", "Tmin",
		"Crop (common name)", "Crop (Family plant)",
		"Foliar damage (low = 1-25%; middle=26-50%; high>50%)",
		"Foliar damage (no damage=1;low = 2; middle=3; high=4)",
		"leafminer fly adults", "Parasitoid adults", "Pupae (without emerging adults)",
		"Total (LMF + Par + Pupae)", "Larva/ leaflet"
	)
	taxa <- setdiff(names(r), known)
	taxa_clean <- gsub("[[:space:]]+", " ", trimws(taxa))
	taxa_clean[taxa_clean == "L. sativae"] <- "Liriomyza sativae"
	taxa_clean[taxa_clean == "L. huidobrensis"] <- "Liriomyza huidobrensis"

	pieces <- vector("list", length(taxa))
	for (i in seq_along(taxa)) {
		pieces[[i]] <- data.frame(
			record_id = d$record_id,
			date = d$date,
			order = as.integer(i),
			insect_species = taxa_clean[i],
			insect_abundance = as.character(as.integer(r[[taxa[i]]]))
		)
	}
	dlong <- do.call(rbind, pieces)
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
