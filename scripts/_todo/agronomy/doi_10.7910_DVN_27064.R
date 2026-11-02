# R script for "carob"
# license: GPL (>=3)

## NOTES
# Farmer-managed "baby trials" of participatory varietal selection (PVS) under STRASA in Bangladesh, India and Nepal (Project Details.docx: year 2009, 646 farmers).
# Each farmer compared 1-3 stress-tolerant test varieties with a local check (VARTYPE 0 = local, 1 = test). One farmer (COUNTRY x RESPNO) = one trial.
# Codebook: "Baby Trial Monitoring Survey PVS-STRASA_TParis and DVillanueva.xlsx"; questionnaire: the .docx with the same name.
# Records from "6. Overall performance.xlsx" (farmer x variety): grain yield (kg/ha), milling recovery (%), male and female rank.
# Joined to "2. Trial details.xlsx" (plot area m2, establishment method, sowing and transplanting dates) by country x farmer x variety name
#   (names normalized: lower case, only letters and digits, because spelling differs between files, e.g. "BRRI dhan52" vs "BRRI dhan-52").
# Joined to "1. General information.xlsx" (village, district, stress type, season, soil texture, fertility, land type, tenure, irrigation) by country x farmer.
# DTSOWN (date sown) -> planting_date; DTTRANS -> transplanting_date; DTHARV is the *expected* harvest date (codebook) -> expected_harvest_date_.

## ISSUES
# No coordinates. Georeference villages (location) by district; districts spelled as in the data except obvious fixes (e.g. "24th South Parganas" -> "South 24 Parganas"). adm1 (state/division) not given.
# Season in general information is "2009 ..." for all, but sowing dates are 2006, 2008, 2009 and 2011 (Bangladesh mostly 2011; BRRI dhan52 was released in 2010). Check dates/years.
# About 15% of yield records have no general information, and about 18% do not match a variety in the trial details (even after name normalization); a few farmer x variety names are duplicated.
# 254 India and 23 Bangladesh yield records are NA; some yields are 0 (crop loss?).
# TYPSTRESS: codebook 1 = submergence, 2 = salinity, 3 = sodicity, but the questionnaire says 3 = drought. Mapped 1 = "flood", 2 = "soil salinity", 3 kept as "sodicity".
# FIELDTYP 3 (semi-irrigated) -> irrigated NA. Soil texture combinations (codes 7-19) kept only in soil_type.
# plot_area (AREA, m2) up to 40000 m2 is outside terminag bounds (max 350); some values may be in another unit. Not changed.
# Some records have no variety name (write_files "missing values in treatment variable variety"); write_files also reports duplicate records.
# No fertilizer, yield moisture or harvest date data (write_files "missing variables").
# Not processed: farmer ratings vs local variety at 2nd and 3rd visit (4., 5.), submergence and salinity conditions (3.1, 3.2), seed distribution (7.), technology tracking (8.), ranking remarks (MREM*, FEMREM*).

## NEW VARIABLES
# milling_recovery_: milling recovery (%)
# rank_male_, rank_female_: rank of the variety by the principal male / female farmer (1 = best)
# expected_harvest_date_: expected harvest date at the first visit
# soil_fertility_: farmer-reported soil fertility (very fertile, fertile, unfertile)


carob_script <- function(path) {

"
Baby Trial Monitoring Survey of Participatory Varietal Selection under STRASA

To evaluate the performance of stress-tolerant varieties in farmer's field and compared it with the common varieties (local check).
"

	uri <- "doi:10.7910/DVN/27064"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=2,
		data_organization = "IRRI",
		publication = NA,
		project = "STRASA",
		design = NA,
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 0.6
	)

	rd <- function(name) carobiner::read.excel(ff[basename(ff) == name], na=c("", "."))
	rG <- rd("1. General information.xlsx")
	rT <- rd("2. Trial details.xlsx")
	rO <- rd("6. Overall performance.xlsx")

	vkey <- function(r) paste(r$COUNTRY, r$RESPNO, gsub("[^a-z0-9]", "", tolower(r$VARNAME)))
	tr <- match(vkey(rO), vkey(rT))
	g <- match(paste(rO$COUNTRY, rO$RESPNO), paste(rG$COUNTRY, rG$RESPNO))

	d <- data.frame(
		country = rO$COUNTRY,
		trial_id = paste(rO$COUNTRY, rO$RESPNO, sep="-"),
		variety = trimws(rO$VARNAME),
		variety_type = ifelse(rO$VARTYPE == 1, "stress-tolerant test variety", "local check"),
		yield = rO$GRNYLDKGHA,
		milling_recovery_ = rO$MILRECPER,
		rank_male_ = as.integer(rO$MALERANK),
		rank_female_ = as.integer(rO$FEMRANK),
		plot_area = rT$AREA[tr],
		planting_method = c("transplanting", "direct seeding")[rT$ESTMETH[tr]],
		planting_date = as.character(as.Date(rT$DTSOWN[tr])),
		transplanting_date = as.character(as.Date(rT$DTTRANS[tr])),
		expected_harvest_date_ = as.character(as.Date(rT$DTHARV[tr])),
		adm2 = trimws(rG$DISTRICT[g]),
		location = trimws(rG$VILLAGE[g]),
		season = tolower(gsub("^2009 |T\\.", "", rG$SEASON[g])),
		stress = c("flood", "soil salinity", "sodicity")[rG$TYPSTRESS[g]],
		soil_type = c("loamy", "sandy", "sandy-loamy", "clay", "clay-loam", "loamy-sand", "saline/alkaline",
			"sandy & clay", "sandy-loamy & saline/alkaline", "sandy, sandy-loamy & clay", "loamy, sandy-loamy & clay",
			"sandy & sandy-loamy", "clay & saline/alkaline", "clay-loam & saline/alkaline", "sandy loam/clay loam",
			"loamy & clay-loam", "loamy & sandy-loam", "clay & clay-loam", "sandy-loam & clay")[rG$SOILTEX[g]],
		soil_texture = c("loam", "sand", "sandy loam", "clay", "clay loam", "loamy sand")[rG$SOILTEX[g]],
		soil_fertility_ = c("very fertile", "fertile", "unfertile")[rG$FERTLITY[g]],
		landscape_position = c("lowland", "midland", "upland", "lowland & midland", "lowland, midland & upland",
			"lowland & upland", "midland & upland")[rG$LANDTYPE[g]],
		land_tenure = c("owned", "not owned", "owned & not owned")[rG$TENURE[g]],
		irrigated = c(TRUE, FALSE, NA)[rG$FIELDTYP[g]]
	)

	d$season[d$season == "wet season"] <- "wet"
	adm <- c("24th South Parganas"="South 24 Parganas", "Chaipainaw"="Chapai Nawabganj", "Chapainawa"="Chapai Nawabganj",
		"Jagalsinghpur"="Jagatsinghpur", "Kendrapada"="Kendrapara", "Madhugani"="Madhubani", "Naogoan"="Naogaon")
	i <- match(d$adm2, names(adm))
	d$adm2[!is.na(i)] <- adm[i[!is.na(i)]]
	# Nepal districts are GADM level 3
	d$adm3 <- ifelse(d$country == "Nepal", d$adm2, NA)
	d$adm2[d$country == "Nepal"] <- NA

	d$treatment <- d$variety
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
