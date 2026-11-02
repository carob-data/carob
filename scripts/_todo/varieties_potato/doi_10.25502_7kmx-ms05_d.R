# R script for "carob"
# license: GPL (>=3)

## NOTES
# On-farm potato tricot, Rwanda, second season. 150 packages. Yield weights exist for 45 packages.
# Varieties are blinded (A, B, C). There is no package-to-variety key in the file.
# Dictionary: row spacing, in-row spacing, plants harvested, and actual weight in kg, separately for options A, B and C.
# Spacing is not given a unit. Values are 70-90 (rows) and 25-40 (plants), which is the usual centimetre spacing for potato, and those are the terminag units for row_spacing and plant_spacing. Treated as cm.
# Harvested plant counts are 2 or 6 (A) and 6 (B, C), so this is a sample harvest. yield (kg/ha) = kg / (n_plants * row_m * plant_m) * 10000.
# Marketable kg uses the dictionary columns "actual yield in kg of marketable tubers".
# District (adm2) and sector (adm3) are in the registration form. Gucumbi -> Gicumbi and Burihukiro -> Buruhukiro where the corrected spelling is also in the file. Longitude and latitude columns are empty.
# Form timestamps are March 2021 (3/17/2021 is month/day because the day is 17). They were not used as planting or harvest dates.
# Country Rwanda. Organizations IITA and CIP from the draft metadata.

## ISSUES
# Variety names are not in the file.
# Confirm spacing is centimetres. One option-B row spacing is 30, which may be a plant spacing entered in the row-spacing field. It was left as recorded and will inflate yield.
# 105 packages have no weighed yield. Those rows have yield NA.
# 15 of the 45 weighed packages are missing in-row spacing, so yield is NA there too.
# No coordinates. Georeference sectors in Nyabihu, Nyamagabe and Gicumbi, Rwanda.
# N/P/K and irrigation are not reported.
# Estimated (non-weighed) weights were not used as yield.
# write_files: NA yield; missing longitude, latitude, N/P/K, irrigated, planting_date, harvest_date. Possible out-of-bounds yield where spacing is 30 cm.

## NOT PROCESSED
# Farmer estimated weights and estimated marketable / non-marketable weights (the weighed kg was used).
# Non-marketable tuber counts and weights, except marketable kg used for yield_marketable.
# Preference, dormancy, taste, bacterial-wilt and storage questions (option names, not rates).
# Telephone, consent, device id, empty GPS columns on the registration form.
# data_dictionary.csv (used to name the abbreviated columns).

## NEW VARIABLES
# package_id_: tricot package code.
# plants_harvested_: number of plants in the yield sample.


carob_script <- function(path) {

"
Results from the second season of analysis for 11 potato varieties

A challenge observed is the nature of RTB crops which demand more strategic logistics as the ‘seed’ of RTB crops are not grains but cuttings, suckers or vines. These are bulkier and can be stored for a limited amount of time. Because of this also less pronounced seed systems exist for RTB crops, which can also be considered an advantage to use tricot as an extra important variety dissemination strategy when compared to the approach within grain crops. Participatory variety evaluation efforts have sometimes failed in the past because of their failure to work within established research and extension systems. A main challenge for the project will be to forge lasting collaborations between research and extension around the tricot approach, and to embed the tricot approach in work routines. This means that to the extension system we need to demonstrate that tricot facilitates their work and provides them with tools to generate recommendation domains. It also means we need to demonstrate to research institutes the scientific value of the results (data quality, representation of target population environments, validity of the results).
"

	uri <- "doi:10.25502/7kmx-ms05/d"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA; CIP",
		publication = NA,
		project = NA,
		design = "tricot",
		data_type = "on-farm experiment",
		treatment_vars = "variety",
		response_vars = "yield;yield_marketable",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.9,
		notes = NA
	)

	f <- ff[basename(ff) == "report_data_pot21b.csv"]
	r <- read.csv(f, na.strings=c("", "NA", "None"), check.names=FALSE)

	num <- function(x) as.numeric(trimws(as.character(x)))
	adm2 <- trimws(r$Whais)
	adm2[adm2 == "Gucumbi"] <- "Gicumbi"
	adm3 <- trimws(r$Whaisthfadi)
	adm3[adm3 == "Burihukiro"] <- "Buruhukiro"

	opt <- c("A", "B", "C")
	row_cm <- list(num(r$WhaisA), num(r$WhaisthdiberoofthhaplfoyimeofOpB), num(r$WhaisthdiberoofthhaplfoyimeofOpC))
	plt_cm <- list(num(r$WhaisthdiberoofthhaplfoyimeofOpofOpA), num(r$WhaisthdibeplinliofthhaplfoyimeofOpB), num(r$WhaisthdibeplinliofthhaplfoyimeofOpC))
	nplant <- list(num(r$Howmathdibeplinliofthhaplfoyime), num(r$HowmaplhayohafoyidaofOpA), num(r$HowmaplhayohafoyidaofOpB))
	kg <- list(num(r$WhaisthfaesweofOpCanweOpA), num(r$WhaisthacweinkgofeaofthvaUsabaanweOpB), num(r$WhaisthacweinkgofeaofthvaUsabaanweOpC))
	mkt <- list(num(r$WhaisthfaesweweofnomatuofOpCweOpA), num(r$WhaisthacyiinkgofmatuofUsabaanweOpB), num(r$WhaisthacyiinkgofmatuofUsabaanweOpC))

	parts <- list()
	for (i in 1:3) {
		area <- nplant[[i]] * (row_cm[[i]] / 100) * (plt_cm[[i]] / 100)
		parts[[i]] <- data.frame(
			package_id_ = as.character(r$Package_code),
			adm2 = adm2,
			adm3 = adm3,
			variety = opt[i],
			treatment = opt[i],
			row_spacing = row_cm[[i]],
			plant_spacing = plt_cm[[i]],
			plants_harvested_ = nplant[[i]],
			plant_density = 1e8 / (row_cm[[i]] * plt_cm[[i]]),
			yield = kg[[i]] / area * 10000,
			yield_marketable = mkt[[i]] / area * 10000
		)
	}
	d <- do.call(rbind, parts)
	d$country <- "Rwanda"
	d$location <- d$adm3
	d$crop <- "potato"
	d$yield_part <- "tubers"
	d$yield_isfresh <- TRUE
	d$trial_id <- paste0("POT21B_", d$package_id_)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- FALSE

	carobiner::write_files(path, meta, d)
}
