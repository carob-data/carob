# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field phenotypes of BC1S1 progenies and parents under Striga at Abuja (ABSTR) and Mokwa (MOSTR), 2019-2020.
# YIELD is grain yield in kg/ha. There is no grain-moisture column.
# Striga damage (RAT1 at 8 weeks, RAT2 at 10 weeks) and emerged counts (CO1, CO2) go to the long table. STRC0_1 and STRC0_2 are the transformed counts and were not used.
# E_ASPIN is the infested-trial ear aspect. EPPIN is ears per plant under infestation.
# Parents TZdEI 352 and TZEI 811 are variety_type "parent"; the other pedigrees are "BC1S1".
# The marker file is not field data and was not processed.

## ISSUES
# No coordinates, fertilizer, irrigation, grain moisture, or day of planting. Year only. Georeference Abuja and Mokwa.
# RAT1 is missing on half the rows and CO1 on a quarter. The sites are Striga sites (ABSTR, MOSTR), so those gaps were left as missing ratings, not as uninfested plots.
# Ear aspect (E_ASPIN) includes values far above 9 (up to 85). Ear height includes values below 10 cm. Left as recorded.
# The marker matrix (about 1900 SNPs by progeny) is a separate table and has no plot phenotypes.
# Expect missing longitude, latitude, geo_from_source, yield_moisture, irrigated, N/P/K, harvest_date, out-of-bounds ear aspect and ear height, and an unknown-variable warning for ears_per_plant_.

## NOT PROCESSED
# mapping-quantitative-hermonthica_pheno.csv, ID: row number.
# mapping-quantitative-hermonthica_pheno.csv, ENV: environment index. trial_id is location plus year.
# mapping-quantitative-hermonthica_pheno.csv, STRC0_1, STRC0_2: transformed Striga counts. CO1 and CO2 are the counts.
# mapping-quantitative-hermonthica-resistance.csv: DArTseq SNP allele matrix (0/1/2/-) for the progenies and parents. Not crop or field measurements.
# column_dictionary.csv: definitions, including the marker coding. Used to confirm phenotype columns.

## NEW VARIABLES
# ears_per_plant_: ears per plant on the Striga-infested plots (dimensionless).


carob_script <- function(path) {

"
Phenotypic dataset involving BC1S1 progenies and their parental lines

Stress Tolerant Maize for Africa for Improved Livelihoods
"

	uri <- "doi:10.25502/pjv5-5h23/d"
	group <- "varieties_maize"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=NA, minor=NA,
		data_organization = "IITA",
		publication = NA,
		project = "Stress Tolerant Maize for Africa (STMA) Phase II",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;asi;plant_height;ear_height;ear_aspect",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 64,
		carob_effort = 0.5,
		notes = NA
	)

	f <- ff[basename(ff) == "mapping-quantitative-hermonthica_pheno.csv"]
	r <- read.csv(f)

	loc <- trimws(as.character(r$LOC))
	location <- loc
	location[loc == "ABSTR"] <- "Abuja"
	location[loc == "MOSTR"] <- "Mokwa"
	variety <- trimws(as.character(r$Pedigree))
	vtype <- ifelse(variety %in% c("TZdEI 352", "TZEI 811"), "parent", "BC1S1")

	d <- data.frame(
		trial_name = trimws(as.character(r$TRIAL)),
		location = location,
		country = "Nigeria",
		planting_date = as.character(r$YEAR),
		plot_id = as.character(r$Plot),
		rep = as.integer(r$Rep),
		block_id = as.character(r$Block),
		variety_code = as.character(r$Entry),
		variety = variety,
		variety_pedigree = variety,
		variety_type = vtype,
		treatment = variety,
		yield = as.numeric(r$YIELD),
		asi = as.numeric(r$ASI),
		plant_height = as.numeric(r$PLHT),
		ear_height = as.numeric(r$EHT),
		husk = trimws(as.character(r$HUSK)),
		ear_aspect = as.numeric(r$E_ASPIN),
		ears_per_plant_ = as.numeric(r$EPPIN),
		str_rat1 = as.numeric(r$RAT1),
		str_rat2 = as.numeric(r$RAT2),
		str_co1 = as.numeric(r$CO1),
		str_co2 = as.numeric(r$CO2),
		crop = "maize",
		yield_part = "grain",
		stress = "striga",
		striga_trial = TRUE,
		striga_infected = TRUE,
		borer_trial = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d$trial_id <- paste(d$location, d$planting_date, sep = "_")
	d$record_id <- as.integer(seq_len(nrow(d)))
	mz_pd_long <- carobiner::get_function("mz_pd_long", path, group)
	long <- mz_pd_long(d)
	d[intersect(c("e_rot", "str_co1", "str_co2", "str_rat1", "str_rat2"), names(d))] <- NULL
	carobiner::write_files(path, meta, d, long = long)
}
