# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. Dataset title mentions 11 banana genotypes in 2021-2022, but df_field.csv
#    actually contains 19 genotypes and dates spanning 2017-2023 (including 154
#    rows formatted as "01/03/22" instead of dd/mm/yyyy, parsed here as
#    2022-03-01).
# 2. Global Gauhl severity (Gaulh, stored in disease_severity) mixes 0-1
#    proportions (817 rows, primarily 2017-2020) and 0-100 percentages (991
#    rows, primarily 2021-2023) in the same column. Values are kept as reported
#    in the source pending verification against the publication scripts.
# 3. Rows 2730-3395 record leaf-fragment lesion/necrosis measurements rather
#    than whole-plant Gauhl severity (Gaulh is NA on those rows). All 3395 rows
#    are retained without dropping records.
# 4. Coordinates in df_field.csv are plant grid positions (e.g., A02, stored in
#    plot_id), not geographic coordinates. CIRAD Neufchateau station coordinates
#    were georeferenced from Mapcarta with 1 km uncertainty.

## NEW
# disease: "black leaf streak" (black Sigatoka; not yet in values_disease.csv)
# pathogen: "Pseudocercospora fijiensis" (not yet in values_pathogen.csv)
# cycle_: banana crop cycle number
# period_: observation period number (Period_nb; "REF" set to NA)
# total_leaves_: total number of functional leaves per plant (TNL)
# leaf_emission_rate_: leaf emission rate, leaves/week (LER)
# yls_: youngest leaf spotted with stage 6 symptoms, leaf rank (YLS)
# sev_yls_: Gauhl severity score (1-6) on youngest leaf spotted (Sev_YLS)
# sex_latent_days_: estimated sexual latent period, days (SexLP)
# insl_: index of non-spotted leaves without stage 6 symptoms, % (INSL)
# ylst_: youngest leaf with visible streaks (stage 2), leaf rank (Ylst)
# sev_ylst_: severity score on Ylst (Sev_Ylst)
# incubation_days_: estimated incubation period, days (IP)
# instl_: index of leaves without visible streaks (stage 2), % (InstL)
# ylmst_: youngest leaf with mature streaks (stage 3), leaf rank (Ylmst)
# sev_ylmst_: severity score on Ylmst (Sev_YmstL)
# asex_latent_days_: estimated asexual latent period, days (AsexLP)
# inmstl_: index of leaves without mature streaks (stage 3), % (InstmL)
# fragment_id_: leaf fragment identifier (ID_fragment)
# lesion_id_: lesion identifier on fragment (ID_lesion)
# lesion_length_: lesion length, mm
# lesion_width_: lesion width, mm
# lesion_area_: lesion area, mm2 (surface_lesion)
# conidiophore_count_: counted stomata with mature conidiophores per lesion (nb_conidio_lesion)
# conidiophore_density_: mature conidiophore density, stomata/cm2 (conidio_lesion)
# necrosis_id_: necrosis identifier on fragment (ID_necrosis)
# necrosis_length_: necrosis length, mm
# necrosis_width_: necrosis width, mm
# necrosis_area_: necrosis area, mm2 (surface_necrosis)
# perithecia_count_: perithecia count per necrosis (nb_perithecia_necrosis)
# perithecia_density_: perithecia density, perithecia/cm2 (perithecia_necrosis)

## MISSING
# yield, yield_moisture, yield_part, irrigated, K_fertilizer, N_fertilizer,
# P_fertilizer, planting_date, harvest_date (not reported in df_field.csv)

## NOTES
# Field resistance screening of banana and plantain genotypes to black leaf
# streak disease (Pseudocercospora fijiensis) at CIRAD Neufchateau station,
# Guadeloupe (2017-2023; Jolivet et al., 2025, Plant Pathology). Non-numeric
# missing-symptom tokens ("non", "Non", "pas de PJFN", "pas de PJFL",
# "pas de PJFM", "REF") are converted to NA before numeric coercion.

## NOT PROCESSED
# Observer (observer name), add_obs (free-text field notes), MLR (marked leaf
# rank used to derive LER), and YLS+1 / YLmstL+1 (auxiliary adjacent-leaf ranks).
# df_field_dictionary.csv and README.txt are codebook/documentation files.


carob_script <- function(path) {

"
Field observations of eleven banana genotypes used to estimate their resistance levels to Pseudocercospora fijiensis in Guadeloupe (2021-2022)

This dataset contains field observations of eleven banana genotypes conducted in 2021 and 2022 at the CIRAD experimental station in Neufchâteau, Guadeloupe (details of the experimental design are provided in the associated publication). These observations were used to estimate different quantitative resistance components of banana to Pseudocercospora fijiensis infectious cycle.
"

	uri <- "doi:10.18167/DVN1/G0ZX4E"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIRAD",
		publication = "doi:10.1111/ppa.70062",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Gemini 3.1 Pro",
		carob_date = "2026-11-26",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "df_field.csv"]
	r <- read.csv(f, na.strings = c("NA", ""), check.names = FALSE)
	names(r) <- trimws(names(r))

	clean_chr <- function(x) {
		x <- trimws(gsub("\u00a0", "", as.character(x)))
		x[x == ""] <- NA_character_
		x
	}
	num <- function(x) {
		x <- clean_chr(x)
		x[tolower(x) %in% c("non", "pas de pjfn", "pas de pjfl", "pas de pjfm", "ref")] <- NA_character_
		as.numeric(x)
	}

	dt_raw <- clean_chr(r$Date)
	dt <- ifelse(
		grepl("^\\d{2}/\\d{2}/\\d{2}$", dt_raw),
		as.character(as.Date(dt_raw, "%d/%m/%y")),
		as.character(as.Date(dt_raw, "%d/%m/%Y"))
	)

	st_raw <- clean_chr(r$State)
	stage_map <- c(V = "vegetative", F = "flowering", f = "flowering", P = "P", R = "R", C = "C", Mort = "dead")
	vtype <- clean_chr(r[["Type Genotype"]])

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "Neufchateau",
		site = clean_chr(r$Field),
		plot_id = clean_chr(r$Coordinates),
		country = "Guadeloupe",
		adm1 = "Basse-Terre",
		adm2 = "Capesterre-Belle-Eau",
		location = "Neufchateau",
		latitude = 16.0808,
		longitude = -61.6029,
		geo_uncertainty = 1000,
		geo_source = "Mapcarta (CIRAD Station de Neufch\u00e2teau)",
		geo_from_source = FALSE,
		crop = ifelse(!is.na(vtype) & vtype == "Plantain", "plantain", "banana"),
		variety = clean_chr(r$Genotype),
		variety_type = vtype,
		treatment = clean_chr(r$Genotype),
		rep = as.integer(num(r$Rep)),
		cycle_ = as.integer(num(r$Cycle)),
		period_ = as.integer(num(r$Period_nb)),
		date = dt,
		growth_stage = unname(stage_map[st_raw]),
		stress = "disease",
		disease = "black leaf streak",
		pathogen = "Pseudocercospora fijiensis",
		disease_severity = clean_chr(r$Gaulh),
		total_leaves_ = num(r$TNL),
		leaf_emission_rate_ = num(r$LER),
		yls_ = num(r$YLS),
		sev_yls_ = num(r$Sev_YLS),
		sex_latent_days_ = num(r$SexLP),
		insl_ = num(r$INSL),
		ylst_ = num(r$Ylst),
		sev_ylst_ = num(r$Sev_Ylst),
		incubation_days_ = num(r$IP),
		instl_ = num(r$InstL),
		ylmst_ = num(r$Ylmst),
		sev_ylmst_ = num(r$Sev_YmstL),
		asex_latent_days_ = num(r$AsexLP),
		inmstl_ = num(r$InstmL),
		fragment_id_ = clean_chr(r$ID_fragment),
		lesion_id_ = clean_chr(r$ID_lesion),
		lesion_length_ = num(r$lesion_length),
		lesion_width_ = num(r$lesion_width),
		lesion_area_ = num(r$surface_lesion),
		conidiophore_count_ = num(r$nb_conidio_lesion),
		conidiophore_density_ = num(r$conidio_lesion),
		necrosis_id_ = clean_chr(r$ID_necrosis),
		necrosis_length_ = num(r$necrosis_length),
		necrosis_width_ = num(r$necrosis_width),
		necrosis_area_ = num(r$surface_necrosis),
		perithecia_count_ = num(r$nb_perithecia_necrosis),
		perithecia_density_ = num(r$perithecia_necrosis),
		on_farm = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
