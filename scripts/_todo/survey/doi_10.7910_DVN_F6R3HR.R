# R script for "carob"
# license: GPL (>=3)

## NOTES
# IRRI MISTIR / RICE area-based farm household survey, Indonesia, crop calendar 2016, n=800 (West Java, Yogyakarta, South Kalimantan, South Sulawesi, South Sumatera, Lampung, Papua).
# SPSS modules share a structure. Crop-management file is adopt_crop_mgt_clean.sav (not adopt_crop_mgt.sav).
# Improved_rice_varieties.sav is one row per household x variety slot.
# Labelled codes converted with haven label attributes.
# Variety from adoption_is_rice_var (Indonesia list); "Others (specify)" uses adoption_other_variety. adoption_C_PlotSize is hectares.

## ISSUES
# No coordinates or admin names in the .sav files (only country). Human must georeference the seven provinces; do not geocode in-script.
# Unmapped modules: adopt_crop_mgt_clean, adopt_farm_mach, adopt_nutrient_mgt, adopt_pest_mgt, adopt_post_tech, adopt_water_mgt.
# year_adoption is variety adoption year, not 2016 planting_date. Some values are 2-digit.
# write_files: missing latitude/longitude/geo_from_source; unknown variables variety_trait_, variety_source_, variety_adoption_year_, years_using_variety_; duplicate HH x variety slots possible.

## NEW VARIABLES
# variety_trait_: labelled trait of the improved variety (high yielding, flood-tolerant, …)
# variety_source_: information source for the variety
# variety_adoption_year_: year the household started using the variety
# years_using_variety_: reported years using the variety (c_year_adoption)


carob_script <- function(path) {

"Area Based Farm Household Survey in Indonesia for the Metrics and Indicators for Tracking in RICE project

An Area-Based Farm Household Survey is a structured field methodology used within international agricultural initiatives like the Metrics and Indicators for Tracking in RICE project. Spearheaded by the International Rice Research Institute (IRRI), this dual approach integrates ground-level microeconomic data with macro spatial tracking to measure the sustainability, productivity, and socio-economic transformation of smallholder rice systems.

In the CGIAR Research Program on Rice (formerly known as the Global Rice Science Partnership, or GRiSP), monitoring and evaluation (M&E) relies on an integrated framework of metrics to track progress. This structure was refined under the MISTIG project (Metrics and Indicators for Tracking in GRiSP) to map directly to CGIAR’s Intermediate Development Outcomes (IDOs).

The primary M&E tracking metrics and indicators are categorized below by their targeted developmental impact areas:
1. Productivity and Yield Indicators
2. Economic and Livelihood Indicators
3. Resource-Use Efficiency and Environmental Sustainability
4. Technology Adoption and Scale Metrics
5. Institutional Capacity and Science Quality Indicators

The area of focus is in 7 provinces of Indonesia (see below) with a total sampling size of 800 conducted during the crop calendar of 2016 (2 season data capture).   
1. West Java
2. Yogyakarta
3. South Kalimantan
4. South Sulawesi
5. South Sumatera
6. Lampung
7. Papua

Trained Partner Enumerators to perform Data Collection: Indonesian Center for Rice Research"

	uri <- "doi:10.7910/DVN/F6R3HR"
	group <- "survey"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = NA,
		project = "MISTIR",
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 25,
		carob_effort = 0.3
	)

	unlab <- function(d) {
		d <- as.data.frame(d)
		for (i in seq_along(d)) {
			v <- d[[i]]
			if (inherits(v, "haven_labelled")) {
				labs <- attr(v, "labels")
				if (length(labs)) {
					nm <- names(labs)[match(as.numeric(v), labs)]
					miss <- is.na(nm) & !is.na(v)
					nm[miss] <- as.character(as.numeric(v[miss]))
					d[[i]] <- nm
				} else {
					d[[i]] <- as.numeric(v)
				}
			}
		}
		d
	}

	f <- ff[basename(ff) == "Improved_rice_varieties.sav"]
	r <- unlab(haven::read_sav(f))

	# skipped: adopt_crop_mgt_clean, adopt_farm_mach, adopt_nutrient_mgt, adopt_pest_mgt, adopt_post_tech, adopt_water_mgt

	variety <- r$adoption_is_rice_var
	use_other <- is.na(variety) | grepl("Others", variety, ignore.case=TRUE)
	variety[use_other] <- r$adoption_other_variety[use_other]

	d <- data.frame(
		hhid = as.character(r$HHID),
		country = "Indonesia",
		crop = "rice",
		variety = variety,
		field_size = as.numeric(r$adoption_C_PlotSize),
		variety_trait_ = r$adoption_Improved_rice_varieties,
		variety_source_ = r$adoption_source_technology,
		variety_adoption_year_ = r$year_adoption,
		years_using_variety_ = as.numeric(r$c_year_adoption)
	)

	d$variety[trimws(as.character(d$variety)) == ""] <- NA
	d$on_farm <- TRUE
	d$is_survey <- TRUE

	carobiner::write_files(path, meta, d)
}
