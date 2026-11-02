# R script for "carob"
# license: GPL (>=3)

## NOTES
# Grain from the 2015 TAMASA Nigeria nutrient-omission trials. The grain file does not name the crop. It is the maize trial series (hybrid and OPV) described with the companion soil set.
# The variable sheet says the percentages are soil concentrations. The dataset title and the DATA sheet are grain, with variety and treatment. Values are stored as grain concentrations.
# Percent N, P, K, Ca, and Mg are converted to mg/g by multiplying by 10. Na, Mn, Fe, Cu, and Zn are ppm, divided by 1000 to mg/g.
# Latitude and longitude are decimal degrees with a hemisphere letter. Nigeria longitudes in this file are east.
# Sites are the 95 on-farm trial sites named in the dataset description.

## ISSUES
# Grain concentrations below the vocabulary minimum are left as calculated. The variable sheet text still says soil.
# write_files: missing variables: yield, yield_moisture, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. out of bounds: grain_N (3.892, 20.588), grain_P (0.528, 4.631), grain_K (0.694, 7.293), grain_Ca (0.403, 5.422), grain_Mg (0.131, 1.928), grain_Na (0.001, 0.086), grain_Mn (0.001, 0.047), grain_Fe (0.008, 0.463), grain_Cu (0, 0.009), grain_Zn (0.002, 0.073).

## NOT PROCESSED
# DATA HISTORY.
# The variable sheet's summary statistics.


carob_script <- function(path) {

"
TAMASA Nigeria. Nutrient omission trials (NOT) grain nutrient analysis, 2015

This dataset consists of Result of Grain Analysis for the 95 on-farm field trial sites x 2 varieties (Hybrid and OPV) x 6 treatments (Control, PK, NK, NP, NPK, NPK+S+Ca+Mg+Zn+B).
"

	uri <- "hdl:11529/280436"
	group <- "nutrition"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=7,
		data_organization = "CIMMYT",
		publication = NA,
		project = "TAMASA",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;variety",
		response_vars = "grain_N;grain_P;grain_K;grain_Zn",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.4
	)

	f <- ff[grepl("xlsx$", basename(ff), ignore.case=TRUE)]
	s <- carobiner::read.excel(f, sheet="SITE_INFORMATION")
	g <- carobiner::read.excel(f, sheet="DATA")

	coord <- function(x, negative_letter) {
		x <- trimws(as.character(x))
		x[x %in% c("", "NA")] <- NA_character_
		neg <- !is.na(x) & grepl(negative_letter, x)
		num <- sub("[NnSsEeWw]", "", x)
		out <- as.numeric(trimws(num))
		out[neg] <- -abs(out[neg])
		out
	}
	elev <- function(x) {
		z <- sub("[mM]$", "", trimws(as.character(x)))
		z <- trimws(z)
		z[z %in% c("", "NA", "N/A", ".")] <- NA_character_
		as.numeric(z)
	}
	pct <- function(x) as.numeric(x) * 10
	ppm <- function(x) as.numeric(x) / 1000

	site <- data.frame(
		site_code = trimws(as.character(s[["SITE CODE"]])),
		country = trimws(as.character(s$COUNTRY)),
		adm1 = carobiner::fix_name(trimws(as.character(s$STATE)), "title"),
		adm2 = carobiner::fix_name(trimws(as.character(s$LGA)), "title"),
		location = carobiner::fix_name(trimws(as.character(s$COMMUNITY)), "title"),
		latitude = coord(s$Latitude, "S"),
		longitude = coord(s$Longitude, "W"),
		elevation = elev(s$Altitude)
	)

	d <- data.frame(
		site_code = trimws(as.character(g$SiteCode)),
		sample_id = trimws(as.character(g[["Sample ID"]])),
		treatment = trimws(as.character(g$Treatment)),
		variety = trimws(as.character(g$Variety)),
		grain_N = pct(g$N),
		grain_P = pct(g$P),
		grain_K = pct(g[["% K"]]),
		grain_Ca = pct(g$Ca),
		grain_Mg = pct(g$Mg),
		grain_Na = ppm(g$Na),
		grain_Mn = ppm(g$Mn),
		grain_Fe = ppm(g$Fe),
		grain_Cu = ppm(g$Cu),
		grain_Zn = ppm(g$Zn)
	)
	d <- merge(d, site, by="site_code", all.x=TRUE)
	d$site_code <- NULL
	d$crop <- "maize"
	d$yield_part <- "grain"
	d$trial_id <- paste(d$location, d$sample_id)
	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$geo_from_source <- TRUE
	carobiner::write_files(path, meta, d)
}
