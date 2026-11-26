# R script for "carob"
# license: GPL (>=3)

## NOTES
# Soil and leaf files are different plots (IDs do not match), so they are stacked. Leaf rows have no soil fields.
# Soil N_tot is Kjeldahl total N in percent, stored as soil_N_total in mg/kg (percent x 10000), not as soil_N.
# C_org is percent (Walkley-Black) and is soil_SOC. P_avail is ppm (Olsen, modified Dabin) and is soil_P.
# K, Ca and Mg are Metson exchangeable cmol/kg.
# The description says soil was taken at 0-30 cm, so soil rows use that depth.
# Leaf N, P, K, Ca and Mg are percent and are stored as mg/g (x 10). Leaf Fe, Mn, B, Cu, Zn and Mo are ppm (mg/kg) and are converted to mg/g (/ 1000) to match terminag crop_chemistry units.
# Produc is dry bean yield in kg/ha (leaf-file note). Leaf samples were collected in March 2015 (description). Coordinates are Lon and Lat in both files.
# Country is geographicCoverage Côte d'Ivoire, also named in the title. region is stored as location, not as an admin unit.
# The cited paper has no DOI in the Dataverse record (agritrop URL only).

## ISSUES
# soil_Zn reaches 376 mg/kg; left as reported.
# Soil rows have no yield or yield_part because those fields are only in the leaf file.
# The Dataverse record has French and English descriptions; yuri::cleaner is collapsed to one string inside this script so get_metadata can build the citation.
# write_files: out of bounds soil_Zn (0.13, 376); NA detected yield, yield_part.

## NOT PROCESSED
# C_N: ratio of the carbon and nitrogen columns already mapped
# Satur is soil_BS. Fe, Mn, Cu, Zn and B in the soil file are mapped.
# CIV_Cacao_feuilles Nguessan.txt: same table as the file of this name that is read.
# CIV_Cacao_sols Nguessan.txt: same table as the file of this name that is read.

carob_script <- function(path) {

"
Dataset of soil and cacao leaf samples collected in all regions of Côte d'Ivoire

[EN]Data of chemical analysis of soil and cacao (Theobroma cacao L.) leaf samples collected in adult cacao plantations selected in all regions of Southern Côte d'Ivoire.
<br>.................................<br>
The soil and cacao (Theobroma cacao L.) leaf samples were collected in adult cacao plantations selected in all pedological units of all regions of Southern Côte d'Ivoire. Soil samples were taken in the 0-30 cm horizon at 1 m from the cacao trees. The soil samples were analysed for their clay content and chemical parameters; i‧e. pH, C, N, P, K, Ca, Mg, CEC, Al, Zn, B, Mo, Mn. <br>
Leaf samples were collected in March 2015. The harvests for the year 2015 were estimated from pod counting.<br>
Each sampled cacao plot was geolocated to be associated with the corresponding soil pedological unit. The information was further used to build a thematic map providing localised fertilizer recommendations for cacao cultivation (see N'Guessan et al., 2017).
"

	uri <- "doi:10.18167/DVN1/PWM9WW"
	group <- "soil_samples"
	ff <- carobiner::get_data(uri, path, group)

	# French and English dsDescription values make yuri build a two-row metadata table,
	# and get_citation then stops on if (is.na(year)). Collapse them to one string.
	ns <- asNamespace("yuri")
	orig_cleaner <- get("cleaner", envir=ns)
	unlockBinding("cleaner", ns)
	assign("cleaner", function(x) paste(orig_cleaner(x), collapse="\n\n"), envir=ns)
	on.exit({
		assign("cleaner", orig_cleaner, envir=ns)
		lockBinding("cleaner", ns)
	}, add=TRUE)

	meta <- carobiner::get_metadata(uri, path, group, major=2, minor=7,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = NA,
		response_vars = NA,
		carob_LLM = "Gemini 4 Neon",
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_completion = 80,
		carob_effort = 0
	)

	f1 <- ff[basename(ff) == "CIV_Cacao_sols Nguessan.xlsx"]
	r1 <- carobiner::read.excel(f1, sheet="Feuil1")
	d1 <- data.frame(
		sample_id = as.character(r1$ID),
		longitude = as.numeric(r1$Lon),
		latitude = as.numeric(r1$Lat),
		location = trimws(as.character(r1$region)),
		country = "Côte d'Ivoire",
		crop = "cocoa",
		depth_top = 0,
		depth_bottom = 30,
		soil_clay = as.numeric(r1$Clay),
		soil_SOC = as.numeric(r1$C_org),
		soil_N_total = as.numeric(r1$N_tot) * 10000,
		soil_P = as.numeric(r1$P_avail),
		soil_P_method = "Olsen-Dabin",
		soil_K_exch = as.numeric(r1$K),
		soil_Ca_exch = as.numeric(r1$Ca),
		soil_Mg_exch = as.numeric(r1$Mg),
		soil_CEC = as.numeric(r1$CEC),
		soil_pH = as.numeric(r1$pH_water),
		soil_BS = as.numeric(r1$Satur),
		soil_Fe = as.numeric(r1$Fe),
		soil_Mn = as.numeric(r1$Mn),
		soil_Cu = as.numeric(r1$Cu),
		soil_Zn = as.numeric(r1$Zn),
		soil_B = as.numeric(r1$B)
	)
	# depth 0-30 cm from the dataset description. N_tot percent to mg/kg. C_org is percent.

	f2 <- ff[basename(ff) == "CIV_Cacao_feuilles Nguessan.xlsx"]
	r2 <- carobiner::read.excel(f2, sheet="Feuil1")
	d2 <- data.frame(
		sample_id = as.character(r2$ID),
		longitude = as.numeric(r2$Lon),
		latitude = as.numeric(r2$Lat),
		location = trimws(as.character(r2$region)),
		country = "Côte d'Ivoire",
		crop = "cocoa",
		# Description: leaf samples collected in March 2015
		date = "2015-03",
		yield = as.numeric(r2$Produc),
		yield_part = "seed",
		yield_moisture = 0,
		leaf_N = as.numeric(r2$N) * 10,
		leaf_P = as.numeric(r2$P) * 10,
		leaf_K = as.numeric(r2$K) * 10,
		leaf_Ca = as.numeric(r2$Ca) * 10,
		leaf_Mg = as.numeric(r2$Mg) * 10,
		# ppm (mg/kg) -> mg/g
		leaf_Fe = as.numeric(r2$Fe) / 1000,
		leaf_Mn = as.numeric(r2$Mn) / 1000,
		leaf_B = as.numeric(r2$B) / 1000,
		leaf_Cu = as.numeric(r2$Cu) / 1000,
		leaf_Mo = as.numeric(r2$Mo) / 1000,
		leaf_Zn = as.numeric(r2$Zn) / 1000
	)

	d <- carobiner::bindr(d1, d2)
	d$on_farm <- TRUE
	d$is_survey <- TRUE
	d$geo_from_source <- TRUE

	carobiner::write_files(path, meta, d)
}
