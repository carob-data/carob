# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. The 10 numeric rows in Facteurs and Facteurs origine (Altitude, Prof sol,
#    pH, MO, Argile, Limon, Sable, Pluvio jusqu'a obs, Pluvio annuelle, Date
#    semis) are all unit-variance standardized (sd = 1.000, dimensionless)
#    without original scaling factors or units, so they cannot be recovered as
#    physical measurements and are excluded.
# 2. The dataset title spans 1988-1992 (DOC says 1988-1991), but Annee is 1990
#    and Date_obs has only three dates (1990-06-01, 1990-08-01, 1990-10-01).
# 3. Floristique has 155 species rows (description says 157), identified only by
#    5-letter Amatrop/Bayer codes; no species-name lookup sheet is provided.
# 4. Fumure reports total NPK + urea product rates (100 or 200 kg/ha NPK plus
#    100 kg/ha urea) without an NPK grade, so N_fertilizer, P_fertilizer, and
#    K_fertilizer can only be computed for unfertilized plots (0) and urea-only
#    plots (100 kg/ha urea = 46 kg N/ha).
# 5. No coordinates in the source; the 18 named localities (Lieu) in North and
#    Far North Cameroon were georeferenced from locality/commune centers.

## NEW
# earthing_up_: earthing-up / ridging practice (none, manual, mechanical)
# bare_soil_pct_: bare soil cover class (<10%, 10-25%, 25-50%, 50-75%, >75%)
# field_age_years_: years of cultivation class (1-2, 3-4, 5-10, 10-20, >20)
# herbicide_history_: years of prior herbicide use (none, 1-3 years, 4-10 years, >10 years)
# herbicide_product: "dipropetryn" (active ingredient in dipropetrine+metolachlore, not yet in values_herbicides.csv)

## MISSING
# yield, yield_moisture, yield_part, planting_date, harvest_date (not collected in this weed survey)

## NOTES
# 540 phytoecological weed surveys across 18 localities in the Northern Cameroon
# cotton rotation (cotton, maize, sorghum, groundnut). Plot factors are in wide
# format (d) and 1-9 species abundance scores converted to % cover are in long.

## NOT PROCESSED
# Standardized numeric rows in Facteurs (Altitude, Prof sol, pH, MO, Argile,
# Limon, Sable, Pluvio jusqu'a obs, Pluvio annuelle, Date semis): rescaled to
# unit variance without original scale factors.
# Climat (tropical / tropical-sec) and Epoque_obs (redundant with Date_obs).
# Sheets Meta donnees and Facteurs origine, and exported .txt files (FLO, FAC,
# PA-FLO) duplicate the Facteurs and Floristique sheets.


carob_script <- function(path) {

"
Weed survey in cotton rotation in Northern Cameroon (1988-1992)

Characterization of weed communities in cotton rotation (cotton, maiz, sorghum, groundnut) in Northern Cameroon, within the framework of the Projet FAC Regional Malherbologie Garoua Cameroun. Phytoecological study in abundance scale 1-5 transformed to scale 1-9. This dataset covers 540 surveys of plants and environmental factors for 157 weed species.
"

	uri <- "doi:10.18167/DVN1/5ZEQGO"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=5, minor=0,
		data_organization = "CIRAD",
		publication = NA,
		project = "Projet FAC r\u00e9gional malherbologie Garoua Cameroun",
		design = NA,
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;weed_species_cover",
		notes = NA,
		carob_contributor = NA,
		carob_LLM = "Gemini 3.1 Pro",
		carob_date = "2026-11-26",
		carob_completion = 70,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "CAM-LEB-1990-DIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "Facteurs")
	r2 <- carobiner::read.excel(f, sheet = "Floristique")

	get_row <- function(label) trimws(as.character(unname(unlist(r1[r1[[1]] == label, -1]))))

	crop_map <- c(arachide = "groundnut", cotonnier = "cotton", mais = "maize", sorgho = "sorghum")
	prev_map <- c(arachide = "groundnut", cotonnier = "cotton", jachere = "none", mais = "maize", sorgho = "sorghum")
	soil_map <- c(
		"alluvions" = "alluvial",
		"ferrugineux" = "ferruginous",
		"ferrugineux dunaire" = "ferruginous dune",
		"fersialitique" = "fersiallitic",
		"hydromorphe" = "hydromorphic",
		"peu evolue" = "weakly developed",
		"planosol" = "planosol",
		"vertisol" = "vertisol"
	)
	stage_map <- c(
		"levee" = "emergence",
		"deb veg" = "early vegetative",
		"vegetation" = "vegetative",
		"floraison" = "flowering",
		"fructification" = "fruiting",
		"recolte" = "maturity"
	)
	weed_meth_map <- c(
		"0 sarclage" = "none",
		"sarclage man" = "manual",
		"sarclage mec" = "mechanical",
		"sarclage mec+man" = "mechanical;manual"
	)
	but_map <- c("0 butage" = "none", "butage man" = "manual", "butage mec" = "mechanical")
	herbi_map <- c(
		"pas herbi" = "none",
		"atrazine+metolachlore" = "atrazine;metolachlor",
		"dipropetrine+metolachlore" = "dipropetryn;metolachlor"
	)
	age_map <- c(
		"1-2 ans" = "1-2", "3-4 ans" = "3-4", "5-10 ans" = "5-10",
		"10-20 ans" = "10-20", ">20 ans" = ">20"
	)
	hhist_map <- c(
		"jamais herbi" = "none",
		"herbi 1-3 ans" = "1-3 years",
		"herbi 4-10 ans" = "4-10 years",
		"herbi >10 ans" = ">10 years"
	)

	lab <- tolower(get_row("Labour"))
	fum <- tolower(get_row("Fumure"))
	sar <- tolower(get_row("Sarclage"))
	hbt <- tolower(get_row("Herbicide-type"))
	irr <- tolower(get_row("Irrigation"))

	ftype <- c("0 fumure" = "none", "100 uree" = "urea", "100 npk + 100 uree" = "NPK;urea", "200 npk + 100 uree" = "NPK;urea")
	famt <- c("0 fumure" = 0, "100 uree" = 100, "100 npk + 100 uree" = 200, "200 npk + 100 uree" = 300)

	d <- data.frame(
		trial_id = names(r1)[-1],
		plot_id = get_row("Parcelle"),
		date = as.character(as.Date(as.numeric(get_row("Date_obs")), origin = "1899-12-30")),
		country = "Cameroon",
		location = get_row("Lieu"),
		crop = unname(crop_map[tolower(get_row("Type_culture"))]),
		previous_crop = unname(prev_map[tolower(get_row("Precedent"))]),
		growth_stage = unname(stage_map[tolower(get_row("Stade_culture"))]),
		soil_type = unname(soil_map[tolower(get_row("Sol"))]),
		land_prep_method = ifelse(lab == "0 labour", "none", "ploughing"),
		land_prep_traction = ifelse(lab == "0 labour", "none", ifelse(lab == "manuel", "manual", NA_character_)),
		fertilizer_used = fum != "0 fumure",
		fertilizer_type = unname(ftype[fum]),
		fertilizer_amount = unname(famt[fum]),
		N_fertilizer = ifelse(fum == "0 fumure", 0, ifelse(fum == "100 uree", 46, NA_real_)),
		P_fertilizer = ifelse(fum %in% c("0 fumure", "100 uree"), 0, NA_real_),
		K_fertilizer = ifelse(fum %in% c("0 fumure", "100 uree"), 0, NA_real_),
		herbicide_used = tolower(get_row("Herbicide")) == "herbi pre",
		herbicide_product = unname(herbi_map[hbt]),
		weeding_done = sar != "0 sarclage",
		weeding_method = unname(weed_meth_map[sar]),
		earthing_up_ = unname(but_map[tolower(get_row("Butage"))]),
		bare_soil_pct_ = sub("^sn", "", tolower(get_row("Sol_nu"))),
		field_age_years_ = unname(age_map[tolower(get_row("Age_parcelle"))]),
		herbicide_history_ = unname(hhist_map[tolower(get_row("Anciennete_herbi"))]),
		irrigated = irr %in% c("inonde", "inond\u00e9"),
		irrigation_method = ifelse(irr %in% c("inonde", "inond\u00e9"), "flood", "none"),
		stress = "weeds",
		intercropped = FALSE,
		on_farm = TRUE,
		is_survey = TRUE
	)

	geo <- data.frame(
		location = c(
			"B\u00e9nou\u00e9", "Bidzar", "Bocki", "Bogo", "Gashiga", "Guider",
			"Hamakoussou", "Ka\u00e9l\u00e9", "Madingrin", "Mokong", "Mora",
			"Ndock", "Ngong", "Pitoa", "Poli", "Sorombeo", "Tchollir\u00e9", "Touboro"
		),
		adm1 = c(
			"Nord", "Nord", "Nord", "Extr\u00eame-Nord", "Nord", "Nord",
			"Nord", "Extr\u00eame-Nord", "Nord", "Extr\u00eame-Nord", "Extr\u00eame-Nord",
			"Nord", "Nord", "Nord", "Nord", "Nord", "Nord", "Nord"
		),
		latitude = c(
			9.3000, 9.8953, 8.7848, 10.7336, 9.4296, 9.9342,
			9.5800, 10.1000, 8.4600, 10.5990, 11.0500,
			7.9466, 9.0233, 9.3839, 8.4734, 8.2228, 8.3976, 7.7682
		),
		longitude = c(
			13.4000, 14.1132, 13.5133, 14.6093, 13.3616, 13.9486,
			13.4300, 14.4500, 15.0100, 14.0065, 14.1500,
			14.7051, 13.5119, 13.5023, 13.2404, 14.9428, 14.1728, 15.3541
		),
		geo_uncertainty = c(
			30000, 10000, 10000, 10000, 10000, 10000,
			15000, 10000, 10000, 10000, 10000,
			10000, 10000, 10000, 10000, 10000, 10000, 10000
		),
		geo_source = "Locality/commune center (North & Far North Cameroon)",
		geo_from_source = FALSE
	)
	d <- merge(d, geo, by = "location", all.x = TRUE, sort = FALSE)

	# 1-9 abundance scale -> % cover (CAM-LEB-1990-DIV-AD-DOC.txt)
	cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)
	score <- as.matrix(r2[, -1])
	j <- which(!is.na(score) & score > 0, arr.ind = TRUE)
	long <- data.frame(
		trial_id = colnames(score)[j[, "col"]],
		weed_species = trimws(as.character(r2[[1]][j[, "row"]])),
		weed_species_cover = cover[as.integer(score[j])]
	)

	carobiner::write_files(path, meta, d, long = long)
}
