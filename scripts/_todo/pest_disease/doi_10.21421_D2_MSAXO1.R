# R script for "carob"
# license: GPL (>=3)

## ISSUES
# planting_date uses Dataverse timePeriodCoveredStart (2015-06-15); timePeriodCoveredEnd is 2014-09-15 (before start), so harvest_date is omitted.
# Field location conflict: Dataverse geographicUnit says RP 12B (bounding box used here), docx says BP10, and the abstract coordinate (17°30'39.8"N, 78°16'11.7"E) is ~1 km away.
# Seedling vigor scale conflict: the abstract states 1 = low to 5 = high, whereas ICRISAT sorghum shoot-fly screening typically scores 1 = high to 5 = low (values here are 1-3). Leaf sheath pigmentation (1-3) is not defined in the files.
# Recurrent parents ICSB 29004 (entries 7, 14) and SPV 1411 (entries 22, 25) appear under two entry numbers each; entry_number_ is kept to distinguish them. Row 43 (Plot 6011, ICSB 29004, Rep 2) has NA for all measurements.

## NEW
# entry_number_: trial entry number (1-32; distinguishes duplicate recurrent-parent entries)
# glossiness_score_: leaf surface glossiness visual score (1 = glossy to 5 = non-glossy)
# seedling_vigor_: seedling vigor visual score (1-5 scale; see ISSUES on direction)
# leaf_sheath_pigmentation_: leaf sheath pigmentation visual score (1-3 scale)
# shoot_fly_egg_density_: shoot fly eggs per hectare (eggs/ha; derived from SF Total eggs / 3 m2)
# shoot_fly_egg_incidence_: percentage of plants with shoot fly eggs (%)
# pest_species: "Atherigona soccata" (sorghum shoot fly) not yet in values_pest.csv

## MISSING
# yield, yield_part, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date

## NOTES
# Shoot-fly (Atherigona soccata) screening of sorghum MABC introgression lines at ICRISAT Patancheru, 2015 rainy season (kharif); RCBD, 2 replications, 2 rows x 2 m x 0.75 m = 3 m2 (from docx; docx filename says post-rainy but body and Excel sheet are 2015 rainy).
# Associated publication: Gorthy et al. (2017), Front. Plant Sci. 8:1494 (doi:10.3389/fpls.2017.01494).
# pest_severity stores percent deadhearts at 28 DAE (0-100 scale). Per-plot egg counts are normalized to eggs/ha (shoot_fly_egg_density_); SF eggs per plant and plants with eggs are algebraically recoverable via plant_density and plot_area.
# Coordinates are the midpoint of the Dataverse bounding box (78.278587-78.279564 E, 17.501790-17.501851 N).

## NOT PROCESSED
# Phenotypic data ... 2015 rainy season.xlsx: SF eggs per plant (redundant: shoot_fly_egg_density_ / plant_density); Pl.with SF eggs (redundant: shoot_fly_egg_incidence_ / 100 * plant_density * plot_area / 10000).

carob_script <- function(path) {

"
Phenotypic data of shoot fly introgression lines in Sorghum for the year 2015 rainy season

Sorghum is a major cereal crop cultivated mainly for food, feed and fodder purpose. But the major constraint in its production is shoot fly, which effects the plant at very early stage resulting in lower yield. Hence, this study was focused to develop shoot fly resistant varieties using host plant resistance mechanism. In this study, three QTLs associated with shoot fly resistance were introgressed into elite cultivars Parbhani Moti (SPV1411) and ICSB29004 using marker assisted backcrossing (MABC) and IS 18551 and 296B/swarna are used as resistant and susceptible check respectively. This database includes the phenotypic data generated in the shoot fly screening field for the development of shoot fly resistance elite varieties using marker assisted back crossing at ICRISAT Center, Patancheru (17°30'39.8'N 78°16'11.7'E). The data on five component traits contributing to shoot fly resistance were recorded in shoot fly screening block using interlard fish meal techniquethe data was recorded for season viz., 2015 rainy, at ICRISAT-Patancheru, Hyderabad, India. The component traits recorded in the field trial were leaf surface glossiness (GS), trichome density on adaxial (upper; TDU) and abaxial (lower; TDL) leaf surfaces, seedling vigor (SV), oviposition at 28 DAE (EG28) and dead hearts percentage (DH%). Leaf glossiness was visually scored on a scale of 1–5 scores with 1 being glossy (light green, shiny, narrow, and erect leaves), and 5 -non-glossy (dark green, dull, broad, and drooping leaves). Seedling vigor was recorded on a 1–5 scale, where 1 stands for low vigor and 5 for high vigor. Oviposition non-preference is the number of eggs laid on each seedling from each plot at 14, 21, and 28 DAE. The overall shoot fly damage was calculated by taking the percentage of dead hearts (DH%) on 14, 21, and 28 DAE. To record the trichome density both trichomes on the adaxial leaf surface (TDU) and abaxial surface (TDL) were recorded at 14 DAE. Finally the few best progenies among them are being evaluated in on-farm trials (OFTs).        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/MSAXO1"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 3,
		data_organization = "ICRISAT",
		publication = "doi:10.3389/fpls.2017.01494",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_severity",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Phenotypic data of shoot fly introgression lines in sorghum for the year 2015 rainy season.xlsx"]
	r <- carobiner::read.excel(f, sheet = "2015 rainy", na = "NA")

	variety <- trimws(r$Genotype)
	vtype <- rep(NA_character_, nrow(r))
	vtype[variety == "IS18551"] <- "resistant check"
	vtype[variety %in% c("296B", "Swarna")] <- "susceptible check"
	vtype[variety %in% c("ICSB 29004", "SPV 1411")] <- "recurrent parent"
	deadheart <- as.numeric(r[["SFDH percent at 28DAE"]])

	d <- data.frame(
		country = "India",
		adm1 = "Telangana",
		location = "ICRISAT Patancheru",
		site = "RP 12B",
		crop = "sorghum",
		variety = variety,
		variety_type = vtype,
		treatment = variety,
		rep = as.integer(r[["Replication number"]]),
		plot_id = as.character(r[["Plot no"]]),
		entry_number_ = as.integer(r[["Entry no"]]),
		# Plot area: 2 rows x 2 m x 0.75 m = 3 m2 (from docx)
		plant_density = as.numeric(r[["No of plant per plot"]]) * 10000 / 3,
		glossiness_score_ = as.numeric(r[["Glossy score"]]),
		seedling_vigor_ = as.numeric(r[["Vigor  score"]]),
		leaf_sheath_pigmentation_ = as.numeric(r[["Leaf sheath pigmentation"]]),
		shoot_fly_egg_density_ = as.numeric(r[["SF Total eggs"]]) * 10000 / 3,
		shoot_fly_egg_incidence_ = as.numeric(r[["Percent of plants with SF eggs"]]),
		# Percent shoot fly deadhearts at 28 DAE
		pest_severity = as.character(round(deadheart, 2)),
		severity_scale = "0-100",
		pest_species = "Atherigona soccata",
		stress = "pests",
		season = "kharif",
		planting_date = "2015-06-15",
		row_spacing = 75,
		plot_length = 2,
		plot_width = 1.5,
		plot_area = 3,
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		# Midpoint of Dataverse bounding box (~104 m E-W)
		longitude = (78.278587 + 78.279564) / 2,
		latitude = (17.501790 + 17.501851) / 2,
		geo_uncertainty = 55,
		geo_source = "ICRISAT Dataverse bounding box",
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
