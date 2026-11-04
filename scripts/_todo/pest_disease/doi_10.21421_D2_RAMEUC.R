# R script for "carob"
# license: GPL (>=3)

## NOTES
# Shoot-fly screening of sorghum introgression lines, ICRISAT Patancheru, 2014 post-rainy season (rabi).
# Design and plot size are from the docx and the Dataverse notes: RCBD, 2 rows x 2 m x 0.75 m = 3 m2, three replications.
# Glossiness is 1 (glossy) to 5 (non-glossy). Seedling vigor is 1 (low) to 5 (high), from the dataset abstract.
# Plant density is plants per plot x 10000 / 3. pest_severity is deadheart percent at 28 DAE, stored as text.
# Coordinates are the midpoint of the Dataverse bounding box (78.268764–78.270851 E, 17.509449–17.509528 N). Field id in the metadata is BP 12B.
# Publication DOI is the one given in the Dataverse record.

## ISSUES
# No grain yield in the file. Fertilizer, rhizobium inoculation, and irrigation are not reported.
# timePeriodCoveredStart 2014-10-15 is used as planting_date. timePeriodCoveredEnd is 2015-02-15 and dateOfCollectionEnd is 2015-12-15, so harvest_date was left unset.
# ICSB29004 is entry 7 and entry 14, and SPV 1411 is entry 22 and entry 25. Rows were not collapsed.
# Six rows have no plant count and no deadheart percent (stored as the text NA).
# Leaf sheath pigmentation is a 1–3 score; the file does not define the scores.
# pest_species "Atherigona soccata" is not in the pest vocabulary. It is the name in the linked paper.
# write_files: missing carob_contributor, yield, yield_part, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, harvest_date.
# unknown variables are the entry number and the shoot-fly component scores. The hint leaf_S is leaf sulphur, not pigmentation.

## NOT PROCESSED
# None. Entry number is kept as entry_number_ because the same genotype can have two entry numbers.

## NEW VARIABLES
# entry_number_: trial entry number
# glossiness_score_: leaf glossiness, 1 glossy to 5 non-glossy
# seedling_vigor_: seedling vigor, 1 low to 5 high
# leaf_sheath_pigmentation_: leaf sheath pigmentation score (LSP)
# shoot_fly_eggs_: shoot fly eggs per plot
# shoot_fly_eggs_per_plant_: shoot fly eggs per plant
# plants_with_shoot_fly_eggs_: plants in the plot with at least one shoot fly egg
# shoot_fly_egg_incidence_: percent of plants with shoot fly eggs
# deadheart_incidence_: percent deadhearts at 28 days after emergence

carob_script <- function(path) {

"
Phenotypic data of shoot fly introgression lines in sorghum for the year 2014 post-rainy season

Sorghum is a major cereal crop cultivated mainly for food, feed and fodder purpose. But the major constraint in its production is shoot fly, which effects the plant at very early stage resulting in lower yield. Hence, this study was focused to develop shoot fly resistant varieties using host plant resistance mechanism. In this study, three QTLs associated with shoot fly resistance were introgressed into elite cultivars Parbhani Moti (SPV1411) and ICSB29004 using marker assisted backcrossing (MABC) and IS 18551 and 296B are used as resistant and susceptible check respectively. This database includes the phenotypic data generated in the shoot fly screening field for the development of shoot fly resistance elite varieties using marker assisted back crossing at ICRISAT Center, Patancheru . The data on five component traits contributing to shoot fly resistance were recorded in shoot fly screening block using interlard fish meal techniquethe data was recorded for season viz., 2014 post- rainy, at ICRISAT-Patancheru, Hyderabad, India. The component traits recorded in the field trial were leaf surface glossiness (GS), trichome density on adaxial (upper; TDU) and abaxial (lower; TDL) leaf surfaces, seedling vigor (SV), oviposition at 28 DAE (EG28) and dead hearts percentage (DH%). Leaf glossiness was visually scored on a scale of 1–5 scores with 1 being glossy (light green, shiny, narrow, and erect leaves), and 5 -non-glossy (dark green, dull, broad, and drooping leaves). Seedling vigor was recorded on a 1–5 scale, where 1 stands for low vigor and 5 for high vigor. Oviposition non-preference is the number of eggs laid on each seedling from each plot at 14, 21, and 28 DAE. The overall shoot fly damage was calculated by taking the percentage of dead hearts (DH%) on 14, 21, and 28 DAE. To record the trichome density both trichomes on the adaxial leaf surface (TDU) and abaxial surface (TDL) were recorded at 14 DAE. Finally the few best progenies among them are being evaluated in on-farm trials (OFTs).        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/RAMEUC"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 3, minor = 1,
		data_organization = "ICRISAT",
		publication = "doi:10.3389/fpls.2017.01494",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "pest_severity",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 80,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Phenotypic data of shoot fly introgression lines in sorghum for the year 2014 post-rainy season .xlsx"]
	r <- carobiner::read.excel(f, na = "NA")

	variety <- trimws(r$Genotype)
	vtype <- rep(NA_character_, nrow(r))
	vtype[variety == "IS18551"] <- "resistant check"
	vtype[variety == "296B"] <- "susceptible check"
	plants <- as.numeric(r[["No. of plant per plot"]])
	deadheart <- as.numeric(r[["SFDH percent at 28DAE"]])

	d <- data.frame(
		country = "India",
		adm1 = "Telangana",
		location = "ICRISAT Patancheru",
		crop = "sorghum",
		variety = variety,
		variety_type = vtype,
		treatment = variety,
		rep = as.integer(r[["Replication number"]]),
		entry_number_ = as.integer(r[["Entry no"]]),
		plant_density = plants * 10000 / 3,
		glossiness_score_ = as.numeric(r[["Glossy score"]]),
		seedling_vigor_ = as.numeric(r[["Vigor  score"]]),
		leaf_sheath_pigmentation_ = as.numeric(r$LSP),
		shoot_fly_eggs_ = as.numeric(r[["SF Total eggs"]]),
		shoot_fly_eggs_per_plant_ = as.numeric(r[["SF eggs per plant"]]),
		plants_with_shoot_fly_eggs_ = as.numeric(r[["Pl.with SF eggs"]]),
		shoot_fly_egg_incidence_ = as.numeric(r[["Percent of plants with SF eggs"]]),
		deadheart_incidence_ = deadheart,
		pest_severity = as.character(deadheart),
		pest_species = "Atherigona soccata",
		stress = "pests",
		season = "rabi",
		planting_date = "2014-10-15",
		row_spacing = 75,
		plot_length = 2,
		plot_width = 1.5,
		plot_area = 3,
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (78.268764 + 78.270851) / 2,
		latitude = (17.509449 + 17.509528) / 2,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
