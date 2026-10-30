# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station rice trial of five Streptomyces seed/plant inoculants (CAI-17, CAI-68, CAI-78, KAI-26, KAI-27) plus an uninoculated control. ICRISAT B1 rice field, Patancheru.
# "Field harvest analysis.xlsx" is the plot table. Grain, stover and dry matter are g/m2 (x10 = kg/ha). On every row grain + stover = dry matter, so yield is the dry component. Total tillers are per m2. Panicles are per plant. Panicle length is cm.
# Soil on the same row: organic carbon (%), total N and available P (ppm = mg/kg), microbial biomass C and N (µg/g), dehydrogenase (µg TPF g-1 24 h-1).
# "Effect of Streptomyces strains on the morphology-3-4-6.xlsx" has the same values (the replication column name differs by a space) and is not read again.
# Root length, volume and weight by layer (0-15 and 15-30 cm) are the long table.

## ISSUES
# No coordinates, season, variety, fertilizer, or irrigation statement. Georeference ICRISAT B1 rice field, Patancheru, Telangana, India.
# Publication DOI is 10.1139/cjm-2013-0287. The paper was not used to fill management that is absent from the files.
# Root length unit in the docx is "mm−2", which is ambiguous, and the numbers (about 400-8000) match "Effect of the six actinomycetes on the development in Rice.xlsx" in doi:10.21421/D2/NWZJVP, whose docx says m m−2. Stored as recorded under root_length_mm_m2_.
# Filled grain count and filled grain weight have no per-panicle or per-plant statement. Some panicle and grain-count cells are empty.
# Design is not named in this record.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, N/P/K, planting_date, harvest_date; unknown panicle, tiller, grain and root variables; rep and seed_treatment also sit on the root long table.

## NOT PROCESSED
# Effect of Streptomyces strains on the morphology-3-4-6.xlsx: duplicate of Field harvest analysis.xlsx.
# Effect of  temperature tolerance to antibiotics.xlsx: Actinomycetes, Temperature, Reading. In vitro growth scores.
# Effect of  tolerance to antibiotics.xlsx: Treatment (antibiotic), Antibiotics (concentration). In vitro.
# Effect of  tolerance to fungicides.xlsx: Treatment (fungicide), Fungicide (score). In vitro.
# Effect of tolerance to pH.xlsx: Treatment (pH), Pouvair Hydrogene (score). In vitro.
# Effect of tolerance to salinity.xlsx: Parts per million, Salinity (score). In vitro. The "ppm" column is the NaCl level, not soil N.
# Metadata docx files: unit notes used above.

## NEW VARIABLES
# panicles_plant_: panicles per plant
# panicle_length_cm_: panicle length (cm)
# filled_grain_count_: filled grain count, unit not stated
# filled_grain_weight_g_: filled grain weight (g), basis not stated
# tillers_m2_: tillers per m2
# dehydrogenase_: dehydrogenase (µg TPF g-1 soil 24 h-1)
# root_length_mm_m2_: root length as recorded; docx unit mm−2 (see ISSUES)
# root_volume_cm3_m2_: root volume (cm3/m2)
# root_weight_g_m2_: root weight (g/m2), fresh vs dry not stated


carob_script <- function(path) {

"
Data on evaluation of Streptomyces spp. for their plant-growth promotion traits in Rice

The dataset contains five strains of Streptomyces (CAI-17, CAI-68, CAI-78, KAI-26, and KAI-27) were previously reported to have potential for charcoal rot control and plant growth promotion (PGP) in sorghum. In this study, those 5 Streptomyces strains were characterized for their enzymatic activities and evaluated for their PGP capabilities on rice. All the Streptomyces strains were able to produce lipase and β-1,3-glucanase; grew in NaCl (up to 8%), at pH 5–13, and at temperatures 20–40 °C; and were resistant to ampicillin, sensitive to nalidixic acid, and highly sensitive to chloramphenicol, kanamycin, streptomycin, and tetracycline. They were highly tolerant to the fungicide bavistin but were highly sensitive to benlate, benomyl, and radonil. When evaluated on Rice in the field, Streptomyces significantly enhanced tiller and panicle numbers, stover and grain yields, dry matter, root length, volume and dry weight, compared with the control. In the rhizosphere at harvest, microbial biomass carbon and nitrogen, dehydrogenase activity, total nitrogen, available phosphorus, and % organic carbon were also found significantly higher in Streptomyces-treated plots than in the control plots.         Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/LYDOZ7"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = "doi:10.1139/cjm-2013-0287",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield;dmy_residue;dmy_total;plant_height",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "Field harvest analysis.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(r$`Treatment`)

	d <- data.frame(
		trial_id = "Patancheru_B1",
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = "ICRISAT field B1",
		crop = "rice",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r$`Replication  number`),
		plant_height = as.numeric(r$`Plant height`),
		panicles_plant_ = as.numeric(r$`Total panicle`),
		panicle_length_cm_ = as.numeric(r$`Panicle length`),
		filled_grain_count_ = as.numeric(r$`Filled grain count`),
		filled_grain_weight_g_ = as.numeric(r$`Filled grain weight`),
		tillers_m2_ = as.numeric(r$`Total tillers`),
		yield = as.numeric(r$`Grain yield`) * 10,
		dmy_residue = as.numeric(r$`Stover yield`) * 10,
		dmy_total = as.numeric(r$`Dry matter`) * 10,
		soil_MBC = as.numeric(r$MBC),
		soil_MBN = as.numeric(r$MBN),
		dehydrogenase_ = as.numeric(r$Dehydrogenase),
		soil_N_total = as.numeric(r$Nitrogen),
		soil_P = as.numeric(r$Phosphorus),
		soil_SOC = as.numeric(r$`Organic carbon`),
		yield_part = "grain",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	fr <- ff[basename(ff) == "Effect of Streptomyces strains root development of rice.xlsx"]
	rr <- carobiner::read.excel(fr, na="NA")
	depth <- rr$Depth
	long <- data.frame(
		trial_id = "Patancheru_B1",
		seed_treatment = trimws(rr$Treatment),
		rep = as.integer(rr$`Replication number`),
		depth_top = as.numeric(sub("-.*", "", depth)),
		depth_bottom = as.numeric(sub(".*-", "", depth)),
		root_length_mm_m2_ = as.numeric(rr$`Root length`),
		root_volume_cm3_m2_ = as.numeric(rr$`Root volume`),
		root_weight_g_m2_ = as.numeric(rr$`Root weight`)
	)

	carobiner::write_files(path, meta, d, long=long)
}
