# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station rice trial of five Streptomyces seed/plant inoculants (CAI-17, CAI-68, CAI-78, KAI-26, KAI-27) plus an uninoculated control on ICRISAT field B1, Patancheru, India.
# Field harvest analysis.xlsx: grain, stover, and total dry matter are in g/m2 (x10 -> kg/ha). Grain + stover = dry matter on every row -> dry basis (yield_isfresh = FALSE, yield_moisture = 0). Total tillers are per m2; panicles per plant; panicle length in cm.
# Harvest rhizosphere soil (0-15 cm) sits on the same rows: soil_SOC (%), soil_N_total and soil_P (ppm = mg/kg), soil_MBC and soil_MBN (ug/g), dehydrogenase (ug TPF g-1 24 h-1).
# Root length, volume, and weight by depth layer (0-15 and 15-30 cm) are stored in the long table linked via record_id.
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.
# Effect of Streptomyces strains on the morphology-3-4-6.xlsx duplicates Field harvest analysis.xlsx and is not read twice.

## ISSUES
# No season/year, variety, fertilizer rates, irrigation statement, or experimental design in the files.
# Root length docx unit says "mm-2", which is ambiguous; numeric range (400-8000) matches m/m2 in sister dataset doi:10.21421/D2/NWZJVP. Stored as recorded under root_length_m_m2_.
# Filled grain count and weight have no stated per-panicle vs per-plant basis; some cells are NA.
# write_files: missing carob_contributor, irrigated, K/N/P_fertilizer, planting_date, harvest_date; unknown panicle, tiller, grain, dehydrogenase, and root variables.

## NOT PROCESSED
# Effect of Streptomyces strains on the morphology-3-4-6.xlsx: duplicate of Field harvest analysis.xlsx.
# In vitro tolerance assays (temperature, antibiotics, fungicides, pH, salinity): lab isolates, not crop field records.
# Metadata docx files: unit notes used above.

## NEW VARIABLES
# panicles_plant_: panicles per plant.
# panicle_length_cm_: panicle length (cm).
# filled_grain_count_: filled grain count (basis not stated).
# filled_grain_weight_g_: filled grain weight (g; basis not stated).
# tillers_m2_: tillers per m2.
# dehydrogenase_: soil dehydrogenase activity (ug TPF g-1 soil 24 h-1).
# root_length_m_m2_: root length per area as recorded (docx writes mm-2; matches m/m2 in sister dataset).
# root_volume_cm3_m2_: root volume (cm3/m2).
# root_weight_g_m2_: root weight (g/m2).


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "Field harvest analysis.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(as.character(r$Treatment))

	d <- data.frame(
		record_id = seq_len(nrow(r)),
		trial_id = "Patancheru_B1",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT field B1",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "rice",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(r[["Replication  number"]]),
		plant_height = as.numeric(r[["Plant height"]]),
		panicles_plant_ = as.numeric(r[["Total panicle"]]),
		panicle_length_cm_ = as.numeric(r[["Panicle length"]]),
		filled_grain_count_ = as.numeric(r[["Filled grain count"]]),
		filled_grain_weight_g_ = as.numeric(r[["Filled grain weight"]]),
		tillers_m2_ = as.numeric(r[["Total tillers"]]),
		yield = as.numeric(r[["Grain yield"]]) * 10,
		dmy_residue = as.numeric(r[["Stover yield"]]) * 10,
		dmy_total = as.numeric(r[["Dry matter"]]) * 10,
		soil_MBC = as.numeric(r$MBC),
		soil_MBN = as.numeric(r$MBN),
		dehydrogenase_ = as.numeric(r$Dehydrogenase),
		soil_N_total = as.numeric(r$Nitrogen),
		soil_P = as.numeric(r$Phosphorus),
		soil_SOC = as.numeric(r[["Organic carbon"]]),
		yield_part = "grain",
		yield_isfresh = FALSE,
		yield_moisture = 0,
		on_farm = FALSE,
		is_survey = FALSE
	)

	f1 <- ff[basename(ff) == "Effect of Streptomyces strains root development of rice.xlsx"]
	r1 <- carobiner::read.excel(f1, na="NA")
	dkey <- paste(d$seed_treatment, d$rep, sep="|")
	rkey <- paste(trimws(as.character(r1$Treatment)), as.integer(r1[["Replication number"]]), sep="|")
	depth <- as.character(r1$Depth)
	dlong <- data.frame(
		record_id = d$record_id[match(rkey, dkey)],
		depth_top = as.numeric(sub("-.*", "", depth)),
		depth_bottom = as.numeric(sub(".*-", "", depth)),
		root_length_m_m2_ = as.numeric(r1[["Root length"]]),
		root_volume_cm3_m2_ = as.numeric(r1[["Root volume"]]),
		root_weight_g_m2_ = as.numeric(r1[["Root weight"]])
	)

	carobiner::write_files(path, meta, d, long=dlong)
}
