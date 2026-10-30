# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station rice trial (SRI) of six actinomycetes, a consortia, and an uninoculated control. ICRISAT B1 rice field. RCBD, three replications in the description; the yield file has four replicate numbers. Plots are 10 m x 7.5 m. Spacing is 25 cm x 25 cm. The abstract says the inoculum was applied with irrigation, so irrigated is TRUE.
# Grain, shoot and dry matter are g/m2 (x10 = kg/ha). Where all three are present, shoot + grain = dry matter, so yield is the dry component. Shoot is mapped to dmy_residue. Thousand-seed weight is g. Panicles are per plant. Panicle length is cm. SPAD is the chlorophyll index.
# Soil at harvest (0-15 cm, three replicates) is merged by treatment and replicate: organic carbon (%), total N and available P (ppm), microbial biomass C and N (µg/g), dehydrogenase (µg TPF g-1 24 h-1).
# Roots by layer (0-15 and 15-30 cm) are the long table. Root length is m/m2, volume cm3/m2, weight g/m2.

## ISSUES
# No coordinates, season, or variety (described only as a medium-duration rice variety). Georeference ICRISAT B1, Patancheru, Telangana, India.
# Fertilizer rates are not in the files.
# Four yield-file rows have no grain, shoot, or dry matter (CAI-140 rep 2, KAI-180 rep 1, Consortia rep 1, Control rep 3). They are kept because plant height is filled. Yield is then NA.
# "No of tillers" has no unit. Values are 430-705, which is the same range as tillers per m2 in doi:10.21421/D2/LYDOZ7 and is too large for a per-plant count. Stored as tillers_ without a unit.
# Replicate 4 has no soil row. One soil nitrogen value is missing.
# Root weight is not labeled fresh or dry.
# In vitro salinity, pH, temperature and fungicide scores are not crop measurements.
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, N/P/K, planting_date, harvest_date; unknown tiller, panicle and root variables; NA yield on the four incomplete plots; rep and seed_treatment also sit on the root long table.

## NOT PROCESSED
# Effect of salinity, pH, temperature and Fungi.xlsx: Salinity 0-14, Pouvoir hydrogen 5-13, Temperature 20-50, Bavistin, Captan, Radonil, Thiram, Benomyl, Benlate. In vitro growth scores of the six isolates (no consortia, no control).
# Metadata docx files: unit notes used above.

## NEW VARIABLES
# tillers_: tiller count as recorded (unit not stated; see ISSUES)
# panicles_plant_: panicles per plant
# panicle_length_cm_: panicle length (cm)
# dehydrogenase_: dehydrogenase (µg TPF g-1 soil 24 h-1)
# root_length_m_m2_: root length (m/m2)
# root_volume_cm3_m2_: root volume (cm3/m2)
# root_weight_g_m2_: root weight (g/m2)


carob_script <- function(path) {

"
Data on evaluation of Streptomyces strains isolated from herbal vermi compost for their plant growth-promotion traits in Rice

This dataset contains, Actinomycetes (CAI-13, CAI-85, CAI-93, CAI-140, CAI-155 and KAI-180) were evaluated for production of PGP traits including siderophore, chitinase, cellulase, lipase, protease, hydrocyanic acid (HCN) and indole acetic acid (IAA) and β-1,3-glucanase. Siderophore production was estimated as per the protocol. The field experiment was conducted as with a medium duration rice variety at ICRISAT. The experiment was conducted in randomized complete block design (RCBD) with three replications and subplot sizes of 10 m × 7.5 m. Rice was grown by the system of rice intensification (SRI) method. The 12-day-old single rice seedlings were uprooted from the nursery and the roots were dipped in the respective actinomycete broth (containing 108 CFU/ ml) for 45 min and transplanted at a row-to row and a plant-to-plant spacing of 25 cm. The actinomycetes (1000 ml; 108 CFU/ mL) were applied once in two weeks until the flowering stage along with the irrigation. observed for plant height (cm), total tillers, total panicles (per plant), panicle length (cm), filled grain number and weight (g), SPAD reading, stover yield, grain yield, total dry matter and 1000 grain weight. Root samples were also collected from 0–15 and 15–30 cm soil profile and analyzed for root length volume and dry weight Soil samples were collected from 0–15 cm soil profile, at harvest, and analyzed for soil chemistry and biological traits.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/NWZJVP"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "ICRISAT",
		publication = "doi:10.1016/j.micres.2013.09.008",
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield;dmy_residue;dmy_total;plant_height;SPAD;seed_weight",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "Effect of the six actinomycetes and yield of Rice.xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	trt <- trimws(r$Treatment)

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
		rep = as.integer(r$`Replication number`),
		plot_area = 10 * 7.5,
		row_spacing = 25,
		plant_spacing = 25,
		irrigated = TRUE,
		plant_height = as.numeric(r$`Plant height`),
		tillers_ = as.numeric(r$`No of tillers`),
		panicles_plant_ = as.numeric(r$Panicles),
		panicle_length_cm_ = as.numeric(r$`Panicle length`),
		SPAD = as.numeric(r$SPAD),
		seed_weight = as.numeric(r$`Thousand seed weight`),
		yield = as.numeric(r$`Grain weight`) * 10,
		dmy_residue = as.numeric(r$`Shoot weight`) * 10,
		dmy_total = as.numeric(r$`Dry matter`) * 10,
		yield_part = "grain",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	fs <- ff[basename(ff) == "Effect of the six actinomycetes on soil properties.xlsx"]
	rs <- carobiner::read.excel(fs, na="NA")
	s <- data.frame(
		seed_treatment = trimws(rs$Treatment),
		rep = as.integer(rs$`Replication number`),
		soil_N_total = as.numeric(rs$Nitrogen),
		soil_P = as.numeric(rs$Phosphorus),
		soil_SOC = as.numeric(rs$`Organic carbon`),
		soil_MBC = as.numeric(rs$`Microbial boimass C`),
		soil_MBN = as.numeric(rs$`Microbial boimass N`),
		dehydrogenase_ = as.numeric(rs$Dehydrogenase)
	)
	d <- merge(d, s, by=c("seed_treatment", "rep"), all.x=TRUE)

	fr <- ff[basename(ff) == "Effect of the six actinomycetes on the development in Rice.xlsx"]
	rr <- carobiner::read.excel(fr, na="NA")
	depth <- rr$Depth
	long <- data.frame(
		trial_id = "Patancheru_B1",
		seed_treatment = trimws(rr$Treatment),
		rep = as.integer(rr$`Replication number`),
		depth_top = as.numeric(sub("-.*", "", depth)),
		depth_bottom = as.numeric(sub(".*-", "", depth)),
		root_length_m_m2_ = as.numeric(rr$`Root length`),
		root_volume_cm3_m2_ = as.numeric(rr$`Root volume`),
		root_weight_g_m2_ = as.numeric(rr$`Root weight`)
	)

	carobiner::write_files(path, meta, d, long=long)
}
