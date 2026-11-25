# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station rice trial (SRI) of six actinomycetes (CAI-13, CAI-85, CAI-93, CAI-140, CAI-155, KAI-180), a consortia, and an uninoculated control on ICRISAT field B1, Patancheru, India. RCBD; plots 10 m x 7.5 m; 25 cm x 25 cm spacing; irrigated = TRUE (inoculum applied with irrigation).
# Grain, shoot, and dry matter are in g/m2 (x10 -> kg/ha). Shoot + grain = dry matter -> dry basis (yield_isfresh = FALSE, yield_moisture = 0). Shoot is mapped to dmy_residue; 1000-seed weight to seed_weight; SPAD to SPAD.
# Harvest rhizosphere soil (0-15 cm, reps 1-3) is merged onto d: soil_SOC (%), soil_N_total and soil_P (ppm = mg/kg), soil_MBC and soil_MBN (ug/g), dehydrogenase (ug TPF g-1 24 h-1).
# Root measurements by depth (0-15 and 15-30 cm) are stored in the long table linked via record_id.
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# No season/year, variety name, fertilizer rates, or planting/harvest dates in the files.
# Four yield-file rows have missing grain/shoot/dry matter (CAI-140 rep 2, KAI-180 rep 1, Consortia rep 1, Control rep 3); kept as recorded (NA yield).
# "No of tillers" has no stated unit (values 430-705 match tillers/m2 in sister dataset LYDOZ7; stored as tillers_).
# Replicate 4 has no soil measurements; one soil N value is missing. Root weight is not labeled fresh vs dry.
# write_files: missing carob_contributor, K/N/P_fertilizer, planting_date, harvest_date; NA yield on 4 incomplete rows; unknown tiller, panicle, dehydrogenase, and root variables.

## NOT PROCESSED
# Effect of salinity, pH, temperature and Fungi.xlsx: in vitro tolerance assays of isolates.
# Metadata docx files: documentation used to verify units.

## NEW VARIABLES
# tillers_: tiller count as recorded (likely per m2; unit not stated in source).
# panicles_plant_: panicles per plant.
# panicle_length_cm_: panicle length (cm).
# dehydrogenase_: soil dehydrogenase activity (ug TPF g-1 soil 24 h-1).
# root_length_m_m2_: root length per area (m/m2).
# root_volume_cm3_m2_: root volume per area (cm3/m2).
# root_weight_g_m2_: root weight per area (g/m2).


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
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0.2
	)

	f <- ff[basename(ff) == "Effect of the six actinomycetes and yield of Rice.xlsx"]
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
		rep = as.integer(r[["Replication number"]]),
		plot_area = 10 * 7.5,
		row_spacing = 25,
		plant_spacing = 25,
		irrigated = TRUE,
		plant_height = as.numeric(r[["Plant height"]]),
		tillers_ = as.numeric(r[["No of tillers"]]),
		panicles_plant_ = as.numeric(r$Panicles),
		panicle_length_cm_ = as.numeric(r[["Panicle length"]]),
		SPAD = as.numeric(r$SPAD),
		seed_weight = as.numeric(r[["Thousand seed weight"]]),
		yield = as.numeric(r[["Grain weight"]]) * 10,
		dmy_residue = as.numeric(r[["Shoot weight"]]) * 10,
		dmy_total = as.numeric(r[["Dry matter"]]) * 10,
		yield_part = "grain",
		yield_isfresh = FALSE,
		yield_moisture = 0,
		on_farm = FALSE,
		is_survey = FALSE
	)

	f1 <- ff[basename(ff) == "Effect of the six actinomycetes on soil properties.xlsx"]
	r1 <- carobiner::read.excel(f1, na="NA")
	s <- data.frame(
		seed_treatment = trimws(as.character(r1$Treatment)),
		rep = as.integer(r1[["Replication number"]]),
		soil_N_total = as.numeric(r1$Nitrogen),
		soil_P = as.numeric(r1$Phosphorus),
		soil_SOC = as.numeric(r1[["Organic carbon"]]),
		soil_MBC = as.numeric(r1[["Microbial boimass C"]]),
		soil_MBN = as.numeric(r1[["Microbial boimass N"]]),
		dehydrogenase_ = as.numeric(r1$Dehydrogenase)
	)
	d <- merge(d, s, by=c("seed_treatment", "rep"), all.x=TRUE, sort=FALSE)
	d <- d[order(d$record_id), ]

	f2 <- ff[basename(ff) == "Effect of the six actinomycetes on the development in Rice.xlsx"]
	r2 <- carobiner::read.excel(f2, na="NA")
	dkey <- paste(d$seed_treatment, d$rep, sep="|")
	rkey <- paste(trimws(as.character(r2$Treatment)), as.integer(r2[["Replication number"]]), sep="|")
	depth <- as.character(r2$Depth)
	dlong <- data.frame(
		record_id = d$record_id[match(rkey, dkey)],
		depth_top = as.numeric(sub("-.*", "", depth)),
		depth_bottom = as.numeric(sub(".*-", "", depth)),
		root_length_m_m2_ = as.numeric(r2[["Root length"]]),
		root_volume_cm3_m2_ = as.numeric(r2[["Root volume"]]),
		root_weight_g_m2_ = as.numeric(r2[["Root weight"]])
	)
	rownames(d) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
