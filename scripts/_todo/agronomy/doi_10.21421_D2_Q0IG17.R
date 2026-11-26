# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field trial of three Streptomyces strains (CAI-21, CAI-26, MMA-32) plus an uninoculated control on SRI rice at ICRISAT Patancheru, India.
# Grain yield, stover yield, and root dry weight are in g/m2 (x10 -> kg/ha). Stover is not explicitly labeled dry, so it is mapped to fwy_residue; root dry weight is mapped to dmy_roots.
# Rhizosphere soil (0-15 cm) was sampled at vegetative stage (60 DAS) and harvest (120 DAS); both timepoints are stored in the long table linked via record_id. At 60 DAS ("Vegitative"), the Nitrogen column (~1.5) and OC column (~2400) are transposed relative to harvest rows and stated units (ppm vs %) and are swapped back for those rows.
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# Confirm the 60 DAS Nitrogen/OC column swap against the paper (doi:10.1080/09583157.2012.719151).
# Grain yield is NA on some plots that have plant height, tillers, or roots; kept as recorded.
# Rice variety, year/dates, fertilizer rates, irrigation statement, yield moisture, and experimental design are not in the files.
# Root length metadata unit string is "mm_2" (ambiguous; kept as recorded under root_length_).
# write_files: missing carob_contributor, yield_moisture, irrigated, K/N/P_fertilizer, planting_date, harvest_date; NA yield; unknown tiller, panicle, filled-grain, root, and actinomycete variables.

## NOT PROCESSED
# Effect of Streptomyces strains on Rice seed.xlsx: laboratory seed germination assay (10 replicates), not field plots.

## NEW VARIABLES
# tiller_m2_: tillers per m2.
# panicle_length_: panicle length (cm).
# panicle_weight_: panicle weight (g/plant).
# filled_grain_: filled grains per plant.
# filled_grain_weight_: filled-grain weight (g/plant).
# root_length_: root length as recorded (metadata unit string "mm_2").
# root_volume_: root volume (cm3/m2).
# actinomycetes_log10_: rhizosphere actinomycete count (log10 CFU/g soil) at 60 and 120 DAS in long table.


carob_script <- function(path) {

"
Data on plant growth-promoting traits of Streptomyces with biocontrol potential isolated from herbal vermicompost

This dataset contains the three Streptomyces (CAI-21, CAI-26 and MMA-32) were characterised for their physiological traits (tolerance of salinity, temperature, pH and resistance to antibiotics) and further evaluated in the field for their PGP of rice, grown by a system of rice intensification methods. When evaluated for their PGP activity on seedlings of   The crop was harvested manually and observed for plant height, number of tillers (m_2), panicle length, panicle weight, filled grain number, filled grain weight, 1000 seed weight, grain yield, stover yield and total dry matter. Root samples were collected from the top 15 cm soil profile and analysed for root length density, volume and dry weight. Soil samples were collected from 0 to 15 cm soil profile at 75 DAT and at harvesting. These were analysed for soil chemistry (% organic carbon, available phosphorous and total nitrogen as per the protocols and biological analysis (dehydrogenase activity, microbial biomass nitrogen and microbial biomass carbon. For actinomycete counts, the soil samples (10 g) were serially suspended in 90 ml of physiological saline (0.85% NaCl) in a flask and placed on an orbital shaker (at 100 rpm with 25 mm throw) at room temperature (28 to O28C) for 1 h. At the end of shaking, the soil samples were serially diluted 10-fold up to 106 dilution with physiological saline.         Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/Q0IG17"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "ICRISAT",
		publication = "doi:10.1080/09583157.2012.719151",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment;inoculated",
		response_vars = "yield;fwy_residue;plant_height;dmy_roots",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0
	)

	f2 <- ff[basename(ff) == "Effect of Streptomyces strains on soil chemical.xlsx"]
	f3 <- ff[basename(ff) == "Effect of Streptomyces strains on the morphology.xlsx"]

	r2 <- carobiner::read.excel(f2, na="NA")
	r3 <- carobiner::read.excel(f3, na="NA")

	trt <- trimws(as.character(r3$Treatment))
	d <- data.frame(
		record_id = seq_len(nrow(r3)),
		treatment = trt,
		rep = as.integer(r3[["Replication number"]]),
		plant_height = as.numeric(r3[["Plant height"]]),
		tiller_m2_ = as.numeric(r3[["No of tillers"]]),
		panicle_length_ = as.numeric(r3[["Panicle length"]]),
		panicle_weight_ = as.numeric(r3[["Panicle weight"]]),
		filled_grain_ = as.numeric(r3[["Filled grain"]]),
		filled_grain_weight_ = as.numeric(r3[["Filled grain weight"]]),
		seed_weight = as.numeric(r3[["Thousand seed weight"]]),
		yield = as.numeric(r3[["Grain yield"]]) * 10,
		fwy_residue = as.numeric(r3[["Stover yield"]]) * 10,
		root_length_ = as.numeric(r3[["Root length"]]),
		root_volume_ = as.numeric(r3[["Root volume"]]),
		dmy_roots = as.numeric(r3[["Root dry weight"]]) * 10,
		inoculated = trt != "Control",
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		crop = "rice",
		yield_part = "grain",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		trial_id = "Patancheru_rice",
		on_farm = FALSE,
		is_survey = FALSE
	)

	veg <- trimws(as.character(r2$Stage)) == "Vegitative"
	# 60 DAS Nitrogen and OC columns are transposed relative to the stated units (ppm vs %)
	oc <- as.numeric(r2$OC)
	nn <- as.numeric(r2$Nitrogen)
	oc[veg] <- as.numeric(r2$Nitrogen[veg])
	nn[veg] <- as.numeric(r2$OC[veg])

	dkey <- paste(d$treatment, d$rep, sep="|")
	skey <- paste(trimws(as.character(r2$Treatment)), as.integer(r2[["Replication number"]]), sep="|")
	dlong <- data.frame(
		record_id = d$record_id[match(skey, dkey)],
		DAP = as.integer(r2[["Days after sowing"]]),
		growth_stage = ifelse(veg, "vegetative", "harvest"),
		depth_top = 0,
		depth_bottom = 15,
		soil_SOC = oc,
		soil_N_total = nn,
		soil_P = as.numeric(r2$Phosphorus),
		actinomycetes_log10_ = as.numeric(r2$Actinomycetes)
	)

	carobiner::write_files(path, meta, d, long=dlong)
}
