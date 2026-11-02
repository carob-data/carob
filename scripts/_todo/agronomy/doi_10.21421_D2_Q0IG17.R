# R script for "carob"
# license: GPL (>=3)

## NOTES
# Field test of three Streptomyces strains and a control on rice grown by SRI at ICRISAT, Patancheru. Replication is given; the design is not named.
# Grain and stover are g/m2 in the morphology metadata, multiplied by 10 to kg/ha. Stover is not described as dry, so it is fwy_residue. Root dry weight is g/m2, multiplied by 10 to kg/ha (dmy_roots).
# Soil metadata: P and N in ppm, organic carbon in percent, actinomycetes as log10. At 60 DAS (stage "Vegitative") the Nitrogen column is about 1.5 and OC is about 2400, the reverse of the harvest rows and of the stated units. Those two columns were swapped for the 60 DAS rows only.
# Harvest soil is soil_SOC, soil_N_total and soil_P. The 60 DAS soil is kept separately. Depth 0-15 cm is from the dataset description. No year or coordinates are in this dataset.

## ISSUES
# The vegetative N/OC swap should be checked against the paper (doi:10.1080/09583157.2012.719151).
# Grain yield is missing on some plots that still have height, tillers or roots. Those rows were kept.
# Rice variety, year, fertilizer and irrigation are not in the files. Patancheru is not georeferenced here.
# Root length unit in the metadata is the string "mm_2", which is ambiguous, so the values were not converted.
# Available-P method is not stated. Tillers are per m2, not a plant density.
# The seed file is a laboratory germination assay (roots and shoots of a few centimetres, 10 replicates) and is not a field plot.
# write_files: missing carob_contributor; missing latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date; unknown variables tiller_m2_, panicle_length_, panicle_weight_, filled_grain_, filled_grain_weight_, root_length_, root_volume_, soil_SOC_vegetative_, soil_N_vegetative_, soil_P_vegetative_, actinomycetes_log10_, actinomycetes_log10_vegetative_; NA detected in yield.

## NOT PROCESSED
# Effect of Streptomyces strains on Rice seed.xlsx: laboratory germination (%), root length (cm) and shoot length (cm) for CAI-21, CAI-26, MMA-32 and a control. Not field measurements.
# Soil sheet, Days after sowing: 60 and 120, used only to separate vegetative and harvest samples.
# Morphology, values already used to compute yield, fwy_residue and dmy_roots.

## NEW VARIABLES
# tiller_m2_: tillers per m2
# panicle_length_: panicle length, cm
# panicle_weight_: panicle weight, g/plant
# filled_grain_: filled grains per plant
# filled_grain_weight_: filled-grain weight, g/plant
# root_length_: root length as recorded; metadata unit string is "mm_2"
# root_volume_: root volume, cm3/m2
# soil_SOC_vegetative_: organic carbon at 60 DAS, percent, after the column swap
# soil_N_vegetative_: nitrogen at 60 DAS, mg/kg, after the column swap
# soil_P_vegetative_: available P at 60 DAS, mg/kg
# actinomycetes_log10_: actinomycete count at harvest, log10
# actinomycetes_log10_vegetative_: actinomycete count at 60 DAS, log10

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
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.7
	)

	f2 <- ff[basename(ff) == "Effect of Streptomyces strains on soil chemical.xlsx"]
	f3 <- ff[basename(ff) == "Effect of Streptomyces strains on the morphology.xlsx"]

	r2 <- carobiner::read.excel(f2, na="NA")
	r3 <- carobiner::read.excel(f3, na="NA")

	trt <- trimws(r3$Treatment)
	d <- data.frame(
		treatment = trt,
		rep = as.integer(r3$`Replication number`),
		plant_height = as.numeric(r3$`Plant height`),
		tiller_m2_ = as.numeric(r3$`No of tillers`),
		panicle_length_ = as.numeric(r3$`Panicle length`),
		panicle_weight_ = as.numeric(r3$`Panicle weight`),
		filled_grain_ = as.numeric(r3$`Filled grain`),
		filled_grain_weight_ = as.numeric(r3$`Filled grain weight`),
		seed_weight = as.numeric(r3$`Thousand seed weight`),
		yield = as.numeric(r3$`Grain yield`) * 10,
		fwy_residue = as.numeric(r3$`Stover yield`) * 10,
		root_length_ = as.numeric(r3$`Root length`),
		root_volume_ = as.numeric(r3$`Root volume`),
		dmy_roots = as.numeric(r3$`Root dry weight`) * 10,
		inoculated = trt != "Control",
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		crop = "rice",
		yield_part = "grain",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		trial_id = "Patancheru_rice",
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE,
		yield_moisture = NA_real_,
		yield_isfresh = NA,
		depth_top = 0,
		depth_bottom = 15
	)

	veg <- r2$Stage == "Vegitative"
	# 60 DAS Nitrogen and OC columns are transposed relative to the stated units
	oc <- as.numeric(r2$OC)
	nn <- as.numeric(r2$Nitrogen)
	oc[veg] <- as.numeric(r2$Nitrogen[veg])
	nn[veg] <- as.numeric(r2$OC[veg])

	sH <- data.frame(
		treatment = trimws(r2$Treatment[!veg]),
		rep = as.integer(r2$`Replication number`[!veg]),
		soil_SOC = oc[!veg],
		soil_N_total = nn[!veg],
		soil_P = as.numeric(r2$Phosphorus[!veg]),
		actinomycetes_log10_ = as.numeric(r2$Actinomycetes[!veg])
	)
	d <- merge(d, sH, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	sV <- data.frame(
		treatment = trimws(r2$Treatment[veg]),
		rep = as.integer(r2$`Replication number`[veg]),
		soil_SOC_vegetative_ = oc[veg],
		soil_N_vegetative_ = nn[veg],
		soil_P_vegetative_ = as.numeric(r2$Phosphorus[veg]),
		actinomycetes_log10_vegetative_ = as.numeric(r2$Actinomycetes[veg])
	)
	d <- merge(d, sV, by=c("treatment", "rep"), all.x=TRUE, sort=FALSE)

	carobiner::write_files(path, meta, d)
}
