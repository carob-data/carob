# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea trial of four Bacillus isolates (SBI-23, VBI-19, VBI-23, VBI-4) plus an uninoculated control. ICRISAT, 2013-14 post-rainy season. Field is BP-13B; a glasshouse run is in the same plant file.
# Wide table is the field harvest at 100 DAS. Stover and grain are t/ha (x1000 = kg/ha). The docx says dry matter is g/m2, but on every harvest row it equals stover + grain in t/ha, so it is treated as t/ha and yield is the dry component.
# Glasshouse at 30 and 90 DAS, and the field at 60 DAS, are additional rows. They have no grain yield. No plant density, so g/plant is not converted to kg/ha.
# Soil (flowering = 60 DAS, harvest = 100 DAS, two replicates, 0-15 cm) is the long table: organic carbon (%), total N and available P (ppm), dehydrogenase (µg TPF g-1 24 h-1), microbial biomass C (µg/g).

## ISSUES
# No coordinates, variety, fertilizer, or irrigation. Georeference ICRISAT BP-13B, Patancheru, Telangana, India. The season predates the state of Telangana.
# Plot size is "4 m x 3 ridges", not m2. Design is not named.
# Glasshouse root length is about 3400-3900 (docx: cm), so it is a scanned total, not a taproot. Root volume is labeled cm2.
# Nodule weight is mg/plant. One glasshouse plant at 30 DAS is missing most scores.
# The draft metadata listed JNTU as well as ICRISAT. This script follows the ICRISAT Dataverse publisher.
# In vitro antagonism and biochemical ratings are not crop measurements.
# nodule_weight_mg_plant_ is mg/plant, not nodule_weight (kg/ha).
# write_files: missing carob_contributor, yield_moisture, latitude, longitude, irrigated, N/P/K, harvest_date; unknown per-plant variables; NA yield on rows that are not the field harvest; DAP, rep and seed_treatment also sit on the soil long table.

## NOT PROCESSED
# Effect of bacterial isolated from compost and rhizosphere soils.xlsx: Isolate, Method, Rhizoctonia bataticola6, Rhizoctonia bataticola24, Rhizoctonia bataticola115, FOC, S.rolfsii. Dual-culture scores.
# Plant growth promoting and biocontrol traits of bacterial isolates.xlsx: Sid, Cellulase, Lipase, Protaeses, HCN, IAA, Glu. In vitro ratings and concentrations.
# Soil column Time ("At flowering" / "At harvesting") matches Days after sowing 60 / 100, so only DAP is kept.
# Metadata docx files: unit notes used above.

## NEW VARIABLES
# nodule_weight_mg_plant_: nodule weight (mg/plant)
# leaf_area_cm2_: leaf area (cm2)
# leaf_weight_g_: leaf weight (g)
# stem_weight_g_plant_: stem weight (g/plant)
# root_length_cm_: root length as recorded (cm)
# root_volume_cm2_: root volume as labeled (cm2)
# pod_weight_g_: pod dry weight (g)
# pod_number_plant_: pods per plant
# dehydrogenase_: dehydrogenase (µg TPF g-1 soil 24 h-1)


carob_script <- function(path) {

"
Data on direct and indirect plant growth-promoting abilities of Bacillus species on Chickpea, isolated from compost and rhizosphere soils

This data set contains replicated observations of selected bacterial isolates (VBI-4, VBI-19, VBI-23 and SBI-23) for their biochemical traits such as production of siderophore, lipase, protease, cellulase, HCN, IAA, and β-1,3-glucanase as per the protocols. Observations for production of siderophore, lipase, and protease were recorded on a 0–4 rating scale, and HCN was recorded on a 0–3 rating scale. The selected bacteria were also tested for their physiological traits such as tolerance to salinity, pH, temperature, and fungicides. In greenhouse conditions, at 30 days after sowing (DAS) nodule number, nodule dry weight, plant height, leaf area, leaf weight, shoot weight, root length, and root volume were recorded. At harvesting stage, stem weight, pod weight, and pod number were recorded. The PGP potential of the bacterial isolates was also tested on chickpea under field conditions during 2013–2014 cropping seasons (post-rainy) at ICRISAT. Plant growth parameters including nodule number, leaf area, leaf weight, pod number, shoot weight, and plant height were taken at 60 DAS and compared with control. At harvested stage the stover yield and grain yield were observed.  Rhizosphere soil samples (0–15 cm) were collected at flowering and harvest and analyzed for total N, available P, and organic carbon (OC) %. Data sets were analysed and published.        Experiment location on Google Map
"

	uri <- "doi:10.21421/D2/6CPVMB"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "ICRISAT",
		publication = "doi:10.1007/s13165-015-0141-3",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "seed_treatment",
		response_vars = "yield;dmy_residue;dmy_total",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.8
	)

	f <- ff[basename(ff) == "Effect of the four selected bacterial isolated from compost and rhizosphere soils .xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	rh <- r[r$Location == "Field" & r$`Days after sowing` == 100, ]
	trt <- trimws(rh$Treatment)

	d <- data.frame(
		trial_id = "Patancheru_2013",
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = "ICRISAT field BP-13B",
		crop = "chickpea",
		treatment = trt,
		seed_treatment = trt,
		inoculant = ifelse(trt == "Control", NA_character_, trt),
		inoculated = trt != "Control",
		rep = as.integer(rh$`Replication number`),
		planting_date = "2013",
		harvest_days = 100,
		DAP = 100L,
		yield = as.numeric(rh$`Grain yield`) * 1000,
		dmy_residue = as.numeric(rh$`Stover yield`) * 1000,
		dmy_total = as.numeric(rh$`Dry matter`) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)

	rp <- r[!(r$Location == "Field" & r$`Days after sowing` == 100), ]
	tp <- trimws(rp$Treatment)
	fieldp <- rp$Location == "Field"
	dplant <- data.frame(
		trial_id = ifelse(fieldp, "Patancheru_2013", "glasshouse_2013"),
		country = "India",
		adm1 = "Telangana",
		location = "Patancheru",
		site = ifelse(fieldp, "ICRISAT field BP-13B", "ICRISAT glasshouse"),
		crop = "chickpea",
		treatment = tp,
		seed_treatment = tp,
		inoculant = ifelse(tp == "Control", NA_character_, tp),
		inoculated = tp != "Control",
		rep = as.integer(rp$`Replication number`),
		planting_date = "2013",
		DAP = as.integer(rp$`Days after sowing`),
		nodule_plant = as.numeric(rp$`Nodule number plant`),
		nodule_weight_mg_plant_ = as.numeric(rp$`Nodule weight`),
		leaf_area_cm2_ = as.numeric(rp$`Leaf area`),
		leaf_weight_g_ = as.numeric(rp$`Leaf weight`),
		stem_weight_g_plant_ = as.numeric(rp$`Stem weight`),
		plant_height = as.numeric(rp$`Plant height`),
		root_length_cm_ = as.numeric(rp$`Root length`),
		root_volume_cm2_ = as.numeric(rp$`Root volume`),
		pod_weight_g_ = as.numeric(rp$`Pod dry weight`),
		pod_number_plant_ = as.numeric(rp$`Pod number`),
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE,
		geo_from_source = FALSE
	)
	d <- carobiner::bindr(d, dplant)

	fs <- ff[basename(ff) == "Effect of the selected isolates on soil minerals.xlsx"]
	rs <- carobiner::read.excel(fs, na="NA")
	dsoil <- data.frame(
		trial_id = "Patancheru_2013",
		seed_treatment = trimws(rs$Treatment),
		rep = as.integer(rs$`Replication number`),
		DAP = as.integer(rs$`Days after sowing`),
		depth_top = 0,
		depth_bottom = 15,
		soil_SOC = as.numeric(rs$`Organic Carbon`),
		soil_N_total = as.numeric(rs$Nitrogen),
		soil_P = as.numeric(rs$Phosphorus),
		dehydrogenase_ = as.numeric(rs$`Dehydogen activity`),
		soil_MBC = as.numeric(rs$`Microbial Biological Carbon`)
	)

	carobiner::write_files(path, meta, d, long=dsoil)
}
