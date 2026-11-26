# R script for "carob"
# license: GPL (>=3)

## NOTES
# Station chickpea trial of four Bacillus isolates (SBI-23, VBI-19, VBI-23, VBI-4) plus an uninoculated control at ICRISAT Patancheru (field BP-13B and glasshouse), 2013-14 post-rainy season.
# Wide table d has one row per plot (15 field plots with 100 DAS harvest yields + 15 glasshouse plots). Stover and grain yields are in t/ha (x1000 -> kg/ha). On every harvest row, Dry matter equals Stover + Grain in t/ha (dry basis -> yield_isfresh = FALSE, yield_moisture = 0).
# Repeated in-season plant measurements (field 60 DAS; glasshouse 30 and 90 DAS) and rhizosphere soil properties (field 60 and 100 DAS, 0-15 cm, two replicates) are stored in the long table linked via record_id.
# Estimated ICRISAT Patancheru coordinates (17.511, 78.275) added with geo_from_source = FALSE and geo_uncertainty = 2000 m.

## ISSUES
# No variety, fertilizer rates, irrigation statement, or exact calendar harvest date in the files.
# Plot size is "4 m x 3 ridges" (area in m2 not stated) and plant density is not given, so per-plant weights remain per plant.
# Glasshouse root length is ~3400-3900 cm (total scanned root length); root volume is labeled cm2 in source docx.
# Glasshouse rows have no grain yield (yield is NA on those rows).
# write_files: missing carob_contributor, irrigated, K/N/P_fertilizer, harvest_date; NA yield on glasshouse plots; unknown per-plant variables and dehydrogenase_.

## NOT PROCESSED
# Effect of bacterial isolated from compost and rhizosphere soils.xlsx: in vitro dual-culture antagonism scores.
# Plant growth promoting and biocontrol traits of bacterial isolates.xlsx: in vitro biochemical ratings (siderophore, cellulase, lipase, protease, HCN, IAA, glucanase).
# Soil column Time ("At flowering" / "At harvesting") duplicates Days after sowing (60 / 100).
# Metadata docx files: documentation used to verify units.

## NEW VARIABLES
# nodule_weight_mg_plant_: nodule dry weight (mg/plant).
# leaf_area_cm2_: leaf area (cm2/plant).
# leaf_weight_g_: leaf weight (g/plant).
# stem_weight_g_plant_: stem weight (g/plant).
# root_length_cm_: scanned total root length (cm/plant).
# root_volume_cm2_: root volume as labeled in source (cm2/plant).
# pod_weight_g_: pod dry weight (g/plant).
# pod_number_plant_: pods per plant (count).
# dehydrogenase_: soil dehydrogenase activity (ug TPF g-1 soil 24 h-1).


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
		response_vars = "yield;dmy_residue;dmy_total;plant_height;nodule_plant",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 85,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Effect of the four selected bacterial isolated from compost and rhizosphere soils .xlsx"]
	r <- carobiner::read.excel(f, na="NA")
	r$Treatment <- trimws(as.character(r$Treatment))
	r$Location <- trimws(as.character(r$Location))
	r$rep <- as.integer(r[["Replication number"]])
	r$dap <- as.integer(r[["Days after sowing"]])

	rh <- r[r$Location == "Field" & r$dap == 100, ]
	dfield <- data.frame(
		trial_id = "Patancheru_2013",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT field BP-13B",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "chickpea",
		treatment = rh$Treatment,
		seed_treatment = rh$Treatment,
		inoculant = ifelse(rh$Treatment == "Control", NA_character_, rh$Treatment),
		inoculated = rh$Treatment != "Control",
		rep = rh$rep,
		planting_date = "2013",
		harvest_days = 100,
		yield = as.numeric(rh[["Grain yield"]]) * 1000,
		dmy_residue = as.numeric(rh[["Stover yield"]]) * 1000,
		dmy_total = as.numeric(rh[["Dry matter"]]) * 1000,
		yield_part = "seed",
		yield_isfresh = FALSE,
		yield_moisture = 0,
		on_farm = FALSE,
		is_survey = FALSE
	)

	rgh <- unique(r[r$Location != "Field", c("Treatment", "rep")])
	dgh <- data.frame(
		trial_id = "glasshouse_2013",
		country = "India",
		adm1 = "Telangana",
		adm2 = "Sangareddy",
		location = "Patancheru",
		site = "ICRISAT glasshouse",
		latitude = 17.511,
		longitude = 78.275,
		geo_from_source = FALSE,
		geo_uncertainty = 2000,
		crop = "chickpea",
		treatment = rgh$Treatment,
		seed_treatment = rgh$Treatment,
		inoculant = ifelse(rgh$Treatment == "Control", NA_character_, rgh$Treatment),
		inoculated = rgh$Treatment != "Control",
		rep = rgh$rep,
		planting_date = "2013",
		yield_part = "seed",
		yield_isfresh = FALSE,
		on_farm = FALSE,
		is_survey = FALSE
	)

	d <- carobiner::bindr(dfield, dgh)
	d$record_id <- seq_len(nrow(d))

	dkey <- paste(d$trial_id, d$seed_treatment, d$rep, sep="|")

	# Repeated plant measurements (field 60 DAS; glasshouse 30 and 90 DAS) in long
	rp <- r[!(r$Location == "Field" & r$dap == 100), ]
	ptrial <- ifelse(rp$Location == "Field", "Patancheru_2013", "glasshouse_2013")
	l_plant <- data.frame(
		record_id = d$record_id[match(paste(ptrial, rp$Treatment, rp$rep, sep="|"), dkey)],
		DAP = rp$dap,
		nodule_plant = as.numeric(rp[["Nodule number plant"]]),
		nodule_weight_mg_plant_ = as.numeric(rp[["Nodule weight"]]),
		leaf_area_cm2_ = as.numeric(rp[["Leaf area"]]),
		leaf_weight_g_ = as.numeric(rp[["Leaf weight"]]),
		stem_weight_g_plant_ = as.numeric(rp[["Stem weight"]]),
		plant_height = as.numeric(rp[["Plant height"]]),
		root_length_cm_ = as.numeric(rp[["Root length"]]),
		root_volume_cm2_ = as.numeric(rp[["Root volume"]]),
		pod_weight_g_ = as.numeric(rp[["Pod dry weight"]]),
		pod_number_plant_ = as.numeric(rp[["Pod number"]])
	)

	f1 <- ff[basename(ff) == "Effect of the selected isolates on soil minerals.xlsx"]
	r1 <- carobiner::read.excel(f1, na="NA")
	l_soil <- data.frame(
		record_id = d$record_id[match(paste("Patancheru_2013", trimws(as.character(r1$Treatment)), as.integer(r1[["Replication number"]]), sep="|"), dkey)],
		DAP = as.integer(r1[["Days after sowing"]]),
		depth_top = 0,
		depth_bottom = 15,
		soil_SOC = as.numeric(r1[["Organic Carbon"]]),
		soil_N_total = as.numeric(r1$Nitrogen),
		soil_P = as.numeric(r1$Phosphorus),
		dehydrogenase_ = as.numeric(r1[["Dehydogen activity"]]),
		soil_MBC = as.numeric(r1[["Microbial Biological Carbon"]])
	)

	dlong <- carobiner::bindr(l_plant, l_soil)
	rownames(d) <- NULL
	rownames(dlong) <- NULL

	carobiner::write_files(path, meta, d, long=dlong)
}
