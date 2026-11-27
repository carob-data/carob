# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Dataverse metadata/abstract describe a 2017 groundnut rosette disease trial in Malawi, but the uploaded xlsx/docx files are a pigeonpea regional variety trial (ICEAP/ICPL lines, 119-200 cm tall, Fusarium wilt score). Confirm with ICRISAT or move to _pending before publishing.
# Plot area is not reported, so grain_weight_plot_ and plant_stand_ cannot be converted to kg/ha or plants/ha.
# Replication 1 GWPT_Wgh_g values (2.601-10.655) are in kg/plot while Replications 2-3 (2225-10439) are in g/plot for the same varieties; Rep 1 was multiplied by 1000 to match g/plot (verify).
# Fusarium wilt score (1-3) is 1 on every row (no variation).
# Dataverse dates (2016-2017) were omitted because they may refer to the groundnut rosette trial rather than this pigeonpea file.

## NEW
# seeds_per_pod_: seeds per pod (count).
# plant_stand_: final plant stand per plot (plants/plot; plot area unknown).
# grain_weight_plot_: grain weight per plot (g/plot; Rep 1 converted from kg/plot).
# disease: fusarium wilt

## MISSING
# yield, yield_part, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date

## NOTES
# Replicated pigeonpea variety trial (7 ICEAP/ICPL entries x 3 reps) attributed to Chitedze Research Station, Lilongwe, Malawi.
# 100-seed weight (10.89-13.87 g; description mistakenly says grams/plot) is multiplied by 10 for 1000-seed weight.
# Coordinates are the midpoint of the ICRISAT Dataverse bounding box for Chitedze.

## NOT PROCESSED
# xlsx, Treatment: numeric codes 1-7 corresponding 1-to-1 with Variety Name.

carob_script <- function(path) {

"
Reaction of selected medium duration breeding lines to groundnut rosette disease in Malawi during the year 2017

Research work carried out to identify promising groundnut breeding lines with resistance to groundnut rosette disease. The experiment was carried out in Chitedze research station during the 2016-2017 growing season. The infector row technique was used to induce a high disease pressure. At sowing time, one infector row of susceptible cultivar was sown after every two contiguous rows of test lines, such that every test row was adjacent to one infector row. Six weeks before planting on the field, a large number of seedlings were raised in the green house and inoculated with Groundnut Rosette Virus (GRV), using a greenhouse culture of viruliferous aphids, which had been reared on GRV-infected plants. These were then transplanted into the infector rows at 1.5-2 m spacing.  Each test line was assessed for disease incidence at 8 weeks and 12 weeks after sowing. The total number of plants in each plot and the number of plants showing rosette symptoms with severe stunting are counted and percentage of disease incidence computed. Plants showing severe symptoms are stunted and bushy in appearance due to reduced internodes length. Leaves of the infected plants are reduced in size and the plants do not produce pods. Breeding lines were considered resistant when no susceptible plants were found within the complete entry (0% incidence), highly susceptible when no resistant plants were present (100% incidence) and moderately resistant when at least one plant within the entry has mild symptoms (< 10% incidence).
"

	uri <- "doi:10.21421/D2/UQE8UE"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major = 1, minor = 0,
		data_organization = "ICRISAT",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "disease_severity;plant_height;flowering_days;maturity_days;seed_weight",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 70,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "Data file of Regional variety evaluation data..xlsx"]
	r <- carobiner::read.excel(f)

	variety <- trimws(r[["Variety Name"]])
	rep <- as.integer(r[["Replication number"]])
	# Rep 1 values (2.601-10.655) are in kg/plot while Reps 2-3 (2225-10439) are in g/plot
	gw <- as.numeric(r$GWPT_Wgh_g)
	gw <- ifelse(rep == 1L, gw * 1000, gw)

	d <- data.frame(
		country = "Malawi",
		adm1 = "Lilongwe",
		location = "Chitedze",
		crop = "pigeon pea",
		variety = variety,
		treatment = variety,
		rep = rep,
		plot_id = as.character(r[["Plot No"]]),
		flowering_days = as.numeric(r$FLOW50_Count_days),
		maturity_days = as.numeric(r$MT_Count_days),
		plant_height = as.numeric(r$PHT_Avg_cm),
		plant_stand_ = as.numeric(r$PST_Count_plnt),
		seeds_per_pod_ = as.numeric(r$SPP_Count_SeedPod),
		# 100-seed weight in g (docx mistakenly says g/plot), multiplied by 10 for 1000-seed weight
		seed_weight = as.numeric(r$SWT_Wgh_g) * 10,
		grain_weight_plot_ = gw,
		disease = "fusarium wilt",
		disease_severity = as.character(as.integer(r$FW_Comp_Pct)),
		severity_scale = "1-3",
		stress = "disease",
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (33.634410 + 33.639732) / 2,
		latitude = (-13.982117 + -13.982200) / 2,
		geo_uncertainty = 287,
		geo_source = "ICRISAT Dataverse bounding box",
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
