# R script for "carob"
# license: GPL (>=3)

## NOTES
# The xlsx and its docx are a replicated variety trial: ICEAP and ICPL lines, days to 50% flowering, days to 75% maturity, seeds per pod, plant height (cm), final plant stand, a fusarium wilt score, grain weight, and 100-seed weight.
# Those variety names, plant heights (119–200 cm), and 100-seed weights (about 11–14 g) are pigeonpea, so crop is pigeon pea. The dataset abstract describes a groundnut rosette trial; that description does not match this table.
# The docx says the wilt column is a visual score of 1–3. Every plot is 1. severity_scale is 1-3.
# seed_weight is the 100-seed weight in grams times 10. The docx line "grams/plot" for that column does not match the values.
# Coordinates are the midpoint of the Dataverse bounding box for Chitedze (33.634410–33.639732 E, 13.982117–13.982200 S).

## ISSUES
# Abstract and table disagree (groundnut rosette versus pigeonpea wilt and agronomy). Confirm which file belongs to this DOI before using the rows.
# Plot area is not given, so grain weight was not converted to kg/ha. Replication 1 grain weights are about 2.6–10.7 while replications 2 and 3 are about 2200–10400. The docx says grams/plot for all of them; values were not rescaled.
# Wilt does not vary. Fertilizer, inoculation, and irrigation are not in the file.
# Collection dates in the metadata are 2017-02-16 to 2017-03-18. They were not used as planting or harvest dates because they may belong to the groundnut description rather than this table.
# "fusarium wilt" is not in the disease vocabulary. No publication DOI in the Dataverse record.
# write_files: missing carob_contributor, yield, yield_part, yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer, planting_date, harvest_date.
# unknown variables: plant_stand_, seeds_per_pod_, grain_weight_g_plot_. invalid terms: disease fusarium wilt.

## NOT PROCESSED
# xlsx, Treatment: codes 1–7, one per variety name.

## NEW VARIABLES
# seeds_per_pod_: seeds per pod
# plant_stand_: final plant stand, plants per plot
# grain_weight_g_plot_: grain weight as recorded; the docx says grams per plot (see ISSUES on replication 1)

carob_script <- function(path) {

"
Reaction of selected medium duration breeding lines to groundnut rosette disease in Malawi during the year 2017

Research work carried out to identify promising groundnut breeding lines with resistance to groundnut rosette disease. The experiment was carried out in Chitedze research station during the 2016-2017 growing season. The infector row technique was used to induce a high disease pressure. At sowing time, one infector row of susceptible cultivar was sown after every two contiguous rows of test lines, such that every test row was adjacent to one infector row. Six weeks before planting on the field, a large number of seedlings were raised in the green house and inoculated with Groundnut Rosette Virus (GRV), using a greenhouse culture of viruliferous aphids, which had been reared on GRV-infected plants. These were then transplanted into the infector rows at 1.5-2 m spacing.  Each test line was assessed for disease incidence at 8 weeks and 12 weeks after sowing. The total number of plants in each plot and the number of plants showing rosette symptoms with severe stunting are counted and percentage of disease incidence computed. Plants showing severe symptoms are stunted and bushy in appearance due to reduced internodes length. Leaves of the infected plants are reduced in size and the plants do not produce pods. Breeding lines were considered resistant when no susceptible plants were found within the complete entry (0% incidence), highly susceptible when no resistant plants were present (100% incidence) and moderately resistant when at least one plant within the entry has mild symptoms (
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
		response_vars = "disease_severity;plant_height;flowering_days;maturity_days",
		notes = NA,
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.6
	)

	f <- ff[basename(ff) == "Data file of Regional variety evaluation data..xlsx"]
	r <- carobiner::read.excel(f)

	variety <- trimws(r[["Variety Name"]])

	d <- data.frame(
		country = "Malawi",
		location = "Chitedze",
		crop = "pigeon pea",
		variety = variety,
		treatment = variety,
		rep = as.integer(r[["Replication number"]]),
		plot_id = as.character(r[["Plot No"]]),
		flowering_days = as.numeric(r$FLOW50_Count_days),
		maturity_days = as.numeric(r$MT_Count_days),
		plant_height = as.numeric(r$PHT_Avg_cm),
		plant_stand_ = as.numeric(r$PST_Count_plnt),
		seeds_per_pod_ = as.numeric(r$SPP_Count_SeedPod),
		seed_weight = as.numeric(r$SWT_Wgh_g) * 10,
		grain_weight_g_plot_ = as.numeric(r$GWPT_Wgh_g),
		disease = "fusarium wilt",
		disease_severity = as.character(as.integer(r$FW_Comp_Pct)),
		severity_scale = "1-3",
		stress = "disease",
		trial_id = "1",
		on_farm = FALSE,
		is_survey = FALSE,
		longitude = (33.634410 + 33.639732) / 2,
		latitude = (-13.982117 + -13.982200) / 2,
		geo_from_source = TRUE
	)

	carobiner::write_files(path, meta, d)
}
