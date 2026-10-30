# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2015 mangrove participatory evaluation trials (PET) at two sites in Gambia. One row per genotype.
# Dictionnary.txt: YIELD is paddy at 14% moisture, t/ha (stored as kg/ha). FLWR is days to 50% flowering. PLTHGT and PNL are cm. GW1000 is g per 1000 grains.
# No replicate column. Values are fractional, so they are not single-plot records. Replicates were not filled in.
# Production system is M (mangrove) on every row. Trial is PET on every row.
# Named-site trials, so on_farm is FALSE.

## ISSUES
# Mangrove is not described as irrigated or rainfed, so irrigated is omitted.
# No coordinates in the workbooks or the supplementary PDF. Kaiaf and Lamin Abuko still need georeferencing.
# No fertilizer rates. Year is 2015, but planting and harvest dates are not reported.
# No genotype is labeled as a check.
# write_files: missing variables: carob_contributor; missing variables: geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date; unknown variables: panicle_length_.

## NOT PROCESSED
# Dictionnary.txt TILL, BIOM, FERT: those columns are not in the workbooks.
# 2015-M-PET-Supplementary information.pdf: LSD, heritability, and p-value. Trial statistics, not plot or genotype rows.
# PS and Trial: constant (M and PET).
# Year: the trial year, not a planting or harvest date.

## NEW VARIABLES
# panicle_length_: panicle length, cm (dictionary PNL).


carob_script <- function(path) {

"
2015 Africa-wide Breeding Task Force Trials for Mangrove - PET

In 2010, the Africa-wide Rice Breeding Task Force was launched by AfricaRice involving National Agricultural Research System (NARS) from about 30 countries. The objectives of the network are to evaluate the stability of traits incorporated in breeding processes and to identify varieties best fit to growth conditions in target regions and to markets. The Task Force also accumulates data on performance of new elite lines, thereby facilitating varietal release procedures. Furthermore, by exposing breeders from NARS and farmers to these elite lines during the testing phase, dissemination will be facilitated.

The activities conducted by the Task Force consists of a series of consecutive trials. Promising breeding lines developed by AfricaRice or by national and international partners, such as IRRI, CIAT and the NARS are nominated for evaluation in one or several rice cultivation environments: rainfed lowland, irrigated lowland, rainfed upland, high elevation and mangrove. All nominated lines should be fixed and accompanied by supporting data on traits incorporated during the breeding process and with information on yield performance. These characteristics are checked at AfricaRice before incorporation into the network.

The first phase (MET, Multi-Environment Testing) consists of an initial evaluation of about 100 lines selected from the nominated lines. Each national partner evaluates these lines at sites in his/her country. Such sites may be at an experimental station under optimal management to evaluate yield potential, or may be 'hot spots' to check the performance of the nominations in a stressed growth environment. Trials are replicated three times and include at least a common and a local check.

The second phase (PET, Participatory Evaluation Trial) serves to evaluate and confirm the performance of the selected lines. These lines are cultivated using the same experimental design with 3 replications. An important feature of PET is that farmer and other stakeholders such as miller and traders are invited to participate in varietal selection and their opinion on the performance of all entries (i.e. participatory varietal selection, PVS) collected. Based on the data collected, observations by the breeders and the opinion of stakeholder groups, NARS partners select up to 10 lines.

Further, NARS evaluated these lines in at least three sites per country and during one or more growing seasons, depending on varietal release requirements. All stakeholders are again invited to get acquainted with the new lines and voice their opinion to help select lines for further advancement. Among the 10 lines, farmers are invited to select three lines and cultivate these in their own fields, together with a common check and their own variety.
"

	uri <- "doi:10.7910/DVN/RYODX5"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height;seed_weight",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 75,
		carob_effort = 0.4
	)

	xl <- ff[grepl("\\.xlsx$", basename(ff))]
	xl <- xl[order(basename(xl))]
	d <- lapply(xl, function(f) {
		r <- carobiner::read.excel(f)
		variety <- trimws(as.character(r$Genotype))
		data.frame(
			country = trimws(as.character(r$Country)),
			location = trimws(as.character(r$Site)),
			season = tolower(trimws(as.character(r$Season))),
			variety = variety,
			treatment = variety,
			flowering_days = as.numeric(r$FLWR),
			plant_height = as.numeric(r$PLTHGT),
			seed_weight = as.numeric(r$GW1000),
			panicle_length_ = as.numeric(r$PNL),
			# Dictionnary.txt: paddy yield at 14% moisture, t/ha
			yield = as.numeric(r$YIELD) * 1000
		)
	})
	d <- do.call(rbind, d)

	d$crop <- "rice"
	d$yield_part <- "grain"
	# Dictionnary.txt: 14% humidity
	d$yield_moisture <- 14
	d$yield_isfresh <- FALSE
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$trial_id <- as.character(as.integer(as.factor(paste(d$country, d$location, d$season))))

	carobiner::write_files(path, meta, d)
}
