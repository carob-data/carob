# R script for "carob"
# license: GPL (>=3)

## NOTES
# 2016 irrigated-lowland MET. Each country workbook is one row per genotype at one site.
# Trait values are continuous. There is no replicate column, so these are not single-plot records.
# Dictionnary.txt: YIELD is paddy at 14% humidity (t/ha); FLWR is days to 50% flowering;
# PLTHGT is plant height at harvest (cm); GW1000 is g per 1000 grains; TILL is tillers per plant;
# BIOM is dry biomass (t/ha); FERT is spikelet fertility (%); PNL is panicle length (cm).

## ISSUES
# No coordinates, fertilizer rates, or planting/harvest dates in the files. Do not invent them.
# Sites still need georeferencing: Vallee du Kou, Sapu, Niono, Fanaye, Ndiaye, Namulongue.
# The description says three replicates and allows station or hot-spot sites. Files do not mark either.
# Mali Niono genotype ARS 1045-14D-19-16D-RFL-32 has no yield and no grain weight. The row is kept.
# Sapu panicle length reaches 46 cm, longer than the other sites (about 14-34 cm).
# Checks are not identified. Genotype strings mix released names and line codes.
# No linked publication in the Dataverse record.
# write_files: missing variables: geo_from_source, latitude, longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date
# write_files: unknown variables: panicle_length_, tiller_number_, spikelet_fertility_
# write_files: NA detected: yield

## NOT PROCESSED
# PS and Trial are IL and MET on every row.
# Year is 2016 on every row and is not a planting date.
# 2016-IL-MET-Supplementary information.pdf: LSD, heritability, and p-value. ANOVA summary, not records.

## NEW VARIABLES
# panicle_length_: panicle length (cm), from PNL
# tiller_number_: tillers per plant, from TILL
# spikelet_fertility_: spikelet fertility (%), from FERT


carob_script <- function(path) {

"2016 Africa-wide Breeding Task Force Trials for Irrigated Lowland - MET

In 2010, the Africa-wide Rice Breeding Task Force was launched by AfricaRice involving National Agricultural Research System (NARS) from about 30 countries. The objectives of the network are to evaluate the stability of traits incorporated in breeding processes and to identify varieties best fit to growth conditions in target regions and to markets. The Task Force also accumulates data on performance of new elite lines, thereby facilitating varietal release procedures. Furthermore, by exposing breeders from NARS and farmers to these elite lines during the testing phase, dissemination will be facilitated.

The activities conducted by the Task Force consists of a series of consecutive trials. Promising breeding lines developed by AfricaRice or by national and international partners, such as IRRI, CIAT and the NARS are nominated for evaluation in one or several rice cultivation environments: rainfed lowland, irrigated lowland, rainfed upland, high elevation and mangrove. All nominated lines should be fixed and accompanied by supporting data on traits incorporated during the breeding process and with information on yield performance. These characteristics are checked at AfricaRice before incorporation into the network.

The first phase (MET, Multi-Environment Testing) consists of an initial evaluation of about 100 lines selected from the nominated lines. Each national partner evaluates these lines at sites in his/her country. Such sites may be at an experimental station under optimal management to evaluate yield potential, or may be hot spots to check the performance of the nominations in a stressed growth environment. Trials are replicated three times and include at least a common and a local check.

The second phase (PET, Participatory Evaluation Trial) serves to evaluate and confirm the performance of the selected lines. These lines are cultivated using the same experimental design with 3 replications. An important feature of PET is that farmer and other stakeholders such as miller and traders are invited to participate in varietal selection and their opinion on the performance of all entries (i.e. participatory varietal selection, PVS) collected. Based on the data collected, observations by the breeders and the opinion of stakeholder groups, NARS partners select up to 10 lines.

Further, NARS evaluated these lines in at least three sites per country and during one or more growing seasons, depending on varietal release requirements. All stakeholders are again invited to get acquainted with the new lines and voice their opinion to help select lines for further advancement. Among the 10 lines, farmers are invited to select three lines and cultivate these in their own fields, together with a common check and their own variety."

	uri <- "doi:10.7910/DVN/P4U8BN"
	group <- "varieties_other"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "AfricaRice",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety",
		response_vars = "yield;flowering_days;plant_height;seed_weight;dmy_total;panicle_length_;tiller_number_;spikelet_fertility_",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 70,
		carob_effort = 0.5
	)

	xlfiles <- grep("\\.xlsx$", ff, value=TRUE)
	d <- vector("list", length(xlfiles))
	for (i in seq_along(xlfiles)) {
		r <- carobiner::read.excel(xlfiles[i])
		names(r) <- tolower(names(r))
		d[[i]] <- data.frame(
			country = trimws(r$country),
			location = trimws(r$site),
			season = tolower(trimws(r$season)),
			variety = trimws(r$genotype),
			flowering_days = if ("flwr" %in% names(r)) as.numeric(r$flwr) else NA_real_,
			plant_height = if ("plthgt" %in% names(r)) as.numeric(r$plthgt) else NA_real_,
			seed_weight = if ("gw1000" %in% names(r)) as.numeric(r$gw1000) else NA_real_,
			panicle_length_ = if ("pnl" %in% names(r)) as.numeric(r$pnl) else NA_real_,
			tiller_number_ = if ("till" %in% names(r)) as.numeric(r$till) else NA_real_,
			dmy_total = if ("biom" %in% names(r)) as.numeric(r$biom) * 1000 else NA_real_,
			spikelet_fertility_ = if ("fert" %in% names(r)) as.numeric(r$fert) else NA_real_,
			yield = as.numeric(r$yield) * 1000
		)
	}
	d <- do.call(rbind, d)

	d$trial_id <- as.character(as.integer(as.factor(paste(d$country, d$location, d$season))))
	d$treatment <- d$variety
	d$crop <- "rice"
	d$yield_part <- "grain"
	# Dictionnary.txt: paddy yield at 14% humidity
	d$yield_moisture <- 14
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	# Dictionnary.txt: IL = irrigated lowland
	d$irrigated <- TRUE

	carobiner::write_files(path, meta, d)
}
