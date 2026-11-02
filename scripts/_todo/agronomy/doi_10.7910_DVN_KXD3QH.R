# R script for "carob"
# license: GPL (>=3)

## NOTES
# Zn biofortification field trials with 10-14 rice genotypes x Zn fertilizer treatment, Philippines,
#   2012WS (IRRI, Bay, Bohol, Bukidnon), 2013DS (Bay), 2014WS (IRRI, Bay, Bohol), 2015DS (IRRI).
# One "agronomic data" workbook per site-season; the raw sheet of each is used (yield, grain Zn, Zn uptake).
#   Column layout and yield units differ between files (t/ha or kg/ha, both at 14% moisture).
# "General MASTERFILE.xlsx" documents the Bay 2012WS trial (RCBD, 4 reps, 1 seedling/hill, 4.8 m2 harvest area,
#   N 100 kg/ha in 3 splits, 20 kg/ha P2O5 and K2O, coordinates N14.176907 E121.290273).
# grain Zn mg/kg -> mg/g (/1000).

## ISSUES
# Zn treatment codes differ by file (T1/T2, Z0/Z1, Z0/Z10, Zn0-Zn3) and their rates are not documented
#   (masterfile: "control (no Zn) and 4% zinc oxide 5 kg/ha basal"). Zn_fertilizer set to 0 only for Z0/Zn0 codes.
# N/P/K, planting/harvest dates only documented for Bay 2012WS (Schedule sheet not parsed; dates missing).
# Coordinates only for Bay (masterfile, assumed valid for all Bay trials); IRRI (Los Banos), Bohol and
#   Bukidnon (Maramag) need georeferencing.
# Genotype names are inconsistent across files (e.g. "KP", "KPatong", "Kinandang Patong", "Kinandong Patong";
#   "IR68144-166" vs "IR68144-2B-2-2-3-1-166"); kept as recorded.
# Bay 2012WS: the "14%MC (kg/ha)" column (~0.7 t/ha) treats grain yield g/m2 as g/plot; yield taken from
#   "GRAIN YIELD (g/m2)" x10 with the recorded moisture content as yield_moisture (not converted to 14%).
#   The unit of "GRAIN YIELD (g/m2)" (derived from "GRAIN BIOMASS (g)"/1.12) should be verified.
# Bay/IRRI 2014WS: a second, re-sorted block below the raw data (with a different "Zn (mg/kg)" column) is ignored;
#   grain Zn from the first block (re-analysed by Francis from stored grain, per Notes sheet).
# Bohol 2012WS: IR26 T2 rep 1 appears twice (and T1 rep 1 is missing); 4 records missing in that file.
# 2012WS files: "uptake" assumed to be g Zn/ha (yield t/ha x mg/kg); unit not documented.
# Bay 2013DS: grain Zn is in a separate block with its own plot numbering (1..n); assumed row-aligned with the yield block.
# Remaining messages: Zn_fertilizer missing/no variation (see above), irrigated and harvest_date missing,
#   new variables, missing coordinates for non-Bay sites.
# Not mapped: masterfile modules (root counts, biomass by stage, tissue Zn, soil redox, leaf bronzing score,
#   background soil), Bay 2012WS moisture content and harvest area details.

## NEW VARIABLES
# Zn_treatment_: Zn treatment code as recorded
# genotype_group_: genotype group code (ZnT Zn-deficiency tolerant, ZnS susceptible, ZnB biofortified, Check)
# grain_Zn_uptake_: grain Zn uptake (g/ha)


carob_script <- function(path) {

"
Raw data: Grain Zn concentrations and yields of Zn-biofortified and Zn-efficient rice genotypes under contrasting growth conditions

The data sets reveal grain Zn biofortfication performance of Zn-efficient and Zn-biofortified rice genotypes grown in different seasons in contrasting soils. Specifically, This study investigated grain yields and grain Zn concentrations of Zn-biofortified rice genotypes compared with Zn-efficient and -inefficient genotypes in field experiments in the Philippines in four contrasting paddy soils: one Zn-sufficient and three Zn-deficient with deficiency associated with (1) strongly reducing conditions and high soil organic C content; (2) high soil pH and presence of free calcium carbonate; and (3) strongly-weathered soil. In wet season 2012, results revealed that Zn-biofortified genotypes achieved both grain yield (4.0 t ha-1) and grain Zn  concentration (30 mg kg-1) targets compared with Zn-efficient, Check and Zn-inefficient genotypes when grown under Zn-sufficient conditions (IRRI soils). Conversely, when grown in Zn-deficient soils (Bohol and Bukidnon), only the  Zn-biofortified genotypes were able to achieve, or come close to, the Zn concentration requirement but not the yield target. Zn fertilization during the wet season 2014 significantly improved grain Zn concentration in Zn deficient soils but not the grain yield suggesting that grain yield was limited by other soil constraints rather than Zn deficiency. Further, results from two seasons in Zn-sufficient IRRI soils revealed that only the Zn fertilized rice genotypes  were able to achieve the Zn biofortification target during the wet season, while during dry season, regardless of Zn treatments (no Zn and plus Zn), grain Zn levels were all above the threshold. This suggests that Zn fertilization should be given more importance during the wet season than the dry season particularly in soils with adequate levels of Zn that could become readily available to plants as a result of greater oxidation.  
"

	uri <- "doi:10.7910/DVN/KXD3QH"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "IRRI",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety;Zn_fertilizer",
		response_vars = "yield;grain_Zn", 
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 55,
		carob_effort = 1
	)

	# some raw sheets have a second (re-sorted, analysis) block below the first; only the first block is used
	rd <- function(fname, sheet, skip=0) {
		r <- carobiner::read.excel(ff[basename(ff) == fname], sheet=sheet, col_names=FALSE, skip=skip)
		blank <- which(is.na(r[[2]]))
		if (length(blank) > 0) r <- r[seq_len(blank[1] - 1), ]
		r[!duplicated(r), ]
	}

	# 2012 WS, yields in t/ha
	r1 <- rd("IRRI Ws2012- agronomic data.xlsx", "Raw-ImpaAsSource", 1)
	d1 <- data.frame(location="IRRI", season="wet", year="2012", variety=r1[[1]], genotype_group_=r1[[2]],
		Zn_treatment_=r1[[3]], rep=r1[[4]], grain_Zn=r1[[5]], yield=r1[[6]] * 1000, grain_Zn_uptake_=r1[[7]])
	r2 <- rd("Bohol Ws2012- agronomic data.xlsx", "Raw-ImpaAsSource", 1)
	d2 <- data.frame(location="Bohol", season="wet", year="2012", variety=r2[[1]], genotype_group_=r2[[2]],
		rep=r2[[3]], Zn_treatment_=r2[[4]], yield=r2[[5]] * 1000, grain_Zn=r2[[6]], grain_Zn_uptake_=r2[[7]])
	r3 <- rd("Bukidnon Ws2012- agronomic data.xlsx", "Raw-ImpaAsSource", 1)
	d3 <- data.frame(location="Maramag", season="wet", year="2012", variety=r3[[1]], genotype_group_=r3[[2]],
		rep=r3[[3]], Zn_treatment_=r3[[4]], yield=r3[[5]] * 1000, grain_Zn=r3[[6]], grain_Zn_uptake_=r3[[7]])
	# plot numbers are prefixed with the Zn treatment ("T2-1020"); 
	# the "14%MC (kg/ha)" column treats g/m2 as g/plot, so grain yield g/m2 at measured moisture is used
	r4 <- rd("Bay Ws2012- agronomic data.xlsx", "1-Raw", 1)
	d4 <- data.frame(location="Bay", season="wet", year="2012", variety=r4[[2]], rep=r4[[3]],
		plot_id=r4[[4]], Zn_treatment_=substr(r4[[4]], 1, 2), yield=r4[[7]] * 10, 
		yield_moisture=r4[[8]], grain_Zn=r4[[11]])

	# 2013 DS Bay, kg/ha
	r5 <- rd("Bay DS 2013-Agronomic.xlsx", "RAw", 1)
	r5 <- r5[!is.na(r5[[2]]), ]
	d5 <- data.frame(location="Bay", season="dry", year="2013", plot_id=as.character(r5[[1]]), rep=r5[[2]],
		Zn_treatment_=r5[[3]], variety=r5[[5]], yield=r5[[6]], grain_Zn=r5[[10]])

	# 2014 WS, kg/ha (Bohol t/ha)
	r6 <- rd("Bay WS2014- agronomic data.xlsx", "1-Raw", 1)
	r6 <- r6[!is.na(r6[[2]]), ]
	d6 <- data.frame(location="Bay", season="wet", year="2014", plot_id=as.character(r6[[1]]), rep=r6[[2]],
		Zn_treatment_=r6[[3]], variety=r6[[5]], yield=r6[[11]], grain_Zn=r6[[12]])
	r7 <- rd("IRRI WS2014- agronomic data.xlsx", "1-Raw", 1)
	r7 <- r7[!is.na(r7[[2]]), ]
	d7 <- data.frame(location="IRRI", season="wet", year="2014", plot_id=as.character(r7[[1]]), rep=r7[[2]],
		Zn_treatment_=r7[[3]], variety=r7[[5]], yield=r7[[11]], grain_Zn=r7[[13]])
	r8 <- rd("Bohol WS2014- agronomic data.xlsx", "1-Raw", 1)
	r8 <- r8[!is.na(r8[[2]]), ]
	d8 <- data.frame(location="Bohol", season="wet", year="2014", plot_id=as.character(r8[[1]]), rep=r8[[2]],
		Zn_treatment_=r8[[3]], variety=r8[[5]], yield=r8[[8]] * 1000, grain_Zn=r8[[9]])

	# 2015 DS IRRI, kg/ha
	r9 <- rd("IRRI DS2015- agronomic data.xlsx", "1-Raw", 6)
	r9 <- r9[!is.na(r9[[2]]), ]
	d9 <- data.frame(location="IRRI", season="dry", year="2015", plot_id=as.character(r9[[1]]), rep=r9[[2]],
		Zn_treatment_=r9[[3]], variety=r9[[5]], yield=r9[[6]], grain_Zn=r9[[8]])

	d <- carobiner::bindr(d1, d2, d3, d4, d5, d6, d7, d8, d9)
	d$rep <- as.integer(d$rep)
	# "-NS-" (no sample)
	d$grain_Zn[d$grain_Zn == "-NS-"] <- NA
	d$grain_Zn <- as.numeric(d$grain_Zn) / 1000
	d$yield <- as.numeric(d$yield)
	d$grain_Zn_uptake_ <- as.numeric(d$grain_Zn_uptake_)
	d$variety <- trimws(d$variety)
	d$Zn_fertilizer <- ifelse(d$Zn_treatment_ %in% c("Z0", "Zn0"), 0, NA)
	d$treatment <- paste(d$variety, d$Zn_treatment_, sep="_")
	d$trial_id <- paste(d$location, d$year, d$season, sep="_")
	d$planting_date <- d$year
	d$year <- NULL

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$yield_moisture <- as.numeric(d$yield_moisture)
	d$yield_moisture[is.na(d$yield_moisture)] <- 14
	d$planting_method <- "transplanting"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$country <- "Philippines"
	d$adm1 <- ifelse(d$location %in% c("IRRI", "Bay"), "Laguna", d$location)
	d$adm1[d$location == "Maramag"] <- "Bukidnon"
	d$adm2 <- ifelse(d$location == "Bay", "Bay", ifelse(d$location == "IRRI", "Los Baños", ifelse(d$location == "Maramag", "Maramag", NA)))
	d$location[d$location == "IRRI"] <- "IRRI, Los Banos"

	# Bay coordinates from the masterfile Metadata sheet
	d$longitude <- ifelse(d$location == "Bay", 121.290273, NA)
	d$latitude <- ifelse(d$location == "Bay", 14.176907, NA)
	d$geo_from_source <- d$location == "Bay"

	# NPK documented for the Bay 2012WS trial only (masterfile "N P K Zn" sheet)
	i <- d$trial_id == "Bay_2012_wet"
	d$N_fertilizer <- ifelse(i, 100, NA)
	d$N_splits <- ifelse(i, 3L, NA)
	d$P_fertilizer <- ifelse(i, 20 / 2.29, NA)
	d$K_fertilizer <- ifelse(i, 20 / 1.2051, NA)

	carobiner::write_files(path, meta, d)
}
