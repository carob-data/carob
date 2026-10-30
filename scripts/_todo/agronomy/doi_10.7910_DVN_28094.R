# R script for "carob"
# license: GPL (>=3)

## NOTES
# OryzaSNP panel (20 genotypes) in field and container root studies (Wade et al. 2015, PLoS ONE 10:e0124127).
# Mapped here: the six field trials with plot-level yield (sheet "Observation" of each file):
#   IRRI lowland field 2008DS, 2009DS, 2009WS, 2010DS (RCBD, control vs drought: drained 4 weeks after
#   transplanting), and TNAU (Agricultural Research Station, Paramakudi, India) 2010WS and 2011WS (direct seeded in
#   dry soil, flooded control vs rainfed drought from 5-6 weeks after emergence, per the paper).
# Sowing/transplanting dates, plot size, rows and row spacing from sheet "Description" of each file.
# Units per sheet "Description": IRRI yield and straw in g/m2 (x10 -> kg/ha); TNAU yield and straw in kg/ha.
# Root length density (cm/cm3), root dry weight (g per core) and % lateral roots by soil layer are in the long
#   table (one row per plot x soil layer), linked by record_id.

## ISSUES
# No coordinates in data or paper. Georeference IRRI, Los Banos (Laguna, Philippines) and ARS Paramakudi
#   (Ramanathapuram, Tamil Nadu, India).
# Fertilizer: "FERTILIZER APPLIED = yes" but no rates; N/P/K left out.
# Yield moisture: description says "with a moisture..." (truncated in the sheet); 14% assumed by IRRI convention?
#   Not set; verify. 2008DS yields are very low (mostly < 1.5 t/ha), check units.
# 2008DS treatment "vr" assumed to be drought ("vegetative-reproductive" stress); paper code IR08FL.
# 2009WS has no rep column (plot numbers only). 2009DS plant height and straw biomass columns are empty.
# 2010DS harvest date only given for control plots. TNAU 2011WS harvest date read as yyyymmdd number.
# TNAU plot size 0.8 m2, 2 rows with row spacing "200" (unit unclear; not mapped).
# Not mapped: Ibadan 2012 (AfricaRice; only yield per plant, many time-series tiller/height/leaf rolling scores; could
#   be added as long data), Barwale Hyderabad (cylinder study), lysimeter (IRRI, ICRISAT, "Breeding lines 2009DS"),
#   Aberdeen (herbicide, hydroponics, non-woven, rhizotron), Nagoya line-source (SNPs results), Rice_Expt_1-3
#   (container studies), TNAU root traits (MRL, TRDW, RT), TNAU panicle traits (NOG, NOC, PL, SF, YPP),
#   the 2009WS canopy temperature, and the SNP introgression files (genotype data, R template).
# TNAU (Tamil Nadu Agricultural University) is not in the data_organization vocabulary; only IRRI used.
# TNAU 2010WS has one DFF = 0 (out of bounds), left as recorded. TNAU plot_area 0.8 m2 is below the vocabulary minimum.
# write_files remaining: missing coordinates, N/P/K fertilizer, yield_moisture; NA yield; out of bounds
#   flowering_days and plot_area; unknown (new) variables (also in the long table).

## NEW VARIABLES
# tillers_plant_: number of tillers per plant (TNAU, NOT)
# productive_tillers_plant_: number of productive tillers per plant (TNAU, NPT)
# canopy_temperature_: canopy temperature (C), 2009WS, measured 2009-09-18
# long table: depth_top, depth_bottom (cm); root_length_density_ (cm/cm3); root_dw_ (g per soil core);
#   root_laterals_ (% of root length as lateral roots < 0.2 mm diameter)


carob_script <- function(path) {

"
Environmental response and genomic regions correlated with rice root growth and yield under drought in the OryzaSNP panel across multiple study systems

The rapid progress in genotyping of rice must be matched by advances in phenotyping.  A better understanding of the genetic variation in rice for drought response and root  traits as well as practical methods for studying them are needed. In this study, the  OryzaSNP set (a set of 20 diverse genotypes that have been mapped for SNP  markers) was phenotyped in a range of field and container studies to better  characterize the diversity of rice root responses to drought. Of the root traits measured  across more than 20 root experiments, root dry weight showed the most stable  genotypic performance across studies. The environment (E) component had the  strongest effect on yield and root traits. Based on the SNP data, we identified genomic  regions correlated with root dry weight, percent deep roots, maximum root depth, and  grain yield. Two genomic regions were identified as hot spots in which root traits and  grain yield were co-located; on chromosome 1 (39.7 - 40.7 Mb) and on chromosome 8  (20.3-21.9 Mb). Rice root and drought-yield QTL have been previously identified in  these regions. Across experiments, the particular soil type or growth medium used  likely had just as strong of an effect on plant growth as the type of setup and/or  container dimensions. The correlation and co-location of root traits from a range of  study syste ms points to the potential utility of container root study systems to represent  responses in field studies, but the best correlations between field and container studies  were observed when the two setups had some similar properties. Due to the co- location of the identified genomic regions with QTL for a number of previously reported  root and drought traits, these regions are good candidates for detailed characterization  to contribute to understanding rice improvement for response to drought.
"

	uri <- "doi:10.7910/DVN/28094"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1371/journal.pone.0124127",
		project = "GCP G3008.06",
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "variety;stress",
		response_vars = "yield;dmy_residue;harvest_index;flowering_days;plant_height",
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 45,
		carob_effort = 1
	)

	obs <- function(name) {
		carobiner::read.excel(ff[basename(ff) == name], sheet="Observation")
	}

	r1 <- obs("OryzaSNP lowland field 2008DS_MODIFIED BY whe.xls")
	d1 <- data.frame(
		trial_id = "IRRI_2008DS", plot_id = as.character(r1$Plot), treatment = r1$Trt,
		rep = as.integer(r1$Rep), variety_code = r1$EntryNo, variety = r1$Name,
		yield = r1$yield * 10, dmy_residue = r1$strawbiomass * 10, harvest_index = r1$HI * 100,
		flowering_days = r1$DTF,
		planting_date = "2008-01-14", transplanting_date = "2008-02-07",
		plot_area = 2.25, row_spacing = 25
	)

	r2 <- obs("OryzaSNP lowland field 2009DS_mod.xls")
	d2 <- data.frame(
		trial_id = "IRRI_2009DS", plot_id = as.character(r2$plot), treatment = r2$trt,
		rep = as.integer(r2$rep), variety = r2[["entry name"]],
		yield = r2[["yield (g/m2)"]] * 10, harvest_index = r2$HI * 100,
		flowering_days = r2$dtf,
		planting_date = "2008-12-19", transplanting_date = "2009-01-20",
		plot_area = 3.75, row_spacing = 25
	)

	r3 <- obs("OryzaSNP lowland field 2009WS_mod.xls")
	d3 <- data.frame(
		trial_id = "IRRI_2009WS", plot_id = as.character(r3$PLOTNO), treatment = r3$Trt,
		variety_code = r3$ENTRYNO, variety = r3$Designation,
		plant_height = r3[["ave plt ht"]], flowering_days = r3$dtf,
		yield = r3[["yield (g/m2)"]] * 10, dmy_residue = r3[["straw (g/m2)"]] * 10, harvest_index = r3$HI * 100,
		canopy_temperature_ = r3[["canopy temp 9.18.09"]],
		planting_date = "2009-06-22", transplanting_date = "2009-07-14",
		plot_area = 3, row_spacing = 25
	)

	r4 <- obs("OryzaSNP lowland field 2010DS_mod.xls")
	d4 <- data.frame(
		trial_id = "IRRI_2010DS", plot_id = as.character(r4$Plot), treatment = r4$Trt,
		rep = as.integer(r4$REP), variety = r4$DESIGNATION,
		plant_height = r4[["ave ht"]], harvest_date = as.character(as.Date(r4[["harv. Date"]])),
		yield = r4[["yield g/m2"]] * 10, dmy_residue = r4[["straw g/m2"]] * 10, harvest_index = r4$HI * 100,
		flowering_days = r4$dtf,
		planting_date = "2009-12-18", transplanting_date = "2010-01-10",
		plot_area = 3, row_spacing = 25
	)

	r5 <- obs("TNAU Oryza SNP 2010-2011 Wet season.xls")
	d5 <- data.frame(
		trial_id = "TNAU_2010WS", plot_id = as.character(r5$PLOT), treatment = r5$TRT,
		rep = as.integer(r5$REP), variety_code = as.character(r5[["ENTRY NUMBER"]]), variety = r5[["ENTRY NAME"]],
		harvest_date = as.character(as.Date(r5[["Harvest Date"]])),
		flowering_days = r5$DFF, plant_height = r5$PH,
		tillers_plant_ = r5$NOT, productive_tillers_plant_ = r5$NPT,
		yield = r5[["Grain yield kg/ha"]], dmy_residue = r5[["Straw yield kg/ha"]], harvest_index = r5$HI * 100,
		planting_date = "2010-09-17", plot_area = 0.8
	)

	r6 <- obs("TNAU Oryza SNP 2011-2012 Wet season.xls")
	d6 <- data.frame(
		trial_id = "TNAU_2011WS", plot_id = as.character(r6$Plot), treatment = r6$Trt,
		rep = as.integer(r6$REP), variety_code = as.character(r6[["ENTRY NO"]]), variety = r6[["ENTRY NAME"]],
		harvest_date = as.character(as.Date(as.character(r6[["Harvest Date"]]), "%Y%m%d")),
		plant_height = r6$PH, tillers_plant_ = r6$NOT, productive_tillers_plant_ = r6$NPT,
		yield = r6[["GY (kg/ha)"]], dmy_residue = r6[["SY (kg/ha)"]], harvest_index = r6$HI * 100,
		planting_date = "2011-09-21", plot_area = 0.8
	)

	d <- carobiner::bindr(d1, d2, d3, d4, d5, d6)
	d$variety <- trimws(d$variety)
	d$record_id <- 1:nrow(d)

	## root traits by soil layer, long format
	rr <- list(r1, r2, r3, r4)
	roots <- lapply(1:4, function(i) {
		r <- rr[[i]]
		ids <- d$record_id[d$trial_id == unique(d$trial_id)[i]]
		cn <- grep("^(RLD|RDW|% laterals)", names(r), value=TRUE)
		out <- lapply(cn, function(v) {
			lyr <- as.numeric(strsplit(gsub("[^0-9-]", "", v), "-")[[1]])
			data.frame(record_id=ids, var=gsub("[ 0-9-]", "", v), depth_top=lyr[1], depth_bottom=lyr[2], value=r[[v]])
		})
		do.call(rbind, out)
	})
	roots <- do.call(rbind, roots)
	roots$var <- c(RLD="root_length_density_", RDW="root_dw_", "%laterals"="root_laterals_")[roots$var]
	dlong <- reshape(roots, idvar=c("record_id", "depth_top", "depth_bottom"), timevar="var", direction="wide")
	names(dlong) <- gsub("^value\\.", "", names(dlong))
	dlong <- dlong[rowSums(!is.na(dlong[, -(1:3), drop=FALSE])) > 0, ]
	rownames(dlong) <- NULL

	d$stress <- ifelse(tolower(d$treatment) %in% c("s", "stress", "vr"), "drought",
		ifelse(tolower(d$treatment) %in% c("c", "control", "fc"), "none", NA))
	d$irrigated <- d$stress == "none" | grepl("IRRI", d$trial_id)
	d$season <- ifelse(grepl("WS", d$trial_id), "wet", "dry")
	d$planting_method <- ifelse(grepl("IRRI", d$trial_id), "transplanting", "direct seeding")
	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- FALSE
	d$is_survey <- FALSE

	irri <- grepl("IRRI", d$trial_id)
	d$country <- ifelse(irri, "Philippines", "India")
	d$adm1 <- ifelse(irri, "Laguna", "Tamil Nadu")
	d$location <- ifelse(irri, "IRRI, Los Banos", "ARS Paramakudi")

	carobiner::write_files(path, meta, d, long=dlong)
}
