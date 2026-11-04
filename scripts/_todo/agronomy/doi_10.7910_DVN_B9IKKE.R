# R script for "carob"
# license: GPL (>=3)

## NOTES
# 14 rice genotypes (7 pairs contrasting in harvest index) under well-watered (WW) and reproductive-stage
#   drought, at IRRI (2017WS, 2018DS, 2018WS, 2019DS) and ICAR-RCER Patna (2017WS-2019WS).
# Patna: "Patna both trts all traits.xlsx" has genotype and rep. IRRI: "IRRI both trts all traits.xlsx" has no
#   genotype/rep; these are taken from "sol sugar starch stages.csv" and "IRRI PSN gs ME.csv" by season, treatment and plot.
#   In IRRI 2019DS the "Plot" column holds the genotype name; plot and rep are unknown for that season
#   (row order does not match plot order in the key files), so photosynthesis/gs are not merged for 2019DS.
# IRRI GY14 (g/m2, 14% moisture) and straw biomass (g/m2) x10 -> kg/ha; HI fraction -> %.
# Patna GY, straw and biological yield in g/m2 x10 -> kg/ha; tillers/m2 x10000 -> stem_density.
# Photosynthesis / gs from the csv files (flowering) are merged for IRRI 2018WS-2019DS; Patna values are in the xlsx.

## ISSUES
# Coordinates, planting dates, fertilizer, irrigation details and drought imposition dates not in the data.
#   Georeference IRRI (Los Banos, Laguna, Philippines) and ICAR-RCER Patna (Bihar, India).
# Patna yield moisture not stated.
# IRRI has fewer than 56 plots in several season x treatment combinations (e.g. 32 in 2017WS); missing plots not explained.
# IRRI GY14 values of 0 (failed plots under drought) are kept.
# DTF assumed days after sowing (mapped to flowering_days).
# Stem sugar/starch units (presumably mg/g) and stem DW10 (g per 10 stems) inferred from column names.
# Not mapped: stem sugar/starch by stage from "sol sugar starch stages.csv" (beyond flowering/maturity in xlsx),
#   HI class/pair/selection columns, photosynthesis "all same date" values.
# Remaining messages: missing coordinates/dates/fertilizer, new variables.

## NEW VARIABLES
# photosynthesis_: leaf photosynthesis at flowering (umol/m2/s)
# gs_: stomatal conductance at flowering (mol/m2/s)
# mobilization_eff_: stem carbohydrate mobilization efficiency (ME, %)
# stem_reserve_mob_: stem reserve mobilization (SRM, %), Patna
# plant_height_flowering_: plant height at flowering (cm), IRRI
# stem_dw10_flowering_, stem_dw10_maturity_: dry weight of 10 stems (g), IRRI
# stem_dw_anthesis_, stem_dw_maturity_: stem dry weight (g), Patna
# stem_length_anthesis_, stem_length_maturity_: stem length (cm), Patna
# ssw_anthesis_, ssw_maturity_: specific stem weight, Patna
# stem_sugar_flowering_, stem_sugar_maturity_: stem soluble sugar (mg/g), IRRI
# stem_starch_flowering_, stem_starch_maturity_: stem starch (mg/g), IRRI


carob_script <- function(path) {

"
Replication Data for: Disentangling the Roles of Plant Water Status and Stem Carbohydrate Remobilization on Rice Harvest Index Under Drought

Harvest index is an important component of grain yield and is typically reduced by reproductive stage drought stress in rice. Multiple drought response mechanisms can affect harvest index including plant water status and the degree of stem carbohydrate mobilization during grain filling. In this study, we aimed to dissect the contributions of plant water status and stem carbohydrate mobilization to harvest index. Pairs of genotypes selected for contrasting harvest index but similar biomass and days to flowering were characterized at ICAR-RCER, Patna, India and at IRRI, Philippines. Multiple traits were related with harvest index across experiments, including mobilization efficiency at both sites as indicated by groupings in principal component analysis, and plant water status as indicated by direct correlations. Biomass-related traits were positively correlated with harvest index at IRRI but biomass was negatively correlated with harvest index at ICER-RCER, Patna. We observed that some pairs of genotypes showed differences in harvest index across environments, whereas other showed differences in harvest index only under drought. Of all time points measured when all genotypes were considered together, the stem carbohydrate levels at maturity were most consistently (negatively) correlated with harvest index under drought, but not under well-watered conditions. However, in the pairs of genotypes grouped as those whose differences in harvest index were stable across environments, improved plant water status resulted in a greater ability to both accumulate and remobilize stored carbohydrate, i.e. starch. By distinguishing between genotypes whose harvest index was improved across conditions as opposed to specifically under drought, we can attribute the mechanisms behind the stable high-harvest index genotypes to be more related to stem carbohydrate remobilization than to plant water status. The stable high-harvest index lines in this study (Aus 257 and Wanni Dahanala) may confer mechanisms to improve harvest index that are independent of drought response and therefore may be useful for breeding improved rice varieties.
"

	uri <- "doi:10.7910/DVN/B9IKKE"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "IRRI",
		publication = "doi:10.1186/s12284-023-00631-6",
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety;drought_stress",
		response_vars = "yield;harvest_index", 
		carob_date = "2026-10-29",
		carob_LLM = "Cursor Grok 4.6",
		carob_completion = 60,
		carob_effort = 1
	)

	f1 <- ff[basename(ff) == "IRRI both trts all traits.xlsx"]
	f2 <- ff[basename(ff) == "Patna both trts all traits.xlsx"]
	f3 <- ff[basename(ff) == "IRRI PSN gs ME.csv"]
	f4 <- ff[basename(ff) == "sol sugar starch stages.csv"]

	trt <- function(x) ifelse(x %in% c("drought", "S", "RS"), "drought", "WW")

	## IRRI
	r1 <- carobiner::read.excel(f1)
	season <- paste0(ifelse(nchar(r1$Expt) == 4, "20", ""), r1$Expt)
	d1 <- data.frame(
		location = "IRRI, Los Banos",
		country = "Philippines",
		adm1 = "Laguna",
		year_season = season,
		treatment = trt(r1$Trt),
		plot = r1$Plot,
		harvest_index = r1$HI * 100,
		plant_height_flowering_ = r1[["Plant ht - flowering"]],
		plant_height = r1[["Plant ht - harvest"]],
		stem_dw10_flowering_ = r1[["stem DW10 - flowering"]],
		stem_dw10_maturity_ = r1[["stem DW10 - maturity"]],
		mobilization_eff_ = r1$ME,
		stem_sugar_flowering_ = r1[["stem sol sugar - 90DAS flowering"]],
		stem_sugar_maturity_ = r1[["stem sol sugar -111DAS"]],
		stem_starch_flowering_ = r1[["stem starch - 90DAS flowering"]],
		stem_starch_maturity_ = r1[["stem starch -111DAS"]],
		flowering_days = r1$DTF,
		dmy_residue = r1[["Straw biomass"]] * 10,
		yield = r1$GY14 * 10,
		yield_moisture = 14
	)
	# 2019DS: "Plot" holds the genotype; plot number and rep unknown
	i <- d1$year_season == "2019DS"
	g19 <- ifelse(i, d1$plot, NA)
	d1$plot[i] <- NA
	d1$plot <- as.integer(d1$plot)

	k4 <- read.csv(f4)
	k3 <- read.csv(f3)
	k <- rbind(k4[, c("Season", "Plot", "Genotype", "Rep", "Trt")], k3[, c("Season", "Plot", "Genotype", "Rep", "Trt")])
	k$Trt <- trt(k$Trt)
	k <- unique(k)
	colnames(k) <- c("year_season", "plot", "variety", "rep", "treatment")
	k <- k[k$year_season != "2019DS", ]
	d1$variety_ <- g19
	d1 <- merge(d1, k, by=c("year_season", "plot", "treatment"), all.x=TRUE)
	d1$variety[!is.na(d1$variety_)] <- d1$variety_[!is.na(d1$variety_)]
	d1$variety_ <- NULL

	k3$treatment <- trt(k3$Trt)
	ps <- data.frame(year_season = k3$Season, plot = k3$Plot, treatment = k3$treatment,
		photosynthesis_ = k3[["Photosynthesis...flowering"]], gs_ = k3[["stomatal.conductance...flowering"]])
	d1 <- merge(d1, ps, by=c("year_season", "plot", "treatment"), all.x=TRUE)

	## Patna
	r2 <- carobiner::read.excel(f2)
	d2 <- data.frame(
		location = "ICAR-RCER, Patna",
		country = "India",
		adm1 = "Bihar",
		year_season = paste0("20", r2$Expt),
		treatment = trt(r2$Trt),
		plot = as.integer(r2$Plot),
		rep = as.integer(r2$Rep),
		variety = r2$Genotype,
		harvest_index = r2$HI,
		photosynthesis_ = r2$Photosynthesis,
		gs_ = r2[["stomatal conductance"]],
		flowering_days = r2$DTF,
		stem_density = r2[["No of Tiller/m2"]] * 10000,
		yield = r2[["GY g/m2"]] * 10,
		dmy_residue = r2[["Straw biomass g/m2"]] * 10,
		dmy_total = r2[["Biological yield g/m2"]] * 10,
		plant_height = r2[["Plant ht"]],
		stem_dw_anthesis_ = r2[["stem dry wt anthesis"]],
		stem_dw_maturity_ = r2[["stem dry wt maturity"]],
		stem_length_anthesis_ = r2[["stem length anthesis"]],
		stem_length_maturity_ = r2[["stem length maturity"]],
		ssw_anthesis_ = r2[["SSW anthesis"]],
		ssw_maturity_ = r2[["SSW maturity"]],
		stem_reserve_mob_ = r2$SRM,
		mobilization_eff_ = r2$ME
	)

	d <- carobiner::bindr(d1, d2)
	d$variety[d$variety == "WANNI DAHANALA"] <- "Wanni Dahanala"
	d$variety[d$variety == "E Zi 124"] <- "E ZI 124"
	d$drought_stress <- ifelse(d$treatment == "drought", "reproductive stage", "none")
	d$season <- ifelse(grepl("WS", d$year_season), "wet", "dry")
	d$planting_date <- substr(d$year_season, 1, 4)
	d$trial_id <- paste(d$location, d$year_season, d$treatment, sep="_")
	d$year_season <- NULL
	d$plot_id <- as.character(d$plot)
	d$plot <- NULL

	d$crop <- "rice"
	d$yield_part <- "grain"
	d$on_farm <- FALSE
	d$is_survey <- FALSE
	d$irrigated <- NA
	d$longitude <- as.numeric(NA)
	d$latitude <- as.numeric(NA)
	d$geo_from_source <- NA

	carobiner::write_files(path, meta, d)
}
