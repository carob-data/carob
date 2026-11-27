# R script for "carob"
# license: GPL (>=3)

## ISSUES
# 1. ExperimentalDesign_ToppingCotton.docx states 200 kg/ha of an unnamed
#    fertilizer plus 50 kg/ha urea; because the NPK grade is unstated, total
#    fertilizer_amount = 250 kg/ha is recorded while elemental N/P/K are omitted.
# 2. The Plot numbering in YellowCotton&CottonYield differs from the Plot
#    numbering in Pests and Shedding, but (Station, Year, Block, Treatment)
#    uniquely identifies all 144 plots 1:1 across all three sheets and is used
#    to link long records via record_id.
# 3. No coordinates in the source; approximate coordinates of the IER Farako and
#    Finkolo stations in Sikasso, Mali are assigned (geo_uncertainty = 5000 m).

## NEW
# topping_rate_: percentage of cotton plants topped manually (%)
# yellow_cotton_: percentage of harvested cotton classified as yellow cotton (%)
# plants_observed_: number of cotton plants inspected for bollworm larvae
# healthy_shed_squares_: count of healthy shed flower buds (squares) between lines 7-8 and 8-9
# attacked_shed_squares_: count of pest-attacked shed flower buds (squares) between lines 7-8 and 8-9
# healthy_shed_bolls_: count of healthy shed bolls between lines 7-8 and 8-9
# attacked_shed_bolls_: count of pest-attacked shed bolls between lines 7-8 and 8-9
# pest_species values not yet in terminag: Helicoverpa armigera, Diparopsis watersi, Earias

## MISSING
# yield_moisture, irrigated, N_fertilizer, P_fertilizer, K_fertilizer,
# planting_date, harvest_date.

## NOTES
# Four on-station RCBD trials (6 treatments x 6 blocks = 144 plots) at Farako
# (2011, 2012, 2014) and Finkolo (2012), Sikasso, Mali, comparing manual cotton
# topping rates (0, 2, 10, 20, 100%) and chemical insecticide control against
# bollworms (related earlier study: Renou et al. 2011, doi:10.1016/j.cropro.2011.05.020).
# Wide table d holds plot yields and yellow cotton %; long holds dated bollworm
# larval counts (Pests) and shed square/boll counts (Shedding) linked by record_id.

## NOT PROCESSED
# Per-spray insecticide rates in g/ha from ExperimentalDesign_ToppingCotton.docx
# (number of 14-day spray applications is not recorded per trial).

carob_script <- function(path) {

"
Données brutes sur l'effet du taux d'ecimage de cotonniers sur les populations de chenilles de la capsule

The aim of the experiment was to study the effect of different pest control strategies (most of them involving plant topping) on the population of cotton bollworms, Helicoverpa armigera, Diparopsis watersi and Earias spp, in Mali. The impact of these pest control practices on the percentage of yellow cotton and cotton yield was also assessed.
"

	uri <- "doi:10.18167/DVN1/PCTL8J"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIRAD;IER",
		publication = NA,
		project = NA,
		design = "RCBD",
		data_type = "experiment",
		treatment_vars = "topping_rate_;insecticide_used",
		response_vars = "yield;yellow_cotton_;pest_incidence",
		notes = NA,
		carob_contributor = NA,
		carob_date = "2026-11-26",
		carob_LLM = "Gemini 3.1 Pro",
		carob_completion = 90,
		carob_effort = 0
	)

	f <- ff[basename(ff) == "RawData_ToppingCotton.xlsx"]
	r1 <- carobiner::read.excel(f, sheet = "YellowCotton&CottonYield")
	r2 <- carobiner::read.excel(f, sheet = "Pests")
	r3 <- carobiner::read.excel(f, sheet = "Shedding")

	# Comments / Table 1: 1 control, 2 chemical control, 3-6 = 2, 10, 20, 100% topping
	top_pct <- c("1" = 0, "2" = 0, "3" = 2, "4" = 10, "5" = 20, "6" = 100)
	topping <- unname(top_pct[as.character(r1$Treatment)])
	chem <- r1$Treatment == 2
	prod <- rep("none", nrow(r1))
	prod[chem & r1$Year == 2011] <- "cypermethrin;acetamiprid;flubendiamide;spirotetramat"
	prod[chem & r1$Year == 2012] <- "cypermethrin;chlorpyrifos"
	prod[chem & r1$Year == 2014] <- "cypermethrin;imidacloprid"

	geo <- data.frame(
		location = c("Farako", "Finkolo"),
		longitude = c(-5.4833, -5.5111),
		latitude = c(11.2167, 11.2681)
	)
	gidx <- match(as.character(r1$Station), geo$location)

	d <- data.frame(
		record_id = seq_len(nrow(r1)),
		trial_id = paste0(r1$Station, "_", as.integer(r1$Year)),
		date = as.character(as.integer(r1$Year)),
		country = "Mali",
		adm1 = "Sikasso",
		location = as.character(r1$Station),
		longitude = geo$longitude[gidx],
		latitude = geo$latitude[gidx],
		crop = "cotton",
		variety = "STAM 59A",
		treatment = ifelse(chem, "chemical control", paste0("topping ", topping, "%")),
		topping_rate_ = topping,
		insecticide_used = chem,
		insecticide_product = prod,
		fertilizer_used = TRUE,
		fertilizer_amount = 250, # 200 kg/ha NPK (grade unstated) + 50 kg/ha urea
		rep = as.integer(r1$Block),
		plot_id = as.character(r1$Plot),
		yield = as.numeric(r1$CottonYield_kgPerHect),
		yield_part = "seed",
		yellow_cotton_ = as.numeric(r1$PercentageYellowCotton),
		row_spacing = 80,
		plant_spacing = 40,
		plot_length = 8,
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE
	)
	d$geo_from_source <- FALSE
	d$geo_uncertainty <- 5000
	d$geo_source <- "IER Sikasso station coordinates (Farako 11.217N, 5.483W; Finkolo 11.268N, 5.511W)"

	## Match Pests and Shedding to d via (Station, Year, Block, Treatment)
	key_d <- paste(r1$Station, r1$Year, r1$Block, r1$Treatment, sep = "|")
	k2 <- match(paste(r2$Station, r2$Year, r2$Block, r2$Treatment, sep = "|"), key_d)
	k3 <- match(paste(r3$Station, r3$Year, r3$Block, r3$Treatment, sep = "|"), key_d)

	spp <- c(H_armigera = "Helicoverpa armigera", D_watersi = "Diparopsis watersi", Earias = "Earias")
	long_pests <- do.call(rbind, lapply(names(spp), function(col) {
		data.frame(
			record_id = d$record_id[k2],
			trial_id = d$trial_id[k2],
			date = as.character(as.Date(r2$Date, tz = "UTC")),
			pest_species = unname(spp[[col]]),
			pest_incidence = as.integer(round(as.numeric(r2[[col]]))),
			plants_observed_ = as.integer(r2$NPlantsObs)
		)
	}))

	long_shed <- data.frame(
		record_id = d$record_id[k3],
		trial_id = d$trial_id[k3],
		date = as.character(as.Date(r3$Date, tz = "UTC")),
		healthy_shed_squares_ = as.integer(round(as.numeric(r3$HealthyFlowerbud_L7_L8) + as.numeric(r3$HealthyFlowerbud_L8_L9))),
		attacked_shed_squares_ = as.integer(round(as.numeric(r3$AttackedFlowerbud_L7_L8) + as.numeric(r3$AttackedFlowerbud_L8_L9))),
		healthy_shed_bolls_ = as.integer(round(as.numeric(r3$HealthyBoll_L7_L8) + as.numeric(r3$HealthyBoll_L8_L9))),
		attacked_shed_bolls_ = as.integer(round(as.numeric(r3$AttackedBoll_L7_L8) + as.numeric(r3$AttackedBoll_L8_L9)))
	)

	long <- carobiner::bindr(long_pests, long_shed)

	carobiner::write_files(path, meta, d, long = long)
}
