# R script for "carob"
# license: GPL (>=3)

## NOTES
# Plot yields and yellow-cotton percent are one row per station, year, and plot. Larval counts stay in the long table, one row per date and species, as counted on NPlantsObs plants (not per ha).
# The pest sheet and the yield sheet use different plot numbers, so the long table keeps pest_plot_ and is linked only by trial_id (station and year).
# Topping percent and the chemical-control level are the Comments sheet codes. Variety STAM 59A, 0.8 m between rows and 0.4 m between plants, and the insecticide products, are in ExperimentalDesign_ToppingCotton.docx.
# Country Mali, adm1 Sikasso, and the station names Farako and Finkolo are the geographicCoverage places. The files have no coordinates.

## ISSUES
# The design note gives 200 kg/ha of an unnamed fertilizer plus 50 kg/ha urea. The NPK grade of the first product is not stated, so elemental N, P, and K are omitted.
# Cotton yield is recorded as kg/ha. The sheet does not say seed cotton or lint. yield_part is seed because the same harvest is scored for percent yellow cotton.
# write_files: missing yield_moisture, geo_from_source, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# unknown variables: topping_rate_, yellow_cotton_, pest_plot_, plants_observed_. invalid terms: pest_species: Diparopsis watersi, Earias, Helicoverpa armigera.

## NOT PROCESSED
# Shedding: healthy and attacked squares and bolls between lines 7-8 and 8-9, by date. A second time series, not joined to the larval counts.
# Insecticide grams per hectare in the design note. Applications were every 14 days, and the number of sprays is not a column.


carob_script <- function(path) {

"
Données brutes sur l'effet du taux d'ecimage de cotonniers sur les populations de chenilles de la capsule

The aim of the experiment was to study the effect of different pest control strategies (most of them involving plant topping) on the population of cotton bollworms, Helicoverpa armigera, Diparopsis watersi and Earias spp, in Mali.  The impact of these pest control practices on the percentage of yellow cotton and cotton yield was also assessed.
"

	uri <- "doi:10.18167/DVN1/PCTL8J"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=2,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = "topping_rate_;insecticide_used",
		response_vars = "yield;yellow_cotton_;pest_incidence",
		carob_LLM = "Cursor Grok 4.7",
		carob_date = "2026-10-31",
		carob_completion = 60,
		carob_effort = 0.5
	)

	f <- ff[basename(ff) == "RawData_ToppingCotton.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="YellowCotton&CottonYield")
	r2 <- carobiner::read.excel(f, sheet="Pests")

	# Comments: 1 control, 2 chemical control, 3 to 6 are 2, 10, 20, and 100 percent topping without insecticide.
	top_pct <- c("1"=0, "2"=0, "3"=2, "4"=10, "5"=20, "6"=100)
	topping <- unname(top_pct[as.character(r1$Treatment)])
	chem <- r1$Treatment == 2
	prod <- rep("none", nrow(r1))
	prod[chem & r1$Year == 2011] <- "cypermethrin;acetamiprid;flubendiamide;spirotetramat"
	prod[chem & r1$Year == 2012] <- "cypermethrin;chlorpyrifos"
	prod[chem & r1$Year == 2014] <- "cypermethrin;imidacloprid"

	d <- data.frame(
		trial_id = paste(r1$Station, r1$Year),
		country = "Mali",
		adm1 = "Sikasso",
		location = as.character(r1$Station),
		crop = "cotton",
		variety = "STAM 59A",
		treatment = as.character(r1$Treatment),
		topping_rate_ = topping,
		insecticide_used = chem,
		insecticide_product = prod,
		rep = as.integer(r1$Block),
		plot_id = as.character(r1$Plot),
		yield = as.numeric(r1$CottonYield_kgPerHect),
		yield_part = "seed",
		yellow_cotton_ = as.numeric(r1$PercentageYellowCotton),
		row_spacing = 80,
		plant_spacing = 40,
		stress = "pests",
		on_farm = FALSE,
		is_survey = FALSE
	)

	spp <- c(H_armigera="Helicoverpa armigera", D_watersi="Diparopsis watersi", Earias="Earias")
	long <- NULL
	for (col in names(spp)) {
		one <- data.frame(
			trial_id = paste(r2$Station, r2$Year),
			pest_plot_ = as.character(r2$Plot),
			date = as.character(as.Date(r2$Date, tz="UTC")),
			pest_species = unname(spp[[col]]),
			pest_incidence = as.integer(r2[[col]]),
			plants_observed_ = as.integer(r2$NPlantsObs)
		)
		long <- rbind(long, one)
	}

	carobiner::write_files(path, meta, d, long=long)
}
