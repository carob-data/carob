# R script for "carob"
# license: GPL (>=3)

## NOTES
# Sorghum ridging and tie-ridging at Gumara-Maksegnit, 2014 and 2015. Grain and stover are in ton/ha in the dictionary and are stored as kg/ha.
# The dictionary's replicate sentence says teff, and its treatment sentence says N and P2O5 rates. The treatment table and the study title are sorghum ridging and tie-ridging, and that is what is stored. No N or P rate is in the data.
# Year 1 is 2014-06-01 to 2014-12-01 and year 2 is 2015-06-01 to 2015-12-31. Those are the dictionary's season windows.
# Latitude_Location 346012 and Longitude_Location 1373577 are a projected grid. The dictionary says the coordinate reference system is unknown, so they are not stored as latitude and longitude.
# Stover is ton/ha and is not labeled fresh or dry, so it is not dmy_residue or fwy_residue.

## ISSUES
# Grain yield is not adjusted for moisture. No moisture value is given.
# No coordinates in decimal degrees, and on-farm is not stated.
# write_files: , on_farm, yield_moisture, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer. unknown variables: stover_yield_kg_ha_, head_length_cm_, head_weight_g_, tillers_per_plant_.

## NEW VARIABLES
# stover_yield_kg_ha_: stover in kg/ha. The file says ton/ha and does not say fresh or dry.
# head_length_cm_: head length, cm.
# head_weight_g_: head weight, g.
# tillers_per_plant_: tillers per plant at harvest.


## NOT PROCESSED
# Stand count is plants per field, and the field area is not given.


carob_script <- function(path) {

"
Effect of ridging and tie-ridging time on the yield performance of sorghum at Gumara-Maksegnit, north-western Ethiopia. Year 1 ran from 2014-06-01 to 2014-12-01 and year 2 from 2015-06-01 to 2015-12-31. Eight treatments and three replications.
"

	uri <- "hdl:20.500.11766.1/FK2/P024VP"
	group <- "agronomy"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "ICARDA",
		publication = NA,
		project = "Reducing land degradation and farmers' vulnerability to climate change in the highland dry areas of north-western Ethiopia",
		design = NA,
		data_type = "experiment",
		treatment_vars = "treatment",
		response_vars = "yield",
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 50,
		carob_effort = 0.45
	)

	f <- ff[basename(ff) == "ExperimentalData.csv"]
	t <- ff[basename(ff) == "Treatment.csv"]
	r <- read.csv(f, sep=";", check.names=FALSE)
	tr <- read.csv(t, sep=";", check.names=FALSE, fill=TRUE)
	tr <- tr[, 1:2]
	names(tr) <- c("Treatment", "treatment")
	r <- r[!is.na(r$Year), ]
	r$treatment <- tr$treatment[match(r$Treatment, tr$Treatment)]

	start <- c("1" = "2014-06-01", "2" = "2015-06-01")
	end <- c("1" = "2014-12-01", "2" = "2015-12-31")
	yr <- as.character(r$Year)

	d <- data.frame(
		trial_id = paste0("year", yr),
		country = "Ethiopia",
		location = "Gumara-Maksegnit",
		plot_id = paste(yr, r$Treatment, r$Replicate, sep="_"),
		rep = r$Replicate,
		crop = "sorghum",
		treatment = trimws(r$treatment),
		planting_date = unname(start[yr]),
		harvest_date = unname(end[yr]),
		yield = as.numeric(r$Yield) * 1000,
		yield_part = "grain",
		stover_yield_kg_ha_ = as.numeric(r$Stover) * 1000,
		plant_height = as.numeric(r$Plant_Height),
		head_length_cm_ = as.numeric(r$Head_Lenght),
		head_weight_g_ = as.numeric(r$Head_Weight),
		tillers_per_plant_ = as.numeric(r$Tillers_N),
		geo_from_source = FALSE,
		is_survey = FALSE
	)

	carobiner::write_files(path, meta, d)
}
