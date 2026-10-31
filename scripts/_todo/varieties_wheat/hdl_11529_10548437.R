# R script for "carob"
# license: GPL (>=3)

## NOTES
# International wheat nursery. RawData and Loc_data are tab-separated files with an .xls extension. There is no EnvData file, so sowing, harvest, and irrigation variables that proc_wheat reads from EnvData are absent.
# The location file names the east/west column Longitude. proc_wheat expects Longitud, so that column is renamed before proc_wheat reads the files.
# A one-row EnvData placeholder is passed so proc_wheat can run. Its trait name is not a nursery trait and is not copied into the output.
# Coordinates and country come from the location and raw files. geo_from_source is set inside proc_wheat.

## ISSUES
# There is no EnvData file, so harvest date is missing. proc_wheat kept records with missing yield. Grain Zn is outside the vocabulary range. Those values were not changed.
# write_files: missing variables: harvest_date. out of bounds: grain_Zn (16.8, 64.9). NA detected: yield. contributor:


carob_script <- function(path) {

"10th Harvest Plus Advanced Nursery

The Harvest Plus Advanced Nursery (HPAN) contains spring bread wheat (Triticum aestivum) germplasm adapted to ME1 (Optimally Irrigated, low rainfall environment) and ME5 (Warmer area environment) environments. It has 100-200 entries with a single replication, white grain color and distributed to more than 35 locations."

	uri <- "hdl:11529/10548437"
	group <- "varieties_wheat"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "CIMMYT",
		publication = NA,
		project = "10th Harvest Plus Advanced Nursery",
		design = NA,
		data_type = "experiment",
		treatment_vars = "variety_code",
		response_vars = "yield",
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 60,
		carob_effort = 0.3
	)

	fraw <- ff[grep("RawData", basename(ff))]
	floc <- ff[grep("Loc_data", basename(ff))]
	fenv <- ff[grep("EnvData", basename(ff))]
	if (length(fenv) == 0) {
		loc <- read.csv(floc, sep="\t", fileEncoding="latin1", check.names=FALSE)
		names(loc)[names(loc) == "Longitude"] <- "Longitud"
		raw1 <- read.csv(fraw, sep="\t", fileEncoding="latin1", nrows=1)
		tmp <- tempfile("hpan")
		dir.create(tmp)
		floc <- file.path(tmp, "10HPAN_Loc_data.xls")
		fenv <- file.path(tmp, "10HPAN_EnvData.xls")
		write.table(loc, floc, sep="\t", row.names=FALSE, quote=FALSE, fileEncoding="latin1")
		env <- data.frame(
			Trait.name = "NOT_IN_SOURCE",
			Value = "0",
			Trial.name = raw1$Trial.name[1],
			Loc_no = raw1$Loc_no[1],
			Country = raw1$Country[1],
			Cycle = raw1$Cycle[1],
			check.names = FALSE
		)
		write.table(env, fenv, sep="\t", row.names=FALSE, quote=FALSE, fileEncoding="latin1")
		ff <- c(fraw, floc, fenv)
	}

	proc_wheat <- carobiner::get_function("proc_wheat", path, group)
	d <- proc_wheat(ff)
	carobiner::write_files(path, meta, d$wide, d$long)
}
