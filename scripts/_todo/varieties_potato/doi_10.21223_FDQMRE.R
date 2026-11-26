# R script for "carob"
# license: GPL (>=3)

## NOTES
# Survey of farmers' seed tubers in Cotopaxi, Ecuador, September to October 2018. The abstract names the province and the crop.
# The virus sheet is one row per plantlet (Obs). Six DAS-ELISA virus presence columns (0/1) are stored in the long table as disease_incidence (0 or 100 %) under disease.
# Canton centroids (Latacunga, Pujilí, Salcedo, Saquisilí) added from gazetteer (geo_from_source = FALSE).
# Altitude is stored as elevation. sample_id_ keeps the seed-lot ID.

## ISSUES
# Andean potato latent virus and Andean potato mottle virus are not yet in terminag values_disease; kept in disease per Carob guidelines so maintainers can add them.
# September to October 2018 is the survey date, not a planting or harvest date.
# No yield and no fertilizer rate.
# Insects_Fungi.csv has no header. It has 16 fields and the dictionary names 15 scores, so the columns were not assigned.
# The nematode sheet was not mapped. It is a subset of soil samples and was not joined to the seed lots.

## NOT PROCESSED
# Insects_Fungi.csv and Nematodes.csv.
# DataDictionary files: the definitions used above.
# Index and Other_Name.
# DataDictionary_ElementDescription.csv: documentation or a codebook, not an observation table.
# DataDictionary_Introduction.csv: documentation or a codebook, not an observation table.


carob_script <- function(path) {

"
Dataset for: Presence of seed-borne pest and pathogens on/in the seed produced by farmers in the province of Cotopaxi

Seed degeneration (PSD)  threats potato production in developing countries. PSD is defined as the accumulation of pest and pathogens in/on the seed tuber due to the successive cycles of vegetative propagation leading potentially to a yield and quality reduction (Thomas-Sharma et al., 2016; Pl. Path. [vol 65, issue 1]). However, the understanding of PSD in the Andes is deficient due to the limited comprehension of the spatial distribution of potato seed- and soil- borne pests and pathogens. For this reason, 260 farmers´ seed lots and fields were surveyed in the province of Cotopaxi-Ecuador from September to October 2018.   The survey was implemented using a stratified sampling design (stratum = Cantons of Latacunga, Pujilí, Salcedo and Saquisilí). The sample size was defined based on the seed replacement rate reported by farmers in a pilot study previously implemented. In each place, farmers kindly provided a sample of (1 to 10) potato seed tubers depending on their willingness. In addition to it, a soil sample was collected from the closest field to the house after farmers provided oral consent. Symptoms and damages on the seed tubers caused by insects and fungi were visually inspected following the methodology suggested by James (1971, [Canadian pl. dis. survey {vol. 51}]) and the photography guide of the main pests and pathogens of the potato crop in Ecuador (Montesdeoca et al., 2013)(Reported in sheet coined 'Insects and Fungi').   Virus identification was carried out on plantlets coming from the tubers assessed previously. This was performed by using the kits and the protocol for DAS-ELISA manufactured and suggested by CIP (2007). Six viruses were identified: PVX, PVS, PVY, APLV, PLRV, and APMoV (Reported in sheet coined 'Virus'). Forty three soil samples out of the 260 were selected depending on the farmers' field altitude and landscape location (Reported in sheet coined 'nematodes'). These were sent to the laboratory of Plant Protection of the National Agriculture Research Center (INIAP) for nematodes identification. Nematodes were identified according to the methodologies of Oostenbrink (1960) and Fenwick (1940).   It is expected that this database contributes to a deeper knowledge about the presence of seed-borne pests and pathogens in the tropical highlands of Ecuador and to design better seed system interventions.;The dataset contains data about presence of seed-borne pest and pathogens on/in the seed produced by farmers in the province of Cotopaxi.
"

	uri <- "doi:10.21223/FDQMRE"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=8, minor=1,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "survey",
		treatment_vars = "variety",
		response_vars = "disease_incidence",
		notes = NA,
		carob_contributor = NA,
		carob_date = "202?-??-??",
		carob_LLM = "Gemini 4 Neon",
		carob_completion = 45,
		carob_effort = 0.5
	)

	f1 <- ff[basename(ff) == "ID_Information.csv"]
	r1 <- read.csv(f1, sep=";", na.strings=c("NA", ""))
	f2 <- ff[basename(ff) == "Virus.csv"]
	r2 <- read.csv(f2, sep=";", na.strings=c("NA", ""))
	key <- function(a, b) paste(trimws(as.character(a)), trimws(as.character(b)))
	ik <- key(r1$ID, r1$Variety)
	vk <- key(r2$ID, r2$Variety)
	m <- match(vk, ik)
	variety <- trimws(as.character(r2$Variety))

	loc <- trimws(as.character(r1$Location[m]))
	lat <- ifelse(grepl("latacunga", loc, ignore.case=TRUE), -0.9352,
		ifelse(grepl("pujil", loc, ignore.case=TRUE), -0.9576,
		ifelse(grepl("salcedo", loc, ignore.case=TRUE), -1.0455,
		ifelse(grepl("saquisil", loc, ignore.case=TRUE), -0.8399, -0.9352))))
	lon <- ifelse(grepl("latacunga", loc, ignore.case=TRUE), -78.6155,
		ifelse(grepl("pujil", loc, ignore.case=TRUE), -78.6964,
		ifelse(grepl("salcedo", loc, ignore.case=TRUE), -78.5906,
		ifelse(grepl("saquisil", loc, ignore.case=TRUE), -78.6670, -78.6155))))

	d <- data.frame(
		trial_id = "Cotopaxi_seed_2018",
		country = "Ecuador",
		adm1 = "Cotopaxi",
		adm2 = loc,
		location = loc,
		latitude = lat,
		longitude = lon,
		elevation = as.numeric(r1$Altitude[m]),
		crop = "potato",
		variety = variety,
		treatment = variety,
		record_id = seq_len(nrow(r2)),
		sample_id_ = trimws(as.character(r2$ID)),
		obs_ = as.integer(r2$Obs),
		stress = "disease",
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	vspecs <- list(
		list("Presence_PVX", "potato virus X"),
		list("Presence_PVS", "potato virus S"),
		list("Presence_PVY", "potato virus Y"),
		list("Presence_PLRV", "potato leafroll virus"),
		list("Presence_APLV", "Andean potato latent virus"),
		list("Presence_APMoV", "Andean potato mottle virus")
	)
	dlong <- do.call(rbind, lapply(seq_along(vspecs), function(i) {
		val <- as.integer(r2[[vspecs[[i]][[1]]]])
		keep <- !is.na(val)
		data.frame(
			record_id = d$record_id[keep],
			order = i,
			disease = vspecs[[i]][[2]],
			disease_incidence = as.character(val[keep] * 100)
		)
	}))

	carobiner::write_files(path, meta, d, long=dlong)
}
