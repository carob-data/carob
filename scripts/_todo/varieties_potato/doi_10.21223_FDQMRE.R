# R script for "carob"
# license: GPL (>=3)

## NOTES
# Survey of farmers' seed tubers in Cotopaxi, Ecuador, September to October 2018. The abstract names the province and the crop.
# The virus sheet is one row per plantlet (Obs). Location and altitude come from ID_Information, matched on sample ID and variety.
# Presence columns are 0 for absence and 1 for presence, as the dictionary says for the same kind of score.
# Altitude is stored as elevation.
# sample_id in the vocabulary identifies a soil sample. The seed-lot code stays sample_id_.

## ISSUES
# No coordinates. The cantons need georeferencing.
# Variety is not an applied treatment. It is the variety of the seed lot, and it is the treatment variable because it is what differs among rows.
# September to October 2018 is not a planting or harvest date.
# No yield and no fertilizer rate.
# Insects_Fungi.csv has no header. It has 16 fields and the dictionary names 15 scores, so the columns were not assigned.
# The nematode sheet was not mapped. It is a subset of soil samples and was not joined to the seed lots.
# write_files: missing variables: carob_contributor, yield, yield_moisture, yield_part, latitude, longitude, irrigated, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date. unknown variable: sample_id_ (sample_id?). unknown variables: obs_, virus_pvx_, virus_pvs_, virus_pvy_, virus_plrv_, virus_aplv_, virus_apmov_.

## NOT PROCESSED
# Insects_Fungi.csv and Nematodes.csv.
# DataDictionary files: the definitions used above.
# Index and Other_Name.


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
		response_vars = "virus_pvx_",
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 35,
		carob_effort = 0.4
	)

	id <- read.csv(ff[basename(ff) == "ID_Information.csv"], sep=";", na.strings=c("NA", ""))
	v <- read.csv(ff[basename(ff) == "Virus.csv"], sep=";", na.strings=c("NA", ""))
	key <- function(a, b) paste(trimws(as.character(a)), trimws(as.character(b)))
	ik <- key(id$ID, id$Variety)
	vk <- key(v$ID, v$Variety)
	m <- match(vk, ik)
	variety <- trimws(as.character(v$Variety))

	d <- data.frame(
		trial_id = "Cotopaxi_seed_2018",
		country = "Ecuador",
		adm1 = "Cotopaxi",
		adm2 = trimws(as.character(id$Location[m])),
		location = trimws(as.character(id$Location[m])),
		elevation = as.numeric(id$Altitude[m]),
		crop = "potato",
		variety = variety,
		treatment = variety,
		record_id = seq_len(nrow(v)),
		sample_id_ = trimws(as.character(v$ID)),
		obs_ = as.integer(v$Obs),
		virus_pvx_ = as.integer(v$Presence_PVX),
		virus_pvs_ = as.integer(v$Presence_PVS),
		virus_pvy_ = as.integer(v$Presence_PVY),
		virus_plrv_ = as.integer(v$Presence_PLRV),
		virus_aplv_ = as.integer(v$Presence_APLV),
		virus_apmov_ = as.integer(v$Presence_APMoV),
		stress = "disease",
		on_farm = TRUE,
		is_survey = TRUE,
		geo_from_source = FALSE
	)

	carobiner::write_files(path, meta, d)
}
