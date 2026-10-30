# R script for "carob"
# license: GPL (>=3)

## ISSUES
# Measurement files are restricted on CIP Dataverse and were not downloaded. Only unrestricted documentation is in the raw folder, so no data table was built.
# Restricted files not on disk: subchapter 2.3_Insecticides dataset.xls.

carob_script <- function(path) {

"
Replication data for: Efficacy of Insecticides to Control the Leafminer Fly Liriomyza Huidobrensis (Blanchard) And its Impacts on Associated Natural Enemies in Potato Cropping Systems of the Cañete Region

The study was conducted in the winter cropping season (from July to November 2007) in the localities of San Juan de Ihuanco (13º 05' S, 76º 27' W, 105 m asl) and Cañete (13º 05' S, 76º 24' W, 40-150 m asl) in the Cañete region, situated 130 and 144 km south of Lima, respectively. Both localities represented complex (Ihuanco valley) and simple (Cañete valley) agricultural landscapes described in subchapter 4.1. During the potato vegetation period the average temperature was 15.6 ºC, with a mean monthly rainfall of 0.6 mm and a relative humidity of >80%. The experiment was laid out in a completely randomized block design involving three treatments with two replicates (n=6 plots). The treatments included: (i) control without the use of insecticides, (ii) selective insecticides and (iii) broad-spectrum insecticides. Each plot consisted of 25 rows by 25 m long (625 m2). All plots were separated by a distance of 4 m. Potato (cv. Canchan) was planted at a distance of 0.3 x 0.9 m. Three experimental fields were installed in each landscape. Additionally, five commercial potato farmers (n=5 plots) participated in the study in each landscape providing 0.1 ha potato plots within their broad-spectrum insecticide treated fields. Evaluations were made in a similar plot size as for experimental fields. The concentrations used were based on recommended field rates of these products. (i) Selective insecticide. Two translaminar insecticides abamectin (Vertimec 18EC Syngenta) and cyromazine (Trigard 75 WP Novartis) were applied at recommended rates (abamectin: 1.5 cc/liter; cyromazine: 0.035 g/liter). Abamectin was applied at GS30, GS40 and GS50, and cyromazine at GS60 and GS70. (ii) Broad-spectrum insecticides. Two translaminar (abamectin and cyromazine) and two non-translaminar (metamidophos: Tamaron 600SL and alphacypermetrin: Cipermex Super ® 100 EC) insecticides were used at 3 and 1.5 cc/liter, respectively. Metamidophos and cypermetrin are the most common broad-spectrum insecticides used by farmers in the main potato production areas in the Peruvian coast (Cañete, Barranca, and Arequipa) (CIP, 2004). Insecticides were applied five times during the potato season at growth stages GS30 (alphacypermetrin), GS40 (abamectin), GS50-60 (cyromazine + metamidophos), GS60-70 (cyromazine + alphacypermetrin) and GS70-80 (abamectin). (iii) Control treatment. In non-treated plots water was used as control.  The assessments were: larval infestation, parasitism and parasitoid species,  Soil-inhabiting predators, insecticide impact, and potato yield.
"

	uri <- "doi:10.21223/P3/IMD8V3"
	group <- "varieties_potato"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=3,
		data_organization = "CIP",
		publication = NA,
		project = NA,
		design = NA,
		data_type = "experiment",
		treatment_vars = NA,
		response_vars = NA,
		notes = NA,
		carob_date = "2026-10-30",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 0,
		carob_effort = 0.15
	)

	# No measurement table is available in the raw folder.
	return(FALSE)
}

