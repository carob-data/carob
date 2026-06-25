# R script for "carob"


carob_script <- function(path) {

"In deze dataset en het bijbehorende rapport worden de resultaten van het biologische bedrijfssysteem op WUR-proeflocatie Vredepeel in de periode 2001-2016 gepresenteerd met focus op gewasopbrengst, bodemkwaliteit, bemesting en stikstofuitspoeling.
Het biologisch systeem heeft opbrengsten die gemiddeld 6% onder het streven liggen, een goede bodemvruchtbaarheid en een nitraatconcentratie in het grondwater onder de norm van de nitraatrichtlijn. De nutriëntenoverschotten van stikstof, fosfaat en kali zijn hoger dan de streefwaarde. De opbrengsten liggen gemiddeld ruim onder die van de gangbare teelt door het optreden van ziekten en plagen. Veranderingen in opbrengst, bodemkwaliteit en uitspoeling in de periode 2000-2016 zijn niet met metingen vastgesteld behalve een stijging in organisch stofgehalte. Hierdoor kan ook niet aangetoond worden dat door biologische landbouw de bodemkwaliteit verbetert en ecosysteemdiensten verbeteren. Onduidelijk is hoe het biologisch systeem tot een lage uitspoeling komt bij een relatief hoge stikstof- en organische stofaanvoer. Nader onderzoek is hiervoor nodig."

	uri <- "doi:10.17026/dans-x3n-64rp"
	group <- "agronomy"

	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=3, minor=0,
		data_organization = "WUR",
		publication=NA,
		project=NA,
		data_type= "experiment",
		carob_contributor= "Robert Hijmans",
		carob_effort = NA,
		carob_date="2024-07-15",
		response_vars = "yield",
		treatment_vars = ""
	)
	
	f <- ff[basename(ff) == "  "]
	  
	carobiner::write_files(meta, d, path=path)
}

