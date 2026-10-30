# REJECTED 
# Reason: no source columns matched draft() terminag heuristics (only empty data.frame() stubs or no tabular sheets)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Participatory mapping for long-term monitoring and red listing of cultivated potato populations in La Paz, Bolivia.

Biodiversity sustains a wide variety of genetic traits that are very valuable for the potato's adaption to changing environments and successful cultivation in the future. It is assumed that several factors are threatening biodiversity in the Andes. Without robust baselines and indicators used for reliable time lines, it is impossible to assess diversity loss or increase. Long-term studies in situ are necessary to distinguish short term fluctuation from general tendencies caused by environmental and socio-cultural shifts and to understand ongoing evolutionary principles within the potato genepool. Powerful tools are needed that facilitate the management, evolution and exchange of mainly oral transmitted traditional knowledge. Participatory mapping is such a tool where cognitive spatial knowledge is transformed into maps at grassroots level by trained community members. Local knowledge can be documented in context with social, political, biotic and abiotic factors and if accompanied with comprehensive capacity building and the application of robust indicators will strengthen the cross-generation knowledge exchange and sensitization towards land use shifts, resource dynamics affected by climate and societal related factors and pressure.

Most local farmers practice traditional small scale agriculture with focus on potato production, partly organized in sectoral fellow systems. Because of the high biological diversity, coexistence of wild potato species and the extensive local knowledge owned by the potato farmers, communities of this study site contribute to generate knowledge on dynamics of potato diversity in situ and complement the diversity hotspot network 'Chirapaq Ñan' for long-term in situ monitoring of cultivated potato. This dataset contributes to i) elaborate robust baseline to assess spatial and temporal dynamics and distribution ranges of potato diversity within and among fields, families and altitudinal belts, ii) elaborate on the application of utilized diversity indexes for red listing of potato landraces and cultivar types. The dataset contains parcel and community maps, geographic distribution of cultivated potato populations, red listing of potato landraces and cultivar types and supplementary files (surveys, protocols) used for this study.
"

	uri <- "doi:10.21223/P3/PYYPSD"
	group <- "survey"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=5,
		data_organization = "CIP; Universidad Publica de El Alto; Instituto Nacional de Investigaciones Agropecuarias",
		publication = "",
		project = NA,
		carob_date = "2026-10-30",
		design = NA,
		data_type = NA,
		treatment_vars = "",
		response_vars = "", 
		carob_contributor = "Your Name",
		completion = 0,	
		notes = "",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "Household_Dataset_LaPaz.xlsx"]
	f2 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.CPG"]
	f3 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.dbf"]
	f4 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.prj"]
	f5 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.rar"]
	f6 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.sbn"]
	f7 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.sbx"]
	f8 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.shp"]
	f9 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.shp.xml"]
	f10 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Coromata.shx"]
	f11 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.CPG"]
	f12 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.dbf"]
	f13 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.prj"]
	f14 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.rar"]
	f15 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.sbn"]
	f16 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.sbx"]
	f17 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.shp"]
	f18 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.shp.xml"]
	f19 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Icrana.shx"]
	f20 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.CPG"]
	f21 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.dbf"]
	f22 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.prj"]
	f23 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.rar"]
	f24 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.sbn"]
	f25 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.sbx"]
	f26 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.shp"]
	f27 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.shp.xml"]
	f28 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pairumani.shx"]
	f29 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.CPG"]
	f30 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.dbf"]
	f31 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.prj"]
	f32 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.rar"]
	f33 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.sbn"]
	f34 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.sbx"]
	f35 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.shp"]
	f36 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.shp.xml"]
	f37 <- ff[basename(ff) == "CommunityBoundaries_Bolivia_Pongonhuyo.shx"]

	r1a <- carobiner::read.excel(f1, sheet="data")
	r1b <- carobiner::read.excel(f1, sheet="DataDiccionary")
	#r2 <- read.???(f2)
	#r3 <- read.???(f3)
	#r4 <- read.???(f4)
	#r5 <- read.???(f5)
	#r6 <- read.???(f6)
	#r7 <- read.???(f7)
	#r8 <- read.???(f8)
	#r9 <- read.???(f9)
	#r10 <- read.???(f10)
	#r11 <- read.???(f11)
	#r12 <- read.???(f12)
	#r13 <- read.???(f13)
	#r14 <- read.???(f14)
	#r15 <- read.???(f15)
	#r16 <- read.???(f16)
	#r17 <- read.???(f17)
	#r18 <- read.???(f18)
	#r19 <- read.???(f19)
	#r20 <- read.???(f20)
	#r21 <- read.???(f21)
	#r22 <- read.???(f22)
	#r23 <- read.???(f23)
	#r24 <- read.???(f24)
	#r25 <- read.???(f25)
	#r26 <- read.???(f26)
	#r27 <- read.???(f27)
	#r28 <- read.???(f28)
	#r29 <- read.???(f29)
	#r30 <- read.???(f30)
	#r31 <- read.???(f31)
	#r32 <- read.???(f32)
	#r33 <- read.???(f33)
	#r34 <- read.???(f34)
	#r35 <- read.???(f35)
	#r36 <- read.???(f36)
	#r37 <- read.???(f37)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
