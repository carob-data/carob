# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: DArTSeq data generated from 662 parents of the global breeding program of the International Potato Center

Molecular markers dataset of 662 sweetpotato parents from CIP Peru Breeding platform genotyped using DArTSeq. The single nucleotide polymorphism (SNP) markers were analyzed to develop a low-density highly informative marker set for quality assurance and control in sweetpotato breeding.   The dataset was included in the article: Development of diagnostic SNP markers for quality assurance and control in sweetpotato. S1.Data and S1.Table
"

## when done, remove all the default comments, such as this one, from the script
## only keep the comments you added that are specific to this dataset

	uri <- "doi:10.21223/A3PZVW"
	group <- "genotypes"
	ff  <- carobiner::get_data(uri, path, group)


	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		# include the data provider and/or all institutes listed as authors (if any)
		data_organization = "CIP",
		publication = "",
		project = NA,
		# if available report the experimental or survey design
		design = NA,
		
		# data_type can be e.g. "on-farm experiment", "survey", "compilation"
		data_type = NA,
		
		# treatment_vars has semi-colon separated variable names that represent the
		# treatments if the data is from an experiment. E.g. "N_fertilizer;P_fertilizer;K_fertilizer"
		treatment_vars = "",
		
		# response variables of interest such as yield, fwy_residue, disease incidence, etc. Do not include variables
		# that describe management for all treatments or other observations that were not related to the aim of 
		# the trial (e.g. the presence of a disease).
		response_vars = "", 

		# notes for the end-user
		notes = "",

		carob_contributor = "Your Name",
		carob_date = "2026-10-30",
		# The percentage of relevant variables that have been standardized (between 0 and 100%) 
		carob_completion = 0,	
		# The number of hours spent creating this script
		carob_effort = -1
	)
	

	f1 <- ff[basename(ff) == "Dictionary_journal.pone.0232173.s008.xlsx"]
	f2 <- ff[basename(ff) == "S1.Table_journal.pone.0232173.s006.xlsx"]
	f3 <- ff[basename(ff) == "S1.Data_journal.pone.0232173.s001.csv"]

	r1 <- carobiner::read.excel(f1)
	r2 <- carobiner::read.excel(f2)
	r3 <- read.csv(f3)

## select the variables of interest and assign them to the correct name

	d1 <- data.frame(
		rep = r1[["File name: Report_DSp18-2632_SNP_2.csv"]]
	)
##r1: "...2", "...3"


	d2 <- data.frame(
		country = r2[["Country"]]
	)
##r2: "No", "Names", "Code"


	d1 <- data.frame(
		rep = r1[["RepAvg"]]
	)
##r1: "No", "AlleleID", "AlleleSequence", "TrimmedSequence", "Chrom_Sweet_Potato_Trifida_MSU_v3", "ChromPos_Sweet_Potato_Trifida_MSU_v3", "AlnCnt_Sweet_Potato_Trifida_MSU_v3", "AlnEvalue_Sweet_Potato_Trifida_MSU_v3", "SNP", "SnpPosition", "CallRate", "OneRatioRef", "OneRatioSnp", "FreqHomRef", "FreqHomSnp", "FreqHets", "PICRef", "PICSnp", "AvgPIC", "AvgCountRef", "AvgCountSnp", "CIP189129.12", "CIP106700.1", "CIP107800.3", "CIP113063.2", "CIP189168.32", "CIP189127.1", "CIP189151.5", "CIP189125.9", "CIP189134.1", "CIP194529.6", "CIP106437.3", "CIP106629.1", "CIP189148.18", "CIP107807.3", "CIP107806.2", "CIP113078.1", "CIP190089.6", "CIP194568.12", "CIP189206.14", "CIP189165.38", "CIP190094.36", "CIP106093.1", "CIP106595.1", "CIP106631.1", "CIP194515.15", "CIP107766.2", "CIP107806.3", "CIP113091.1", "CIP190094.29", "CIP189151.8", "CIP189164.71", "CIP194506.8", "CIP194519.28", "CIP106625.2", "CIP106082.1", "CIP106319.1", "CIP194539.36", "CIP107807.2", "CIP113001.1", "CIP113100.1", "CIP190094.91", "CIP194575.8", "CIP189165.9", "CIP194531.35", "CIP194519.5", "CIP106161.1", "CIP106449.1", "CIP106385.1", "CIP194540.5", "CIP107766.1", "CIP113002.1", "CIP113112.1", "CIP194513.23", "CIP194579.10", "CIP189153.7", "CIP194544.2", "CIP189145.21", "CIP106250.1", "CIP106752.1", "CIP106385.2", "CIP189151.34", "CIP107796.1", "CIP113003.1", "CIP113113.1", "CIP194581.4", "CIP189153.8", "CIP189153.18", "CIP194533.44", "CIP189153.3", "CIP106415.1", "CIP106385.3", "CIP106437.1", "CIP189148.2", "CIP107806.4", "CIP113008.1", "CIP194583.2", "CIP189153.1", "CIP189123.3", "CIP194541.55", "CIP189152.31", "CIP106672.1", "CIP106840.1", "CIP106586.1", "CIP189165.26", "CIP107786.2", "CIP113063.1", "CIP189157.6", "CIP189151.32", "CIP189125.2", "CIP194557.14", "CIP194556.56", "CIP106373.1", "CIP106308.1", "CIP106538.1", "CIP113114.1", "CIP113257.2", "CIP113445.4", "CIP105066.2", "CIP113232.1", "CIP113315.1", "CIP113431.1", "CIP113076.1", "CIP113144.2", "CIP113377.1", "CIP113235.1", "CIP113242.2", "CIP113119.1", "CIP113439.2", "CIP113445.6", "CIP105076.2", "CIP113233.1", "CIP113317.1", "CIP113433.1", "CIP113076.4", "CIP113144.3", "CIP113377.2", "CIP113238.2", "CIP113242.3", "CIP113138.1", "CIP113443.2", "CIP113445.7", "CIP105083.1", "CIP113255.1", "CIP113328.1", "CIP113433.2", "CIP113078.36", "CIP113144.4", "CIP113380.4", "CIP113238.4", "CIP113242.4", "CIP113140.1", "CIP113443.4", "CIP105152.4", "CIP105080.1", "CIP113279.1", "CIP113349.1", "CIP113434.1", "CIP113078.3", "CIP113145.2", "CIP113381.1", "CIP113238.5", "CIP113242.5", "CIP113154.1", "CIP113443.5", "CIP105154.1", "CIP105080.6", "CIP113280.1", "CIP113364.1", "CIP113459.1", "CIP113138.3", "CIP113145.4", "CIP113381.2", "CIP113238.7", "CIP113242.6", "CIP113159.1", "CIP113444.1", "CIP105072.3", "CIP105167.2", "CIP113280.2", "CIP113415.1", "CIP113468.2", "CIP113138.4", "CIP113335.1", "CIP113391.1", "CIP113239.1", "CIP113242.7", "CIP113159.2", "CIP113444.2", "CIP105157.1", "CIP113280.3", "CIP113418.1", "CIP113065.5", "CIP113143.1", "CIP113339.1", "CIP113391.2", "CIP113239.2", "CIP113249.1", "CIP113166.1", "CIP113445.1", "CIP105066.1", "CIP113283.1", "CIP113424.1", "CIP113065.6", "CIP113144.1", "CIP113339.2", "CIP113224.3", "CIP113240.1", "CIP113250.1", "CIP105126.3", "CIP113530.1", "CIP113635.1", "CIP113486.2", "CIP105100.2", "CIP105096.1", "CIP105523.1", "CIP107257.2", "CIP107160.3", "CIP113482.2", "CIP113490.2", "CIP113513.1", "CIP105229.1", "CIP113534.1", "CIP113636.1", "CIP113489.1", "CIP105139.1", "CIP105097.1", "CIP105217.1", "CIP107300.1", "CIP107160.4", "CIP113482.3", "CIP113498.1", "CIP113514.1", "CIP105086.1", "CIP113548.1", "CIP113638.2", "CIP113491.2", "CIP105058.2", "CIP105511.1", "CIP107359.3", "CIP107304.2", "CIP107284.3", "CIP113484.1", "CIP113499.1", "CIP113524.1", "CIP105373.1", "CIP113562.1", "CIP113638.3", "CIP113491.3", "CIP105030.1", "CIP105008.1", "CIP107279.2", "CIP107350.1", "CIP107341.8", "CIP113486.3", "CIP113499.2", "CIP113525.1", "CIP105458.1", "CIP113564.1", "CIP113638.4", "CIP113563.1", "CIP105212.1", "CIP105213.1", "CIP107215.2", "CIP107238.1", "CIP107095.1", "CIP113486.4", "CIP113504.1", "CIP113525.2", "CIP105460.1", "CIP113576.1", "CIP113638.5", "CIP113563.2", "CIP105269.1", "CIP105160.1", "CIP107285.1", "CIP107252.1", "CIP107143.1", "CIP113486.5", "CIP113505.1", "CIP113525.3", "CIP105103.1", "CIP113582.1", "CIP113639.2", "CIP105495.1", "CIP105175.1", "CIP107285.3", "CIP107050.2", "CIP113480.1", "CIP113488.1", "CIP113505.2", "CIP113526.1", "CIP105100.1", "CIP113584.1", "CIP113486.1", "CIP105071.1", "CIP105107.1", "CIP107038.3", "CIP107154.1", "CIP113482.1", "CIP113490.1", "CIP113512.1", "CIP113527.1", "CIP113563.3", "CIP113615.3", "CIP113618.4", "CIP113476.8", "CIP113478.3", "CIP113502.3", "CIP113632.3", "CIP113563.4", "CIP113615.4", "CIP113618.7", "CIP113476.9", "CIP113479.1", "CIP113502.4", "CIP113583.1", "CIP113615.5", "CIP113619.3", "CIP113476.10", "CIP113479.2", "CIP113502.5", "CIP113605.2", "CIP113615.7", "CIP113619.4", "CIP113477.1", "CIP113479.3", "CIP113612.1", "CIP113605.3", "CIP113615.8", "CIP113619.5", "CIP113477.2", "CIP113493.2", "CIP113616.1", "CIP113609.1", "CIP113615.9", "CIP113476.1", "CIP113477.4", "CIP113501.1", "CIP113616.4", "CIP113609.2", "CIP113618.1", "CIP113476.6", "CIP113477.5", "CIP113502.1", "CIP113623.1", "CIP113615.2", "CIP113618.3", "CIP113476.7", "CIP113478.2", "CIP113502.2", "CIP113632.2", "MUSG16038.10", "MUSG014061.10", "MUSG16080.12", "MUSG14010.54", "MUSG01501.28", "MGSG13136.6", "MUSG012068.13", "MGSG114018.1", "MGCL01", "MGSGR1507.14", "MGSG12135.14", "MCKSG13003.1295", "MGSG12015.7", "TIS2534", "MUSG16114.10", "MUSG16004.5", "SGR15063.3", "MCKSG13003.1035", "MGSG14011.1", "MGSG15003.19", "MGSGR15002.16", "MGSG15019.21", "MGSGR15013.11", "MUSG012004.14", "MGSG12046.16", "MUSG0120047.24", "MUSG15091.11", "MUSG16013.5", "USG012068.13", "MUSG01412.76", "TIOJOE", "MUSG15108.12", "MUSG14045.3", "MUSG0120146.2", "MUSG012004.14.1", "MUSG014044.7", "ININDA", "MUSG16147.17", "MUSG01505.52", "MGSG11005.46", "USG11040.16", "MGSG15003.19.1", "MGSG14017.10", "MGSG14016.9", "MGSG11002.32", "MUSG01509.117", "MUSG012001.9", "MUSG01505.41", "MGSG15001.3", "MUSG16119.17", "MUSG01124.6", "MGSG16030.10", "Xitsekele", "MUS014001.3", "MUSG014065.8", "ESTHER", "MGSGR15075.10", "MAPHUTA", "MUSGP0646.126", "JEWEL13", "MUSG0120113.8", "MUSG014015.12", "MUSG16019.11", "MUSG16047.18", "USG012041.10", "CECILIA", "MUSG01452.21", "CN1448.49.26", "MUSG014012.26", "MGSGR15068.21", "IRENE", "MGSG13139.3", "MUSG15108.18", "MUSG16031.14", "MUSG16086.17", "VICTORIA", "MGSG16057.2", "MUSG012061.13", "MCKSG13003.1292", "MGSG15003.16", "MUSG14008.21", "MGSG11003.3", "MUSG16014.16", "BELA", "X105199.29", "MUSG14008.14", "RESISTO", "MUSG014046.20", "MUSG1605.17", "MGSG15027.2", "MUSG014012.17", "CEMSA", "ALISHA", "MGSGR15008.5", "RAK808", "MSK1040", "Ukerewe", "MUSG012033.33", "MUSG16112.4", "NHACUTSE3", "MUSG16078.2", "MUSG15134.14", "MUSG01503.33", "SilkOmuyaka", "Silk1254", "X107040.12", "MPG1146", "Dimbuka.Bukulula", "KM188", "MUSG01505.66", "XIPHONE", "MUSG01502.57", "MUSG16148.8", "MUSG11010.5", "CarrotC", "RAK786", "RAK819", "MGSG012081.3", "ARA352", "Oguroiwe", "MPG1148", "MGSG14016.11", "MUSG014044.12", "MUSG16007.8", "MGSGR15075.8", "MUSG15023.8", "Ejumula", "MPG1128", "SPK004", "SUMAIA", "LUW1274", "KM159", "IGA983", "MUSG16014.16.1", "MCKSG08020.6", "MGSG16100.9", "MGSG15033.12", "MUSG01509.42", "Mayai", "Tanzania", "NK259L", "MGSGR15006.15", "MKN1168", "PAL108", "MUSG01501.78", "MGSGR15091.3", "X199062.1", "NHACOONGO1", "MUSG16127.4", "Kala", "KML881", "MUSG15017.16", "NASPOT7", "APA335", "RAK835", "MUSG01508.49", "MUSG16143.20", "XIADLA.XAKAU", "MUSG01509.42.1", "MUSG15117.14", "kamamanzi", "KBL648", "NASPOT9.O", "MUSG15105.1", "NASPOT10.O", "APA323", "MUSG16043.15", "MUSG15040.9", "CHISSICUANA2", "MUSG16078.7", "MUSG16059.11", "KM161", "KML956", "Tororo3", "MUSG16032.2", "KML872", "Otada", "MGSGR15047.14", "ADMARC", "CHINGORO", "MUSG15047.8", "MUSG16045.14", "KBL613", "BSH740", "NASPOT1", "MGSG15018.1", "Naveto", "Kyabafuruku", "PG17305.N1", "RESISTO.1", "Mary", "MBR536", "IGA978", "SPK004.CIP", "Wagabolige", "KoromojoRed", "Bitambi", "MCKSG0820.8", "Xushu18", "Tainung64", "ARA209", "KML942", "KRS662", "Sowola", "MPG158", "LIR258", "DLP3163", "KBL611", "PAL133Tengerere", "Yanshu1", "KRE696", "HMA496", "LIR302", "MLE179", "HMA490", "Jonathan", "PAL148", "MSK1094", "MKN1171", "MSD384", "Zambezi", "Raihna", "MSD380", "KBL650", "MSD431", "APA356", "KML960", "RAK865", "TIS9101", "MLE191", "SRT27", "Jewel", "TIS9265", "LUW1230", "MPG1151", "MLE163", "MKN1210", "BSH741", "MPG1122", "NASPOT3", "IGA998", "NK318L", "Caromax", "CIP199062", "SRT43", "MKN1180", "Magabali", "NASPOT5", "BAND154L", "MSK1079", "NASPOT11", "PAL134", "Resisto.CIP", "SantaAmaro", "SRT28", "LUW1257", "KBL618", "SRT41", "RAK848", "K.566632", "WT.237", "Beauregard", "Huarmayana", "MLE199", "KRE691", "MBR552", "ARA236", "SRT01", "Mugande", "Newkawogo", "PAL133", "PG17305.N1.1", "PGA12086.18", "PGA14008.9", "PGA12164.26", "CIP440390", "CRI.AGRA.SP09", "PGN16130.4", "PGA12010.15", "PG17140.N2", "Jitihada", "CRI.Gavana", "Patron", "Tu.Orange", "CRI.AGRA.SP13", "PGA14372.3", "Tiebele.2", "PG17136.N1", "PGA12135.13", "PG17205.N1", "PGA14229.2", "CRI.Ligri", "PGN16024.27", "PGN16203.18", "MothersDelight", "PG17212.N3", "Bohye", "PG17171.N1", "Okumkom", "NKO3A", "PGA14351.36", "PGA14171.1", "PGA16070.1", "Nanungungungu", "PGA12166.30", "CRI.Mbofara", "Hi.starch", "PGA12151.79", "PGA14008.15", "PGA16026.1", "Tu.Purple", "Sauti", "PGA14351.4", "PGA14398.4", "BF59xCIP4", "PGN16030.30", "PGA14011.13", "PGA14011.43", "Obare", "Blueblue", "Otoo", "PGA14442.1", "PGN16092.6", "PGA14008.22", "Ogyefo", "Dadanyuie", "SantomPona", "Apomuden", "CRI.Kofi.Annan", "PGN16024.28", "Faara"


## separate individual trials. For example trials in different locations or years. 
## do _not_ separate by treatments within a trial. For a survey, each row gets a unique trial_id
	d$trial_id <- as.character(as.integer(as.factor( ____ )))
	
## about the data (TRUE/FALSE)
	d$on_farm <- 
	d$is_survey <- 
	d$irrigated <-
	
## crop rotation. If available, add all crops, including "d$crop". Use an underscore for intercrops 
    d$crop_rotation <- "crop1;crop2;crop3_crop4"
	
## each site must have corresponding longitude and latitude
## if the raw data do not provide them you can estimate them from the location/adm data 
## see carobiner::geocode
	d$longitude <- 
	d$latitude <- 
# are the coordinates from the source (data/publication) or estimated by you?	
	d$geo_from_source <- TRUE/FALSE


## time can be year ("2023", four characters), year-month ("2023-07", 7 characters) or date ("2023-07-21", 10 characters).
## if dates come as character values, you can use as.character(as.Date()) for dates to assure the correct format.
	d$planting_date <- as.character(as.Date(   ))
	d$harvest_date  <- as.character(as.Date(    ))

### Fertilizers 
## note that we use P and K, not P2O5 and K2O
## P <- P2O5 / 2.29
## K <- K2O / 1.2051
   d$P_fertilizer <- 
   d$K_fertilizer <-
   d$N_fertilizer <- 
   d$S_fertilizer <- 
   d$lime <- 
## normalize names 
   d$fertlizer_type <- 

## for legumes   
   d$inoculated <- TRUE or FALSE
   d$inoculant <- "name of inoculant"
   
### in general, add comments to your script if computations are
### based on information gleaned from metadata, a publication, 
### or when they are not immediately obvious for other reasons

### Yield

	yield <- r$yield_tonha * 1000
	#what plant part does yield refer to?
	d$yield_part <- "tubers"
	d$yield_moisture <- r$moisture * 100

#NOTE: yield is the _fresh weight_ production (kg/ha) of the "yield_part 
# Also record fresh and/or dry weight production of other organs (or "residue" or "total")
# if the data allow for that 

	d$fwy_storage <- r$yield_tonha * 1000
	d$dmy_storage <- (1-r$moisture) * r$yield_tonha * 1000
	d$dmy_totat <- r$dry_biomass
	
# all scripts must end like this
	carobiner::write_files(path, meta, d)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)

