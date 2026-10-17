# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. the NPK grade isn’t stated
#2. Geolocation data not provided in raw data


carob_script <- function(path) {

"
Enhancing farmers’ access to technology and market for increased sorghum productivity in the selected staple crop processing zones

The Agricultural Transformation Agenda Support Program Phase 1 (AT ASP-1) of the Federal Government of Nigeria was launched m 2015 as a follow up to the previous. Agricultural  Transformation Agenda (ATA). It is expected to create in 120.000 jobs along the value chain of priority commodities and add additional 20 million metric tons of food. Project activities included thematic training, on-farm technology  demonstrations, community seed production and formation of Innovation Platforms for market linkages. The project has made remarkable progress in enhancing access to quality seeds and other inputs to over 34.300 farmers while expanding knowledge of best-bet productron technologies in over 100 communities across three staple crop processing zones (SCPZ). During the 2016 cropping season, farmers produced over 70,268 Mt of grains valued at £i9.135billion (US$29M). The use of improved varieties increased yields by 32%. 42% and 64% in Bida-Badeggi. Kano-Jigawa and Sokoto-Kebbi SCPZ. respectively. Seed dressing increased yields by 38%. 27%. and 30% m the three SCPZs respectively, while tillage practices increased yields by 20% and 55% m Kano - Jigawa and Sokoto - Kebbi SCPZs. Through Innovation Platforms set up with other stakeholders and market linkages to large scale processors, 109.76 tons of seeds were procured and planted. Average yield obtained on the improved technologies was 1.5 tha compared to 1 1 t/ha by other farmers giving a 40% increase. A total of 1,093 women farmers comprising of about 34.2% of the total number of participating farmers benefited directly from the project. Seed fairs, rural radios and audio-visual broadcasts on improved sorghum production technologies were used to reach non-participating farmers within the zones.        Experimental location on Google Map
"
  
	uri <- "doi:10.21421/D2/ORJQWZ"
	group <- "agronomy"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
	data_organization = "ICRISAT",
		publication = NA, #publication paper has no doi https://oar.icrisat.org/10373/1/ASN_Conference%20Paper.pdf
		project = "ATASP-1",
		design = NA,
		data_type = "experiment",
	  treatment_vars = "fertilizer_type;OM_type;variety",
		response_vars = "yield", 
		notes = NA,
		carob_contributor = "Mitchelle Njukuya",
		carob_date = "2026-10-05",
		carob_completion = 100,	
		carob_effort = 2
	)
	

	f <- ff[basename(ff) == "Data file of Icrisat on farm fertilizer demonstration..xlsx"]

	r <- carobiner::read.excel(f)

	d <- data.frame(
	  country = "Nigeria",
	  site = r$SCPZ,
	  adm1 = r$State,
	  adm2 = r$LGA,
	  location = r$Community,
	  crop = "sorghum",
	  yield_part = "grain",
	  variety = r$Variety,
	  treatment = r$Treatment,
	  yield = r$GHvYld_C_kgha
	)

	## Coordinates from GeoNames: https://www.geonames.org/search.html?q=<location>&country=NG
	## Not found, LGA seat used: Kutirko, Magaji, Nankokan -> Agaie (LGA seat); Somazhiko -> Lemu (LGA seat); Mantuntu, Saku -> Katcha (LGA); Ndayako -> Mokwa (LGA seat); Gamahuwai -> Auyo (LGA seat); Shamakeri -> Kafin Hausa (LGA seat); Fankurin -> Garun Malam (LGA seat); Gidan Kwano -> Wara (LGA seat); Fana Sabo, Kwandage, Sabuwar Tunga -> Dakingari (LGA seat)
	## Spelling-variant matches: Rugachibo = Rugan Chipo, Tungankawu = Tungar Kawo, Zandam = Zandan, Sarawa = Tsarawa
	geo <- data.frame(
	  location = c("Kutirko", "Magaji", "Nankokan", "Shabalegbo", "Somazhiko", "Mantuntu", "Saku", "Kutigi", "Lanle", "Rugachibo", "Ndayako", "Makusidi", "Tungankawu", "Auyo", "Ayama", "Gamahuwai", "Gamsarka", "Fagam", "Farin Dutse", "Kila", "Sara", "Zandam", "Atafi 1", "Atafi 2", "Gagulmari", "Rumfa", "Yankoli", "Ruba", "Sarawa", "Shamakeri", "Dunari", "M/Madori", "Mai Rakumi", "Makaddari", "Shayya", "Tonikutara", "Bebeji", "Damau", "Gwarmai", "Wak", "Gafan", "Dawakin Kudu", "Tamburawa Tambari", "Tamburawa Zango", "Unguwar Duniya", "Fankurin", "Dalili", "Dan Hassan", "Rurum S/Gari", "Rurum T/Gari", "Saji", "Yalwa", "Zurgu", "Fana", "Geza", "Gorun Yamma", "Kamba", "Kyangakwai", "Gidan Kwano", "Kambuwa", "Libata", "Ngaski", "Utono", "Buya", "Ganten Tudu", "Kaoje", "Karallaje", "Takalafiya", "Duguraha", "Giron Masa", "Shanga", "Fana Sabo", "Kwandage", "Sabuwar Tunga", "Durbawa", "Hamma Ali"),
	  longitude = c(6.3182, 6.3182, 6.3182, 6.0719, 6.0279, 6.2333, 6.2333, 5.5950, 5.6464, 5.6122, 5.0541, 6.1466, 6.0524, 9.9389, 9.8697, 9.9389, 9.8591, 9.9791, 9.9095, 9.7667, 9.6503, 9.8922, 10.0467, 10.0467, 10.0418, 10.0449, 10.0663, 9.8667, 9.9922, 9.9108, 9.8902, 9.8808, 9.9825, 9.8225, 9.8361, 10.0253, 8.2619, 8.3036, 8.2572, 8.3578, 8.4497, 8.5869, 8.5314, 8.5314, 8.6158, 8.3698, 8.3954, 8.5245, 8.4699, 8.4509, 8.5636, 8.5182, 8.5285, 3.9053, 3.8784, 3.6992, 3.6548, 3.7479, 4.6236, 4.9696, 4.5932, 4.8307, 4.6739, 4.0629, 4.2017, 4.1200, 4.0256, 3.9836, 4.5594, 4.7212, 4.5794, 4.0617, 4.0617, 4.0617, 5.3400, 5.3167),
	  latitude = c(9.0085, 9.0085, 9.0085, 9.4432, 9.3964, 9.1500, 9.1500, 9.2010, 9.2272, 9.2682, 9.2948, 9.5754, 9.6679, 12.3333, 12.2671, 12.3333, 12.3128, 11.0485, 11.1889, 11.3333, 11.3525, 11.3464, 12.4479, 12.4479, 12.4557, 12.4504, 12.4487, 12.0197, 12.2614, 12.2392, 12.5986, 12.5647, 12.5842, 12.4961, 12.4450, 12.5203, 11.6675, 11.6956, 11.5297, 11.5861, 11.6736, 11.8372, 11.8692, 11.8692, 11.8781, 11.6857, 11.7680, 11.7853, 11.5159, 11.4510, 11.4621, 11.4086, 11.5476, 11.6932, 12.0124, 11.9725, 11.8517, 11.9692, 10.2288, 10.9133, 10.1495, 10.3748, 10.5837, 11.0755, 11.2194, 11.1823, 11.1986, 11.1827, 11.1667, 11.1026, 11.2137, 11.6481, 11.6481, 11.6481, 13.0639, 13.1667))
	
	d$location <- trimws(d$location)
	
	d <- merge(d, geo, by = "location", all.x = TRUE)
  
	trt <- tolower(trimws(d$treatment))
	d$fertilizer_used <- grepl("npk|urea", trt)
	d$OM_used <- grepl("fym|manure", trt)
	d$fertilizer_type <- ifelse(grepl("urea", trt), "NPK;urea", ifelse(grepl("npk", trt), "NPK", "none"))
	d$OM_type <- ifelse(grepl("fym", trt), "farmyard manure", ifelse(grepl("manure", trt), "poultry manure", "none"))
	
	d$on_farm <- TRUE 
	d$is_survey <- FALSE 
	d$irrigated <- FALSE
  d$geo_from_source <- FALSE
  d$trial_id <- paste(tolower(d$adm2), tolower(d$location), sep = "_")
  d$planting_date <- NA
	d$harvest_date  <- NA
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$yield_moisture <- d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}

