# R script for "carob"
# license: GPL (>=3)

## ISSUES
#1. the NPK grade isn't stated
#2. No coordinates in the raw data. Communities are georeferenced with GeoNames
#   where found; the 14 communities not found get their LGA (adm2) point-radius
#   (GADM 4.1). Community points were checked against GADM 4.1 LGA polygons;
#   8 GeoNames matches fell outside their reported LGA and use the LGA instead.


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
		site = carobiner::fix_name(r$SCPZ, "title"),
		adm1 = carobiner::fix_name(r$State, "title"),
		adm2 = carobiner::fix_name(r$LGA, "title"),
		location = trimws(r$Community),
		crop = "sorghum",
		yield_part = "grain",
		variety = r$Variety,
		treatment = r$Treatment,
		yield = r$GHvYld_C_kgha
	)

	## LGA names to GADM 4.1
	d$adm2[d$adm2 == "G/mallam"] <- "Garum Mallam"
	d$adm2[d$adm2 == "M/madori"] <- "Malam Maduri"

	## 1. community (location) coordinates
	## from GeoNames: https://www.geonames.org/search.html?q=<location>&country=NG
	## spelling variants: Rugachibo = Rugan Chipo, Tungankawu = Tungar Kawo, Zandam = Zandan, Sarawa = Tsarawa
	## not found, so not in this table (they get adm2 below): Kutirko, Magaji, Nankokan (Agaie);
	## Somazhiko (Gbako); Mantuntu, Saku (Katcha); Ndayako (Mokwa); Gamahuwai (Auyo);
	## Shamakeri (Kafin Hausa); Fankurin (Garum Mallam); Gidan Kwano (Ngaski);
	## Fana Sabo, Kwandage, Sabuwar Tunga (Suru)
	## found, but the GeoNames point is outside the reported LGA (GADM 4.1), so they also get adm2:
	## Sara (in Buji, not Gwaram); Yankoli (Kiri Kasama, not Hadejia); Dunari (Biriniwa, not Malam Maduri);
	## Damau, Wak (Garum Mallam, not Bebeji); Tamburawa Tambari, Tamburawa Zango (Kumbotso, not Dawakin Kudu);
	## Saji (Kibiya, not Rano)
	geo_loc <- data.frame(
		adm2 = c("Gbako", "Lavun", "Lavun", "Lavun", "Wushishi", "Wushishi", "Auyo", "Auyo", "Auyo", "Gwaram", "Gwaram", "Gwaram", "Gwaram", "Hadejia", "Hadejia", "Hadejia", "Hadejia", "Kafin Hausa", "Kafin Hausa", "Malam Maduri", "Malam Maduri", "Malam Maduri", "Malam Maduri", "Malam Maduri", "Bebeji", "Bebeji", "Bunkure", "Dawakin Kudu", "Dawakin Kudu", "Kura", "Kura", "Rano", "Rano", "Rano", "Rano", "Dandi", "Dandi", "Dandi", "Dandi", "Dandi", "Ngaski", "Ngaski", "Ngaski", "Ngaski", "Bagudo", "Bagudo", "Bagudo", "Bagudo", "Bagudo", "Shanga", "Shanga", "Shanga", "Kware", "Kware"),
		location = c("Shabalegbo", "Kutigi", "Lanle", "Rugachibo", "Makusidi", "Tungankawu", "Auyo", "Ayama", "Gamsarka", "Fagam", "Farin Dutse", "Kila", "Zandam", "Atafi 1", "Atafi 2", "Gagulmari", "Rumfa", "Ruba", "Sarawa", "M/Madori", "Mai Rakumi", "Makaddari", "Shayya", "Tonikutara", "Bebeji", "Gwarmai", "Gafan", "Dawakin Kudu", "Unguwar Duniya", "Dalili", "Dan Hassan", "Rurum S/Gari", "Rurum T/Gari", "Yalwa", "Zurgu", "Fana", "Geza", "Gorun Yamma", "Kamba", "Kyangakwai", "Kambuwa", "Libata", "Ngaski", "Utono", "Buya", "Ganten Tudu", "Kaoje", "Karallaje", "Takalafiya", "Duguraha", "Giron Masa", "Shanga", "Durbawa", "Hamma Ali"),
		longitude = c(6.0719, 5.5950, 5.6464, 5.6122, 6.1466, 6.0524, 9.9389, 9.8697, 9.8591, 9.9791, 9.9095, 9.7667, 9.8922, 10.0467, 10.0467, 10.0418, 10.0449, 9.8667, 9.9922, 9.8808, 9.9825, 9.8225, 9.8361, 10.0253, 8.2619, 8.2572, 8.4497, 8.5869, 8.6158, 8.3954, 8.5245, 8.4699, 8.4509, 8.5182, 8.5285, 3.9053, 3.8784, 3.6992, 3.6548, 3.7479, 4.9696, 4.5932, 4.8307, 4.6739, 4.0629, 4.2017, 4.1200, 4.0256, 3.9836, 4.5594, 4.7212, 4.5794, 5.3400, 5.3167),
		latitude = c(9.4432, 9.2010, 9.2272, 9.2682, 9.5754, 9.6679, 12.3333, 12.2671, 12.3128, 11.0485, 11.1889, 11.3333, 11.3464, 12.4479, 12.4479, 12.4557, 12.4504, 12.0197, 12.2614, 12.5647, 12.5842, 12.4961, 12.4450, 12.5203, 11.6675, 11.5297, 11.6736, 11.8372, 11.8781, 11.7680, 11.7853, 11.5159, 11.4510, 11.4086, 11.5476, 11.6932, 12.0124, 11.9725, 11.8517, 11.9692, 10.9133, 10.1495, 10.3748, 10.5837, 11.0755, 11.2194, 11.1823, 11.1986, 11.1827, 11.1667, 11.1026, 11.2137, 13.0639, 13.1667),
		geo_source = "GeoNames"
	)

	## 2. LGA (adm2) coordinates: carobiner::adm_pointRadius("Nigeria", 2)
	geo_adm2 <- data.frame(
		adm1 = c("Jigawa", "Jigawa", "Jigawa", "Jigawa", "Jigawa", "Kano", "Kano", "Kano", "Kano", "Kano", "Kano", "Kebbi", "Kebbi", "Kebbi", "Kebbi", "Kebbi", "Niger", "Niger", "Niger", "Niger", "Niger", "Niger", "Sokoto"),
		adm2 = c("Auyo", "Gwaram", "Hadejia", "Kafin Hausa", "Malam Maduri", "Bebeji", "Bunkure", "Dawakin Kudu", "Garum Mallam", "Kura", "Rano", "Bagudo", "Dandi", "Ngaski", "Shanga", "Suru", "Agaie", "Gbako", "Katcha", "Lavun", "Mokwa", "Wushishi", "Kware"),
		lon = c(9.9969, 9.9918, 10.0311, 10.0102, 9.9821, 8.2841, 8.568, 8.6459, 8.373, 8.47, 8.4988, 3.9641, 3.8375, 4.7743, 4.7267, 4.1363, 6.4082, 5.9861, 6.2422, 5.6833, 5.1464, 5.9333, 5.312),
		lat = c(12.3155, 11.1783, 12.4271, 12.1324, 12.5263, 11.4917, 11.6632, 11.7965, 11.6469, 11.7708, 11.4665, 11.3169, 11.7948, 10.5439, 11.2084, 11.7707, 8.9306, 9.2846, 9.1167, 9.2735, 9.2437, 9.6762, 13.1497),
		uncertainty = c(32584, 43960, 4913, 27090, 25498, 24763, 18688, 17536, 13222, 16798, 20758, 56649, 35224, 62519, 39141, 41146, 40982, 39850, 55993, 75197, 87356, 40941, 27349),
		source = "GADM 4.1, adm2"
	)

	## 3. combine: location first, adm2 only where the location was not found
	## merging on adm2 + location also checks that each community is in its reported LGA
	d <- merge(d, geo_loc, by = c("adm2", "location"), all.x = TRUE)
	d <- merge(d, geo_adm2, by = c("adm1", "adm2"), all.x = TRUE)
	i <- is.na(d$longitude)
	d$longitude[i] <- d$lon[i]
	d$latitude[i] <- d$lat[i]
	d$geo_uncertainty <- NA
	d$geo_uncertainty[i] <- d$uncertainty[i]
	d$geo_source[i] <- d$source[i]
	d$lon <- d$lat <- d$uncertainty <- d$source <- NULL
	d$geo_from_source <- FALSE

	trt <- tolower(trimws(d$treatment))
	d$fertilizer_used <- grepl("npk|urea", trt)
	d$OM_used <- grepl("fym|manure", trt)
	d$fertilizer_type <- ifelse(grepl("urea", trt), "NPK;urea", ifelse(grepl("npk", trt), "NPK", "none"))
	d$OM_type <- ifelse(grepl("fym", trt), "farmyard manure", ifelse(grepl("manure", trt), "poultry manure", "none"))

	d$on_farm <- TRUE
	d$is_survey <- FALSE
	d$irrigated <- FALSE
	d$trial_id <- paste(tolower(d$adm2), tolower(d$location), sep = "_")
	d$planting_date <- NA
	d$harvest_date  <- NA
	d$P_fertilizer <- d$K_fertilizer <-d$N_fertilizer <- d$yield_moisture <- d$yield_isfresh <- NA

	carobiner::write_files(path, meta, d)
}
