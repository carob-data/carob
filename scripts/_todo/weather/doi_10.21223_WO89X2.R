# REJECTED 
# Reason: no .xlsx/.xls/.csv/.dta/.rds files in download (draft cannot auto-map columns)
# R script for "carob"
# license: GPL (>=3)

## ISSUES
# list processing issues here so that an editor can look at them


carob_script <- function(path) {

"
Dataset for: Regional climatic data from Burundi

Rainfall and Temperature of certain localities of Burundi
"

	uri <- "doi:10.21223/WO89X2"
	group <- "weather"
	ff  <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=0,
		data_organization = "Amhara Regional Agricultural Research Institute, Gondar Agricultural Research Center - ARARI-GARC; CIP",
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
	

	f1 <- ff[basename(ff) == "Regional_climatic_data_from_Burundi.rar"]
	f2 <- ff[basename(ff) == "1_Rain_Rusizi.02Feb16.hobo"]
	f3 <- ff[basename(ff) == "1_RAIN_Rusizi.10 Dec 14.hobo"]
	f4 <- ff[basename(ff) == "1_RAIN_Rusizi.11feb15.hobo"]
	f5 <- ff[basename(ff) == "1_RAIN_Rusizi.20nov2015.hobo"]
	f6 <- ff[basename(ff) == "1_RAIN_Rusizi.28jul15.hobo"]
	f7 <- ff[basename(ff) == "1_Rain_Rusizi.29apr16.hobo"]
	f8 <- ff[basename(ff) == "1_Rain_Rusizi.5sep16.hobo"]
	f9 <- ff[basename(ff) == "1_RAIN_Rusizi.8sep15.hobo"]
	f10 <- ff[basename(ff) == "1_Temp_Rusizi.06dec16.hobo"]
	f11 <- ff[basename(ff) == "1_Temp_Rusizi.02Feb16.hobo"]
	f12 <- ff[basename(ff) == "1_Temp_Rusizi.06dec16.hobo"]
	f13 <- ff[basename(ff) == "1_Temp_Rusizi.10 Dec 14.hobo"]
	f14 <- ff[basename(ff) == "1_TEMP_Rusizi.11feb15.hobo"]
	f15 <- ff[basename(ff) == "1_Temp_Rusizi.20Nov2015.hobo"]
	f16 <- ff[basename(ff) == "1_Temp_Rusizi.28jul15.hobo"]
	f17 <- ff[basename(ff) == "1_Temp_Rusizi.29apr16.hobo"]
	f18 <- ff[basename(ff) == "1_Temp_Rusizi.5Sep16.hobo"]
	f19 <- ff[basename(ff) == "1_Temp_Rusizi.8sep15.hobo"]
	f20 <- ff[basename(ff) == "1_Temp_Rusizi07april15.hobo"]
	f21 <- ff[basename(ff) == "2_RAIN_Rusizi.02Feb16.hobo"]
	f22 <- ff[basename(ff) == "2_RAIN_Rusizi.10dec14.hobo"]
	f23 <- ff[basename(ff) == "2_RAIN_Rusizi.11feb15.hobo"]
	f24 <- ff[basename(ff) == "2_RAIN_Rusizi.20nov2015.hobo"]
	f25 <- ff[basename(ff) == "2_Rain_Rusizi.28apr16.hobo"]
	f26 <- ff[basename(ff) == "2_RAIN_Rusizi.28jul15.hobo"]
	f27 <- ff[basename(ff) == "2_Rain_Rusizi.5sep16.hobo"]
	f28 <- ff[basename(ff) == "2_RAIN_Rusizi.8sep15.hobo"]
	f29 <- ff[basename(ff) == "2_RAIN_Rusizi07april15.hobo"]
	f30 <- ff[basename(ff) == "2_Temp_Rusizi.02Feb16.hobo"]
	f31 <- ff[basename(ff) == "2_Temp_Rusizi.06dec16.hobo"]
	f32 <- ff[basename(ff) == "2_Temp_Rusizi.10 Dec 14.hobo"]
	f33 <- ff[basename(ff) == "2_TEMP_Rusizi.11feb15.hobo"]
	f34 <- ff[basename(ff) == "2_Temp_Rusizi.20Nov2015.hobo"]
	f35 <- ff[basename(ff) == "2_Temp_Rusizi.28apr16.hobo"]
	f36 <- ff[basename(ff) == "2_Temp_Rusizi.28jul15.hobo"]
	f37 <- ff[basename(ff) == "2_Temp_Rusizi.5sep16.hobo"]
	f38 <- ff[basename(ff) == "2_Temp_Rusizi.8sep15.hobo"]
	f39 <- ff[basename(ff) == "2_Temp_Rusizi07april15.hobo"]
	f40 <- ff[basename(ff) == "3_RAIN_Rusizi-20nov2015.hobo"]
	f41 <- ff[basename(ff) == "3_RAIN_Rusizi.02Feb16.hobo"]
	f42 <- ff[basename(ff) == "3_Rain_Rusizi.06dec16.hobo"]
	f43 <- ff[basename(ff) == "3_RAIN_Rusizi.10 dec14.hobo"]
	f44 <- ff[basename(ff) == "3_RAIN_Rusizi.11feb15.hobo"]
	f45 <- ff[basename(ff) == "3_RAIN_Rusizi.28jul15.hobo"]
	f46 <- ff[basename(ff) == "3_RAIN_Rusizi.29apr16.hobo"]
	f47 <- ff[basename(ff) == "3_RAIN_Rusizi.5sep16.hobo"]
	f48 <- ff[basename(ff) == "3_RAIN_Rusizi.8sep15.hobo"]
	f49 <- ff[basename(ff) == "3_RAIN_Rusizi07april15.hobo"]
	f50 <- ff[basename(ff) == "3_Temp_Rusizi.02Feb16.hobo"]
	f51 <- ff[basename(ff) == "3_Temp_Rusizi.06dec16.hobo"]
	f52 <- ff[basename(ff) == "3_Temp_Rusizi.10 Dec 14.hobo"]
	f53 <- ff[basename(ff) == "3_Temp_Rusizi.11feb15.hobo"]
	f54 <- ff[basename(ff) == "3_Temp_Rusizi.28jul15.hobo"]
	f55 <- ff[basename(ff) == "3_Temp_Rusizi.29apr16.hobo"]
	f56 <- ff[basename(ff) == "3_Temp_Rusizi.5sep16.hobo"]
	f57 <- ff[basename(ff) == "3_Temp_Rusizi.8sep15.hobo"]
	f58 <- ff[basename(ff) == "3_Temp_Rusizi_20Nov 15.hobo"]
	f59 <- ff[basename(ff) == "3_Temp_Rusizi07april15.hobo"]
	f60 <- ff[basename(ff) == "4_RAIN_Rusizi-23nov2015.hobo"]
	f61 <- ff[basename(ff) == "4_RAIN_Rusizi.01Feb16.hobo"]
	f62 <- ff[basename(ff) == "4_RAIN_Rusizi.10dec14.hobo"]
	f63 <- ff[basename(ff) == "4_RAIN_Rusizi.11feb15.hobo"]
	f64 <- ff[basename(ff) == "4_RAIN_Rusizi.28apr16.hobo"]
	f65 <- ff[basename(ff) == "4_RAIN_Rusizi.28jul15.hobo"]
	f66 <- ff[basename(ff) == "4_RAIN_Rusizi.5sep16.hobo"]
	f67 <- ff[basename(ff) == "4_RAIN_Rusizi.6dec16.hobo"]
	f68 <- ff[basename(ff) == "4_RAIN_Rusizi.8sep15.hobo"]
	f69 <- ff[basename(ff) == "4_RAIN_Rusizi08april15.hobo"]
	f70 <- ff[basename(ff) == "4_Temp_Rusizi.01Feb16.hobo"]
	f71 <- ff[basename(ff) == "4_Temp_Rusizi.10 Dec 14.hobo"]
	f72 <- ff[basename(ff) == "4_TEMP_Rusizi.11feb15.hobo"]
	f73 <- ff[basename(ff) == "4_Temp_Rusizi.28apr16.hobo"]
	f74 <- ff[basename(ff) == "4_Temp_Rusizi.28jul15.hobo"]
	f75 <- ff[basename(ff) == "4_Temp_Rusizi.5sep16.hobo"]
	f76 <- ff[basename(ff) == "4_Temp_Rusizi.6dec16.hobo"]
	f77 <- ff[basename(ff) == "4_Temp_Rusizi.8sep15.hobo"]
	f78 <- ff[basename(ff) == "4_Temp_Rusizi_23Nov2015.hobo"]
	f79 <- ff[basename(ff) == "4_Temp_Rusizi08april15.hobo"]
	f80 <- ff[basename(ff) == "5_RAIN_Rusizi.01Feb16.hobo"]
	f81 <- ff[basename(ff) == "5_RAIN_Rusizi.10 Dec 14.hobo"]
	f82 <- ff[basename(ff) == "5_RAIN_Rusizi.11feb15.hobo"]
	f83 <- ff[basename(ff) == "5_RAIN_Rusizi.23noV2015.hobo"]
	f84 <- ff[basename(ff) == "5_RAIN_Rusizi.28jul15.hobo"]
	f85 <- ff[basename(ff) == "5_RAIN_Rusizi.29apr16.hobo"]
	f86 <- ff[basename(ff) == "5_RAIN_Rusizi.5sep16.hobo"]
	f87 <- ff[basename(ff) == "5_RAIN_Rusizi.6dec16.hobo"]
	f88 <- ff[basename(ff) == "5_RAIN_Rusizi.8sep15.hobo"]
	f89 <- ff[basename(ff) == "5_RAIN_Rusizi08april15.hobo"]
	f90 <- ff[basename(ff) == "5_Temp_Rusizi.01Feb16.hobo"]
	f91 <- ff[basename(ff) == "5_Temp_Rusizi.10 Dec 14.hobo"]
	f92 <- ff[basename(ff) == "5_TEMP_Rusizi.11feb.15.hobo"]
	f93 <- ff[basename(ff) == "5_Temp_Rusizi.23Nov2015.hobo"]
	f94 <- ff[basename(ff) == "5_Temp_Rusizi.28jul15.hobo"]
	f95 <- ff[basename(ff) == "5_Temp_Rusizi.29apr16.hobo"]
	f96 <- ff[basename(ff) == "5_Temp_Rusizi.5sep16.hobo"]
	f97 <- ff[basename(ff) == "5_Temp_Rusizi.6dec16.hobo"]
	f98 <- ff[basename(ff) == "5_Temp_Rusizi.8sep15.hobo"]
	f99 <- ff[basename(ff) == "5_Temp_Rusizi08april15.hobo"]
	f100 <- ff[basename(ff) == "6_RAIN_Rusizi-23nov2015.hobo"]
	f101 <- ff[basename(ff) == "6_RAIN_Rusizi.01Feb16.hobo"]
	f102 <- ff[basename(ff) == "6_RAIN_Rusizi.10 Dec 14.hobo"]
	f103 <- ff[basename(ff) == "6_RAIN_Rusizi.11feb15.hobo"]
	f104 <- ff[basename(ff) == "6_RAIN_Rusizi.28apr16.hobo"]
	f105 <- ff[basename(ff) == "6_RAIN_Rusizi.28jul15.hobo"]
	f106 <- ff[basename(ff) == "6_RAIN_Rusizi.6dec16.hobo"]
	f107 <- ff[basename(ff) == "6_RAIN_Rusizi.8sep15.hobo"]
	f108 <- ff[basename(ff) == "6_RAIN_Rusizi.9sep16.hobo"]
	f109 <- ff[basename(ff) == "6_RAIN_Rusizi07april15.hobo"]
	f110 <- ff[basename(ff) == "6_Temp_Rusizi.01Feb16.hobo"]
	f111 <- ff[basename(ff) == "6_Temp_Rusizi.10 Dec 14.hobo"]
	f112 <- ff[basename(ff) == "6_Temp_Rusizi.11feb15.hobo"]
	f113 <- ff[basename(ff) == "6_Temp_Rusizi.28Apr16.hobo"]
	f114 <- ff[basename(ff) == "6_Temp_Rusizi.28jul15.hobo"]
	f115 <- ff[basename(ff) == "6_Temp_Rusizi.6dec16.hobo"]
	f116 <- ff[basename(ff) == "6_Temp_Rusizi.8sep15.hobo"]
	f117 <- ff[basename(ff) == "6_Temp_Rusizi.9sep16.hobo"]
	f118 <- ff[basename(ff) == "6_Temp_Rusizi_23Nov2015.hobo"]
	f119 <- ff[basename(ff) == "6_Temp_Rusizi07april15.hobo"]
	f120 <- ff[basename(ff) == "7_RAIN_Rusizi-23nov2015.hobo"]
	f121 <- ff[basename(ff) == "7_RAIN_Rusizi.01Feb16.hobo"]
	f122 <- ff[basename(ff) == "7_RAIN_Rusizi.10 Dec 14.hobo"]
	f123 <- ff[basename(ff) == "7_RAIN_Rusizi.11feb15.hobo"]
	f124 <- ff[basename(ff) == "7_RAIN_Rusizi.28jul15.hobo"]
	f125 <- ff[basename(ff) == "7_RAIN_Rusizi.6dec16.hobo"]
	f126 <- ff[basename(ff) == "7_RAIN_Rusizi.8sep15.hobo"]
	f127 <- ff[basename(ff) == "7_RAIN_Rusizi.9sep16.hobo"]
	f128 <- ff[basename(ff) == "7_RAIN_Rusizi08april15.hobo"]
	f129 <- ff[basename(ff) == "7_Temp_Rusizi.29apr16.hobo"]
	f130 <- ff[basename(ff) == "7_Temp_Rusizi.01Feb16.hobo"]
	f131 <- ff[basename(ff) == "7_Temp_Rusizi.10dec14.hobo"]
	f132 <- ff[basename(ff) == "7_Temp_Rusizi.11feb15.hobo"]
	f133 <- ff[basename(ff) == "7_Temp_Rusizi.28jul15.hobo"]
	f134 <- ff[basename(ff) == "7_Temp_Rusizi.29apr16.hobo"]
	f135 <- ff[basename(ff) == "7_Temp_Rusizi.6dec16.hobo"]
	f136 <- ff[basename(ff) == "7_Temp_Rusizi.8sep15.hobo"]
	f137 <- ff[basename(ff) == "7_Temp_Rusizi.9sep16.hobo"]
	f138 <- ff[basename(ff) == "7_Temp_Rusizi_23Nov2015.hobo"]
	f139 <- ff[basename(ff) == "7_Temp_Rusizi08april15.hobo"]
	f140 <- ff[basename(ff) == "8_RAIN_Rusizi-4dec2015.hobo"]
	f141 <- ff[basename(ff) == "8_Rain_RUSIZI.02Feb16.hobo"]
	f142 <- ff[basename(ff) == "8_RAIN_Rusizi.10dec14.hobo"]
	f143 <- ff[basename(ff) == "8_RAIN_Rusizi.11feb15.hobo"]
	f144 <- ff[basename(ff) == "8_Rain_RUSIZI.29apr16.hobo"]
	f145 <- ff[basename(ff) == "8_Rain_RUSIZI.5sep16.hobo"]
	f146 <- ff[basename(ff) == "8_RAIN_Rusizi08april15.hobo"]
	f147 <- ff[basename(ff) == "8_Temp_Rusizi-4dec2015.hobo"]
	f148 <- ff[basename(ff) == "8_Temp_Rusizi.02Feb16.hobo"]
	f149 <- ff[basename(ff) == "8_Temp_Rusizi.10 Dec 14.hobo"]
	f150 <- ff[basename(ff) == "8_Temp_Rusizi.11feb15.hobo"]
	f151 <- ff[basename(ff) == "8_Temp_Rusizi.28jul15.hobo"]
	f152 <- ff[basename(ff) == "8_Temp_Rusizi.29apr16.hobo"]
	f153 <- ff[basename(ff) == "8_Temp_Rusizi.5sep16.hobo"]
	f154 <- ff[basename(ff) == "8_Temp_Rusizi.6dec16.hobo"]
	f155 <- ff[basename(ff) == "8_Temp_Rusizi.8sep15.hobo"]
	f156 <- ff[basename(ff) == "8_Temp_Rusizi08april15.hobo"]
	f157 <- ff[basename(ff) == "9_RAIN_Rusizi-24nov15.hobo"]
	f158 <- ff[basename(ff) == "9_RAIN_Rusizi.02Feb16.hobo"]
	f159 <- ff[basename(ff) == "9_RAIN_Rusizi.03may16.hobo"]
	f160 <- ff[basename(ff) == "9_RAIN_Rusizi.11dec14.hobo"]
	f161 <- ff[basename(ff) == "9_RAIN_Rusizi.12feb15.hobo"]
	f162 <- ff[basename(ff) == "9_RAIN_Rusizi.29jul15.hobo"]
	f163 <- ff[basename(ff) == "9_RAIN_Rusizi.6dec16.hobo"]
	f164 <- ff[basename(ff) == "9_RAIN_Rusizi.8sep16.hobo"]
	f165 <- ff[basename(ff) == "9_RAIN_Rusizi.9sep15.hobo"]
	f166 <- ff[basename(ff) == "9_RAIN_Rusizi10april15.hobo"]
	f167 <- ff[basename(ff) == "9_Temp_Rusizi-24Nov15.hobo"]
	f168 <- ff[basename(ff) == "9_Temp_Rusizi.02Feb16.hobo"]
	f169 <- ff[basename(ff) == "9_Temp_Rusizi.03may16.hobo"]
	f170 <- ff[basename(ff) == "9_Temp_Rusizi.11dec14.hobo"]
	f171 <- ff[basename(ff) == "9_Temp_Rusizi.12feb15.hobo"]
	f172 <- ff[basename(ff) == "9_Temp_Rusizi.29jul15.hobo"]
	f173 <- ff[basename(ff) == "9_Temp_Rusizi.6dec16.hobo"]
	f174 <- ff[basename(ff) == "9_Temp_Rusizi.8sep16.hobo"]
	f175 <- ff[basename(ff) == "9_Temp_Rusizi.9sep15.hobo"]
	f176 <- ff[basename(ff) == "9_Temp_Rusizi10april15.hobo"]
	f177 <- ff[basename(ff) == "10_RAIN_Rusizi-24nov2015.hobo"]
	f178 <- ff[basename(ff) == "10_RAIN_Rusizi.02Feb16.hobo"]
	f179 <- ff[basename(ff) == "10_RAIN_Rusizi.03may16.hobo"]
	f180 <- ff[basename(ff) == "10_RAIN_Rusizi.11dec14.hobo"]
	f181 <- ff[basename(ff) == "10_RAIN_Rusizi.12feb15.hobo"]
	f182 <- ff[basename(ff) == "10_RAIN_Rusizi.29jul15.hobo"]
	f183 <- ff[basename(ff) == "10_RAIN_Rusizi.6dec16.hobo"]
	f184 <- ff[basename(ff) == "10_RAIN_Rusizi.8sep15.hobo"]
	f185 <- ff[basename(ff) == "10_RAIN_Rusizi.8sep16.hobo"]
	f186 <- ff[basename(ff) == "10_RAIN_Rusizi08april15.hobo"]
	f187 <- ff[basename(ff) == "10_Temp_10.29jul15.hobo"]
	f188 <- ff[basename(ff) == "10_Temp_10.8sep15.hobo"]
	f189 <- ff[basename(ff) == "10_Temp_1008april15.hobo"]
	f190 <- ff[basename(ff) == "10_Temp_Rusizi-24Nov2015.hobo"]
	f191 <- ff[basename(ff) == "10_Temp_Rusizi.02Feb16.hobo"]
	f192 <- ff[basename(ff) == "10_Temp_Rusizi.03may16.hobo"]
	f193 <- ff[basename(ff) == "10_TEMP_Rusizi.12feb15.hobo"]
	f194 <- ff[basename(ff) == "10_Temp_Rusizi.6dec16.hobo"]
	f195 <- ff[basename(ff) == "10_Temp_Rusizi.8sep16.hobo"]
	f196 <- ff[basename(ff) == "10_Temp_Rusizi_11 Dec 14.hobo"]
	f197 <- ff[basename(ff) == "11_RAIN_Rusizi-24nov15.hobo"]
	f198 <- ff[basename(ff) == "11_RAIN_Rusizi.02Feb16.hobo"]
	f199 <- ff[basename(ff) == "11_RAIN_Rusizi.03may16.hobo"]
	f200 <- ff[basename(ff) == "11_RAIN_Rusizi.11 Dec 14.hobo"]
	f201 <- ff[basename(ff) == "11_RAIN_Rusizi.12feb15.hobo"]
	f202 <- ff[basename(ff) == "11_RAIN_Rusizi.29jul15.hobo"]
	f203 <- ff[basename(ff) == "11_RAIN_Rusizi.7sep16.hobo"]
	f204 <- ff[basename(ff) == "11_RAIN_Rusizi.8dec16.hobo"]
	f205 <- ff[basename(ff) == "11_RAIN_Rusizi.8sep15.hobo"]
	f206 <- ff[basename(ff) == "11_RAIN_Rusizi08april15.hobo"]
	f207 <- ff[basename(ff) == "11_Temp_Rusizi-24Nov2015.hobo"]
	f208 <- ff[basename(ff) == "11_Temp_Rusizi.02Feb16.hobo"]
	f209 <- ff[basename(ff) == "11_Temp_Rusizi.03may16.hobo"]
	f210 <- ff[basename(ff) == "11_Temp_Rusizi.11 Dec 14.hobo"]
	f211 <- ff[basename(ff) == "11_Temp_Rusizi.12feb15.hobo"]
	f212 <- ff[basename(ff) == "11_Temp_Rusizi.29jul15.hobo"]
	f213 <- ff[basename(ff) == "11_Temp_Rusizi.7sep16.hobo"]
	f214 <- ff[basename(ff) == "11_Temp_Rusizi.8dec16.hobo"]
	f215 <- ff[basename(ff) == "11_Temp_Rusizi.8sep15.hobo"]
	f216 <- ff[basename(ff) == "11_Temp_Rusizi08april15.hobo"]
	f217 <- ff[basename(ff) == "12_RAIN_Rusizi-25nov2015.hobo"]
	f218 <- ff[basename(ff) == "12_RAIN_Rusizi.04Feb16.hobo"]
	f219 <- ff[basename(ff) == "12_RAIN_Rusizi.11dec14.hobo"]
	f220 <- ff[basename(ff) == "12_RAIN_Rusizi.12feb15.hobo"]
	f221 <- ff[basename(ff) == "12_RAIN_Rusizi.29jul15.hobo"]
	f222 <- ff[basename(ff) == "12_RAIN_Rusizi.7sep16.hobo"]
	f223 <- ff[basename(ff) == "12_RAIN_Rusizi.8dec16.hobo"]
	f224 <- ff[basename(ff) == "12_RAIN_Rusizi.9sep15.hobo"]
	f225 <- ff[basename(ff) == "12_RAIN_Rusizi10april15.hobo"]
	f226 <- ff[basename(ff) == "12_Temp_Rusizi.03may16.hobo"]
	f227 <- ff[basename(ff) == "12_Temp_Rusizi.03may16.hobo"]
	f228 <- ff[basename(ff) == "12_Temp_Rusizi.04Feb16.hobo"]
	f229 <- ff[basename(ff) == "12_Temp_Rusizi.11 Dec 14.hobo"]
	f230 <- ff[basename(ff) == "12_Temp_Rusizi.12feb15.hobo"]
	f231 <- ff[basename(ff) == "12_Temp_Rusizi.29jul15.hobo"]
	f232 <- ff[basename(ff) == "12_Temp_Rusizi.7sep16.hobo"]
	f233 <- ff[basename(ff) == "12_Temp_Rusizi.8dec16.hobo"]
	f234 <- ff[basename(ff) == "12_Temp_Rusizi.9sep15.hobo"]
	f235 <- ff[basename(ff) == "12_Temp_Rusizi_25Nov2015.hobo"]
	f236 <- ff[basename(ff) == "12_Temp_Rusizi10april15.hobo"]
	f237 <- ff[basename(ff) == "13_RAIN_Rusizi-26nov2015.hobo"]
	f238 <- ff[basename(ff) == "13_Rain_Rusizi.006dec16.hobo"]
	f239 <- ff[basename(ff) == "13_RAIN_Rusizi.03Feb16.hobo"]
	f240 <- ff[basename(ff) == "13_RAIN_Rusizi.06may16.hobo"]
	f241 <- ff[basename(ff) == "13_Temp_Rusizi.03Feb16.hobo"]
	f242 <- ff[basename(ff) == "13_Temp_Rusizi.06may16.hobo"]
	f243 <- ff[basename(ff) == "13_Temp_Rusizi.13feb15.hobo"]
	f244 <- ff[basename(ff) == "13_Temp_Rusizi.27jul15.hobo"]
	f245 <- ff[basename(ff) == "13_Temp_Rusizi.6sep16.hobo"]
	f246 <- ff[basename(ff) == "13_Temp_Rusizi.7dec16.hobo"]
	f247 <- ff[basename(ff) == "13_Temp_Rusizi.8dec14.hobo"]
	f248 <- ff[basename(ff) == "13_Temp_Rusizi.9sep15.hobo"]
	f249 <- ff[basename(ff) == "13_Temp_Rusizi_26Nov2015.hobo"]
	f250 <- ff[basename(ff) == "13_Temp_Rusizi09april15.hobo"]
	f251 <- ff[basename(ff) == "14_RAIN_Rusizi-27nov2015.hobo"]
	f252 <- ff[basename(ff) == "14_RAIN_Rusizi.03feb16.hobo"]
	f253 <- ff[basename(ff) == "14_RAIN_Rusizi.06msay16.hobo"]
	f254 <- ff[basename(ff) == "14_RAIN_Rusizi.13feb15.hobo"]
	f255 <- ff[basename(ff) == "14_RAIN_Rusizi.6sep16.hobo"]
	f256 <- ff[basename(ff) == "14_RAIN_Rusizi.7dec16.hobo"]
	f257 <- ff[basename(ff) == "14_RAIN_Rusizi.9 Dec 14.hobo"]
	f258 <- ff[basename(ff) == "14_RAIN_Rusizi.9sep15.hobo"]
	f259 <- ff[basename(ff) == "14_RAIN_Rusizi09april15.hobo"]
	f260 <- ff[basename(ff) == "14_RAIN_Rusizi27jul15.hobo"]
	f261 <- ff[basename(ff) == "14_Temp_Rusizi-27Nov2015.hobo"]
	f262 <- ff[basename(ff) == "14_Temp_Rusizi.03Feb16.hobo"]
	f263 <- ff[basename(ff) == "14_Temp_Rusizi.06may16.hobo"]
	f264 <- ff[basename(ff) == "14_Temp_Rusizi.13feb15.hobo"]
	f265 <- ff[basename(ff) == "14_Temp_Rusizi.6sep16.hobo"]
	f266 <- ff[basename(ff) == "14_Temp_Rusizi.7dec16.hobo"]
	f267 <- ff[basename(ff) == "14_Temp_Rusizi.9dec14.hobo"]
	f268 <- ff[basename(ff) == "14_Temp_Rusizi.9sep15.hobo"]
	f269 <- ff[basename(ff) == "14_Temp_Rusizi09april15.hobo"]
	f270 <- ff[basename(ff) == "14_Temp_Rusizi27jul15.hobo"]
	f271 <- ff[basename(ff) == "15_RAIN_Rusizi-27nov2015.hobo"]
	f272 <- ff[basename(ff) == "15_RAIN_Rusizi.03Feb16.hobo"]
	f273 <- ff[basename(ff) == "15_RAIN_Rusizi.06may16.hobo"]
	f274 <- ff[basename(ff) == "15_RAIN_Rusizi.13feb15.hobo"]
	f275 <- ff[basename(ff) == "15_RAIN_Rusizi.6sep16.hobo"]
	f276 <- ff[basename(ff) == "15_RAIN_Rusizi.8dec14.hobo"]
	f277 <- ff[basename(ff) == "15_RAIN_Rusizi.9sep15.hobo"]
	f278 <- ff[basename(ff) == "15_RAIN_Rusizi09april15.hobo"]
	f279 <- ff[basename(ff) == "15_RAIN_Rusizi27jul15.hobo"]
	f280 <- ff[basename(ff) == "15_Temp_rusizi-27nov2015.hobo"]
	f281 <- ff[basename(ff) == "15_Temp_Rusizi.03Feb16.hobo"]
	f282 <- ff[basename(ff) == "15_Temp_Rusizi.06may16.hobo"]
	f283 <- ff[basename(ff) == "15_Temp_Rusizi.13feb15.hobo"]
	f284 <- ff[basename(ff) == "15_Temp_Rusizi.6sep16.hobo"]
	f285 <- ff[basename(ff) == "15_Temp_Rusizi.7dec16.hproj"]
	f286 <- ff[basename(ff) == "15_Temp_Rusizi.8dec14.hobo"]
	f287 <- ff[basename(ff) == "15_Temp_Rusizi.9sep15.hobo"]
	f288 <- ff[basename(ff) == "15_Temp_Rusizi09april15.hobo"]
	f289 <- ff[basename(ff) == "15_Temp_Rusizi27apl15.hobo"]
	f290 <- ff[basename(ff) == "16_RAIN_Rusizi.06may16.hobo"]
	f291 <- ff[basename(ff) == "16_RAIN_Rusizi.13feb15.hobo"]
	f292 <- ff[basename(ff) == "16_RAIN_Rusizi.9 Dec 14.hobo"]
	f293 <- ff[basename(ff) == "16_RAIN_Rusizi_0.03Feb16.hobo"]
	f294 <- ff[basename(ff) == "16_RAIN_Rusizi_0.10sep15.hobo"]
	f295 <- ff[basename(ff) == "16_RAIN_Rusizi_0.27jul15.hobo"]
	f296 <- ff[basename(ff) == "16_RAIN_Rusizi_0.6sep16.hobo"]
	f297 <- ff[basename(ff) == "16_RAIN_Rusizi_0.7dec16.hobo"]
	f298 <- ff[basename(ff) == "16_RAIN_Rusizi_09april15.hobo"]
	f299 <- ff[basename(ff) == "16_RAIN_Rusizi27nov2015.hobo"]
	f300 <- ff[basename(ff) == "17_RAIN_Rusizi.7dec16.hobo"]
	f301 <- ff[basename(ff) == "16_Tem_Rusizi.10sep15.hobo"]
	f302 <- ff[basename(ff) == "16_Tem_Rusizi.9 Dec 14.hobo"]
	f303 <- ff[basename(ff) == "16_Tem_Rusizi09april15.hobo"]
	f304 <- ff[basename(ff) == "16_Temp_Rusizi-27Nov2015.hobo"]
	f305 <- ff[basename(ff) == "16_Temp_Rusizi.03Feb16.hobo"]
	f306 <- ff[basename(ff) == "16_TEMP_Rusizi.13feb15.hobo"]
	f307 <- ff[basename(ff) == "16_Temp_Rusizi.27jul15.hobo"]
	f308 <- ff[basename(ff) == "16_Temp_Rusizi.6sep16.hobo"]
	f309 <- ff[basename(ff) == "16_Temp_Rusizi.7dec16.hobo"]
	f310 <- ff[basename(ff) == "17_Temp_Rusizi.7dec16.hobo"]
	f311 <- ff[basename(ff) == "17_RAIN_Rusizi-26nov2015.hobo"]
	f312 <- ff[basename(ff) == "17_RAIN_Rusizi.03Feb16.hobo"]
	f313 <- ff[basename(ff) == "17_RAIN_Rusizi.06may16.hobo"]
	f314 <- ff[basename(ff) == "17_RAIN_Rusizi.13feb15.hobo"]
	f315 <- ff[basename(ff) == "17_RAIN_Rusizi.6sep16.hobo"]
	f316 <- ff[basename(ff) == "17_RAIN_Rusizi.9 Dec 14.hobo"]
	f317 <- ff[basename(ff) == "17_RAIN_Rusizi.9sep15.hobo"]
	f318 <- ff[basename(ff) == "17_RAIN_Rusizi09april15.hobo"]
	f319 <- ff[basename(ff) == "17_RAIN_Rusizi27jul15.hobo"]
	f320 <- ff[basename(ff) == "17_Temp_Rusizi.03Feb16.hobo"]
	f321 <- ff[basename(ff) == "17_Temp_Rusizi.06may16.hobo"]
	f322 <- ff[basename(ff) == "17_Temp_Rusizi.13feb15.hobo"]
	f323 <- ff[basename(ff) == "17_Temp_Rusizi.6sep16.hobo"]
	f324 <- ff[basename(ff) == "17_Temp_Rusizi.9 Dec 14.hobo"]
	f325 <- ff[basename(ff) == "17_Temp_Rusizi.9sep15.hobo"]
	f326 <- ff[basename(ff) == "17_Temp_Rusizi_26Nov2015.hobo"]
	f327 <- ff[basename(ff) == "17_Temp_Rusizi09april15.hobo"]
	f328 <- ff[basename(ff) == "17_Temp_Rusizi27jul15.hobo"]
	f329 <- ff[basename(ff) == "18_RAIN_Rusizi-25nov2015.hobo"]
	f330 <- ff[basename(ff) == "18_RAIN_Rusizi.03may16.hobo"]
	f331 <- ff[basename(ff) == "18_RAIN_Rusizi.04Feb16.hobo"]
	f332 <- ff[basename(ff) == "18_RAIN_Rusizi.11 Dec 14.hobo"]
	f333 <- ff[basename(ff) == "18_RAIN_Rusizi.12feb15.hobo"]
	f334 <- ff[basename(ff) == "18_RAIN_Rusizi.29jul15.hobo"]
	f335 <- ff[basename(ff) == "18_RAIN_Rusizi.7sep16.hobo"]
	f336 <- ff[basename(ff) == "18_RAIN_Rusizi.8dec16.hobo"]
	f337 <- ff[basename(ff) == "18_RAIN_Rusizi.9sep15.hobo"]
	f338 <- ff[basename(ff) == "18_RAIN_Rusizi10april15.hobo"]
	f339 <- ff[basename(ff) == "18_Temp_Rusizi.03may16.hobo"]
	f340 <- ff[basename(ff) == "18_Temp_Rusizi.04Feb16.hobo"]
	f341 <- ff[basename(ff) == "18_Temp_Rusizi.11 Dec 14.hobo"]
	f342 <- ff[basename(ff) == "18_Temp_Rusizi.12feb15.hobo"]
	f343 <- ff[basename(ff) == "18_Temp_Rusizi.29jul15.hobo"]
	f344 <- ff[basename(ff) == "18_Temp_Rusizi.7sep16.hobo"]
	f345 <- ff[basename(ff) == "18_Temp_Rusizi.8dec16.hobo"]
	f346 <- ff[basename(ff) == "18_Temp_Rusizi.9sep15.hobo"]
	f347 <- ff[basename(ff) == "18_Temp_Rusizi_25Nov2015.hobo"]
	f348 <- ff[basename(ff) == "18_Temp_Rusizi10april15.hobo"]
	f349 <- ff[basename(ff) == "19_RAIN_Rusizi-2dec2015.hobo"]
	f350 <- ff[basename(ff) == "19_RAIN_Rusizi.03may16.hobo"]
	f351 <- ff[basename(ff) == "19_RAIN_Rusizi.04Feb16.hobo"]
	f352 <- ff[basename(ff) == "19_RAIN_Rusizi.11dec14.hobo"]
	f353 <- ff[basename(ff) == "19_RAIN_Rusizi.12feb15.hobo"]
	f354 <- ff[basename(ff) == "19_RAIN_Rusizi.29jul15.hobo"]
	f355 <- ff[basename(ff) == "19_RAIN_Rusizi.7sep16.hobo"]
	f356 <- ff[basename(ff) == "19_RAIN_Rusizi.8dec16.hobo"]
	f357 <- ff[basename(ff) == "19_RAIN_Rusizi.9sep15.hobo"]
	f358 <- ff[basename(ff) == "19_RAIN_Rusizi10april15.hobo"]
	f359 <- ff[basename(ff) == "19_Temp_Rusizi-2dec2015.hobo"]
	f360 <- ff[basename(ff) == "19_Temp_Rusizi.03may16.hobo"]
	f361 <- ff[basename(ff) == "19_Temp_Rusizi.04Feb16.hobo"]
	f362 <- ff[basename(ff) == "19_Temp_Rusizi.11dec14.hobo"]
	f363 <- ff[basename(ff) == "19_Temp_Rusizi.12feb15.hobo"]
	f364 <- ff[basename(ff) == "19_Temp_Rusizi.29jul15.hobo"]
	f365 <- ff[basename(ff) == "19_Temp_Rusizi.7sep16.hobo"]
	f366 <- ff[basename(ff) == "19_Temp_Rusizi.8dec16.hobo"]
	f367 <- ff[basename(ff) == "19_Temp_Rusizi.9sep15.hobo"]
	f368 <- ff[basename(ff) == "19_Temp_Rusizi10april15.hobo"]
	f369 <- ff[basename(ff) == "20_RAIN_Rusizi.03may16.hobo"]
	f370 <- ff[basename(ff) == "20_RAIN_Rusizi.04Feb16.hobo"]
	f371 <- ff[basename(ff) == "20_RAIN_Rusizi.11dec14.hobo"]
	f372 <- ff[basename(ff) == "20_RAIN_Rusizi.13feb15.hobo"]
	f373 <- ff[basename(ff) == "20_RAIN_Rusizi.29jul15.hobo"]
	f374 <- ff[basename(ff) == "20_RAIN_Rusizi.7dec16.hobo"]
	f375 <- ff[basename(ff) == "20_RAIN_Rusizi.7sep16.hobo"]
	f376 <- ff[basename(ff) == "20_RAIN_Rusizi.9sep15.hobo"]
	f377 <- ff[basename(ff) == "20_RAIN_Rusizi09april15.hobo"]
	f378 <- ff[basename(ff) == "20_Temp_Rusizi-2dec2015.hobo"]
	f379 <- ff[basename(ff) == "20_Temp_Rusizi.03may16.hobo"]
	f380 <- ff[basename(ff) == "20_Temp_Rusizi.04Feb16.hobo"]
	f381 <- ff[basename(ff) == "20_Temp_Rusizi.11dec14.hobo"]
	f382 <- ff[basename(ff) == "20_Temp_Rusizi.13feb15.hobo"]
	f383 <- ff[basename(ff) == "20_Temp_Rusizi.29jul15.hobo"]
	f384 <- ff[basename(ff) == "20_Temp_Rusizi.7dec16.hobo"]
	f385 <- ff[basename(ff) == "20_Temp_Rusizi.7sep16.hobo"]
	f386 <- ff[basename(ff) == "20_Temp_Rusizi.9sep15.hobo"]
	f387 <- ff[basename(ff) == "20_Temp_Rusizi09april15.hobo"]
	f388 <- ff[basename(ff) == "21_RAIN_Rusizi-24nov2015.hobo"]
	f389 <- ff[basename(ff) == "21_RAIN_Rusizi.02Feb16.hobo"]
	f390 <- ff[basename(ff) == "21_RAIN_Rusizi.11 Dec 14.hobo"]
	f391 <- ff[basename(ff) == "21_RAIN_Rusizi.12feb15.hobo"]
	f392 <- ff[basename(ff) == "21_RAIN_Rusizi.29apr16.hobo"]
	f393 <- ff[basename(ff) == "21_RAIN_Rusizi.29jul15.hobo"]
	f394 <- ff[basename(ff) == "21_RAIN_Rusizi.6dec16.hobo"]
	f395 <- ff[basename(ff) == "21_RAIN_Rusizi.7sep16.hobo"]
	f396 <- ff[basename(ff) == "21_RAIN_Rusizi.8sep15.hobo"]
	f397 <- ff[basename(ff) == "21_RAIN_Rusizi08april15.hobo"]
	f398 <- ff[basename(ff) == "21_Temp_Rusizi-24nov2015.hobo"]
	f399 <- ff[basename(ff) == "21_Temp_Rusizi.02Feb16.hobo"]
	f400 <- ff[basename(ff) == "21_Temp_Rusizi.11dec14.hobo"]
	f401 <- ff[basename(ff) == "21_Temp_Rusizi.12feb15.hobo"]
	f402 <- ff[basename(ff) == "21_Temp_Rusizi.29apr16.hobo"]
	f403 <- ff[basename(ff) == "21_Temp_Rusizi.29jul15.hobo"]
	f404 <- ff[basename(ff) == "21_Temp_Rusizi.6dec16.hobo"]
	f405 <- ff[basename(ff) == "21_Temp_Rusizi.7sep16.hobo"]
	f406 <- ff[basename(ff) == "21_Temp_Rusizi.8sep15.hobo"]
	f407 <- ff[basename(ff) == "21_Temp_Rusizi08april15.hobo"]
	f408 <- ff[basename(ff) == "22_RAIN_Rusizi-27nov2015.hobo"]
	f409 <- ff[basename(ff) == "22_RAIN_Rusizi.03Feb16.hobo"]
	f410 <- ff[basename(ff) == "22_RAIN_Rusizi.04may16.hobo"]
	f411 <- ff[basename(ff) == "22_RAIN_Rusizi.10sep15.hobo"]
	f412 <- ff[basename(ff) == "22_RAIN_Rusizi.13feb15.hobo"]
	f413 <- ff[basename(ff) == "22_RAIN_Rusizi.27july15.hobo"]
	f414 <- ff[basename(ff) == "22_RAIN_Rusizi.6sep16.hobo"]
	f415 <- ff[basename(ff) == "22_RAIN_Rusizi.7dec16.hobo"]
	f416 <- ff[basename(ff) == "22_RAIN_Rusizi.9 Dec 14.hobo"]
	f417 <- ff[basename(ff) == "22_RAIN_Rusizi09april15.hobo"]
	f418 <- ff[basename(ff) == "22_Temp_Rusizi-27nov2015.hobo"]
	f419 <- ff[basename(ff) == "22_Temp_Rusizi.03Feb16.hobo"]
	f420 <- ff[basename(ff) == "22_Temp_Rusizi.04may16.hobo"]
	f421 <- ff[basename(ff) == "22_Temp_Rusizi.10sep15.hobo"]
	f422 <- ff[basename(ff) == "22_Temp_Rusizi.13feb15.hobo"]
	f423 <- ff[basename(ff) == "22_Temp_Rusizi.27july15.hobo"]
	f424 <- ff[basename(ff) == "22_Temp_Rusizi.6sep16.hobo"]
	f425 <- ff[basename(ff) == "22_Temp_Rusizi.7dec16.hobo"]
	f426 <- ff[basename(ff) == "22_Temp_Rusizi.9 Dec 14.hobo"]
	f427 <- ff[basename(ff) == "22_Temp_Rusizi09april15.hobo"]
	f428 <- ff[basename(ff) == "23_RAIN_Rusizi-30nov2015.hobo"]
	f429 <- ff[basename(ff) == "23_RAIN_Rusizi.03Feb16.hobo"]
	f430 <- ff[basename(ff) == "23_RAIN_Rusizi.06may16.hobo"]
	f431 <- ff[basename(ff) == "23_RAIN_Rusizi.13feb15.hobo"]
	f432 <- ff[basename(ff) == "23_RAIN_Rusizi.6sep16.hobo"]
	f433 <- ff[basename(ff) == "23_RAIN_Rusizi.9 Dec 14.hobo"]
	f434 <- ff[basename(ff) == "23_RAIN_Rusizi.9sep15.hobo"]
	f435 <- ff[basename(ff) == "23_RAIN_Rusizi09april15.hobo"]
	f436 <- ff[basename(ff) == "23_RAIN_Rusizi27jul15.hobo"]
	f437 <- ff[basename(ff) == "23_Temp_Rusizi.7dec16.hobo"]
	f438 <- ff[basename(ff) == "23_Temp_Rusizi-30nov2015.hobo"]
	f439 <- ff[basename(ff) == "23_Temp_Rusizi.03Feb16.hobo"]
	f440 <- ff[basename(ff) == "23_Temp_Rusizi.06may16.hobo"]
	f441 <- ff[basename(ff) == "23_Temp_Rusizi.13feb15.hobo"]
	f442 <- ff[basename(ff) == "23_Temp_Rusizi.6sep16.hobo"]
	f443 <- ff[basename(ff) == "23_Temp_Rusizi.9 Dec 14.hobo"]
	f444 <- ff[basename(ff) == "23_Temp_Rusizi.9sep15.hobo"]
	f445 <- ff[basename(ff) == "23_Temp_Rusizi09ap15.hobo"]
	f446 <- ff[basename(ff) == "23_Temp_Rusizi27jul15.hobo"]
	f447 <- ff[basename(ff) == "24_rain_Rusizi-1dec2015.hobo"]
	f448 <- ff[basename(ff) == "24_RAIN_Rusizi.13feb15.hobo"]
	f449 <- ff[basename(ff) == "24_RAIN_Rusizi.6sep16.hobo"]
	f450 <- ff[basename(ff) == "24_RAIN_Rusizi.7dec16.hobo"]
	f451 <- ff[basename(ff) == "24_RAIN_Rusizi.9 Dec14.hobo"]
	f452 <- ff[basename(ff) == "24_RAIN_Rusizi.9sep15.hobo"]
	f453 <- ff[basename(ff) == "24_RAIN_Rusizi13april15.hobo"]
	f454 <- ff[basename(ff) == "24_RAIN_Rusizi27jul15.hobo"]
	f455 <- ff[basename(ff) == "24_Temp_Rusizi-1dec2015.hobo"]
	f456 <- ff[basename(ff) == "24_Temp_Rusizi.13feb15.hobo"]
	f457 <- ff[basename(ff) == "24_Temp_Rusizi.27jul15.hobo"]
	f458 <- ff[basename(ff) == "24_Temp_Rusizi.6sep16.hobo"]
	f459 <- ff[basename(ff) == "24_Temp_Rusizi.7dec16.hobo"]
	f460 <- ff[basename(ff) == "24_Temp_Rusizi.9 Dec14.hobo"]
	f461 <- ff[basename(ff) == "24_Temp_Rusizi.9sep15.hobo"]
	f462 <- ff[basename(ff) == "24_Temp_Rusizi13april15.hobo"]
	f463 <- ff[basename(ff) == "25_RAIN_Rusizi-1dec2015.hobo"]
	f464 <- ff[basename(ff) == "25_RAIN_Rusizi.04Feb16.hobo"]
	f465 <- ff[basename(ff) == "25_RAIN_Rusizi.06may16.hobo"]
	f466 <- ff[basename(ff) == "25_RAIN_Rusizi.10sep15.hobo"]
	f467 <- ff[basename(ff) == "25_RAIN_Rusizi.12 Dec 14.hobo"]
	f468 <- ff[basename(ff) == "25_RAIN_Rusizi.14feb15.hobo"]
	f469 <- ff[basename(ff) == "25_RAIN_Rusizi.30jul15.hobo"]
	f470 <- ff[basename(ff) == "25_RAIN_Rusizi.8sep16.hobo"]
	f471 <- ff[basename(ff) == "25_RAIN_Rusizi10april15.hobo"]
	f472 <- ff[basename(ff) == "25_Temp_Rusizi-1dec2015.hobo"]
	f473 <- ff[basename(ff) == "25_Temp_Rusizi.04Feb16.hobo"]
	f474 <- ff[basename(ff) == "25_Temp_Rusizi.06may16.hobo"]
	f475 <- ff[basename(ff) == "25_Temp_Rusizi.10sep15.hobo"]
	f476 <- ff[basename(ff) == "25_Temp_Rusizi.12 Dec 14.hobo"]
	f477 <- ff[basename(ff) == "25_Temp_Rusizi.14feb15.hobo"]
	f478 <- ff[basename(ff) == "25_Temp_Rusizi.30jul15.hobo"]
	f479 <- ff[basename(ff) == "25_Temp_Rusizi.8dec16.hobo"]
	f480 <- ff[basename(ff) == "25_Temp_Rusizi.8sep16.hobo"]
	f481 <- ff[basename(ff) == "25_Temp_Rusizi10april15.hobo"]
	f482 <- ff[basename(ff) == "26_RAIN_Rusizi-2dec2015.hobo"]
	f483 <- ff[basename(ff) == "26_RAIN_Rusizi.04Feb16.hobo"]
	f484 <- ff[basename(ff) == "26_RAIN_Rusizi.06may16.hobo"]
	f485 <- ff[basename(ff) == "26_RAIN_Rusizi.10sep15.hobo"]
	f486 <- ff[basename(ff) == "26_RAIN_Rusizi.14feb15.hobo"]
	f487 <- ff[basename(ff) == "26_RAIN_Rusizi.30jul15.hobo"]
	f488 <- ff[basename(ff) == "26_RAIN_Rusizi.8dec14.hobo"]
	f489 <- ff[basename(ff) == "26_RAIN_Rusizi.8dec16.hobo"]
	f490 <- ff[basename(ff) == "26_RAIN_Rusizi.8sep16.hobo"]
	f491 <- ff[basename(ff) == "26_RAIN_Rusizi13april15.hobo"]
	f492 <- ff[basename(ff) == "26_Temp_Rusizi-2dec2015.hobo"]
	f493 <- ff[basename(ff) == "26_Temp_Rusizi.04Feb16.hobo"]
	f494 <- ff[basename(ff) == "26_Temp_Rusizi.06may16.hobo"]
	f495 <- ff[basename(ff) == "26_Temp_Rusizi.10sep15.hobo"]
	f496 <- ff[basename(ff) == "26_Temp_Rusizi.14feb15.hobo"]
	f497 <- ff[basename(ff) == "26_Temp_Rusizi.30jul15.hobo"]
	f498 <- ff[basename(ff) == "26_Temp_Rusizi.8dec14.hobo"]
	f499 <- ff[basename(ff) == "26_Temp_Rusizi.8dec16.hobo"]
	f500 <- ff[basename(ff) == "26_Temp_Rusizi.8sep16.hobo"]
	f501 <- ff[basename(ff) == "26_Temp_Rusizi13april15.hobo"]
	f502 <- ff[basename(ff) == "27_RAIN_Rusizi-2dec2015.hobo"]
	f503 <- ff[basename(ff) == "27_RAIN_Rusizi.04Feb16.hobo"]
	f504 <- ff[basename(ff) == "27_RAIN_Rusizi.09may16.hobo"]
	f505 <- ff[basename(ff) == "27_RAIN_Rusizi.10sep15.hobo"]
	f506 <- ff[basename(ff) == "27_RAIN_Rusizi.12 Dec 14.hobo"]
	f507 <- ff[basename(ff) == "27_RAIN_Rusizi.14feb15.hobo"]
	f508 <- ff[basename(ff) == "27_RAIN_Rusizi.30jul15.hobo"]
	f509 <- ff[basename(ff) == "27_RAIN_Rusizi.8sep16.hobo"]
	f510 <- ff[basename(ff) == "27_RAIN_Rusizi13april15.hobo"]
	f511 <- ff[basename(ff) == "27_Temp_Rusizi-2dec2015.hobo"]
	f512 <- ff[basename(ff) == "27_Temp_Rusizi.04Feb2016.hobo"]
	f513 <- ff[basename(ff) == "27_Temp_Rusizi.08ma.hobo"]
	f514 <- ff[basename(ff) == "27_Temp_Rusizi.10sep15.hobo"]
	f515 <- ff[basename(ff) == "27_Temp_Rusizi.12 Dec 14.hobo"]
	f516 <- ff[basename(ff) == "27_Temp_Rusizi.14feb15.hobo"]
	f517 <- ff[basename(ff) == "27_Temp_Rusizi.30jul15.hobo"]
	f518 <- ff[basename(ff) == "27_Temp_Rusizi.8dec16.hobo"]
	f519 <- ff[basename(ff) == "27_Temp_Rusizi.8sep16.hobo"]
	f520 <- ff[basename(ff) == "27_Temp_Rusizi13april15.hobo"]

	#r1 <- read.???(f1)
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
	#r38 <- read.???(f38)
	#r39 <- read.???(f39)
	#r40 <- read.???(f40)
	#r41 <- read.???(f41)
	#r42 <- read.???(f42)
	#r43 <- read.???(f43)
	#r44 <- read.???(f44)
	#r45 <- read.???(f45)
	#r46 <- read.???(f46)
	#r47 <- read.???(f47)
	#r48 <- read.???(f48)
	#r49 <- read.???(f49)
	#r50 <- read.???(f50)
	#r51 <- read.???(f51)
	#r52 <- read.???(f52)
	#r53 <- read.???(f53)
	#r54 <- read.???(f54)
	#r55 <- read.???(f55)
	#r56 <- read.???(f56)
	#r57 <- read.???(f57)
	#r58 <- read.???(f58)
	#r59 <- read.???(f59)
	#r60 <- read.???(f60)
	#r61 <- read.???(f61)
	#r62 <- read.???(f62)
	#r63 <- read.???(f63)
	#r64 <- read.???(f64)
	#r65 <- read.???(f65)
	#r66 <- read.???(f66)
	#r67 <- read.???(f67)
	#r68 <- read.???(f68)
	#r69 <- read.???(f69)
	#r70 <- read.???(f70)
	#r71 <- read.???(f71)
	#r72 <- read.???(f72)
	#r73 <- read.???(f73)
	#r74 <- read.???(f74)
	#r75 <- read.???(f75)
	#r76 <- read.???(f76)
	#r77 <- read.???(f77)
	#r78 <- read.???(f78)
	#r79 <- read.???(f79)
	#r80 <- read.???(f80)
	#r81 <- read.???(f81)
	#r82 <- read.???(f82)
	#r83 <- read.???(f83)
	#r84 <- read.???(f84)
	#r85 <- read.???(f85)
	#r86 <- read.???(f86)
	#r87 <- read.???(f87)
	#r88 <- read.???(f88)
	#r89 <- read.???(f89)
	#r90 <- read.???(f90)
	#r91 <- read.???(f91)
	#r92 <- read.???(f92)
	#r93 <- read.???(f93)
	#r94 <- read.???(f94)
	#r95 <- read.???(f95)
	#r96 <- read.???(f96)
	#r97 <- read.???(f97)
	#r98 <- read.???(f98)
	#r99 <- read.???(f99)
	#r100 <- read.???(f100)
	#r101 <- read.???(f101)
	#r102 <- read.???(f102)
	#r103 <- read.???(f103)
	#r104 <- read.???(f104)
	#r105 <- read.???(f105)
	#r106 <- read.???(f106)
	#r107 <- read.???(f107)
	#r108 <- read.???(f108)
	#r109 <- read.???(f109)
	#r110 <- read.???(f110)
	#r111 <- read.???(f111)
	#r112 <- read.???(f112)
	#r113 <- read.???(f113)
	#r114 <- read.???(f114)
	#r115 <- read.???(f115)
	#r116 <- read.???(f116)
	#r117 <- read.???(f117)
	#r118 <- read.???(f118)
	#r119 <- read.???(f119)
	#r120 <- read.???(f120)
	#r121 <- read.???(f121)
	#r122 <- read.???(f122)
	#r123 <- read.???(f123)
	#r124 <- read.???(f124)
	#r125 <- read.???(f125)
	#r126 <- read.???(f126)
	#r127 <- read.???(f127)
	#r128 <- read.???(f128)
	#r129 <- read.???(f129)
	#r130 <- read.???(f130)
	#r131 <- read.???(f131)
	#r132 <- read.???(f132)
	#r133 <- read.???(f133)
	#r134 <- read.???(f134)
	#r135 <- read.???(f135)
	#r136 <- read.???(f136)
	#r137 <- read.???(f137)
	#r138 <- read.???(f138)
	#r139 <- read.???(f139)
	#r140 <- read.???(f140)
	#r141 <- read.???(f141)
	#r142 <- read.???(f142)
	#r143 <- read.???(f143)
	#r144 <- read.???(f144)
	#r145 <- read.???(f145)
	#r146 <- read.???(f146)
	#r147 <- read.???(f147)
	#r148 <- read.???(f148)
	#r149 <- read.???(f149)
	#r150 <- read.???(f150)
	#r151 <- read.???(f151)
	#r152 <- read.???(f152)
	#r153 <- read.???(f153)
	#r154 <- read.???(f154)
	#r155 <- read.???(f155)
	#r156 <- read.???(f156)
	#r157 <- read.???(f157)
	#r158 <- read.???(f158)
	#r159 <- read.???(f159)
	#r160 <- read.???(f160)
	#r161 <- read.???(f161)
	#r162 <- read.???(f162)
	#r163 <- read.???(f163)
	#r164 <- read.???(f164)
	#r165 <- read.???(f165)
	#r166 <- read.???(f166)
	#r167 <- read.???(f167)
	#r168 <- read.???(f168)
	#r169 <- read.???(f169)
	#r170 <- read.???(f170)
	#r171 <- read.???(f171)
	#r172 <- read.???(f172)
	#r173 <- read.???(f173)
	#r174 <- read.???(f174)
	#r175 <- read.???(f175)
	#r176 <- read.???(f176)
	#r177 <- read.???(f177)
	#r178 <- read.???(f178)
	#r179 <- read.???(f179)
	#r180 <- read.???(f180)
	#r181 <- read.???(f181)
	#r182 <- read.???(f182)
	#r183 <- read.???(f183)
	#r184 <- read.???(f184)
	#r185 <- read.???(f185)
	#r186 <- read.???(f186)
	#r187 <- read.???(f187)
	#r188 <- read.???(f188)
	#r189 <- read.???(f189)
	#r190 <- read.???(f190)
	#r191 <- read.???(f191)
	#r192 <- read.???(f192)
	#r193 <- read.???(f193)
	#r194 <- read.???(f194)
	#r195 <- read.???(f195)
	#r196 <- read.???(f196)
	#r197 <- read.???(f197)
	#r198 <- read.???(f198)
	#r199 <- read.???(f199)
	#r200 <- read.???(f200)
	#r201 <- read.???(f201)
	#r202 <- read.???(f202)
	#r203 <- read.???(f203)
	#r204 <- read.???(f204)
	#r205 <- read.???(f205)
	#r206 <- read.???(f206)
	#r207 <- read.???(f207)
	#r208 <- read.???(f208)
	#r209 <- read.???(f209)
	#r210 <- read.???(f210)
	#r211 <- read.???(f211)
	#r212 <- read.???(f212)
	#r213 <- read.???(f213)
	#r214 <- read.???(f214)
	#r215 <- read.???(f215)
	#r216 <- read.???(f216)
	#r217 <- read.???(f217)
	#r218 <- read.???(f218)
	#r219 <- read.???(f219)
	#r220 <- read.???(f220)
	#r221 <- read.???(f221)
	#r222 <- read.???(f222)
	#r223 <- read.???(f223)
	#r224 <- read.???(f224)
	#r225 <- read.???(f225)
	#r226 <- read.???(f226)
	#r227 <- read.???(f227)
	#r228 <- read.???(f228)
	#r229 <- read.???(f229)
	#r230 <- read.???(f230)
	#r231 <- read.???(f231)
	#r232 <- read.???(f232)
	#r233 <- read.???(f233)
	#r234 <- read.???(f234)
	#r235 <- read.???(f235)
	#r236 <- read.???(f236)
	#r237 <- read.???(f237)
	#r238 <- read.???(f238)
	#r239 <- read.???(f239)
	#r240 <- read.???(f240)
	#r241 <- read.???(f241)
	#r242 <- read.???(f242)
	#r243 <- read.???(f243)
	#r244 <- read.???(f244)
	#r245 <- read.???(f245)
	#r246 <- read.???(f246)
	#r247 <- read.???(f247)
	#r248 <- read.???(f248)
	#r249 <- read.???(f249)
	#r250 <- read.???(f250)
	#r251 <- read.???(f251)
	#r252 <- read.???(f252)
	#r253 <- read.???(f253)
	#r254 <- read.???(f254)
	#r255 <- read.???(f255)
	#r256 <- read.???(f256)
	#r257 <- read.???(f257)
	#r258 <- read.???(f258)
	#r259 <- read.???(f259)
	#r260 <- read.???(f260)
	#r261 <- read.???(f261)
	#r262 <- read.???(f262)
	#r263 <- read.???(f263)
	#r264 <- read.???(f264)
	#r265 <- read.???(f265)
	#r266 <- read.???(f266)
	#r267 <- read.???(f267)
	#r268 <- read.???(f268)
	#r269 <- read.???(f269)
	#r270 <- read.???(f270)
	#r271 <- read.???(f271)
	#r272 <- read.???(f272)
	#r273 <- read.???(f273)
	#r274 <- read.???(f274)
	#r275 <- read.???(f275)
	#r276 <- read.???(f276)
	#r277 <- read.???(f277)
	#r278 <- read.???(f278)
	#r279 <- read.???(f279)
	#r280 <- read.???(f280)
	#r281 <- read.???(f281)
	#r282 <- read.???(f282)
	#r283 <- read.???(f283)
	#r284 <- read.???(f284)
	#r285 <- read.???(f285)
	#r286 <- read.???(f286)
	#r287 <- read.???(f287)
	#r288 <- read.???(f288)
	#r289 <- read.???(f289)
	#r290 <- read.???(f290)
	#r291 <- read.???(f291)
	#r292 <- read.???(f292)
	#r293 <- read.???(f293)
	#r294 <- read.???(f294)
	#r295 <- read.???(f295)
	#r296 <- read.???(f296)
	#r297 <- read.???(f297)
	#r298 <- read.???(f298)
	#r299 <- read.???(f299)
	#r300 <- read.???(f300)
	#r301 <- read.???(f301)
	#r302 <- read.???(f302)
	#r303 <- read.???(f303)
	#r304 <- read.???(f304)
	#r305 <- read.???(f305)
	#r306 <- read.???(f306)
	#r307 <- read.???(f307)
	#r308 <- read.???(f308)
	#r309 <- read.???(f309)
	#r310 <- read.???(f310)
	#r311 <- read.???(f311)
	#r312 <- read.???(f312)
	#r313 <- read.???(f313)
	#r314 <- read.???(f314)
	#r315 <- read.???(f315)
	#r316 <- read.???(f316)
	#r317 <- read.???(f317)
	#r318 <- read.???(f318)
	#r319 <- read.???(f319)
	#r320 <- read.???(f320)
	#r321 <- read.???(f321)
	#r322 <- read.???(f322)
	#r323 <- read.???(f323)
	#r324 <- read.???(f324)
	#r325 <- read.???(f325)
	#r326 <- read.???(f326)
	#r327 <- read.???(f327)
	#r328 <- read.???(f328)
	#r329 <- read.???(f329)
	#r330 <- read.???(f330)
	#r331 <- read.???(f331)
	#r332 <- read.???(f332)
	#r333 <- read.???(f333)
	#r334 <- read.???(f334)
	#r335 <- read.???(f335)
	#r336 <- read.???(f336)
	#r337 <- read.???(f337)
	#r338 <- read.???(f338)
	#r339 <- read.???(f339)
	#r340 <- read.???(f340)
	#r341 <- read.???(f341)
	#r342 <- read.???(f342)
	#r343 <- read.???(f343)
	#r344 <- read.???(f344)
	#r345 <- read.???(f345)
	#r346 <- read.???(f346)
	#r347 <- read.???(f347)
	#r348 <- read.???(f348)
	#r349 <- read.???(f349)
	#r350 <- read.???(f350)
	#r351 <- read.???(f351)
	#r352 <- read.???(f352)
	#r353 <- read.???(f353)
	#r354 <- read.???(f354)
	#r355 <- read.???(f355)
	#r356 <- read.???(f356)
	#r357 <- read.???(f357)
	#r358 <- read.???(f358)
	#r359 <- read.???(f359)
	#r360 <- read.???(f360)
	#r361 <- read.???(f361)
	#r362 <- read.???(f362)
	#r363 <- read.???(f363)
	#r364 <- read.???(f364)
	#r365 <- read.???(f365)
	#r366 <- read.???(f366)
	#r367 <- read.???(f367)
	#r368 <- read.???(f368)
	#r369 <- read.???(f369)
	#r370 <- read.???(f370)
	#r371 <- read.???(f371)
	#r372 <- read.???(f372)
	#r373 <- read.???(f373)
	#r374 <- read.???(f374)
	#r375 <- read.???(f375)
	#r376 <- read.???(f376)
	#r377 <- read.???(f377)
	#r378 <- read.???(f378)
	#r379 <- read.???(f379)
	#r380 <- read.???(f380)
	#r381 <- read.???(f381)
	#r382 <- read.???(f382)
	#r383 <- read.???(f383)
	#r384 <- read.???(f384)
	#r385 <- read.???(f385)
	#r386 <- read.???(f386)
	#r387 <- read.???(f387)
	#r388 <- read.???(f388)
	#r389 <- read.???(f389)
	#r390 <- read.???(f390)
	#r391 <- read.???(f391)
	#r392 <- read.???(f392)
	#r393 <- read.???(f393)
	#r394 <- read.???(f394)
	#r395 <- read.???(f395)
	#r396 <- read.???(f396)
	#r397 <- read.???(f397)
	#r398 <- read.???(f398)
	#r399 <- read.???(f399)
	#r400 <- read.???(f400)
	#r401 <- read.???(f401)
	#r402 <- read.???(f402)
	#r403 <- read.???(f403)
	#r404 <- read.???(f404)
	#r405 <- read.???(f405)
	#r406 <- read.???(f406)
	#r407 <- read.???(f407)
	#r408 <- read.???(f408)
	#r409 <- read.???(f409)
	#r410 <- read.???(f410)
	#r411 <- read.???(f411)
	#r412 <- read.???(f412)
	#r413 <- read.???(f413)
	#r414 <- read.???(f414)
	#r415 <- read.???(f415)
	#r416 <- read.???(f416)
	#r417 <- read.???(f417)
	#r418 <- read.???(f418)
	#r419 <- read.???(f419)
	#r420 <- read.???(f420)
	#r421 <- read.???(f421)
	#r422 <- read.???(f422)
	#r423 <- read.???(f423)
	#r424 <- read.???(f424)
	#r425 <- read.???(f425)
	#r426 <- read.???(f426)
	#r427 <- read.???(f427)
	#r428 <- read.???(f428)
	#r429 <- read.???(f429)
	#r430 <- read.???(f430)
	#r431 <- read.???(f431)
	#r432 <- read.???(f432)
	#r433 <- read.???(f433)
	#r434 <- read.???(f434)
	#r435 <- read.???(f435)
	#r436 <- read.???(f436)
	#r437 <- read.???(f437)
	#r438 <- read.???(f438)
	#r439 <- read.???(f439)
	#r440 <- read.???(f440)
	#r441 <- read.???(f441)
	#r442 <- read.???(f442)
	#r443 <- read.???(f443)
	#r444 <- read.???(f444)
	#r445 <- read.???(f445)
	#r446 <- read.???(f446)
	#r447 <- read.???(f447)
	#r448 <- read.???(f448)
	#r449 <- read.???(f449)
	#r450 <- read.???(f450)
	#r451 <- read.???(f451)
	#r452 <- read.???(f452)
	#r453 <- read.???(f453)
	#r454 <- read.???(f454)
	#r455 <- read.???(f455)
	#r456 <- read.???(f456)
	#r457 <- read.???(f457)
	#r458 <- read.???(f458)
	#r459 <- read.???(f459)
	#r460 <- read.???(f460)
	#r461 <- read.???(f461)
	#r462 <- read.???(f462)
	#r463 <- read.???(f463)
	#r464 <- read.???(f464)
	#r465 <- read.???(f465)
	#r466 <- read.???(f466)
	#r467 <- read.???(f467)
	#r468 <- read.???(f468)
	#r469 <- read.???(f469)
	#r470 <- read.???(f470)
	#r471 <- read.???(f471)
	#r472 <- read.???(f472)
	#r473 <- read.???(f473)
	#r474 <- read.???(f474)
	#r475 <- read.???(f475)
	#r476 <- read.???(f476)
	#r477 <- read.???(f477)
	#r478 <- read.???(f478)
	#r479 <- read.???(f479)
	#r480 <- read.???(f480)
	#r481 <- read.???(f481)
	#r482 <- read.???(f482)
	#r483 <- read.???(f483)
	#r484 <- read.???(f484)
	#r485 <- read.???(f485)
	#r486 <- read.???(f486)
	#r487 <- read.???(f487)
	#r488 <- read.???(f488)
	#r489 <- read.???(f489)
	#r490 <- read.???(f490)
	#r491 <- read.???(f491)
	#r492 <- read.???(f492)
	#r493 <- read.???(f493)
	#r494 <- read.???(f494)
	#r495 <- read.???(f495)
	#r496 <- read.???(f496)
	#r497 <- read.???(f497)
	#r498 <- read.???(f498)
	#r499 <- read.???(f499)
	#r500 <- read.???(f500)
	#r501 <- read.???(f501)
	#r502 <- read.???(f502)
	#r503 <- read.???(f503)
	#r504 <- read.???(f504)
	#r505 <- read.???(f505)
	#r506 <- read.???(f506)
	#r507 <- read.???(f507)
	#r508 <- read.???(f508)
	#r509 <- read.???(f509)
	#r510 <- read.???(f510)
	#r511 <- read.???(f511)
	#r512 <- read.???(f512)
	#r513 <- read.???(f513)
	#r514 <- read.???(f514)
	#r515 <- read.???(f515)
	#r516 <- read.???(f516)
	#r517 <- read.???(f517)
	#r518 <- read.???(f518)
	#r519 <- read.???(f519)
	#r520 <- read.???(f520)
	return(FALSE)
}

## now test your function in a _clean_ R environment (no packages loaded, no other objects available)
# carob_script(path=_____)
