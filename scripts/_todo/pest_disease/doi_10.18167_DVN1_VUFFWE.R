# R script for "carob"
# license: GPL (>=3)

## NOTES
# 476 plot surveys on Mayotte (2019-2020). One row per species with a rating > 0.
# Floristique and Enherbement are the DOC 1-9 notes. weed_species is the 5-letter
# code. French crop names are mapped where the whole label resolves. "qq", "un peu",
# "coupée", and a trailing mulch note are dropped. "avant X" is not treated as a
# current crop. Two trailing Floristique rows are empty and do not become records.

## ISSUES
# Labels that include ylang, a cucurbit family name, listea, pomme cannelle,
# bredes mamaftan, jardin mahorais, terrain nu, avocat ?, or "maïs ou sorgho"
# are left as written. No coordinates. Some Lieu spellings differ and are kept.
# write_files: missing variables: yield, yield_moisture, yield_part, latitude,
# longitude, K_fertilizer, N_fertilizer, P_fertilizer, planting_date, harvest_date.
# invalid terms: crop: Banane, Manioc, Curcubitacée, Agrumes; Bananier, Agrumes,
# Curcubitacée, Papayer, Manioc; Bananier, Ananas, Avocat ?; Bananier, Goyavier,
# Pomme cannelle; Bananier, Manioc, Agrumes, Ylang; Bredes mamaftan, salade;
# Cocotier, (Listea); Cucurbitacée; Curcubitacées, Ipomoea batatas; Jardin mahorais;
# Jardin Mahorais; Jardin mahorais, Ananas, Banane, Grenade; Maïs ou Sorgho,
# Embrevade, Manioc; Riz, Maïs, Listea; Salade, jardin mahorais; Terrain nu; Ylang;
# Ylang, Agrumes et Bananier; Ylang, Cocotier.


## NOT PROCESSED
# Facteurs not mapped: Num_Relevé, Période, Zone, Climat, Agriculteur, Saison
# (SP/SS), Parcelle, Type_culture, SdC, SdC regroupe. Mayotte 2019 2020 repeats
# the survey. Méta données, FLO, FAC, and PA repeat the workbook or the DOC.
# MAY-MAR-2020-DIV-AD-FLO.txt: not read. columns: "MAY-MAR-001" "MAY-MAR-002" "MAY-MAR-003" "MAY-MAR-004" "MAY-MAR-005" "MAY-MAR-006" "MAY-MAR-007" "MAY-MAR-008" "MAY-MAR-009" "MAY-MAR-010" "MAY-MAR-011" "MAY-MAR-012" "MAY-MAR-013" "MAY-MAR-014" "MAY-MAR-015" "MAY-MAR-016" "MAY-MAR-017" "MAY-MAR-018" "MAY-MAR-019" "MAY-MAR-020" "MAY-MAR-021" "MAY-MAR-022" "MAY-MAR-023" "MAY-MAR-024" "MAY-MAR-025" "MAY-MAR-026" "MAY-MAR-027" "MAY-MAR-028" "MAY-MAR-029" "MAY-MAR-030" "MAY-MAR-031" "MAY-MAR-032" "MAY-MAR-033" "MAY-MAR-034" "MAY-MAR-035" "MAY-MAR-036" "MAY-MAR-037" "MAY-MAR-038" "MAY-MAR-039" "MAY-MAR-040" "MAY-MAR-041" "MAY-MAR-042" "MAY-MAR-043" "MAY-MAR-044" "MAY-MAR-045" "MAY-MAR-046" "MAY-MAR-047" "MAY-MAR-048" "MAY-MAR-049" "MAY-MAR-050" "MAY-MAR-051" "MAY-MAR-052" "MAY-MAR-053" "MAY-MAR-054" "MAY-MAR-055" "MAY-MAR-056" "MAY-MAR-057" "MAY-MAR-058" "MAY-MAR-059" "MAY-MAR-060" "MAY-MAR-061" "MAY-MAR-062" "MAY-MAR-063" "MAY-MAR-064" "MAY-MAR-065" "MAY-MAR-066" "MAY-MAR-067" "MAY-MAR-068" "MAY-MAR-069" "MAY-MAR-070" "MAY-MAR-071" "MAY-MAR-072" "MAY-MAR-073" "MAY-MAR-074" "MAY-MAR-075" "MAY-MAR-076" "MAY-MAR-077" "MAY-MAR-078" "MAY-MAR-079" "MAY-MAR-080" "MAY-MAR-081" "MAY-MAR-082" "MAY-MAR-083" "MAY-MAR-084" "MAY-MAR-085" "MAY-MAR-086" "MAY-MAR-087" "MAY-MAR-088" "MAY-MAR-089" "MAY-MAR-090" "MAY-MAR-091" "MAY-MAR-092" "MAY-MAR-093" "MAY-MAR-094" "MAY-MAR-095" "MAY-MAR-096" "MAY-MAR-097" "MAY-MAR-098" "MAY-MAR-099" "MAY-MAR-100" "MAY-MAR-101" "MAY-MAR-102" "MAY-MAR-103" "MAY-MAR-104" "MAY-MAR-105" "MAY-MAR-106" "MAY-MAR-107" "MAY-MAR-108" "MAY-MAR-109" "MAY-MAR-110" "MAY-MAR-111" "MAY-MAR-112" "MAY-MAR-113" "MAY-MAR-114" "MAY-MAR-115" "MAY-MAR-116" "MAY-MAR-117" "MAY-MAR-118" "MAY-MAR-119" "MAY-MAR-120" "MAY-MAR-121" "MAY-MAR-122" "MAY-MAR-123" "MAY-MAR-124" "MAY-MAR-125" "MAY-MAR-126" "MAY-MAR-127" "MAY-MAR-128" "MAY-MAR-129" "MAY-MAR-130" "MAY-MAR-131" "MAY-MAR-132" "MAY-MAR-133" "MAY-MAR-134" "MAY-MAR-135" "MAY-MAR-136" "MAY-MAR-137" "MAY-MAR-138" "MAY-MAR-139" "MAY-MAR-140" "MAY-MAR-141" "MAY-MAR-142" "MAY-MAR-143" "MAY-MAR-144" "MAY-MAR-145" "MAY-MAR-146" "MAY-MAR-147" "MAY-MAR-148" "MAY-MAR-149" "MAY-MAR-150" "MAY-MAR-151" "MAY-MAR-152" "MAY-MAR-153" "MAY-MAR-154" "MAY-MAR-155" "MAY-MAR-156" "MAY-MAR-157" "MAY-MAR-158" "MAY-MAR-159" "MAY-MAR-160" "MAY-MAR-161" "MAY-MAR-162" "MAY-MAR-163" "MAY-MAR-164" "MAY-MAR-165" "MAY-MAR-166" "MAY-MAR-167" "MAY-MAR-168" "MAY-MAR-169" "MAY-MAR-170" "MAY-MAR-171" "MAY-MAR-172" "MAY-MAR-173" "MAY-MAR-174" "MAY-MAR-175" "MAY-MAR-176" "MAY-MAR-177" "MAY-MAR-178" "MAY-MAR-179" "MAY-MAR-180" "MAY-MAR-181" "MAY-MAR-182" "MAY-MAR-183" "MAY-MAR-184" "MAY-MAR-185" "MAY-MAR-186" "MAY-MAR-187" "MAY-MAR-188" "MAY-MAR-189" "MAY-MAR-190" "MAY-MAR-191" "MAY-MAR-192" "MAY-MAR-193" "MAY-MAR-194" "MAY-MAR-195" "MAY-MAR-196" "MAY-MAR-197" "MAY-MAR-198" "MAY-MAR-199" "MAY-MAR-200" "MAY-MAR-201" "MAY-MAR-202" "MAY-MAR-203" "MAY-MAR-204" "MAY-MAR-205" "MAY-MAR-206" "MAY-MAR-207" "MAY-MAR-208" "MAY-MAR-209" "MAY-MAR-210" "MAY-MAR-211" "MAY-MAR-212" "MAY-MAR-213" "MAY-MAR-214" "MAY-MAR-215" "MAY-MAR-216" "MAY-MAR-217" "MAY-MAR-218" "MAY-MAR-219" "MAY-MAR-220" "MAY-MAR-221" "MAY-MAR-222" "MAY-MAR-223" "MAY-MAR-224" "MAY-MAR-225" "MAY-MAR-226" "MAY-MAR-227" "MAY-MAR-228" "MAY-MAR-229" "MAY-MAR-230" "MAY-MAR-231" "MAY-MAR-232" "MAY-MAR-233" "MAY-MAR-234" "MAY-MAR-235" "MAY-MAR-236" "MAY-MAR-237" "MAY-MAR-238" "MAY-MAR-239" "MAY-MAR-240" "MAY-MAR-241" "MAY-MAR-242" "MAY-MAR-243" "MAY-MAR-244" "MAY-MAR-245" "MAY-MAR-246" "MAY-MAR-247" "MAY-MAR-248" "MAY-MAR-249" "MAY-MAR-250" "MAY-MAR-251" "MAY-MAR-252" "MAY-MAR-253" "MAY-MAR-254" "MAY-MAR-255" "MAY-MAR-256" "MAY-MAR-257" "MAY-MAR-258" "MAY-MAR-259" "MAY-MAR-260" "MAY-MAR-261" "MAY-MAR-262" "MAY-MAR-263" "MAY-MAR-264" "MAY-MAR-265" "MAY-MAR-266" "MAY-MAR-267" "MAY-MAR-268" "MAY-MAR-269" "MAY-MAR-270" "MAY-MAR-271" "MAY-MAR-272" "MAY-MAR-273" "MAY-MAR-274" "MAY-MAR-275" "MAY-MAR-276" "MAY-MAR-277" "MAY-MAR-278" "MAY-MAR-279" "MAY-MAR-280" "MAY-MAR-281" "MAY-MAR-282" "MAY-MAR-283" "MAY-MAR-284" "MAY-MAR-285" "MAY-MAR-286" "MAY-MAR-287" "MAY-MAR-288" "MAY-MAR-289" "MAY-MAR-290" "MAY-MAR-291" "MAY-MAR-292" "MAY-MAR-293" "MAY-MAR-294" "MAY-MAR-295" "MAY-MAR-296" "MAY-MAR-297" "MAY-MAR-298" "MAY-MAR-299" "MAY-MAR-300" "MAY-MAR-301" "MAY-MAR-302" "MAY-MAR-303" "MAY-MAR-304" "MAY-MAR-305" "MAY-MAR-306" "MAY-MAR-307" "MAY-MAR-308" "MAY-MAR-309" "MAY-MAR-310" "MAY-MAR-311" "MAY-MAR-312" "MAY-MAR-313" "MAY-MAR-314" "MAY-MAR-315" "MAY-MAR-316" "MAY-MAR-317" "MAY-MAR-318" "MAY-MAR-319" "MAY-MAR-320" "MAY-MAR-321" "MAY-MAR-322" "MAY-MAR-323" "MAY-MAR-324" "MAY-MAR-325" "MAY-MAR-326" "MAY-MAR-327" "MAY-MAR-328" "MAY-MAR-329" "MAY-MAR-330" "MAY-MAR-331" "MAY-MAR-332" "MAY-MAR-333" "MAY-MAR-334" "MAY-MAR-335" "MAY-MAR-336" "MAY-MAR-337" "MAY-MAR-338" "MAY-MAR-339" "MAY-MAR-340" "MAY-MAR-341" "MAY-MAR-342" "MAY-MAR-343" "MAY-MAR-344" "MAY-MAR-345" "MAY-MAR-346" "MAY-MAR-347" "MAY-MAR-348" "MAY-MAR-349" "MAY-MAR-350" "MAY-MAR-351" "MAY-MAR-352" "MAY-MAR-353" "MAY-MAR-354" "MAY-MAR-355" "MAY-MAR-356" "MAY-MAR-357" "MAY-MAR-358" "MAY-MAR-359" "MAY-MAR-360" "MAY-MAR-361" "MAY-MAR-362" "MAY-MAR-363" "MAY-MAR-364" "MAY-MAR-365" "MAY-MAR-366" "MAY-MAR-367" "MAY-MAR-368" "MAY-MAR-369" "MAY-MAR-370" "MAY-MAR-371" "MAY-MAR-372" "MAY-MAR-373" "MAY-MAR-374" "MAY-MAR-375" "MAY-MAR-376" "MAY-MAR-377" "MAY-MAR-378" "MAY-MAR-379" "MAY-MAR-380" "MAY-MAR-381" "MAY-MAR-382" "MAY-MAR-383" "MAY-MAR-384" "MAY-MAR-385" "MAY-MAR-386" "MAY-MAR-387" "MAY-MAR-388" "MAY-MAR-389" "MAY-MAR-390" "MAY-MAR-391" "MAY-MAR-392" "MAY-MAR-393" "MAY-MAR-394" "MAY-MAR-395" "MAY-MAR-396" "MAY-MAR-397" "MAY-MAR-398" "MAY-MAR-399" "MAY-MAR-400" "MAY-MAR-401" "MAY-MAR-402" "MAY-MAR-403" "MAY-MAR-404" "MAY-MAR-405" "MAY-MAR-406" "MAY-MAR-407" "MAY-MAR-408" "MAY-MAR-409" "MAY-MAR-410" "MAY-MAR-411" "MAY-MAR-412" "MAY-MAR-413" "MAY-MAR-414" "MAY-MAR-415" "MAY-MAR-416" "MAY-MAR-417" "MAY-MAR-418" "MAY-MAR-419" "MAY-MAR-420" "MAY-MAR-421" "MAY-MAR-422" "MAY-MAR-423" "MAY-MAR-424" "MAY-MAR-425" "MAY-MAR-426" "MAY-MAR-427" "MAY-MAR-428" "MAY-MAR-429" "MAY-MAR-430" "MAY-MAR-431" "MAY-MAR-432" "MAY-MAR-433" "MAY-MAR-434" "MAY-MAR-435" "MAY-MAR-436" "MAY-MAR-437" "MAY-MAR-438" "MAY-MAR-439" "MAY-MAR-440" "MAY-MAR-441" "MAY-MAR-442" "MAY-MAR-443" "MAY-MAR-444" "MAY-MAR-445" "MAY-MAR-446" "MAY-MAR-447" "MAY-MAR-448" "MAY-MAR-449" "MAY-MAR-450" "MAY-MAR-451" "MAY-MAR-452" "MAY-MAR-453" "MAY-MAR-454" "MAY-MAR-455" "MAY-MAR-456" "MAY-MAR-457" "MAY-MAR-458" "MAY-MAR-459" "MAY-MAR-460" "MAY-MAR-461" "MAY-MAR-462" "MAY-MAR-463" "MAY-MAR-464" "MAY-MAR-465" "MAY-MAR-466" "MAY-MAR-467" "MAY-MAR-468" "MAY-MAR-469" "MAY-MAR-470" "MAY-MAR-471" "MAY-MAR-472" "MAY-MAR-473" "MAY-MAR-474" "MAY-MAR-475" "MAY-MAR-476". Observation records are missing from d until this file is standardized.
# MAY-MAR-2020-DIV-FAC.txt: not read. columns: "MAY-MAR-001" "MAY-MAR-002" "MAY-MAR-003" "MAY-MAR-004" "MAY-MAR-005" "MAY-MAR-006" "MAY-MAR-007" "MAY-MAR-008" "MAY-MAR-009" "MAY-MAR-010" "MAY-MAR-011" "MAY-MAR-012" "MAY-MAR-013" "MAY-MAR-014" "MAY-MAR-015" "MAY-MAR-016" "MAY-MAR-017" "MAY-MAR-018" "MAY-MAR-019" "MAY-MAR-020" "MAY-MAR-021" "MAY-MAR-022" "MAY-MAR-023" "MAY-MAR-024" "MAY-MAR-025" "MAY-MAR-026" "MAY-MAR-027" "MAY-MAR-028" "MAY-MAR-029" "MAY-MAR-030" "MAY-MAR-031" "MAY-MAR-032" "MAY-MAR-033" "MAY-MAR-034" "MAY-MAR-035" "MAY-MAR-036" "MAY-MAR-037" "MAY-MAR-038" "MAY-MAR-039" "MAY-MAR-040" "MAY-MAR-041" "MAY-MAR-042" "MAY-MAR-043" "MAY-MAR-044" "MAY-MAR-045" "MAY-MAR-046" "MAY-MAR-047" "MAY-MAR-048" "MAY-MAR-049" "MAY-MAR-050" "MAY-MAR-051" "MAY-MAR-052" "MAY-MAR-053" "MAY-MAR-054" "MAY-MAR-055" "MAY-MAR-056" "MAY-MAR-057" "MAY-MAR-058" "MAY-MAR-059" "MAY-MAR-060" "MAY-MAR-061" "MAY-MAR-062" "MAY-MAR-063" "MAY-MAR-064" "MAY-MAR-065" "MAY-MAR-066" "MAY-MAR-067" "MAY-MAR-068" "MAY-MAR-069" "MAY-MAR-070" "MAY-MAR-071" "MAY-MAR-072" "MAY-MAR-073" "MAY-MAR-074" "MAY-MAR-075" "MAY-MAR-076" "MAY-MAR-077" "MAY-MAR-078" "MAY-MAR-079" "MAY-MAR-080" "MAY-MAR-081" "MAY-MAR-082" "MAY-MAR-083" "MAY-MAR-084" "MAY-MAR-085" "MAY-MAR-086" "MAY-MAR-087" "MAY-MAR-088" "MAY-MAR-089" "MAY-MAR-090" "MAY-MAR-091" "MAY-MAR-092" "MAY-MAR-093" "MAY-MAR-094" "MAY-MAR-095" "MAY-MAR-096" "MAY-MAR-097" "MAY-MAR-098" "MAY-MAR-099" "MAY-MAR-100" "MAY-MAR-101" "MAY-MAR-102" "MAY-MAR-103" "MAY-MAR-104" "MAY-MAR-105" "MAY-MAR-106" "MAY-MAR-107" "MAY-MAR-108" "MAY-MAR-109" "MAY-MAR-110" "MAY-MAR-111" "MAY-MAR-112" "MAY-MAR-113" "MAY-MAR-114" "MAY-MAR-115" "MAY-MAR-116" "MAY-MAR-117" "MAY-MAR-118" "MAY-MAR-119" "MAY-MAR-120" "MAY-MAR-121" "MAY-MAR-122" "MAY-MAR-123" "MAY-MAR-124" "MAY-MAR-125" "MAY-MAR-126" "MAY-MAR-127" "MAY-MAR-128" "MAY-MAR-129" "MAY-MAR-130" "MAY-MAR-131" "MAY-MAR-132" "MAY-MAR-133" "MAY-MAR-134" "MAY-MAR-135" "MAY-MAR-136" "MAY-MAR-137" "MAY-MAR-138" "MAY-MAR-139" "MAY-MAR-140" "MAY-MAR-141" "MAY-MAR-142" "MAY-MAR-143" "MAY-MAR-144" "MAY-MAR-145" "MAY-MAR-146" "MAY-MAR-147" "MAY-MAR-148" "MAY-MAR-149" "MAY-MAR-150" "MAY-MAR-151" "MAY-MAR-152" "MAY-MAR-153" "MAY-MAR-154" "MAY-MAR-155" "MAY-MAR-156" "MAY-MAR-157" "MAY-MAR-158" "MAY-MAR-159" "MAY-MAR-160" "MAY-MAR-161" "MAY-MAR-162" "MAY-MAR-163" "MAY-MAR-164" "MAY-MAR-165" "MAY-MAR-166" "MAY-MAR-167" "MAY-MAR-168" "MAY-MAR-169" "MAY-MAR-170" "MAY-MAR-171" "MAY-MAR-172" "MAY-MAR-173" "MAY-MAR-174" "MAY-MAR-175" "MAY-MAR-176" "MAY-MAR-177" "MAY-MAR-178" "MAY-MAR-179" "MAY-MAR-180" "MAY-MAR-181" "MAY-MAR-182" "MAY-MAR-183" "MAY-MAR-184" "MAY-MAR-185" "MAY-MAR-186" "MAY-MAR-187" "MAY-MAR-188" "MAY-MAR-189" "MAY-MAR-190" "MAY-MAR-191" "MAY-MAR-192" "MAY-MAR-193" "MAY-MAR-194" "MAY-MAR-195" "MAY-MAR-196" "MAY-MAR-197" "MAY-MAR-198" "MAY-MAR-199" "MAY-MAR-200" "MAY-MAR-201" "MAY-MAR-202" "MAY-MAR-203" "MAY-MAR-204" "MAY-MAR-205" "MAY-MAR-206" "MAY-MAR-207" "MAY-MAR-208" "MAY-MAR-209" "MAY-MAR-210" "MAY-MAR-211" "MAY-MAR-212" "MAY-MAR-213" "MAY-MAR-214" "MAY-MAR-215" "MAY-MAR-216" "MAY-MAR-217" "MAY-MAR-218" "MAY-MAR-219" "MAY-MAR-220" "MAY-MAR-221" "MAY-MAR-222" "MAY-MAR-223" "MAY-MAR-224" "MAY-MAR-225" "MAY-MAR-226" "MAY-MAR-227" "MAY-MAR-228" "MAY-MAR-229" "MAY-MAR-230" "MAY-MAR-231" "MAY-MAR-232" "MAY-MAR-233" "MAY-MAR-234" "MAY-MAR-235" "MAY-MAR-236" "MAY-MAR-237" "MAY-MAR-238" "MAY-MAR-239" "MAY-MAR-240" "MAY-MAR-241" "MAY-MAR-242" "MAY-MAR-243" "MAY-MAR-244" "MAY-MAR-245" "MAY-MAR-246" "MAY-MAR-247" "MAY-MAR-248" "MAY-MAR-249" "MAY-MAR-250" "MAY-MAR-251" "MAY-MAR-252" "MAY-MAR-253" "MAY-MAR-254" "MAY-MAR-255" "MAY-MAR-256" "MAY-MAR-257" "MAY-MAR-258" "MAY-MAR-259" "MAY-MAR-260" "MAY-MAR-261" "MAY-MAR-262" "MAY-MAR-263" "MAY-MAR-264" "MAY-MAR-265" "MAY-MAR-266" "MAY-MAR-267" "MAY-MAR-268" "MAY-MAR-269" "MAY-MAR-270" "MAY-MAR-271" "MAY-MAR-272" "MAY-MAR-273" "MAY-MAR-274" "MAY-MAR-275" "MAY-MAR-276" "MAY-MAR-277" "MAY-MAR-278" "MAY-MAR-279" "MAY-MAR-280" "MAY-MAR-281" "MAY-MAR-282" "MAY-MAR-283" "MAY-MAR-284" "MAY-MAR-285" "MAY-MAR-286" "MAY-MAR-287" "MAY-MAR-288" "MAY-MAR-289" "MAY-MAR-290" "MAY-MAR-291" "MAY-MAR-292" "MAY-MAR-293" "MAY-MAR-294" "MAY-MAR-295" "MAY-MAR-296" "MAY-MAR-297" "MAY-MAR-298" "MAY-MAR-299" "MAY-MAR-300" "MAY-MAR-301" "MAY-MAR-302" "MAY-MAR-303" "MAY-MAR-304" "MAY-MAR-305" "MAY-MAR-306" "MAY-MAR-307" "MAY-MAR-308" "MAY-MAR-309" "MAY-MAR-310" "MAY-MAR-311" "MAY-MAR-312" "MAY-MAR-313" "MAY-MAR-314" "MAY-MAR-315" "MAY-MAR-316" "MAY-MAR-317" "MAY-MAR-318" "MAY-MAR-319" "MAY-MAR-320" "MAY-MAR-321" "MAY-MAR-322" "MAY-MAR-323" "MAY-MAR-324" "MAY-MAR-325" "MAY-MAR-326" "MAY-MAR-327" "MAY-MAR-328" "MAY-MAR-329" "MAY-MAR-330" "MAY-MAR-331" "MAY-MAR-332" "MAY-MAR-333" "MAY-MAR-334" "MAY-MAR-335" "MAY-MAR-336" "MAY-MAR-337" "MAY-MAR-338" "MAY-MAR-339" "MAY-MAR-340" "MAY-MAR-341" "MAY-MAR-342" "MAY-MAR-343" "MAY-MAR-344" "MAY-MAR-345" "MAY-MAR-346" "MAY-MAR-347" "MAY-MAR-348" "MAY-MAR-349" "MAY-MAR-350" "MAY-MAR-351" "MAY-MAR-352" "MAY-MAR-353" "MAY-MAR-354" "MAY-MAR-355" "MAY-MAR-356" "MAY-MAR-357" "MAY-MAR-358" "MAY-MAR-359" "MAY-MAR-360" "MAY-MAR-361" "MAY-MAR-362" "MAY-MAR-363" "MAY-MAR-364" "MAY-MAR-365" "MAY-MAR-366" "MAY-MAR-367" "MAY-MAR-368" "MAY-MAR-369" "MAY-MAR-370" "MAY-MAR-371" "MAY-MAR-372" "MAY-MAR-373" "MAY-MAR-374" "MAY-MAR-375" "MAY-MAR-376" "MAY-MAR-377" "MAY-MAR-378" "MAY-MAR-379" "MAY-MAR-380" "MAY-MAR-381" "MAY-MAR-382" "MAY-MAR-383" "MAY-MAR-384" "MAY-MAR-385" "MAY-MAR-386" "MAY-MAR-387" "MAY-MAR-388" "MAY-MAR-389" "MAY-MAR-390" "MAY-MAR-391" "MAY-MAR-392" "MAY-MAR-393" "MAY-MAR-394" "MAY-MAR-395" "MAY-MAR-396" "MAY-MAR-397" "MAY-MAR-398" "MAY-MAR-399" "MAY-MAR-400" "MAY-MAR-401" "MAY-MAR-402" "MAY-MAR-403" "MAY-MAR-404" "MAY-MAR-405" "MAY-MAR-406" "MAY-MAR-407" "MAY-MAR-408" "MAY-MAR-409" "MAY-MAR-410" "MAY-MAR-411" "MAY-MAR-412" "MAY-MAR-413" "MAY-MAR-414" "MAY-MAR-415" "MAY-MAR-416" "MAY-MAR-417" "MAY-MAR-418" "MAY-MAR-419" "MAY-MAR-420" "MAY-MAR-421" "MAY-MAR-422" "MAY-MAR-423" "MAY-MAR-424" "MAY-MAR-425" "MAY-MAR-426" "MAY-MAR-427" "MAY-MAR-428" "MAY-MAR-429" "MAY-MAR-430" "MAY-MAR-431" "MAY-MAR-432" "MAY-MAR-433" "MAY-MAR-434" "MAY-MAR-435" "MAY-MAR-436" "MAY-MAR-437" "MAY-MAR-438" "MAY-MAR-439" "MAY-MAR-440" "MAY-MAR-441" "MAY-MAR-442" "MAY-MAR-443" "MAY-MAR-444" "MAY-MAR-445" "MAY-MAR-446" "MAY-MAR-447" "MAY-MAR-448" "MAY-MAR-449" "MAY-MAR-450" "MAY-MAR-451" "MAY-MAR-452" "MAY-MAR-453" "MAY-MAR-454" "MAY-MAR-455" "MAY-MAR-456" "MAY-MAR-457" "MAY-MAR-458" "MAY-MAR-459" "MAY-MAR-460" "MAY-MAR-461" "MAY-MAR-462" "MAY-MAR-463" "MAY-MAR-464" "MAY-MAR-465" "MAY-MAR-466" "MAY-MAR-467" "MAY-MAR-468" "MAY-MAR-469" "MAY-MAR-470" "MAY-MAR-471" "MAY-MAR-472" "MAY-MAR-473" "MAY-MAR-474" "MAY-MAR-475" "MAY-MAR-476". Observation records are missing from d until this file is standardized.
# MAY-MAR-2020-DIV-PA-FLO.txt: not read. columns: "MAY-MAR-001" "MAY-MAR-002" "MAY-MAR-003" "MAY-MAR-004" "MAY-MAR-005" "MAY-MAR-006" "MAY-MAR-007" "MAY-MAR-008" "MAY-MAR-009" "MAY-MAR-010" "MAY-MAR-011" "MAY-MAR-012" "MAY-MAR-013" "MAY-MAR-014" "MAY-MAR-015" "MAY-MAR-016" "MAY-MAR-017" "MAY-MAR-018" "MAY-MAR-019" "MAY-MAR-020" "MAY-MAR-021" "MAY-MAR-022" "MAY-MAR-023" "MAY-MAR-024" "MAY-MAR-025" "MAY-MAR-026" "MAY-MAR-027" "MAY-MAR-028" "MAY-MAR-029" "MAY-MAR-030" "MAY-MAR-031" "MAY-MAR-032" "MAY-MAR-033" "MAY-MAR-034" "MAY-MAR-035" "MAY-MAR-036" "MAY-MAR-037" "MAY-MAR-038" "MAY-MAR-039" "MAY-MAR-040" "MAY-MAR-041" "MAY-MAR-042" "MAY-MAR-043" "MAY-MAR-044" "MAY-MAR-045" "MAY-MAR-046" "MAY-MAR-047" "MAY-MAR-048" "MAY-MAR-049" "MAY-MAR-050" "MAY-MAR-051" "MAY-MAR-052" "MAY-MAR-053" "MAY-MAR-054" "MAY-MAR-055" "MAY-MAR-056" "MAY-MAR-057" "MAY-MAR-058" "MAY-MAR-059" "MAY-MAR-060" "MAY-MAR-061" "MAY-MAR-062" "MAY-MAR-063" "MAY-MAR-064" "MAY-MAR-065" "MAY-MAR-066" "MAY-MAR-067" "MAY-MAR-068" "MAY-MAR-069" "MAY-MAR-070" "MAY-MAR-071" "MAY-MAR-072" "MAY-MAR-073" "MAY-MAR-074" "MAY-MAR-075" "MAY-MAR-076" "MAY-MAR-077" "MAY-MAR-078" "MAY-MAR-079" "MAY-MAR-080" "MAY-MAR-081" "MAY-MAR-082" "MAY-MAR-083" "MAY-MAR-084" "MAY-MAR-085" "MAY-MAR-086" "MAY-MAR-087" "MAY-MAR-088" "MAY-MAR-089" "MAY-MAR-090" "MAY-MAR-091" "MAY-MAR-092" "MAY-MAR-093" "MAY-MAR-094" "MAY-MAR-095" "MAY-MAR-096" "MAY-MAR-097" "MAY-MAR-098" "MAY-MAR-099" "MAY-MAR-100" "MAY-MAR-101" "MAY-MAR-102" "MAY-MAR-103" "MAY-MAR-104" "MAY-MAR-105" "MAY-MAR-106" "MAY-MAR-107" "MAY-MAR-108" "MAY-MAR-109" "MAY-MAR-110" "MAY-MAR-111" "MAY-MAR-112" "MAY-MAR-113" "MAY-MAR-114" "MAY-MAR-115" "MAY-MAR-116" "MAY-MAR-117" "MAY-MAR-118" "MAY-MAR-119" "MAY-MAR-120" "MAY-MAR-121" "MAY-MAR-122" "MAY-MAR-123" "MAY-MAR-124" "MAY-MAR-125" "MAY-MAR-126" "MAY-MAR-127" "MAY-MAR-128" "MAY-MAR-129" "MAY-MAR-130" "MAY-MAR-131" "MAY-MAR-132" "MAY-MAR-133" "MAY-MAR-134" "MAY-MAR-135" "MAY-MAR-136" "MAY-MAR-137" "MAY-MAR-138" "MAY-MAR-139" "MAY-MAR-140" "MAY-MAR-141" "MAY-MAR-142" "MAY-MAR-143" "MAY-MAR-144" "MAY-MAR-145" "MAY-MAR-146" "MAY-MAR-147" "MAY-MAR-148" "MAY-MAR-149" "MAY-MAR-150" "MAY-MAR-151" "MAY-MAR-152" "MAY-MAR-153" "MAY-MAR-154" "MAY-MAR-155" "MAY-MAR-156" "MAY-MAR-157" "MAY-MAR-158" "MAY-MAR-159" "MAY-MAR-160" "MAY-MAR-161" "MAY-MAR-162" "MAY-MAR-163" "MAY-MAR-164" "MAY-MAR-165" "MAY-MAR-166" "MAY-MAR-167" "MAY-MAR-168" "MAY-MAR-169" "MAY-MAR-170" "MAY-MAR-171" "MAY-MAR-172" "MAY-MAR-173" "MAY-MAR-174" "MAY-MAR-175" "MAY-MAR-176" "MAY-MAR-177" "MAY-MAR-178" "MAY-MAR-179" "MAY-MAR-180" "MAY-MAR-181" "MAY-MAR-182" "MAY-MAR-183" "MAY-MAR-184" "MAY-MAR-185" "MAY-MAR-186" "MAY-MAR-187" "MAY-MAR-188" "MAY-MAR-189" "MAY-MAR-190" "MAY-MAR-191" "MAY-MAR-192" "MAY-MAR-193" "MAY-MAR-194" "MAY-MAR-195" "MAY-MAR-196" "MAY-MAR-197" "MAY-MAR-198" "MAY-MAR-199" "MAY-MAR-200" "MAY-MAR-201" "MAY-MAR-202" "MAY-MAR-203" "MAY-MAR-204" "MAY-MAR-205" "MAY-MAR-206" "MAY-MAR-207" "MAY-MAR-208" "MAY-MAR-209" "MAY-MAR-210" "MAY-MAR-211" "MAY-MAR-212" "MAY-MAR-213" "MAY-MAR-214" "MAY-MAR-215" "MAY-MAR-216" "MAY-MAR-217" "MAY-MAR-218" "MAY-MAR-219" "MAY-MAR-220" "MAY-MAR-221" "MAY-MAR-222" "MAY-MAR-223" "MAY-MAR-224" "MAY-MAR-225" "MAY-MAR-226" "MAY-MAR-227" "MAY-MAR-228" "MAY-MAR-229" "MAY-MAR-230" "MAY-MAR-231" "MAY-MAR-232" "MAY-MAR-233" "MAY-MAR-234" "MAY-MAR-235" "MAY-MAR-236" "MAY-MAR-237" "MAY-MAR-238" "MAY-MAR-239" "MAY-MAR-240" "MAY-MAR-241" "MAY-MAR-242" "MAY-MAR-243" "MAY-MAR-244" "MAY-MAR-245" "MAY-MAR-246" "MAY-MAR-247" "MAY-MAR-248" "MAY-MAR-249" "MAY-MAR-250" "MAY-MAR-251" "MAY-MAR-252" "MAY-MAR-253" "MAY-MAR-254" "MAY-MAR-255" "MAY-MAR-256" "MAY-MAR-257" "MAY-MAR-258" "MAY-MAR-259" "MAY-MAR-260" "MAY-MAR-261" "MAY-MAR-262" "MAY-MAR-263" "MAY-MAR-264" "MAY-MAR-265" "MAY-MAR-266" "MAY-MAR-267" "MAY-MAR-268" "MAY-MAR-269" "MAY-MAR-270" "MAY-MAR-271" "MAY-MAR-272" "MAY-MAR-273" "MAY-MAR-274" "MAY-MAR-275" "MAY-MAR-276" "MAY-MAR-277" "MAY-MAR-278" "MAY-MAR-279" "MAY-MAR-280" "MAY-MAR-281" "MAY-MAR-282" "MAY-MAR-283" "MAY-MAR-284" "MAY-MAR-285" "MAY-MAR-286" "MAY-MAR-287" "MAY-MAR-288" "MAY-MAR-289" "MAY-MAR-290" "MAY-MAR-291" "MAY-MAR-292" "MAY-MAR-293" "MAY-MAR-294" "MAY-MAR-295" "MAY-MAR-296" "MAY-MAR-297" "MAY-MAR-298" "MAY-MAR-299" "MAY-MAR-300" "MAY-MAR-301" "MAY-MAR-302" "MAY-MAR-303" "MAY-MAR-304" "MAY-MAR-305" "MAY-MAR-306" "MAY-MAR-307" "MAY-MAR-308" "MAY-MAR-309" "MAY-MAR-310" "MAY-MAR-311" "MAY-MAR-312" "MAY-MAR-313" "MAY-MAR-314" "MAY-MAR-315" "MAY-MAR-316" "MAY-MAR-317" "MAY-MAR-318" "MAY-MAR-319" "MAY-MAR-320" "MAY-MAR-321" "MAY-MAR-322" "MAY-MAR-323" "MAY-MAR-324" "MAY-MAR-325" "MAY-MAR-326" "MAY-MAR-327" "MAY-MAR-328" "MAY-MAR-329" "MAY-MAR-330" "MAY-MAR-331" "MAY-MAR-332" "MAY-MAR-333" "MAY-MAR-334" "MAY-MAR-335" "MAY-MAR-336" "MAY-MAR-337" "MAY-MAR-338" "MAY-MAR-339" "MAY-MAR-340" "MAY-MAR-341" "MAY-MAR-342" "MAY-MAR-343" "MAY-MAR-344" "MAY-MAR-345" "MAY-MAR-346" "MAY-MAR-347" "MAY-MAR-348" "MAY-MAR-349" "MAY-MAR-350" "MAY-MAR-351" "MAY-MAR-352" "MAY-MAR-353" "MAY-MAR-354" "MAY-MAR-355" "MAY-MAR-356" "MAY-MAR-357" "MAY-MAR-358" "MAY-MAR-359" "MAY-MAR-360" "MAY-MAR-361" "MAY-MAR-362" "MAY-MAR-363" "MAY-MAR-364" "MAY-MAR-365" "MAY-MAR-366" "MAY-MAR-367" "MAY-MAR-368" "MAY-MAR-369" "MAY-MAR-370" "MAY-MAR-371" "MAY-MAR-372" "MAY-MAR-373" "MAY-MAR-374" "MAY-MAR-375" "MAY-MAR-376" "MAY-MAR-377" "MAY-MAR-378" "MAY-MAR-379" "MAY-MAR-380" "MAY-MAR-381" "MAY-MAR-382" "MAY-MAR-383" "MAY-MAR-384" "MAY-MAR-385" "MAY-MAR-386" "MAY-MAR-387" "MAY-MAR-388" "MAY-MAR-389" "MAY-MAR-390" "MAY-MAR-391" "MAY-MAR-392" "MAY-MAR-393" "MAY-MAR-394" "MAY-MAR-395" "MAY-MAR-396" "MAY-MAR-397" "MAY-MAR-398" "MAY-MAR-399" "MAY-MAR-400" "MAY-MAR-401" "MAY-MAR-402" "MAY-MAR-403" "MAY-MAR-404" "MAY-MAR-405" "MAY-MAR-406" "MAY-MAR-407" "MAY-MAR-408" "MAY-MAR-409" "MAY-MAR-410" "MAY-MAR-411" "MAY-MAR-412" "MAY-MAR-413" "MAY-MAR-414" "MAY-MAR-415" "MAY-MAR-416" "MAY-MAR-417" "MAY-MAR-418" "MAY-MAR-419" "MAY-MAR-420" "MAY-MAR-421" "MAY-MAR-422" "MAY-MAR-423" "MAY-MAR-424" "MAY-MAR-425" "MAY-MAR-426" "MAY-MAR-427" "MAY-MAR-428" "MAY-MAR-429" "MAY-MAR-430" "MAY-MAR-431" "MAY-MAR-432" "MAY-MAR-433" "MAY-MAR-434" "MAY-MAR-435" "MAY-MAR-436" "MAY-MAR-437" "MAY-MAR-438" "MAY-MAR-439" "MAY-MAR-440" "MAY-MAR-441" "MAY-MAR-442" "MAY-MAR-443" "MAY-MAR-444" "MAY-MAR-445" "MAY-MAR-446" "MAY-MAR-447" "MAY-MAR-448" "MAY-MAR-449" "MAY-MAR-450" "MAY-MAR-451" "MAY-MAR-452" "MAY-MAR-453" "MAY-MAR-454" "MAY-MAR-455" "MAY-MAR-456" "MAY-MAR-457" "MAY-MAR-458" "MAY-MAR-459" "MAY-MAR-460" "MAY-MAR-461" "MAY-MAR-462" "MAY-MAR-463" "MAY-MAR-464" "MAY-MAR-465" "MAY-MAR-466" "MAY-MAR-467" "MAY-MAR-468" "MAY-MAR-469" "MAY-MAR-470" "MAY-MAR-471" "MAY-MAR-472" "MAY-MAR-473" "MAY-MAR-474" "MAY-MAR-475" "MAY-MAR-476". Observation records are missing from d until this file is standardized.


carob_script <- function(path) {

"
Weed survey in different cropping systems in Mayotte island (2019-2020)

Study of weed communities composition and abundance in different cropping systems (tubers, orchards, pastures, food crops) of the Mayotte island. 2019-2020. Abundance scale notation 1-9. This dataset covers 476 plant surveys for 164 weed species.
"

	uri <- "doi:10.18167/DVN1/VUFFWE"
	group <- "pest_disease"
	ff <- carobiner::get_data(uri, path, group)

	meta <- carobiner::get_metadata(uri, path, group, major=1, minor=1,
		data_organization = "CIRAD",
		publication = NA,
		project = NA,
		design = "weed abundance survey",
		data_type = "survey",
		treatment_vars = "none",
		response_vars = "weed_species;ground_cover;weed_cover",
		notes = NA,
		carob_date = "2026-10-31",
		carob_LLM = "Cursor Grok 4.7",
		carob_completion = 55,
		carob_effort = 0.7
	)

	f <- ff[basename(ff) == "MAY-MAR-2020-DIV-AD.xlsx"]
	r1 <- carobiner::read.excel(f, sheet="Facteurs")
	r2 <- carobiner::read.excel(f, sheet="Floristique")

	get_row <- function(label) unname(unlist(r1[r1[[1]] == label, -1]))

	# MAY-MAR-2020-DIV-AD-DOC.txt: 1-9 note to % cover, same table as Benin 2013.
	rating_to_cover <- c(1, 7, 15, 30, 50, 70, 85, 93, 100)

	crop_map <- c(
		agrumes = "citrus", ail = "garlic", ambrevade = "pigeon pea",
		embrevade = "pigeon pea", ananas = "pineapple", aubergine = "eggplant",
		"aubergine dépôt d'herbe" = "eggplant", banane = "banana",
		bananier = "banana", bananiers = "banana", basilic = "basil",
		brachiaria = "brachiaria", canne = "sugarcane",
		"canne fourragère" = "forage crop", "cannes fourragères" = "forage crop",
		chou = "cabbage", choux = "cabbage", citron = "lemon", coco = "coconut",
		cocotier = "coconut", concombre = "cucumber", courge = "pumpkin",
		courgette = "zucchini", curcuma = "turmeric", curcumin = "turmeric",
		fourrage = "forage crop", gingembre = "ginger", goyaver = "guava",
		goyavier = "guava", grenade = "pomegranate",
		"ipomoea batatas" = "sweetpotato", "jachère" = "none", jachere = "none",
		"jachère inondée" = "none", laitue = "lettuce", salade = "lettuce",
		"maïs" = "maize", mais = "maize", manguier = "mango", manioc = "cassava",
		oignon = "onion", orange = "orange", oranger = "orange",
		"oranger manioc" = "orange_cassava", papaye = "papaya", papayer = "papaya",
		papayers = "papaya", papayes = "papaya", "pastèque" = "watermelon",
		"patate douce" = "sweetpotato", "patates douces" = "sweetpotato",
		piment = "chili pepper", riz = "rice", taro = "taro", tarot = "taro",
		tomate = "tomato", tomates = "tomato",
		"tomate bientôt replantées" = "tomato", vanille = "vanilla"
	)

	map_culture <- function(x) {
		out <- rep(NA_character_, length(x))
		for (i in seq_along(x)) {
			one <- trimws(as.character(x[i]))
			if (is.na(one) || !nzchar(one)) next
			parts <- unlist(strsplit(tolower(one), "\\s*[,;+]\\s*|\\s+et\\s+"))
			parts <- trimws(gsub("[()]", "", parts))
			parts <- parts[nzchar(parts)]
			parts <- parts[!grepl("^avant ", parts)]
			parts <- sub("^qq\\s+", "", parts)
			parts <- sub("^un peu\\s+", "", parts)
			parts <- sub("^et\\s+", "", parts)
			parts <- sub("\\s+mulch$", "", parts)
			parts <- sub("\\s+coup[eé]e?s?$", "", parts)
			parts <- trimws(parts)
			parts <- parts[nzchar(parts)]
			if (length(parts) == 0) {
				out[i] <- one
				next
			}
			mapped <- unname(crop_map[parts])
			if (any(is.na(mapped))) {
				out[i] <- one
			} else {
				out[i] <- paste(unique(mapped), collapse="_")
			}
		}
		out
	}

	src <- as.character(get_row("Culture_origine"))
	crop <- map_culture(src)
	# Date is an Excel serial; the year matches Année.
	obs <- as.numeric(get_row("Date"))
	irr <- tolower(trimws(as.character(get_row("Irrigation"))))
	enh <- as.integer(as.numeric(get_row("Enherbement")))
	d1b <- data.frame(
		trial_id = names(r1)[-1],
		crop = crop,
		date = as.character(as.Date(obs, origin="1899-12-30")),
		country = "Mayotte", # geographicCoverage; accepted country name
		location = trimws(as.character(get_row("Lieu"))),
		weed_cover = rating_to_cover[match(enh, 1:9)],
		irrigated = irr %in% c("inonde", "inondé"),
		stress = "weeds"
	)
	multi <- grepl("[,;+]| et ", tolower(trimws(src)))
	d1b$intercropped <- !is.na(d1b$crop) & (grepl("_", d1b$crop) | multi)
	d1b$on_farm <- TRUE
	d1b$is_survey <- TRUE
	d1b$geo_from_source <- FALSE

	present <- which(!is.na(r2[, -1]) & r2[, -1] > 0, arr.ind=TRUE)
	scores <- as.integer(as.numeric(r2[, -1][present]))
	d <- data.frame(
		trial_id = names(r2)[-1][present[, "col"]],
		weed_species = trimws(as.character(r2[[1]][present[, "row"]])),
		ground_cover = rating_to_cover[match(scores, 1:9)]
	)
	d <- merge(d, d1b, by="trial_id", all.x=TRUE)

	carobiner::write_files(path, meta, d)
}
