This project contains code related to the paper titled: 3D habitat structure drives avian functional and trait diversity across North America

GENERAL INFORMATION
Name of files in order of their use:
1. 2017_NEON_LiDARDownload_UnifiedCode.R
2. LiDAR_DataProcessing_chapter1.R
3. 2017_DataCreation_LiDAR_SpRich.R (Out of date)
4. 3DFragmentationMetrics_DataLoad_chapter1.R (Out of date)
5. 3DMetrics_Chp1_REDO_4Bins_resolution.R (current)
6. 2017_Abundance_TraitDataPrep.R
7. DataPrep_MultiSpeciesDistanceModels.R
8. DistSampling_Full_2017NEON_supercomputer.R
9. ProcessDistanceSamplingOutputs.R
10. Export_SpRange_SubsetNEONDomainsByspRanges.R
11. FilterPointsByNEONAndSpRangeOverlap_20000DistSampleOutput.R
12. DistSamp_SpRichCalculations.R
13. PrepSpAbundanceDataWithTraits.R
14. SuperComp_gawdis_test.R
15. PD_DataPrep_MetricCalculation.R
16. NullModel_DataPrep_and_Calculations.R
17. Chp1_FinalDataPrep_updated.R
18. PCoA_Redo_July2023.R
19. Chapter1_FinalModels.R
20. Density_plots_PaperFigures.R

DATA
Data assoiciated with this project came from the following datasets:
1. National Ecological Observatory Network (NEON) shapefiles: https://www.neonscience.org/data-samples/data/spatial-data-maps 
2. LiDAR Data: NEON data product: DP1.30003.001
3. Breeding landbird point counts/Elevation/latitude: NEON data product DP1.10003.001
4. Trait data: AVONET (Tobias et al. 2022) and EltonTraits 1.0 databases (Wilman et al. 2014).
5. Temperature Range: Daymet (Thornton et al. 2022)
6. Species range maps: BirdLife International (BirdLife International 2022)
7. Phylogenetic Trees: Bird Tree (Jetz et al. 2012) - Ericson backbone

We used data from 385 avian plots from 38 NEON terrestrial sites across 17 of the 20 NEON Domains. 260 species were included across all avian plots. Data came from 2017. Plots were 250m in radius

Final data file: Chp1_FinalData_AllIndicies_UpdatedVoxel_4bins_repair_ShannonReplacementTry1_TotHorFrag_NEW_PCoA.csv
Column names of final data file:
1. plotID: NEON plotID
2. plotNum: plot number (unique to this project only)
3. domainID: NEON domain ID
4. siteID: NEON site ID
5. decimalLatitude: latitutude of plot centroid 
6. decimalLongitude: longitude of plot centroid 
7. Original_SpRich: Uncorrected SpRichness
8: rangefilt_95_SpRich: species richness after distance sampling, filtered by species range, and only containing species with >95% prob of occurance (as estimated by distance sampling model)
9. FEve: funcitonal evenness value
10. FDiv: funcitonal divergance value
11. original_FRic: funcitonal richness, not corrected for species richness
12. SES.FRic: standard effect size of functional richness using 100 random communities (corrected for species richness)
13. FRic.p: p-value of SES.FRic calculation 
14. original_PD: Faith's Phylogenetic Diversity, not corrected for species richness
15. SES.PD: standard effect size of Faith's Phylogenetic Diversity using 100 random communities (corrected for species richness)
16: PD.p: p-value of SES.PD calculation 
17. original_MPD: Mean Pairwise Distance, not corrected for species richness
18. SES.MPD: standard effect size of Mean Pairwise Distance using 100 random communities (corrected for species richness)
19. MPD.p: p-value of MPD.p calculation 
20. nlcdClass: NLCD landcover classification of avian plot centroid (as reported by NEON)
21. elevation: elevation of avian plot centroid (as reported by NEON)
22. temp.range_CHELSA: temperature range (i.e., min-max) of the NEON avian survey period (May-June) during 2017 (NOT USED)
23. temp.ave_CHELSA: average temperature of the NEON avian survey period (May-June) during 2017 (NOT USED)
24. precip.annual_CHELSA: total annual precipitation during 2017 (NOT USED)
25. temp.range_daymet: temperature range (i.e., min-max) of the NEON avian survey period (May-June) during 2017
26. temp.ave_daymet: average temperature of the NEON avian survey period (May-June) during 2017 (NOT USED)
27. precip.total_daymet: total annual precipitation during 2017 (NOT USED)
28. Vert.Variance: OUT OF DATE METRIC (Kept for posterity)
29. Vert.Shannon: OUT OF DATE METRIC (Kept for posterity)
30. Vert.Simpson: OUT OF DATE METRIC (Kept for posterity)
31. Vert.Mean: OUT OF DATE METRIC (Kept for posterity)
32. volume_und: volume of understory (0-5m)
33. volume_mid: volume of midstory (5-15m)
34. volume_subcan: volume of subcan (15-25m)
35. volume_can: volume of canopy (>25m)
36. clumpy_und: clumpiness of understory horizontal raster (not used)
37. clumpy_mid:  clumpiness of midstory horizontal raster (not used)
38. clumpy_subcan:  clumpiness of subcanopy horizontal raster (not used)
39. clumpy_can:  clumpiness of canopy horizontal raster (not used)
40. te_und:  total edge of understory horizontal raster (not used)
41. te_mid:  total edge of understory horizontal raster (not used)
42. te_subcan:  total edge of subcanopy horizontal raster (not used)
43. te_can:  total edge of canopy horizontal raster (not used)
44. np_und:  number of patches of understory horizontal raster
45. np_mid:  number of patches of midstory horizontal raster
46. np_subcan: number of patches of subcanopy horizontal raster
47. np_can: number of patches of canopy horizontal raster
48. Total_veg_amt_NEW: sum of all vegetation amount across all 4 vertical strata
49. shannon_plot: vertical heterogeneity metric (not used)
50. simpson_plot: vertical heterogeneity metric (not used)
51. variance_plot: vertical heterogeneity metric (CURRENT)
52. range_abs: custom vertical heterogeneity metric (not used)
53. dist_traveled: custom vertical heterogeneity metric (not used)
54. clumpy_TotVol: clumpiness of total volume raster (not used)
55. np_TotVol: number of patches of total volume raster (2D configuration)
56. te_TotVol: total edge of total volume raster (not used)
57. PC1: principle component axes 1 value
58. PC2: principle component axes 2 value
59. PC3: principle component axes 3 value
60. PC4: principle component axes 4 value
61. VegRating: vegitation rating of each plot (understory, midstory, subcanopy, canopy), used for plotting

METHODOLOGICAL INFORMATION

