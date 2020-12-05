# Global urban temporal trends in fine particulate matter and attributable health burdens.

The objective of this analysis is to describe temporal trends (2000 to 2018) in population weighted fine particulate matter (PM2.5) and PM2.5-attributable health burdens for ~13,000 global urban centers. 

The following provides the code needed to replicate this analysis with input files provided in the sub-repositories.

All code run in R/3.5.3 and Gdal

Sources:
 Urban extents: https://ghsl.jrc.ec.europa.eu/ghs_smod2019.php
 PM2.5 datasets: Global Estimates (V4.GL.03 / V4.GL.03.NoGWR) http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
 Population datasets: https://www.worldpop.org/geodata/listing?id=64
 Global burden of disease baseline disease rates: http://ghdx.healthdata.org/gbd-results-tool

## Pre-processing
 * 00.agefractions.R -
   Formats country age fractions and baseline disease rates.
   
 * 00.format.urban.extents.R -
   Formats urban GHS-SMOD extents.
   
 * 00.rpg.PM -
   Using command line and gdal, downloads, crops, and warps Hammer et al. 2020 PM2.5 to match urban raster file extent.

## Health impact assessment
 * 01.popw.expsum.HR -
   Creates a population weighted concentration for each urban area using gridded PM2.5 and population datasets.

 * 02.merge.regions.expsum.HR -
   Merges population weighted concentrations with GBD countries and WHO region definitions.

 * 03.combined.allages.HIA.HR -
   Health burden estimates for: lower respiratory infections, lung cancer, COPD and type 2 diabetes.

 * 03.cvd.stroke.HIA.HR -
   Health burden estimates for: stroke and ischemic heart disease.

## Post processing


## Sensitivity analysis
