# Global urban temporal trends in fine particulate matter and attributable health burdens.

Contact for this repository: Veronica Southerland, vtinney@gwu.edu

## Preprocessing files
* 00.agefractions.R - create age fractions through back calculation of GBD data + formatting baseline disease rates                    

#### The RR estimate creation often breaks the code due to lack of HPC memory so here they are parsed out into 3 separate codes

#### CVD RR estimates created from MRBRT GBD estimates
* 00.create.RR.batch.cvd.1.R              
* 00.create.RR.batch.cvd.2.R              
* 00.create.RR.batch.cvd.3.R              

#### All other endpoints RR created from MRBRT GBD estimates
* 00.create.RR.batch1.R                   
* 00.create.RR.batch2.R                   
* 00.create.RR.batch3.R                   
* 00.create.RR.R                          

* 00.format.urban.extents.R - format the GHS-SMOD dataset to match other input files                                     
* 00.rpg.PM.txt - reproject Hammer to match extent of population data                          
* 00.rpg.sensitivity.analysis.txt - reproject GBD for sensitivity analyses
* hammer warp.txt - updated Hammer reprojection 
        
## Population-weighted city-level concentrations
* 01.popw.expsum.GBD.2.R                 
* 01.popw.expsum.GBD.3.R                
* 01.popw.expsum.GBD.R = create city-level population-weighted concentrations for GBD inputs at 10km                 
* 01.popw.expsum.HR.R - create city-level population-weighted concentrations for main analysis at 1km   

## Merge concentrations with baseline disease rates and GBD data
* 02.merge.regions.expsum.GBD.2.R         
* 02.merge.regions.expsum.GBD.R - merge concentrations with baseline disease rates and GBD data for GBD analysis at 10km           
* 02.merge.regions.expsum.HR.R - merge concentrations with baseline disease rates and GBD data for main analysis at 1km    

## HIA code     
* 03.combined.allages.HIA.D.R             
* 03.combined.allages.HIA.GBD.2.R         
* 03.combined.allages.HIA.GBD.R          
* 03.combined.allages.HIA.HR.R            
* 03.combined.allages.HIA.HR.uncertainty.R        
* 03.cvd.stroke.HIA.D.R                 
* 03.cvd.stroke.HIA.HR.R                  
* 03.cvd.stroke.HIA.HR.uncertainty.R     
* 03.cvd.stroke.rb.HIA.GBD.2.R        
* 03.cvd.stroke.rb.HIA.GBD.R  

## Merge results
* 04.merge.dataframes.D.R - merge results across endpoints for the parameter contribution analysis             
* 04.merge.dataframes.GBD.R - merge results across endpoints for 10km GBD analysis         
* 04.merge.dataframes.HR.R - merge results across endpoints for main analysis at 1km  
* 04.merge.dataframes.HR.uncertainty.R - merge results across endpoints for main analysis at 1km + uncertainty estimates

## Post-processing
* 06.preparesupplemental.R - prepare supplemental file        
* 06.summarystats.GBD.R - summary statistics for results using GBD 10km input files                 
* 06.summarystats.HR.R - summary statistics for main results using 1km input files                
* 06.summarystats.HR.uncertainty.R - summary statistics for main results using 1km input files + uncertainty estimates

## Sensitivity analyses
* 07.compare.HR.GBD.2.R               
* 07.compare.HR.GBD.R                
* 07.parameter.contribution.R - calculates parameter contributions to the HIA          
* 08.comparison.SA.R - compares sensitivity analysis results                  
                      
