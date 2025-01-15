# Processing of the archives derived from ACS and Hydroscatt instruments

Two main functions are provided in the code. The first one (01_ACS_run.R) runs the ACS codes for organization and correction of Absorption and Attenuation coefficients measured by the WetLabs ACS sensor. Corrections provided are Temperature/Salinity, Proportional, Offset and Kirk. 
It is also possible to select the CTD that you want to use. Now, the available options are the CTD used in the ACS mount and the CastWay from AQUARELA Lab. For CastWAY CTD, salinity is calculated using the 'gsw_SP_from_C' function on GSW R package. It Converts from Electrical Conductivity to Practical Salinity


1) ACS Processing

For running the ACS codes, clone the repository, open in RStudio the project file (acs_hydroscat.Rproj), and open the script file "01_ACS_run.R"). Change the file paths for the desired field station. 
The files will be saved on the Outputs/campaign/ACS/station_name folder. 

2) Hydroscatt processing

The Hydroscatt processing implemented by now bin the Hydroscat data and converts from the .DAT format to a .CSV merged file. 


