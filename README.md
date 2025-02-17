# SubalpineTopoGrowth

This is the repository for the MRS Subalpine tree growth across topoclimatic gradients chapter.

### Scripts ####################################################################

# The following scripts are called or are not needed to be completed in any particular order.
- libraries
- functions: for processing dataset creation and competition index.
- shapefile_manipulation: Script for preparing MRS stem map shapefiles for dataset creation.
- edge_buffer: for processing edge buffers
- individual_shapefile_creation: Script for creating seperate shapefiles by plot
- soil_air_moisture_temp_prep: For preparing plot sensor data collected at Niwot.

# Workflow to reproduce data - Run in order of S1, S2, S3....
- S1_growth_dataset_creation: Script for preparing dataset for competition index.
- S2_competition_index: For developing Hegyi's indices for each tree
- S3_plot_specific_longformat_prep: For modifying and organizing data in longformat. Includes entering plot specific covariates.


### Data Folder ################################################################
# air_temp: 
  - airtemp_rh_microm.ra.data.csv: CSV data of air temp and relative humidity per plot at NWT

# comp:
  - growth_comp.RData: dataframe of mrs plots with competition indices for each tree. Needed for long format prep.
  
# growth_initial_prep:
  - data_growth.RData: Dataset created from initial plot prep
  - three_census_growth.RData: depricated.
  - two_census_growth.RData: depricated.
  
growth_long:
  - subalpine_non_spatial.RData: Resulting long format dataset without geometry.
  - subalpine.RData: resulting long format dataset with geometry.
  
growth_long
  perm_plots_all.csv: Raw csv data from field data collection
  plot_upper.csv: raw csv data from field data collection for MRS 11, 12, 13
  
shapefiles_complete:
  shapefiles for each plot with data in longformat. Under construction.
  
shapefiles_raw:
  Stem maps and spatial data for each plot.

soils:
  soilmoisture_biweekly_micromet.ra.data.csv: soil moisture
  soilmoisture_continuous_micromet.ra.data.csv
  soiltemp_micromet.ra.data.csv: temperature
  

### Long format growth data structure ##########################################

Dataset Overview

The long format dataset was derived from the subalpine dataset by subsetting for the plot "MRS13". Several variables were added, transformed, or calculated to provide a detailed ecological snapshot of tree growth, competition, and site conditions within this plot. The data focuses primarily on basal area growth and competition indices across multiple censuses, with a final transformation into a long-format dataset for analysis.
Location and Environmental Context

    Plot: Name
    Elevation: m
    Slope: °
    Aspect: °
    Moisture Class: Categorical: Xeric, Mesic, Hydric
    Soil Moisture: %
    Soil Temperature: °C
    Air Temperature: °C
    Relative Humidity: %

Dataset Structure

The dataset underwent several transformations:

    Height class variables (hc1, hc3, hc4) were recoded into descriptive categories.
    Basal areas were calculated for different years.
    Competition indices were rounded to two decimal places.
    Relative growth rates (RGR) were computed using diameter data.
    Mortality status was recoded for clarity.
    Final dataset was transformed into long format with corresponding competition, RGR, and        height class.

Variables:

Column	(Type)	Description

id	(Factor)	Unique identifier for each tree.

Plot	(Factor)	Plot name; all entries are "MRS13".

Spec	(Factor)	Tree species code.

elevation	(Numeric)	Elevation of the plot (meters).

slope	(Numeric)	Slope of the plot (degrees).

aspect	(Numeric)	Aspect of the plot (degrees, 0 = North).

soil_moisture	(Numeric)	Soil moisture percentage.

soil_temperature	(Numeric)	Soil temperature in °C.

air_temperature	(Numeric)	Air temperature in °C.

relative_humidity	(Numeric)	Relative humidity percentage.

moisture_class	(Factor)	Categorical description of moisture availability (e.g., "Xeric").

basal_area_1982	(Numeric)	Basal area calculated from dbh1 (cm²).

basal_area_2016	(Numeric)	Basal area calculated from dbh3 (cm²).

basal_area_2022	(Numeric)	Basal area calculated from dbh4 (cm²).

dead	(Numeric)	Binary indicator of tree mortality (1 = dead, 0 = alive).

dead_census	(Factor)	Census period during which the tree died (or "Alive").

competition	(Numeric)	Tree competition index (rounded).

comp_census	(Factor)	Census period for competition data (ci_1, ci_2, ci_3).

RGR	(Numeric)	Relative growth rate for basal area.

rgr_census	(Factor)	Census period for RGR data (RGR_1, RGR_2, RGR_3).

height	(Factor)	Ordinal height class: "Suppressed", "Co-Dominant", "Dominant".

Census	(Factor)	Census year; only Census 3 is retained in the final dataset.



Data Transformations and Calculations

    Basal Area Calculation:
    Basal Area (cm²)=(DBH (cm)2)×0.005454
    Basal Area (cm²)=(DBH (cm)2)×0.005454

    Relative Growth Rate (RGR) Calculation:
    RGR=ln⁡(π×(dbh4/2)2)−ln⁡(π×(dbh3/2)2)6
    RGR=6ln(π×(dbh4/2)2)−ln(π×(dbh3/2)2)​

    Height Class Recoding:
        1 → "Dominant"
        2 → "Co-Dominant"
        3 → "Suppressed"

    Dead Census Recoding:
        0 → "Alive"
        1–9 → "After Census 1"
        10–12 → "After Census 2"

    Long Format Transformation:
    The dataset was pivoted into a long format for competition indices, RGR, and height class.

Dataset Subsets

    mrs13_comp: Long format dataset with competition indices.
    mrs13_rgr: Long format dataset with RGR values.
    mrs13_hc: Long format dataset with height classes.
    mrs13_long: Integrated long-format dataset combining all of the above.

Notes

    The final dataset retains only records from Census 3.
    Factor levels were set for consistency and interpretability.
    Some RGR values (e.g., RGR_1 and RGR_2) are missing (NA).
    The competition index is continuous and derived from local tree density and basal area.
