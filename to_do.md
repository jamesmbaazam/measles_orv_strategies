# To do
1. Make a design diagram (workflow) for the model and simulation structure
   
2. Reflow *Epidemics7* simulation workflow as follows:
   - Define strategies as a dataframe instead of a list of data frames. 
   - Dynamically generate strategy names instead of hardcoding them
   - Define a single instance of the simulation scenarios: *team_equip_type*
   - Create a separate script for running and saving supply chain analyses per strategy and scenario (equipment type and team dispatch rules) as `.rds` 
   - Create a separate script that loads the supply chain output as input for running the transmission dynamics scenarios
   - Save transmission dynamics resutls as `.rds`
   - Generate each figure with a separate script. Each script should individually load and transform the relevant simulation outputs
   - Reduce verbose subsetting in analysis scripts:
     - `tidyverse` grouping and subsetting? 
     - `data.table`? 
     -  employ the `with()` function?
   