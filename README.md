# EXPAIR
EXPAIR model as part of INDAIR-EXPAIR framework used for the INHABIT project

## EnergyPlus Output Consolidation Script

### Overview
This repository includes a Python script (`consolidate_eplusout.py`) to consolidate EnergyPlus simulation outputs from multiple folders into a single CSV file.

### Features
- Processes multiple folders containing EnergyPlus output files (`eplusout.csv`)
- Extracts columns with "Zone Air Generic Air Contaminant Concentration [ppm](Hourly)" heading
- Consolidates data into a single `AllResults.csv` file
- Handles missing files gracefully with informative error messages
- Renames columns to include folder names for easy identification

### Requirements
Install the required dependencies:
```bash
pip install -r requirements.txt
```

### Usage
```bash
python consolidate_eplusout.py <path_to_folder_containing_subfolders>
```

**Example:**
```bash
python consolidate_eplusout.py ./simulation_results
```

This will:
1. Scan all subfolders in `./simulation_results`
2. Look for `eplusout.csv` in each subfolder
3. Extract matching columns
4. Create `AllResults.csv` in the current directory

### Output
- **AllResults.csv**: Contains all extracted columns from all processed folders
- Console output showing processing status for each folder
