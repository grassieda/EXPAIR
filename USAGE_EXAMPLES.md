# Example Usage of consolidate_eplusout.py

This document provides examples of how to use the EnergyPlus output consolidation script.

## Basic Usage

```bash
python consolidate_eplusout.py /path/to/simulation/folders
```

## Example Scenario

Suppose you have the following directory structure:

```
simulation_results/
├── run_001/
│   └── eplusout.csv
├── run_002/
│   └── eplusout.csv
├── run_003/
│   └── eplusout.csv
└── run_004/
    └── eplusout.csv
```

To consolidate all the air contaminant concentration data:

```bash
python consolidate_eplusout.py simulation_results
```

## Expected Output

The script will:

1. **Print progress messages**:
   ```
   Found 4 subdirectories in 'simulation_results'
   Processing folders...
   
   ✓ Processed run_001: found 2 matching column(s)
   ✓ Processed run_002: found 1 matching column(s)
   run_003 csv missing!
   ✓ Processed run_004: found 1 matching column(s)
   
   ✓ Successfully created 'AllResults.csv' with 4 columns and 8760 rows
   ```

2. **Generate AllResults.csv** with columns named like:
   - `run_001_Zone Air Generic Air Contaminant Concentration [ppm](Hourly)`
   - `run_001_Zone Air Generic Air Contaminant Concentration [ppm](Hourly):Zone1`
   - `run_002_Zone Air Generic Air Contaminant Concentration [ppm](Hourly)`
   - etc.

## Error Handling

The script handles various scenarios:

- **Missing CSV file**: Prints `<folder_name> csv missing!` and continues
- **No matching columns**: Prints warning and continues
- **Invalid folder path**: Exits with error message
- **CSV parsing errors**: Prints error and continues with next folder

## Tips

- The script preserves all rows from the CSV files
- Columns are renamed to include the folder name for easy identification
- The output file `AllResults.csv` is created in the current working directory
- Process approximately 100-150 folders efficiently
