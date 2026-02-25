#!/usr/bin/env python3
"""
Script to consolidate EnergyPlus output files (eplusout.csv) from multiple folders.

This script:
1. Creates a single CSV file "AllResults.csv"
2. Accesses a folder containing multiple subfolders
3. For each subfolder, parses the eplusout.csv file
4. Extracts all columns with heading "Zone Air Generic Air Contaminant Concentration [ppm](Hourly)"
5. Appends these columns to AllResults.csv
6. Skips missing files and prints an error message

Usage:
    python consolidate_eplusout.py <path_to_folder_containing_subfolders>
"""

import os
import sys
import csv
import pandas as pd
from pathlib import Path


def main():
    """Main function to consolidate eplusout.csv files."""
    # Check command line arguments
    if len(sys.argv) != 2:
        print("Usage: python consolidate_eplusout.py <path_to_folder_containing_subfolders>")
        sys.exit(1)
    
    parent_folder = sys.argv[1]
    
    # Validate parent folder exists
    if not os.path.exists(parent_folder):
        print(f"Error: Folder '{parent_folder}' does not exist!")
        sys.exit(1)
    
    if not os.path.isdir(parent_folder):
        print(f"Error: '{parent_folder}' is not a directory!")
        sys.exit(1)
    
    # Initialize the results dataframe
    all_results = pd.DataFrame()
    
    # Column name to search for
    target_column_name = "Zone Air Generic Air Contaminant Concentration [ppm](Hourly)"
    
    # Get all subdirectories in the parent folder
    subdirs = [d for d in os.listdir(parent_folder) 
               if os.path.isdir(os.path.join(parent_folder, d))]
    
    print(f"Found {len(subdirs)} subdirectories in '{parent_folder}'")
    print(f"Processing folders...\n")
    
    # Process each subdirectory
    for folder_name in sorted(subdirs):
        folder_path = os.path.join(parent_folder, folder_name)
        csv_file_path = os.path.join(folder_path, "eplusout.csv")
        
        # Check if the CSV file exists
        if not os.path.exists(csv_file_path):
            print(f"{folder_name} csv missing!")
            continue
        
        try:
            # Read the CSV file
            df = pd.read_csv(csv_file_path)
            
            # Find all columns that contain the target string in their name
            matching_columns = [col for col in df.columns if target_column_name in col]
            
            if matching_columns:
                # Extract matching columns
                extracted_data = df[matching_columns].copy()
                
                # Rename columns to include folder name for identification
                column_mapping = {col: f"{folder_name}_{col}" for col in matching_columns}
                extracted_data.rename(columns=column_mapping, inplace=True)
                
                # Append to all_results
                if all_results.empty:
                    all_results = extracted_data
                else:
                    # Merge on index (row number)
                    all_results = pd.concat([all_results, extracted_data], axis=1)
                
                print(f"✓ Processed {folder_name}: found {len(matching_columns)} matching column(s)")
            else:
                print(f"⚠ {folder_name}: No columns matching '{target_column_name}' found")
                
        except Exception as e:
            print(f"✗ Error processing {folder_name}: {str(e)}")
            continue
    
    # Save the consolidated results
    if not all_results.empty:
        output_file = "AllResults.csv"
        all_results.to_csv(output_file, index=False)
        print(f"\n✓ Successfully created '{output_file}' with {len(all_results.columns)} columns and {len(all_results)} rows")
    else:
        print("\n⚠ No data was collected. AllResults.csv not created.")


if __name__ == "__main__":
    main()
