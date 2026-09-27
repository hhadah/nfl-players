#!/usr/bin/env python3
"""
Script to load RAS (Relative Athletic Score) data for the NFL players dataset.
RAS is published by Kent Lee Platte at https://ras.football and provides 
athletic scores for players from 1987-present on a 0-10 percentile scale.
"""

import os
import sys
import requests
import pandas as pd
from pathlib import Path
import duckdb

# Add project root to path so we can import config
sys.path.append(str(Path(__file__).parent.parent))

from scripts.config import RAW_DIR, DB_PATH

def download_ras_data():
    """
    Download RAS data from ras.football.
    Note: The exact URL for direct CSV download is not publicly documented.
    This script will need to be adapted once the actual data source is identified.
    """
    print("Attempting to download RAS data...")
    
    # For now, create a placeholder with example structure
    # In practice, this would be updated with actual web scraping or API call
    ras_data = {
        'player_name': ['John Smith', 'Jane Doe'],
        'season': [2015, 2016],
        'ras_score': [7.2, 8.1],
        'ras_rank': [1200, 800]
    }
    
    df = pd.DataFrame(ras_data)
    print(f"Created placeholder RAS DataFrame with {len(df)} rows")
    return df

def create_ras_table():
    """
    Create RAS table in the database.
    Based on EXTENDING.md, RAS should be joinable to nfl_combine on player name.
    """
    print("Creating RAS table in database...")
    
    con = duckdb.connect(str(DB_PATH))
    
    # Create RAS table structure
    ras_schema = """
    CREATE TABLE IF NOT EXISTS ras_scores (
        player_name VARCHAR,
        season INTEGER,
        ras_score DOUBLE,
        ras_rank INTEGER,
        PRIMARY KEY (player_name, season)
    );
    """
    
    con.execute(ras_schema)
    print("RAS table created successfully")
    con.close()

def join_ras_with_combine():
    """
    Join RAS data with nfl_combine table to enhance player data.
    This would be done once both tables exist in the database.
    """
    print("Joining RAS data with nfl_combine...")
    
    try:
        con = duckdb.connect(str(DB_PATH))
        
        # Check if both tables exist
        tables = con.execute("SHOW TABLES").fetchall()
        table_names = [t[0] for t in tables]
        
        if 'ras_scores' not in table_names:
            print("RAS table not found. Run the RAS import script first.")
            return
            
        if 'nfl_combine' not in table_names:
            print("nfl_combine table not found.")
            return
            
        # Create a joined view or table with RAS data added to combine data
        join_query = """
        CREATE TABLE IF NOT EXISTS nfl_combine_with_ras AS
        SELECT 
            c.*,
            r.ras_score,
            r.ras_rank
        FROM nfl_combine c
        LEFT JOIN ras_scores r ON c.player_name = r.player_name AND c.season = r.season
        """
        
        con.execute(join_query)
        print("Successfully created nfl_combine_with_ras table with RAS data")
        
        con.close()
        
    except Exception as e:
        print(f"Error joining tables: {e}")

def main():
    """Main function to process RAS data."""
    print("Starting RAS data import...")
    
    # Create the RAS table
    create_ras_table()
    
    # Download RAS data (placeholder implementation)
    ras_df = download_ras_data()
    
    if ras_df.empty:
        print("No data downloaded.")
        return
    
    # Save to database
    con = duckdb.connect(str(DB_PATH))
    ras_df.to_sql('ras_scores', con, if_exists='append', index=False)
    con.close()

    print(f"Successfully imported {len(ras_df)} RAS records")

    # Join with combine data (if both exist)
    join_ras_with_combine()

if __name__ == "__main__":
    main()