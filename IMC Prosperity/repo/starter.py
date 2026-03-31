import pandas as pd
import os

def load_tutorial_data():
    """
    Reads the starter CSV files provided by IMC Prosperity.
    The files are semicolon-separated (;).
    """
    # Define the path to the tutorial data relative to this script
    base_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    data_dir = os.path.join(base_dir, "TUTORIAL_ROUND_1")
    
    print(f"Looking for data in: {data_dir}...")
    
    # Load prices (using semicolon separator)
    prices_day_minus_1 = pd.read_csv(os.path.join(data_dir, "prices_round_0_day_-1.csv"), sep=";")
    prices_day_minus_2 = pd.read_csv(os.path.join(data_dir, "prices_round_0_day_-2.csv"), sep=";")
    
    # Load trades (using semicolon separator)
    trades_day_minus_1 = pd.read_csv(os.path.join(data_dir, "trades_round_0_day_-1.csv"), sep=";")
    trades_day_minus_2 = pd.read_csv(os.path.join(data_dir, "trades_round_0_day_-2.csv"), sep=";")
    
    # Combine the dataframes for easier analysis
    prices = pd.concat([prices_day_minus_2, prices_day_minus_1], ignore_index=True)
    trades = pd.concat([trades_day_minus_2, trades_day_minus_1], ignore_index=True)
    
    return prices, trades

if __name__ == "__main__":
    print("Loading IMC Prosperity Tutorial Data...")
    prices_df, trades_df = load_tutorial_data()
    
    print("\n--- Prices Data Preview ---")
    print(prices_df.head())
    
    print("\n--- Trades Data Preview ---")
    print(trades_df.head())
    
    print(f"\nTotal price records: {len(prices_df)}")
    print(f"Total trade records: {len(trades_df)}")
    
    # Let's see what products we are trading!
    print(f"\nProducts available: {prices_df['product'].unique()}")
