import argparse
import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Function to calculate proportional deck preference per trial
def calculate_deck_preference(dataframe, deck_column="deck"):
    deck_preference = dataframe.groupby(["trial", deck_column]).size().unstack(fill_value=0)
    deck_preference = deck_preference.div(deck_preference.sum(axis=1), axis=0)
    return deck_preference

# Function to calculate mean deck preference
def calculate_mean_deck_preference(dataframe, deck_column="deck"):
    deck_preference = dataframe.groupby(["trial", deck_column]).size().unstack(fill_value=0)
    deck_preference = deck_preference.div(deck_preference.sum(axis=1), axis=0)
    return deck_preference.mean()

# Function to process dataframes
def process_dataframe(df, deck_mapping):
    df[['data_set', 'population', 'information_level']] = df['trial_type'].str.split('-', expand=True)
    df['deckN'] = df['deck'].map(deck_mapping)
    df['subjID'] = df['agentN'].str.strip('Agent_')
    df.drop('agentN', axis=1, inplace=True)
    return df

# Function to plot deck preferences
def plot_deck_preferences(preferences, title, output_path, colors):
    fig, ax = plt.subplots(figsize=(12, 6))
    fig.suptitle(title, fontsize=18, fontweight="bold")

    for deck_name, color in colors.items():
        y_values = preferences[deck_name]
        x_values = y_values.index

        y_values.plot(
            ax=ax,
            kind="line",
            linewidth=2,
            color=color,
            label=deck_name
        )

    ax.legend(title="Deck")
    plt.savefig(output_path)
    plt.close()

# Main function
def main():
    parser = argparse.ArgumentParser(description="Process and plot LLM simulated data.")
    parser.add_argument("--data_dir", type=str, required=True, help="Directory containing the data files.")
    parser.add_argument("--output_dir", type=str, required=True, help="Directory to save processed files and plots.")
    args = parser.parse_args()

    data_dir = args.data_dir
    output_dir = args.output_dir

    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    data_out_dir = os.path.join(output_dir, "LLM_simulated_data")
    plots_out_dir = os.path.join(output_dir, "plots", "LLM_simulated")

    os.makedirs(data_out_dir, exist_ok=True)
    os.makedirs(plots_out_dir, exist_ok=True)

    # Load data
    file_paths = {
        "ahn_control": os.path.join(data_dir, 'Ahn_data_healthy_filtered.csv'),
        "ahn_addict": os.path.join(data_dir, 'Ahn_data_heroin_filtered.csv'),
        "wetzels_control": os.path.join(data_dir, 'Wetzel_data_healthy.csv')
    }

    simulated_files = [
        "results_ahn-addict-high.csv", "results_ahn-addict-low.csv",
        "results_ahn-control-high.csv", "results_ahn-control-low.csv",
        "results_wetzels-addict-high.csv", "results_wetzels-addict-low.csv",
        "results_wetzels-control-high.csv", "results_wetzels-control-low.csv"
    ]

    sim_dataframes = [
        pd.read_csv(os.path.join(data_dir, sim_file)) for sim_file in simulated_files
    ]

    # Process simulated data
    deck_mapping = {'A': 1, 'B': 2, 'C': 3, 'D': 4}
    for df in sim_dataframes:
        process_dataframe(df, deck_mapping)

    # Save processed data
    for df, sim_file in zip(sim_dataframes, simulated_files):
        output_file = os.path.join(data_out_dir, f"processed_{sim_file}")
        df.to_csv(output_file, index=False)

    # Calculate preferences and plot
    deck_colors = {
        "A": "#b10000",  # Dark red
        "B": "#ff0000",  # Light red
        "C": "#089000",  # Dark green
        "D": "#0eff00"   # Light green
    }

    for df, sim_file in zip(sim_dataframes, simulated_files):
        deck_preferences = calculate_deck_preference(df)
        plot_title = f"Deck Preferences for {sim_file.split('.')[0]}"
        plot_file = os.path.join(plots_out_dir, f"{sim_file.split('.')[0]}_preferences.png")
        plot_deck_preferences(deck_preferences, plot_title, plot_file, deck_colors)

if __name__ == "__main__":
    main()
