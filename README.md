# LLM Simulation for Behavioral Modeling

This code contains content for a project evaluating LLMs ability to behave like human beings, and hard-to-get populations. Code covers LLM data simulation for the Iowa Gambling Task and cognitive modeling of data from control and addicts.

## File Structure

```
project/
├── .venv/                              # Virtual environment for Python dependencies
├── in/                                 # Placeholder for input files (e.g., configurations or data)
├── out/                                # Output folder for simulation results
│   ├── results.csv                     # Output file containing simulation results
├── src/                                # Source code folder
│   ├── llm_data_simulation/            # Simulation-related scripts and resources
│   │   ├── payoffs.py                  # Python script for generating payoff structures
│   │   ├── simulating_responses.py     # Python script for running simulations
│   ├── modeling/                       # Placeholder for modeling scripts
│   ├── .gitkeep                        # Empty file to track the directory in version control
├── utils/                              # Utility folder for additional resources
│   ├── task_descriptions.txt           # Text file with task descriptions
├── .env                                # Environment variables (e.g., API keys)
├── .gitignore                          # Git ignore file to exclude files/folders from version control
├── README.md                           # Documentation for the project
├── requirements.txt                    # List of Python dependencies

```

## Prerequisites

- **Python 3.7 or later**
- Required Python packages:
  - `openai`
  - `pandas`
  - `argparse`
  - `huggingface_hub`
  - `dotenv`
- A Hugging Face API key for inference.

## Setup

1. Clone this repository to your local machine.
2. Install the required dependencies:
   ```bash
   pip install -r requirements.txt
   ```
3. Create a `.env` file in the root directory and add your Hugging Face API key:
   ```
   hugging_face_api=<YOUR_HUGGING_FACE_API_KEY>
   ```

4. Ensure the `task_descriptions.txt` file in `utils/` contains the task instructions, formatted like this:
   ```
   [control-high]
   You are playing a card game where your goal is to maximize your winnings.
   ...
   ...

   [control-low]
   You are playing a card game where your goal is to maximize your winnings.
   ...
   ```

5. (Optional) Modify the `payoffs.py` file to customize the payoff structures.

## Running the Simulation

Run the simulation with the following command:
```bash
python main.py --task_type <TASK_TYPE> --payoff <PAYOFF STRUCTURE> --n_trials <TRIALS> --n_agents <AGENTS>
```

### Arguments

- `--api_key`: Hugging Face API key (defaults to value in `.env` file).
- `--payoff`: The payoff structure to simulate. Choices are `ahn` or `wetzels`.
- `--n_trials`: Number of trials per agent (default: 10).
- `--task_type`: The task type to simulate. Choices are:
  - `control-high`
  - `control-low`
  - `addict-high`
  - `addict-low`
- `--n_agents`: Number of agents to simulate (default: 2).

### Example Command

```bash
python main.py --task_type control-high --payoff ahn --n_trials 20 --n_agents 5
```

### Output

Results are saved in the `out/` folder as a CSV file (`results_{args.task_type}_{args.payoff}_{args.n_trials}_{args.n_agents}.csv`). Example output structure:

| trial | deck | gain | loss | net  | agentN    | agentN      | 
|-------|------|------|------|------|-----------|-------------|
| 1     | A    | 100  | -250 | -150 | Agent_1   | control-low |
| 2     | B    | 50   | -50  | 0    | Agent_1   | control-low |
| ...   | ...  | ...  | ...  | ...  | ...       | ...         |


## Modeling Pipeline Template for JAGS

