import os
import pandas as pd
import argparse
from huggingface_hub import InferenceClient
from dotenv import load_dotenv
from datetime import datetime
from tqdm import tqdm

# Load .env file
load_dotenv()

# Import functions from payoffs.py 
from payoffs import payoff_ahn, payoff_wetzels

# Function to parse input arguments
def input_parse():
    parser = argparse.ArgumentParser(description='Run the GPT simulation')
    
    parser.add_argument('--api_key', type=str, help='Hugging Face API key', default=os.getenv('hugging_face_api'))
    parser.add_argument('--n_trials', type=int, help='Number of trials', default=10)
    parser.add_argument(
        '--task_type',
        type=str,
        help='Task to simulate',
        choices=[
            'ahn-control-high', 'ahn-control-low', 
            'ahn-addict-high', 'ahn-addict-low',
            'wetzels-control-high', 'wetzels-control-low',
            'wetzels-addict-high', 'wetzels-addict-low']
    )
    parser.add_argument('--n_agents', type=int, help='Number of agents to simulate', default=2)
    return parser.parse_args()

# Generate initial task instruction
def task_description(task_type):
    """
    Returns the task description for the given task_type from a text file.

    Args:
        task_type (str): The type of task to retrieve the description for.

    Returns:
        dict: A dictionary with the role as "user" and the content containing the task description.
    """
    file_path = os.path.join('utils', 'task_descriptions.txt')

    if not os.path.exists(file_path):
        raise FileNotFoundError(f"The file {file_path} does not exist.")

    with open(file_path, 'r') as f:
        content = f.read()

    sections = content.split('[')
    for section in sections:
        if section.startswith(task_type + ']'):
            description = section.split(']', 1)[1].strip()
            return [{"role": "system", "content": description}]

    raise ValueError(f"Task type '{task_type}' not found in the file.")

# Determine the payoff structure based on task type
def determine_payoff(task_type, n_trials):
    if task_type.startswith('ahn'):
        return payoff_ahn(n_trials)
    elif task_type.startswith('wetzels'):
        return payoff_wetzels(n_trials)
    else:
        raise ValueError(f"Invalid task type specified: {task_type}")

# Generate feedback string
def generate_feedback(win, loss, net, task_type):
    if 'wetzels' in task_type:
        return f"Loss: {loss}\nWin: {win}\nNet: {net}"
    else:
        return f"Win: {win}\nLoss: {loss}\nNet: {net}"

# Main function to simulate responses
def main():
    args = input_parse()

    # Load API key
    load_dotenv()
    client = InferenceClient(api_key=os.getenv("hugging_face_api"))

    # Define output folder and file
    output_folder = os.path.join(os.getcwd(), "out", "LLM_simulated_data")
    os.makedirs(output_folder, exist_ok=True)  # Ensure the folder exists
    date = datetime.now().strftime("%Y-%m-%d_%H-%M-%S")
    output_file = os.path.join(output_folder, f"results_{args.task_type}_{args.n_trials}_{args.n_agents}_{date}.csv")

    # Create and write CSV header
    results_df = pd.DataFrame(columns=["trial", "deck", "gain", "loss", "net", "trial_type", "agentN", "date"])
    results_df.to_csv(output_file, index=False, mode='w')  # Write header to CSV

    # Game loop for multiple agents
    for agent in tqdm(range(args.n_agents), desc="Simulating Agents"):
        agent_id = f"Agent_{agent + 1}"
        messages = task_description(args.task_type)

        # Assign a unique payoff structure to each agent
        payoff = determine_payoff(args.task_type, args.n_trials)

        # Progress bar for trials
        for trial in tqdm(range(args.n_trials), desc=f"Agent {agent_id} Trials", leave=False):
            while True:  # Repeat until valid input is given
                # Get AI's response
                stream = client.chat.completions.create(
                    model="meta-llama/Llama-3.3-70B-Instruct", 
                    messages=messages,
                    max_tokens=1,
                    stream=True
                )

                assistant_response = "".join(chunk.choices[0].delta.content for chunk in stream).strip()
                deck = assistant_response

                # Validate response
                if deck in ['A', 'B', 'C', 'D']:
                    break  # Exit loop if valid response
                else:
                    # Add error message to the conversation
                    messages.append({"role": "system", "content": "Only respond with a letter (A, B, C, or D)."})
                    continue  # Ask again for a valid response

            # Determine payoff based on AI's choice
            deck_index = ord(deck) - ord('A')
            win_key = f"win{deck_index + 1}"
            loss_key = f"loss{deck_index + 1}"

            win = payoff.loc[trial, win_key]
            loss = payoff.loc[trial, loss_key]
            net = win + loss

            feedback = generate_feedback(win, loss, net, args.task_type)

            # Prepare the new row
            new_row = {
                "trial": trial + 1,
                "deck": deck,
                "gain": win,
                "loss": loss,
                "net": net,
                "trial_type": args.task_type,
                "agentN": agent_id,
                "date": datetime.now().strftime("%Y-%m-%d %H:%M:%S")
            }

            # Append the new row to the CSV file
            pd.DataFrame([new_row]).to_csv(output_file, index=False, header=False, mode='a')

            # Add AI's choice and feedback to the conversation
            messages.append({"role": "assistant", "content": deck})
            messages.append({"role": "user", "content": feedback})

    print(f"Simulation complete. Results saved to '{output_file}'.")


if __name__ == '__main__':
    main()
