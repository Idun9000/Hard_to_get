import os
import openai
import pandas as pd
import argparse
from huggingface_hub import InferenceClient
from dotenv import load_dotenv

# Load .env file
load_dotenv()

# Import functions from payoffs.py 
from payoffs import payoff_ahn, payoff_wetzels

# Function to parse input arguments
def input_parse():
    parser = argparse.ArgumentParser(description='Run the GPT simulation')
    
    parser.add_argument('--api_key', type=str, help='Hugging Face API key', default=os.getenv('hugging_face_api'))
    parser.add_argument('--payoff_structure', type=str, help='Payoff structure to simulate', choices=['ahn', 'wetzels'])
    parser.add_argument('--ntrials', type=int, help='Number of trials', default=10)
    parser.add_argument('--task_type', type=str, help='Task to simulate', choices=['control-high', 'control-low', 'addict-high', 'addict-low'])
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
            return [{"role": "user", "content": description}]

    raise ValueError(f"Task type '{task_type}' not found in the file.")

# Main function to simulate responses
def main():
    args = input_parse()
    
    # Load API key
    load_dotenv()
    client = InferenceClient(api_key=os.getenv("hugging_face_api"))

    # Set up the payoff structure
    if args.payoff_structure == 'ahn':
        payoff_structure = payoff_ahn(args.ntrials)
    elif args.payoff_structure == 'wetzels':
        payoff_structure = payoff_wetzels(args.ntrials)
    else:
        raise ValueError("Invalid payoff structure specified.")

    # Initialize results DataFrame
    results_df = pd.DataFrame(columns=["trial", "deck", "gain", "loss", "net", "agentN"])

    # Game loop for multiple agents
    for agent in range(args.n_agents):
        agent_id = f"Agent_{agent + 1}"
        messages = task_description(args.task_type)

        for trial in range(args.ntrials):
            # Get AI's response
            stream = client.chat.completions.create(
                model="meta-llama/Llama-3.3-70B-Instruct", 
                messages=messages, 
                temperature=0.5,
                max_tokens=1,
                top_p=0.7,
                stream=True
            )

            assistant_response = "".join(chunk.choices[0].delta.content for chunk in stream).strip()
            deck = assistant_response

            # Determine payoff based on AI's choice
            deck_index = ord(deck) - ord('A')
            win_key = f"win{deck_index + 1}"
            loss_key = f"loss{deck_index + 1}"

            win = payoff_structure.loc[trial, win_key]
            loss = payoff_structure.loc[trial, loss_key]
            net = win + loss

            feedback = f"Win: {win}\nLoss: {loss}\nNet: {net}"

            # Log results
            new_row = pd.DataFrame([{
                "trial": trial + 1,
                "deck": deck,
                "gain": win,
                "loss": loss,
                "net": net,
                "agentN": agent_id
            }])
            results_df = pd.concat([results_df, new_row], ignore_index=True)

            # Add AI's choice and feedback to the conversation
            messages.append({"role": "assistant", "content": deck})
            messages.append({"role": "user", "content": feedback})
            
    # Find or create the "out" folder
    output_folder = os.path.join(os.getcwd(), "out")
    os.makedirs(output_folder, exist_ok=True)  # Ensure the folder exists

    # Define the output file path
    output_file = os.path.join(output_folder, "results.csv")

   # Save the results DataFrame
    results_df.to_csv(output_file, index=False) # Save results to CSV
    print("Simulation complete. Results saved to 'out/results.csv'.")

if __name__ == '__main__':
    main()
