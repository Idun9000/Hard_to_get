import numpy as np
import pandas as pd

def payoff_wetzels(ntrials):
    """
    Create a pseudorandomized payoff structure for a given number of trials.
    
    Args:
        ntrials (int): Total number of trials. Must be divisible by nstruct.
    
    Returns:
        DataFrame: A DataFrame containing the payoff structure with wins and losses.
    """
    if ntrials % 10 != 0:
        raise ValueError("ntrials must be divisible by 10.")
        
    # Constants
    nstruct = 10  # size of our subdivisions for pseudorandomization
    freq = 0.5  # probability of our frequent losses
    infreq = 0.1  # probability of our infrequent losses
    bad_r = 100  # "bad" winnings
    bad_freq_l = -250  # "bad" frequent loss
    bad_infreq_l = -1250  # "bad" infrequent loss
    good_r = 50  # "good" winnings
    good_freq_l = -50  # "good" frequent loss
    good_infreq_l = -250  # "good" infrequent loss

    # Deck definitions
    A_R = np.full(nstruct, bad_r)
    A_L = np.concatenate((np.full(int(nstruct * freq), bad_freq_l), np.zeros(int(nstruct * (1 - freq)))))
    
    B_R = np.full(nstruct, bad_r)
    B_L = np.concatenate((np.full(int(nstruct * infreq), bad_infreq_l), np.zeros(int(nstruct * (1 - infreq)))))
    
    C_R = np.full(nstruct, good_r)
    C_L = np.concatenate((np.full(int(nstruct * freq), good_freq_l), np.zeros(int(nstruct * (1 - freq)))))
    
    D_R = np.full(nstruct, good_r)
    D_L = np.concatenate((np.full(int(nstruct * infreq), good_infreq_l), np.zeros(int(nstruct * (1 - infreq)))))

    # Initialize arrays
    win1 = np.full(ntrials, bad_r)
    win2 = np.full(ntrials, bad_r)
    win3 = np.full(ntrials, good_r)
    win4 = np.full(ntrials, good_r)

    loss1 = np.empty(ntrials)
    loss2 = np.empty(ntrials)
    loss3 = np.empty(ntrials)
    loss4 = np.empty(ntrials)

    A = np.empty(ntrials)
    B = np.empty(ntrials)
    C = np.empty(ntrials)
    D = np.empty(ntrials)

    # Create the pseudorandomized full payoff structure
    for i in range(ntrials // nstruct):
        loss1_temp = np.random.permutation(A_L)
        loss2_temp = np.random.permutation(B_L)
        loss3_temp = np.random.permutation(C_L)
        loss4_temp = np.random.permutation(D_L)

        start = i * nstruct
        end = (i + 1) * nstruct

        loss1[start:end] = loss1_temp
        loss2[start:end] = loss2_temp
        loss3[start:end] = loss3_temp
        loss4[start:end] = loss4_temp

        A[start:end] = A_R + loss1_temp
        B[start:end] = B_R + loss2_temp
        C[start:end] = C_R + loss3_temp
        D[start:end] = D_R + loss4_temp

    # Combine all four decks as columns
    payoff = np.column_stack((A, B, C, D))

    # Print the sum of payoffs for verification
    payoff_sums = np.sum(payoff, axis=0)
    print("Payoff sums:", payoff_sums)  # Should sum to -2500 for bad decks and 2500 for good decks

    # Create and return a DataFrame
    df = pd.DataFrame({
        'win1': win1,
        'win2': win2,
        'win3': win3,
        'win4': win4,
        'loss1': loss1,
        'loss2': loss2,
        'loss3': loss3,
        'loss4': loss4
    })
    
    return df

def payoff_ahn(ntrials):
    """
    Creates a payoff structure with variable loss rules for four decks (A, B, C, D).
    
    Args:
        ntrials (int): Total number of trials. Must be divisible by 10.
    
    Returns:
        DataFrame: A DataFrame containing the payoff structure with wins and losses for each trial.
    """
    if ntrials % 10 != 0:
        raise ValueError("ntrials must be divisible by 10.")
    
    np.random.seed(2000)

    nstruct = 10  # Size of subdivisions for pseudorandomization
    freq = 0.5  # Probability of frequent losses
    infreq = 0.1  # Probability of infrequent losses
    bad_r = 100  # "Bad" winnings
    bad_freq_l = -250  # "Bad" frequent loss
    bad_infreq_l = -1250  # "Bad" infrequent loss
    good_r = 50  # "Good" winnings
    good_freq_l = -50  # "Good" frequent loss
    good_infreq_l = -250  # "Good" infrequent loss

    # Deck A: Loss increases by 30 every 10 trials, up to 60 trials, then fixed
    A_R = np.full(nstruct, bad_r)
    A_L = np.concatenate([
        np.concatenate((np.full(int(nstruct * freq), bad_freq_l - i * 30),
                        np.zeros(int(nstruct * (1 - freq)))))
        for i in range(min(6, ntrials // nstruct))
    ] + [
        np.concatenate((np.full(int(nstruct * freq), bad_freq_l - 150),
                        np.zeros(int(nstruct * (1 - freq)))))
    ] * max(0, ntrials // nstruct - 6))

    # Deck B: Loss increases by 150 every 10 trials, up to 60 trials, then fixed
    B_R = np.full(nstruct, bad_r)
    B_L = np.concatenate([
        np.concatenate((np.full(int(nstruct * infreq), bad_infreq_l - i * 150),
                        np.zeros(int(nstruct * (1 - infreq)))))
        for i in range(min(6, ntrials // nstruct))
    ] + [
        np.concatenate((np.full(int(nstruct * infreq), bad_infreq_l - 750),
                        np.zeros(int(nstruct * (1 - infreq)))))
    ] * max(0, ntrials // nstruct - 6))

    # Deck C: Loss decreases by 5 every 10 trials
    C_R = np.full(nstruct, good_r)
    C_L = np.concatenate([
        np.concatenate((np.full(int(nstruct * freq), good_freq_l + i * 5),
                        np.zeros(int(nstruct * (1 - freq)))))
        for i in range(min(6, ntrials // nstruct))
    ] + [
        np.concatenate((np.full(int(nstruct * freq), good_freq_l + 25),
                        np.zeros(int(nstruct * (1 - freq)))))
    ] * max(0, ntrials // nstruct - 6))

    # Deck D: Loss decreases by 25 every 10 trials
    D_R = np.full(nstruct, good_r)
    D_L = np.concatenate([
        np.concatenate((np.full(int(nstruct * infreq), good_infreq_l + i * 25),
                        np.zeros(int(nstruct * (1 - infreq)))))
        for i in range(min(6, ntrials // nstruct))
    ] + [
        np.concatenate((np.full(int(nstruct * infreq), good_infreq_l + 125),
                        np.zeros(int(nstruct * (1 - infreq)))))
    ] * max(0, ntrials // nstruct - 6))

    # Initialize arrays for results
    win1, win2, win3, win4 = np.full(ntrials, bad_r), np.full(ntrials, bad_r), np.full(ntrials, good_r), np.full(ntrials, good_r)
    loss1, loss2, loss3, loss4 = np.empty(ntrials), np.empty(ntrials), np.empty(ntrials), np.empty(ntrials)

    # Populate the payoff structure for each deck
    for i in range(ntrials // nstruct):
        idx_start = i * nstruct
        idx_end = (i + 1) * nstruct

        loss1[idx_start:idx_end] = np.random.permutation(A_L[idx_start:idx_end])
        loss2[idx_start:idx_end] = np.random.permutation(B_L[idx_start:idx_end])
        loss3[idx_start:idx_end] = np.random.permutation(C_L[idx_start:idx_end])
        loss4[idx_start:idx_end] = np.random.permutation(D_L[idx_start:idx_end])

    # Create and return a DataFrame
    df = pd.DataFrame({
        'win1': win1,
        'win2': win2,
        'win3': win3,
        'win4': win4,
        'loss1': loss1,
        'loss2': loss2,
        'loss3': loss3,
        'loss4': loss4
    })
    return df