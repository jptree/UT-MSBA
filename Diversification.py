__author__ = "Jason Petri"

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
import itertools
import random


for i in range(5):  # Arbitrarily run this analysis for 5 different random subsets.

    stock_data = pd.read_csv('Data.csv', index_col=0)
    tickers_list = random.sample(list(stock_data.columns), 15)  # careful tweaking this number.
    # This becomes computationally expensive
    n_port_stdev = {}


    def calculate_portfolio_variance(tickers, data):
        num_securities = len(tickers)
        security_weights = 1 / num_securities
        m_weight = np.full((num_securities, 1), security_weights)
        m_cov = data[tickers].cov()
        portfolio_variance = np.dot(np.dot(np.transpose(m_weight), m_cov), m_weight)
        return portfolio_variance


    for L in range(1, len(tickers_list) + 1):
        port_variances_at_n = []
        for subset in itertools.combinations(tickers_list, L):
            # print(subset)
            l_subset = list(subset)
            port_val = calculate_portfolio_variance(l_subset, stock_data)
            port_variances_at_n.append(port_val)

        n_port_stdev[L] = math.sqrt(float(sum(port_variances_at_n) / len(port_variances_at_n))) * math.sqrt(12)

    print(tickers_list)
    print(n_port_stdev)

    plt.plot(n_port_stdev.keys(), n_port_stdev.values())
    plt.xlabel('number of securities')
    plt.ylabel('average annual portfolio risk')
    plt.show()
