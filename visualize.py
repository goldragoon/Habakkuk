import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from pandas import DataFrame
import seaborn as sns


def drawHeatMap(indexes, cols, data):
    print(indexes, cols)
    df = DataFrame(abs(np.random.randn(len(indexes), len(cols))), index=indexes, columns=cols)
    heatmap = sns.heatmap(df, annot=True)
    plt.savefig("performance_heatmap.png")

