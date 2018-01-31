import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
from pandas import DataFrame
import seaborn as sns


def drawHeatMap(indexes, cols, data):
    df = DataFrame(data, index=indexes, columns=cols)
    heatmap = sns.heatmap(df, annot=True)
    plt.savefig("performance_heatmap.png")


