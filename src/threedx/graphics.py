import numpy as np
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from typing import Any

def plot_forecast(
    forecast: np.ndarray[tuple[int, int], np.dtype[Any]],
    y: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> Figure:
    """
    Plot a prediction sample path matrix as quantiles along with training data.
    """
    forecast_index = range(y.size, y.size + forecast.shape[1])
    q92_lower, q66_lower, q50_lower, median, q50_upper, q66_upper, q92_upper = \
        np.quantile(
            forecast,
            [0.04, 0.17, 0.25, 0.5, 0.75, 0.83, 0.96],
            axis = 0
        )

    plt.figure(figsize = (8, 4))
    plt.plot(y, color = "royalblue", label = "Data")
    plt.fill_between(
        forecast_index,
        q92_lower,
        q92_upper,
        color="tomato",
        alpha=0.3,
        label="92%"
    )
    plt.fill_between(
        forecast_index,
        q66_lower, 
        q66_upper,
        color="tomato",
        alpha=0.3,
        label="66%"
    )
    plt.fill_between(
        forecast_index,
        q50_lower,
        q50_upper,
        color="tomato",
        alpha=0.3,
        label="50%"
    )
    plt.plot(forecast_index, median, color="tomato", label="Median")
    plt.legend()
    plt.grid()
