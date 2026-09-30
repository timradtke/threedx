import numpy as np
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from typing import Any

def plot_forecast(
    forecast: np.ndarray[tuple[int, int], np.dtype[Any]],
    y: np.ndarray[tuple[int, ], np.dtype[Any]],
    y_future: None | np.ndarray[tuple[int, ], np.dtype[Any]] = None,
) -> Figure:
    """
    Plot a prediction sample path matrix as quantiles along with training data.

    Requires the optional `threedx[graphics]` dependencies to be installed.

    Parameters
    ----------
    forecast : array_like
        A two-dimensional numpy array of sample path forecasts as returned by
        `Threedx.predict()`.
    y : array_like
        The time series that has been forecasted (i.e., the same as provided to
        `Threedx.fit()`).
    y_future : array_like, optional
        Future observations of `y`, for the same time timesteps as forecasted
        in `forecast`.
    
    Returns
    -------
    Figure
        A matplotlib line chart of the observed time series and marginal
        quantiles derived from the provided sample path forecasts.
    """
    if y_future is not None:
        if y_future.size != forecast.shape[1]:
            raise ValueError(
                f"""
                The number of observations provided in `y_future` must match the
                number of observations forecasted in `forecast`. There are
                {y_future.size} observations in `y_future`, but
                {forecast.shape[1]} observations in `forecast`.
                Adjust `y_future` or set it to None.
                """
            )

    forecast_index = range(y.size, y.size + forecast.shape[1])
    q92_lower, q66_lower, q50_lower, median, q50_upper, q66_upper, q92_upper = \
        np.quantile(
            forecast,
            [0.04, 0.17, 0.25, 0.5, 0.75, 0.83, 0.96],
            axis = 0
        )

    plt.figure(figsize=(8, 4))
    plt.plot(y, color="royalblue", label="Data")
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
    if y_future is not None:
        plt.plot(forecast_index, y_future, color="royalblue", label="Data")
    plt.legend()
    plt.grid()
