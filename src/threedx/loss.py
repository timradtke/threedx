import numpy as np
from typing import Any, Protocol, Literal

class Loss(Protocol):
    def __call__(
        self,
        y_hat: np.ndarray[tuple[int, ], np.dtype[Any]],
        y: np.ndarray[tuple[int, ], np.dtype[Any]]
    ) -> np.ndarray[tuple[Literal[1], ], np.dtype[np.floating]]:
        """
        Calculate the loss for predictions `y_hat` and observations `y`.

        Parameters
        ----------
        y_hat
            A one-dimensional numpy array of one-step ahead predictions.
        y
            A one-dimensional numpy array of observations.
        
        Returns
        -------
        array_like
            The loss of predictions evaluated against actuals, a floating numpy
            scalar.
        """

def mae(
    y_hat: np.ndarray[tuple[int, ], np.dtype[Any]],
    y: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[Literal[1], ], np.dtype[np.floating]]:
    """Calculate the mean absolute error."""
    return np.mean(np.abs(y - y_hat))

def rmse(
    y_hat: np.ndarray[tuple[int, ], np.dtype[Any]],
    y: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[Literal[1], ], np.dtype[np.floating]]:
    """Calculate the root mean squared error."""
    return np.sqrt(np.mean((y - y_hat)**2))

def mae_unbiased(
    y_hat: np.ndarray[tuple[int, ], np.dtype[Any]],
    y: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[Literal[1], ], np.dtype[np.floating]]:
    """
    Calculate a mean absolute error after removing median bias.
    
    An alternative to the standard mean absolute error that ignores the bias
    component in the residuals. Ignoring bias can be useful when bootstrapping
    forecasts from residuals as the residuals will contain the bias missed by
    the model and add it back into the forecast.

    Use it to forecast a trend if you know what you're doing.
    """
    residuals = y - y_hat
    residuals_debiased = residuals - np.median(residuals)
    return np.mean(np.abs(residuals_debiased))

def rmse_unbiased(
    y_hat: np.ndarray[tuple[int, ], np.dtype[Any]],
    y: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[Literal[1], ], np.dtype[np.floating]]:
    """
    Calculate a root mean squared error after removing mean bias.
    
    An alternative to the standard root mean squared error that ignores the bias
    component in the residuals. Ignoring bias can be useful when bootstrapping
    forecasts from residuals as the residuals will contain the bias missed by
    the model and add it back into the forecast.
    
    Use it to forecast a trend if you know what you're doing.
    """
    residuals = y - y_hat
    residuals_debiased = residuals - np.mean(residuals)
    return np.sqrt(np.mean(residuals_debiased**2))
