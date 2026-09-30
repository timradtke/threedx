import numpy as np
from numpy.random import Generator
from typing import Any, Protocol

class Draw(Protocol):
    def __call__(
        rng: Generator,
        size: tuple[int, int],
        residuals: np.ndarray[tuple[int, ], np.dtype[Any]]
    ) -> np.ndarray[tuple[int, int], np.dtype[np.floating]]:
        """
        Draw innovations to generate prediction sample paths.

        Parameters
        ----------
        rng
            Numpy random Generator initialized via `np.random.default_rng()`.
        size
            Tuple of two integers specifying the shape of the matrix to create.
        residuals
            One-dimensional numpy array (vector) of one-step ahead prediction
            residuals (or errors) observed during model training. Can be used
            to determine the characteristics of the innovations to draw.
        
        Returns
        -------
        array_like
            Two-dimensional numpy array (matrix) of innovations.
        """

def draw_normal_with_zero_mean(
    rng: Generator,
    size: tuple[int, int],
    residuals: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[int, int], np.dtype[np.floating]]:
    """
    Draw innovations from a zero-mean Normal distribution with scale equal to
    the empirical standard deviation of the provided residuals.
    """
    return rng.normal(
        loc = 0,
        scale = np.std(residuals, ddof = 1),
        size = size
    )

def draw_normal_with_drift(
    rng: Generator,
    size: tuple[int, int],
    residuals: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[int, int], np.dtype[np.floating]]:
    """
    Draw innovations from a Normal distribution with location and scale equal to
    the empirical mean and standard deviation of the provided residuals.
    """
    return rng.normal(
        loc = np.mean(residuals),
        scale = np.std(residuals, ddof = 1),
        size = size
    )

def draw_bootstrap(
    rng: Generator,
    size: tuple[int, int],
    residuals: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[int, int], np.dtype[Any]]:
    """Draw innovations as bootstrap from the provided residuals."""
    return rng.choice(
        a = residuals,
        size = size,
        replace = True,
        p = None
    )

def draw_bootstrap_zero_mean(
    rng: Generator,
    size: tuple[int, int],
    residuals: np.ndarray[tuple[int, ], np.dtype[Any]]
) -> np.ndarray[tuple[int, int], np.dtype[Any]]:
    """
    Draw innovations as zero-mean bootstrap from the provided residuals.
    """
    return rng.choice(
        a = residuals - np.mean(residuals),
        size = size,
        replace = True,
        p = None
    )
