import numpy as np
from collections.abc import Callable
from enum import Enum
from typing import Any
from warnings import warn

from .initialize import _validate_parameter_grid
from .innovations import Draw
from .loss import Loss
from .weights import (
    _weights_threedx_vec,
    weights_threedx,
    _validate_positive_int
)

class Threedx():
    def __init__(
        self,
        period_length: int,
        parameter_grid: np.ndarray[tuple[int, int], np.dtype[np.float64]],
    ):
        """
        A three-dimensional exponential smoothing model.

        Parameters
        ----------
        period_length
            Defines the length of the seasonal pattern to be fitted. For annual
            seasonality in monthly observations, use 12. For weekly seasonality
            in daily observations, use 7. And so on.
        """
        _validate_positive_int(n=period_length, name="period_length")
        _validate_parameter_grid(grid=parameter_grid)

        # Define the `period_length` upfront for the entire
        # object to ensure consistency across all methods;
        # the period length must not vary across method calls.
        self.period_length = period_length
        self.is_fitted = False

        self.alphas = parameter_grid[:, 0]
        self.alphas_seasonal = parameter_grid[:, 1]
        self.alphas_seasonal_decay = parameter_grid[:, 2]

    def weights(self) -> np.ndarray[tuple[int], np.dtype[np.float64]] | None:
        """
        The three-dimensional exponential smoothing weights of the fitted model.
        """
        if not self.is_fitted:
            return None
        
        weights = weights_threedx(
            alpha = self.best_alpha,
            alpha_seasonal = self.best_alpha_seasonal,
            alpha_seasonal_decay = self.best_alpha_seasonal_decay,
            n = self.n,
            period_length = self.period_length
        )

        return weights

    def fit(
        self,
        y: np.ndarray[tuple[int], np.dtype[Any]],
        loss: Loss
    ) -> None:
        """
        Fit a `threedx` model given a time series `y`.

        Parameters
        ----------
        y
            A numpy array (vector) consisting of the training period of the time
            series to predict.
        loss_function
            A function to calculate the loss during optimization. The function
            should take two positional arguments, `y_hat` and `y`, which will be
            numpy vectors of equal length. From those two vectors, the loss
            should be calculated and returned as a float.
        """
        # TODO:
        # _validate_y(y=y)

        if y.size <= self.period_length:
            raise ValueError(
                f"The provided time series has {y.size} observations but "
                f"Threedx requires at least {(1 + self.period_length)=} "
                f"observations."
            )

        if y.size <= 2*self.period_length:
            warn(
                message = (
                    "You are fitting a model onto not more than two periods of "
                    "data. Optimal parameters will easily vary if one of the "
                    "observations changes or when another observation is added."
                ),
                category=RuntimeWarning,
            )

        self.y = y
        self.n = y.size

        grid_of_predictions = \
            _calculate_grid_of_one_step_ahead_predictions(
                y=y,
                period_length=self.period_length,
                alphas=self.alphas,
                alphas_seasonal=self.alphas_seasonal,
                alphas_seasonal_decay=self.alphas_seasonal_decay,
            )

        self.losses = _evaluate_loss_on_grid_of_predictions(
            loss=loss,
            y=y,
            grid_of_one_step_ahead_predictions=grid_of_predictions,
            period_length=self.period_length,
        )

        self.minimal_loss = np.min(self.losses)
        best_alphas_idx = np.argmin(self.losses)

        self.residuals = y[self.period_length:self.n] - \
            grid_of_predictions[self.period_length:self.n, best_alphas_idx]

        self.best_alpha = self.alphas[best_alphas_idx]
        self.best_alpha_seasonal = self.alphas_seasonal[best_alphas_idx]
        self.best_alpha_seasonal_decay = \
            self.alphas_seasonal_decay[best_alphas_idx]

        self.is_fitted = True
        return self

    def predict(
        self,
        horizon: int,
        n_samples: int,
        observation_driven: bool,
        draw: Draw,
        seed: int = None
    ) -> np.ndarray[tuple[int, int], np.dtype[Any]] | None:
        """
        Predict sample paths from the fitted model.

        Parameters
        ----------
        horizon
            An integer number of steps to predict into the future.
        n_samples
            An integer number of sample paths to generate.
        observation_driven
            A boolean indicating whether samples paths should be drawn from
            historical observations (if `True`) or using an innovation function.
        innovation_function
            A (optionally user-defined) function that draws random samples in
            form of a numpy vector of length `n` based on the residuals of
            the fitted model. See `threedx.innovations` for examples.
            Ignored when `observation_driven` is `True`.
        seed
            An integer seed used during random number generation, default
            `None`. The random number generator initialized with this seed is
            passed to the `innovation_function`.
        
        Returns
        -------
            A numpy array of sample paths, with shape (`n_samples`, `horizon`).
        """

        if not self.is_fitted:
            return None
        
        rng = np.random.default_rng(seed = seed)

        if observation_driven:
            if self.y.size < (2 * self.period_length):
                warn(
                    message=(
                        "You are sampling based on less than "
                        f"{(2 * self.period_length)=} of observations. "
                        "Check results carefully."
                    ),
                    category=RuntimeWarning,
                )
            y_hat_m = _predict_observation_driven(
                horizon=horizon,
                n_samples=n_samples,
                y=self.y,
                period_length=self.period_length,
                alpha=self.best_alpha,
                alpha_seasonal=self.best_alpha_seasonal,
                alpha_seasonal_decay=self.best_alpha_seasonal_decay,
                rng=rng
            )
        else:
            if self.residuals.size < self.period_length:
                warn(
                    message=(
                        "You are sampling based on less than "
                       f"{self.period_length=} residuals. "
                       "Check results carefully."
                    ),
                    category=RuntimeWarning,
                )

            y_hat_m = _predict_innovations_driven(
                horizon=horizon,
                n_samples=n_samples,
                y=self.y,
                residuals=self.residuals,
                period_length=self.period_length,
                alpha=self.best_alpha,
                alpha_seasonal=self.best_alpha_seasonal,
                alpha_seasonal_decay=self.best_alpha_seasonal_decay,
                draw=draw,
                rng=rng
            )

        return y_hat_m

def _nans(shape: tuple[int, int]):
    """Initialize a numpy array with filled with np.nan."""
    m = np.empty(shape = shape)
    m[:] = np.nan
    return m

def _initialize_y_m(y, n_samples):
    """Reshape vector `y` into a matrix with `n_samples` rows equal to `y`."""
    return np.tile(y.reshape((y.size, 1)), n_samples).T

def _initialize_y_hat_m(n_samples, horizon):
    """Initialize matrix of prediction sample paths using np.nan."""
    y_hat_m = _nans(shape = (n_samples, horizon))
    return y_hat_m

def _predict_observation_driven(
    horizon,
    n_samples,
    y,
    period_length,
    alpha,
    alpha_seasonal,
    alpha_seasonal_decay,
    rng
):
    """
    Draw prediction sample paths by sampling historical observations.
    """
    y_m = _initialize_y_m(y=y, n_samples=n_samples)
    y_hat_m = _initialize_y_hat_m(n_samples=n_samples, horizon=horizon)

    for idx in range(horizon):
        sample_indices = rng.choice(
            a = y.size,
            size = n_samples,
            replace = True,
            p = weights_threedx(
                alpha = alpha,
                alpha_seasonal = alpha_seasonal,
                alpha_seasonal_decay = alpha_seasonal_decay,
                n = y.size,
                period_length = period_length
            )
        )

        # For the first horizon (idx=0), samples are drawn solely from y_m.
        # For all additional horizons, samples are drawn from y_m and the
        # previously drawn samples.
        tmp_y_m = np.concatenate((y_m, y_hat_m[:, :idx]), axis = 1)
        
        for sample_idx in range(n_samples):
            y_hat_m[sample_idx, idx] = \
                tmp_y_m[sample_idx, sample_indices[sample_idx]]

    return y_hat_m

def _predict_innovations_driven(
    horizon,
    n_samples,
    y,
    residuals,
    period_length,
    alpha,
    alpha_seasonal,
    alpha_seasonal_decay,
    draw,
    rng
):
    """
    Draw prediction sample paths by iteratively combining point predictions with
    innovations.
    """
    y_m = _initialize_y_m(y=y, n_samples=n_samples)

    # At this point, `y_hat_m` only contains the innovations...
    y_hat_m = draw(
        rng = rng,
        size = (n_samples, horizon),
        residuals = residuals
    )

    _validate_innovations_matrix(
        m=y_hat_m,
        n_samples=n_samples,
        horizon=horizon
    )

    # ... onto which this for loop adds the point prediction.
    # This happens iteratively to be autoregressive.
    for idx in range(horizon):
        y_hat_m[::, idx] = y_hat_m[::, idx] + (
            np.concatenate(
                (y_m, y_hat_m[::, :idx]),
                axis = 1
            ) @ weights_threedx(
                    alpha = alpha,
                    alpha_seasonal = alpha_seasonal,
                    alpha_seasonal_decay = alpha_seasonal_decay,
                    n = y_m.shape[1] + idx,
                    period_length = period_length
                )
        )

    return y_hat_m

def _validate_innovations_matrix(m, n_samples, horizon):
    """
    Validate basic characteristics of the innovations matrix that might
    have been drawn from a user-defined function.
    """
    if not isinstance(m, np.ndarray):
        raise TypeError("The innovations matrix is not an `np.ndarray`.")
    if m.shape != (n_samples, horizon):
        raise ValueError(
            f"""
            The innovations matrix was expected to have shape
            {(n_samples, horizon)=} but has shape {m.shape}.
            """
        )
    return None

def _calculate_grid_of_one_step_ahead_predictions(
    y: np.ndarray[tuple[int], np.dtype[np.float64]],
    period_length: int,
    alphas: np.ndarray[tuple[int], np.dtype[np.float64]],
    alphas_seasonal: np.ndarray[tuple[int], np.dtype[np.float64]],
    alphas_seasonal_decay: np.ndarray[tuple[int], np.dtype[np.float64]],
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    n = y.size
    offset = period_length

    one_step_ahead_predictions = _nans(shape = (n, alphas.size))
    
    for i_steps_back in range(y.size - offset):
        tmp_weights = _weights_threedx_vec(
            alphas = alphas,
            alphas_seasonal = alphas_seasonal,
            alphas_seasonal_decay = alphas_seasonal_decay,
            n = n - i_steps_back - 1,
            period_length = period_length
        )

        tmp_y = y[:(n - i_steps_back - 1)]
        one_step_ahead_predictions[n - i_steps_back - 1] = \
            tmp_y @ tmp_weights.T

    return one_step_ahead_predictions

def _evaluate_loss_on_grid_of_predictions(
    loss: Loss,
    y: np.ndarray[tuple[int], np.dtype[np.float64]],
    grid_of_one_step_ahead_predictions: \
        np.ndarray[tuple[int, int], np.dtype[np.float64]],
    period_length: int,
) -> np.ndarray[tuple[int], np.dtype[np.float64]]:
    n = y.size
    step_ahead_loss = np.apply_along_axis(
        func1d = loss,
        axis = 0,
        arr = grid_of_one_step_ahead_predictions[period_length:n],
        y = y[period_length:n]
    )
    return step_ahead_loss
