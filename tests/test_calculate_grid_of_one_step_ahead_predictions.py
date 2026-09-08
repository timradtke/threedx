import numpy as np
import numpy.testing as npt
from threedx.initialize import initialize_edge_case_parameters
from threedx.threedx import (
    _calculate_grid_of_one_step_ahead_predictions,
)

y = np.arange(1.0, 15.0, step=1.0)
grid = initialize_edge_case_parameters()

def test_returns_ndarray():
    assert isinstance(
        _calculate_grid_of_one_step_ahead_predictions(
            y=y,
            period_length=7,
            alphas=grid[:, 0],
            alphas_seasonal_decay=grid[:, 1],
            alphas_seasonal=grid[:, 2],
        ),
        np.ndarray
    )

def test_returns_as_many_rows_as_observations():
    """
    Each row i contains the prediction for time step i, the same time step i
    as in the input time series `y`.
    """
    matrix_of_predictions = _calculate_grid_of_one_step_ahead_predictions(
        y=y,
        period_length=7,
        alphas=grid[:, 0],
        alphas_seasonal_decay=grid[:, 1],
        alphas_seasonal=grid[:, 2],
    )

    assert matrix_of_predictions.shape[0] == y.size

def test_returns_as_many_columns_as_alphas():
    """
    Each column contains the predictions created by one of the parameter
    combinations available in `grid`'s rows.
    """
    matrix_of_predictions = _calculate_grid_of_one_step_ahead_predictions(
        y=y,
        period_length=7,
        alphas=grid[:, 0],
        alphas_seasonal_decay=grid[:, 1],
        alphas_seasonal=grid[:, 2],
    )

    assert matrix_of_predictions.shape[1] == grid.shape[0]

def test_returns_first_period_length_steps_as_nan():
    period_length = 7
    initial_nans = np.empty(shape = (period_length, grid.shape[0]))
    initial_nans[:] = np.nan

    matrix_of_predictions = _calculate_grid_of_one_step_ahead_predictions(
        y=y,
        period_length=period_length,
        alphas=grid[:, 0],
        alphas_seasonal_decay=grid[:, 1],
        alphas_seasonal=grid[:, 2],
    )

    npt.assert_equal( # Treats nan as if they're "normal" numbers
        actual=matrix_of_predictions[0:period_length, :],
        desired=initial_nans
    ) 

def test_returns_lag_one_for_random_walk_parameters():
    matrix_of_predictions = _calculate_grid_of_one_step_ahead_predictions(
        y=y,
        period_length=7,
        alphas=np.array([1.0], dtype=np.float64),
        alphas_seasonal_decay=np.array([0.0], dtype=np.float64),
        alphas_seasonal=np.array([0.0], dtype=np.float64),
    )

    npt.assert_equal(
        actual=matrix_of_predictions[7:14, 0],
        desired=y[6:13]
    )

def test_returns_cumulative_mean_for_mean_parameters():
    matrix_of_predictions = _calculate_grid_of_one_step_ahead_predictions(
        y=y,
        period_length=7,
        alphas=np.array([0.0], dtype=np.float64),
        alphas_seasonal_decay=np.array([0.0], dtype=np.float64),
        alphas_seasonal=np.array([0.0], dtype=np.float64),
    )

    npt.assert_equal(
        actual=matrix_of_predictions[7, 0],
        desired=np.mean(y[0:7])
    )

    npt.assert_equal(
        actual=matrix_of_predictions[8, 0],
        desired=np.mean(y[0:8])
    )


