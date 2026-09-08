import numpy as np
import numpy.testing as npt
from threedx.initialize import initialize_edge_case_parameters
from threedx.loss import mae
from threedx.threedx import (
    _calculate_grid_of_one_step_ahead_predictions,
    _evaluate_loss_on_grid_of_predictions
)

y = np.arange(0.0, 6.0, step=1.0)
grid = initialize_edge_case_parameters()
matrix_of_predictions = _calculate_grid_of_one_step_ahead_predictions(
    y=y,
    period_length=4,
    alphas=grid[:, 0],
    alphas_seasonal_decay=grid[:, 1],
    alphas_seasonal=grid[:, 2],
)
losses = _evaluate_loss_on_grid_of_predictions(
    loss=mae,
    y=y,
    grid_of_one_step_ahead_predictions=matrix_of_predictions,
    period_length=4,
)

def test_returns_ndarray():
    assert isinstance(
        losses,
        np.ndarray
    )

def test_returns_one_dimensional_array():
    assert len(losses.shape) == 1

def test_returns_as_many_entries_as_columns_in_predictions():
    """
    Each value represents the loss aggregated over one of the columns in
    `matrix_of_predictions`.
    """
    assert losses.shape[0] == matrix_of_predictions.shape[1]

def test_returns_loss_evaluated_for_each_prediction_column():
    npt.assert_array_equal(
        actual=losses,
        desired=np.array([1.0, 2.75, 2.5, 4.0, 4.0], dtype=np.float64)
    )
