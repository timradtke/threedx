import numpy as np
from threedx.initialize import (
    initialize_edge_case_parameters,
    _grid_contains_triple_vector
)

def test_returns_ndarray():
    assert isinstance(
        initialize_edge_case_parameters(),
        np.ndarray
    )

def test_returns_two_dimensional_array():
    assert len(initialize_edge_case_parameters().shape) == 2

def test_returns_five_rows():
    assert initialize_edge_case_parameters().shape[0] == 5

def test_returns_three_columns():
    assert initialize_edge_case_parameters().shape[1] == 3

def test_contains_naive():
    assert _grid_contains_triple_vector(
        triple=np.array([1.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_edge_case_parameters()
    )

def test_contains_mean():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_edge_case_parameters()
    )

def test_contains_seasonal_average():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 0.0], dtype=np.float64),
        grid = initialize_edge_case_parameters()
    )

def test_contains_seasonal_naive():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 1.0], dtype=np.float64),
        grid = initialize_edge_case_parameters()
    )

def test_contains_latest_period_average():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 1.0], dtype=np.float64),
        grid = initialize_edge_case_parameters()
    )
