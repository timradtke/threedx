import numpy as np
from threedx.initialize import (
    initialize_parameters_in_grid,
    _grid_contains_triple_vector
)

def test_returns_ndarray():
    assert isinstance(
        initialize_parameters_in_grid(base_size=10),
        np.ndarray
    )
    assert isinstance(
        initialize_parameters_in_grid(base_size=1),
        np.ndarray
    )

def test_has_defaults_for_everything():
    assert isinstance(initialize_parameters_in_grid(), np.ndarray)

def test_returns_two_dimensional_array():
    assert len(initialize_parameters_in_grid().shape) == 2

def test_returns_three_columns():
    assert initialize_parameters_in_grid().shape[1] == 3

def test_returns_base_size_to_the_power_of_three_rows():
    for base_size in [1, 2, 3, 10, 25]:
        assert (
            initialize_parameters_in_grid(base_size=base_size).shape[0] == \
                (base_size ** 3)
        )

def test_contains_naive():
    assert _grid_contains_triple_vector(
        triple=np.array([1.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=2)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([1.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=3)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([1.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=9)
    )

def test_contains_mean():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=2)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=3)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=9)
    )

def test_contains_seasonal_average():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=2)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=3)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=9)
    )

def test_contains_seasonal_naive():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=2)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=3)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=9)
    )

def test_contains_latest_period_average():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=2)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=3)
    )
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_in_grid(base_size=9)
    )

def test_all_parameters_leq_one():
    grid = initialize_parameters_in_grid(base_size=1)
    assert np.all(grid <= 1.0)
    grid = initialize_parameters_in_grid(base_size=3)
    assert np.all(grid <= 1.0)
    grid = initialize_parameters_in_grid(base_size=9)
    assert np.all(grid <= 1.0)
    grid = initialize_parameters_in_grid(base_size=10)
    assert np.all(grid <= 1.0)

def test_all_parameters_geq_zero():
    grid = initialize_parameters_in_grid(base_size=1)
    assert np.all(grid >= 0.0)
    grid = initialize_parameters_in_grid(base_size=3)
    assert np.all(grid >= 0.0)
    grid = initialize_parameters_in_grid(base_size=9)
    assert np.all(grid >= 0.0)
    grid = initialize_parameters_in_grid(base_size=10)
    assert np.all(grid >= 0.0)
