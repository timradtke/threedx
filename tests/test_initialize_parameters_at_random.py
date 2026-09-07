import numpy as np
import numpy.testing as npt
from threedx.initialize import (
    initialize_parameters_at_random,
    _grid_contains_triple_vector
)

def test_returns_ndarray():
    assert isinstance(
        initialize_parameters_at_random(
            size=1000,
            seed=None,
            include_edge_cases=True
        ),
        np.ndarray
    )
    assert isinstance(
        initialize_parameters_at_random(
            size=1,
            seed=None,
            include_edge_cases=True
        ),
        np.ndarray
    )
    assert isinstance(
        initialize_parameters_at_random(
            size=1,
            seed=None,
            include_edge_cases=False
        ),
        np.ndarray
    )
    assert isinstance(
        initialize_parameters_at_random(
            size=5,
            seed=6281,
            include_edge_cases=False
        ),
        np.ndarray
    )

def test_has_defaults_for_everything():
    assert isinstance(initialize_parameters_at_random(), np.ndarray)

def test_returns_two_dimensional_array():
    assert len(initialize_parameters_at_random().shape) == 2

def test_returns_three_columns():
    assert initialize_parameters_at_random().shape[1] == 3

def test_returns_size_rows():
    # Testing sizes [4, 5, 6] because there are 5 edge cases to be included.
    for size in [1, 3, 4, 5, 6, 1000, 4728]:
        assert initialize_parameters_at_random(
            size=size,
            include_edge_cases=True
        ).shape[0] == size
        assert initialize_parameters_at_random(
            size=size,
            include_edge_cases=False
        ).shape[0] == size

def test_contains_naive():
    assert _grid_contains_triple_vector(
        triple=np.array([1.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_at_random(include_edge_cases=True)
    )

def test_contains_mean():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_at_random(include_edge_cases=True)
    )

def test_contains_seasonal_average():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 0.0], dtype=np.float64),
        grid = initialize_parameters_at_random(include_edge_cases=True)
    )

def test_contains_seasonal_naive():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 1.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_at_random(include_edge_cases=True)
    )

def test_contains_latest_period_average():
    assert _grid_contains_triple_vector(
        triple=np.array([0.0, 0.0, 1.0], dtype=np.float64),
        grid = initialize_parameters_at_random(include_edge_cases=True)
    )

def test_all_parameters_leq_one():
    grid = initialize_parameters_at_random(
        size=10_000,
        include_edge_cases=True
    )
    assert np.all(grid <= 1.0)

def test_all_parameters_geq_zero():
    grid = initialize_parameters_at_random(
        size=10_000,
        include_edge_cases=True
    )
    assert np.all(grid >= 0.0)

def test_setting_a_seed_makes_result_reproducible():
    npt.assert_equal(
        initialize_parameters_at_random(
            size=10_000,
            seed=283,
            include_edge_cases=False
        ),
        initialize_parameters_at_random(
            size=10_000,
            seed=283,
            include_edge_cases=False
        )
    )
    npt.assert_equal(
        initialize_parameters_at_random(
            size=1_000,
            seed=827,
            include_edge_cases=True
        ),
        initialize_parameters_at_random(
            size=1_000,
            seed=827,
            include_edge_cases=True
        )
    )

def test_not_setting_a_seed_implies_varying_results():
    assert not np.allclose(
        initialize_parameters_at_random(
            size = 1_000,
            seed=None,
            include_edge_cases=True
        ),
        initialize_parameters_at_random(
            size = 1_000,
            seed=None,
            include_edge_cases=True
        )
    )
