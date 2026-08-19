import numpy as np
import numpy.testing as npt
import pytest
from threedx.weights import weights_exponential

def test_weights_exponential_returns_ndarray():
    assert isinstance(weights_exponential(alpha=0.5, n=5), np.ndarray)
    assert isinstance(weights_exponential(alpha=1., n=5), np.ndarray)
    assert isinstance(weights_exponential(alpha=0., n=5), np.ndarray)

def test_weights_exponential_returns_array_of_shape_n():
    assert weights_exponential(alpha=0.5, n=5).shape == (5,)
    assert weights_exponential(alpha=0.5, n=100).shape == (100,)

def test_weights_exponential_sum_up_to_one():
    npt.assert_equal(
        actual=np.sum(weights_exponential(alpha=1., n=2749)),
        desired=np.float64(1.0)
    )
    npt.assert_equal(
        actual=np.sum(weights_exponential(alpha=0., n=50)),
        desired=np.float64(1.0)
    )
    npt.assert_equal(
        actual=np.sum(weights_exponential(alpha=0.9, n=82)),
        desired=np.float64(1.0)
    )
    npt.assert_equal(
        actual=np.sum(weights_exponential(alpha=0.382, n=7)),
        desired=np.float64(1.0)
    )
    npt.assert_equal(
        actual=np.sum(weights_exponential(alpha=1./38., n=1)),
        desired=np.float64(1.0)
    )

def test_weights_exponential_are_monotonically_increasing():
    """Compare against sorted array to check if order would change."""

    actual = weights_exponential(alpha=0.5, n=5)
    npt.assert_array_equal(
        actual=actual,
        desired=np.sort(actual, axis=0, descending=False)
    )

    actual = weights_exponential(alpha=0.1, n=10)
    npt.assert_array_equal(
        actual=actual,
        desired=np.sort(actual, axis=0, descending=False)
    )

    actual = weights_exponential(alpha=0., n=4)
    npt.assert_array_equal(
        actual=actual,
        desired=np.sort(actual, axis=0, descending=False)
    )

    actual = weights_exponential(alpha=1., n=2749)
    npt.assert_array_equal(
        actual=actual,
        desired=np.sort(actual, axis=0, descending=False)
    )

def test_weights_exponential_returns_one_for_last_if_alpha_is_one():
    npt.assert_array_equal(
        actual=weights_exponential(alpha=1.0, n=3),
        desired=np.array([0.0, 0.0, 1.0])
    )

def test_weights_exponential_returns_average_for_each_if_alpha_is_zero():
    npt.assert_array_equal(
        actual=weights_exponential(alpha=0.0, n=3),
        desired=np.array([1./3., 1./3., 1./3.])
    )

def test_weights_exponential_returns_exponential_series():
    def rescaled_subset_is_equal_to_short(alpha, n, m):
        """The subset of an exponential function is itself exponential."""
        w_long = weights_exponential(alpha=alpha, n=n)
        w_short = weights_exponential(alpha=alpha, n=m)
        w_long_subset = w_long[(n-m):] / np.sum(w_long[(n-m):])
        npt.assert_almost_equal(
            actual=w_long_subset,
            desired=w_short,
            decimal=7
        )

    rescaled_subset_is_equal_to_short(alpha=0.3827, n=10, m=4)
    rescaled_subset_is_equal_to_short(alpha=0.823, n=8392, m=7002)
    rescaled_subset_is_equal_to_short(alpha=1.0, n=5, m=3)
    rescaled_subset_is_equal_to_short(alpha=0.0, n=72, m=60)

def test_weights_exponential_throws_error_on_alpha_not_float():
    with pytest.raises(TypeError):
            weights_exponential(alpha=1, n=3)
    with pytest.raises(TypeError):
        weights_exponential(alpha="1.5", n=3)

def test_weights_exponential_throws_error_on_alpha_not_in_range():
    with pytest.raises(ValueError):
        weights_exponential(alpha=-0.5, n=3)
    with pytest.raises(ValueError):
        weights_exponential(alpha=82., n=920)

def test_weights_exponential_throws_error_on_n_not_int():
    with pytest.raises(TypeError):
        weights_exponential(alpha=1., n=3.)
    with pytest.raises(TypeError):
        weights_exponential(alpha=0.5, n="3")

def test_weights_exponential_throws_error_on_n_less_than_one():
    with pytest.raises(ValueError):
        weights_exponential(alpha=0.5, n=0)
    with pytest.raises(ValueError):
        weights_exponential(alpha=0.5, n=-5)
