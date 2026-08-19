import numpy as np
import numpy.testing as npt
import pytest
from threedx.weights import weights_seasonal, weights_exponential

def test_weights_seasonal_returns_ndarray():
    assert isinstance(
        weights_seasonal(alpha_seasonal=0.5, n=5, period_length=12),
        np.ndarray
    )
    assert isinstance(
        weights_seasonal(alpha_seasonal=0.9, n=50, period_length=1),
        np.ndarray
    )
    assert isinstance(
        weights_seasonal(alpha_seasonal=0.1, n=493, period_length=7),
        np.ndarray
    )

def test_weights_seasonal_sum_up_to_one():
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal(alpha_seasonal=1., n=2749, period_length=12)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal(alpha_seasonal=0.3532, n=47, period_length=12)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal(alpha_seasonal=0.0, n=3, period_length=7)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal(alpha_seasonal=0.2, n=7, period_length=7)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal(alpha_seasonal=0.7, n=12, period_length=12)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal(alpha_seasonal=1/3, n=1, period_length=12)
        ),
        desired=np.float64(1.0)
    )

def test_weights_seasonal_returns_uniform_when_period_length_is_one():
    npt.assert_almost_equal(
        actual=weights_seasonal(alpha_seasonal=0.5, n=23, period_length=1),
        desired=(np.ones(23) / 23.)
    )

def test_first_is_maximum():
    weekly = weights_seasonal(alpha_seasonal=0.5, n=7, period_length=7)
    npt.assert_equal(
        actual=weekly[0],
        desired=np.max(weekly)
    )
    monthly = weights_seasonal(alpha_seasonal=0.5, n=12, period_length=12)
    npt.assert_equal(
        actual=monthly[0],
        desired=np.max(monthly)
    )
    daily = weights_seasonal(alpha_seasonal=0.5, n=365, period_length=365)
    npt.assert_equal(
        actual=daily[0],
        desired=np.max(daily)
    )

def test_minimum_is_in_middle_when_period_length_is_even():
    monthly = weights_seasonal(alpha_seasonal=0.5, n=12, period_length=12)
    npt.assert_equal(
        actual=monthly[6],
        desired=np.min(monthly)
    )
    quarterly = weights_seasonal(alpha_seasonal=0.5, n=4, period_length=4)
    npt.assert_equal(
        actual=quarterly[2],
        desired=np.min(quarterly)
    )

def test_rest_is_mirrored():
    weekly = weights_seasonal(alpha_seasonal=0.5, n=7, period_length=7)
    npt.assert_array_equal(
        actual=weekly[1:4][::-1],
        desired=weekly[4:7]
    )
    monthly = weights_seasonal(alpha_seasonal=0.5, n=12, period_length=12)
    npt.assert_array_equal(
        actual=monthly[1:6][::-1],
        desired=monthly[7:12]
    )

def test_each_half_is_exponential():
    weekly = weights_seasonal(alpha_seasonal=0.5, n=7, period_length=7)
    weekly_half = weekly[0:4][::-1] / np.sum(weekly[0:4])
    npt.assert_almost_equal(
        actual=weekly_half,
        desired=weights_exponential(alpha=0.5, n=4)
    )
    monthly = weights_seasonal(alpha_seasonal=0.77, n=12, period_length=12)
    monthly_half = monthly[0:6][::-1] / np.sum(monthly[0:6])
    npt.assert_almost_equal(
        actual=monthly_half,
        desired=weights_exponential(alpha=0.77, n=6)
    )

def test_returns_zeros_when_n_less_than_period_length_and_alpha_is_one():
    npt.assert_array_equal(
        actual=weights_seasonal(alpha_seasonal=1.0, n=5, period_length=7),
        desired=np.zeros(shape=(5,), dtype=np.float64)
    )
    npt.assert_array_equal(
        actual=weights_seasonal(alpha_seasonal=1.0, n=11, period_length=12),
        desired=np.zeros(shape=(11,), dtype=np.float64)
    )
    npt.assert_array_equal(
        actual=weights_seasonal(alpha_seasonal=1.0, n=1, period_length=12),
        desired=np.zeros(shape=(1,), dtype=np.float64)
    )

def test_weights_seasonal_throws_error_on_alpha_not_float():
    with pytest.raises(TypeError):
        weights_seasonal(alpha_seasonal=1, n=3, period_length=12)
    with pytest.raises(TypeError):
        weights_seasonal(alpha_seasonal="1.5", n=3, period_length=12)

def test_weights_seasonal_throws_error_on_alpha_not_in_range():
    with pytest.raises(ValueError):
        weights_seasonal(alpha_seasonal=-0.5, n=3, period_length=12)
    with pytest.raises(ValueError):
        weights_seasonal(alpha_seasonal=36., n=3, period_length=12)

def test_weights_seasonal_throws_error_on_n_not_int():
    with pytest.raises(TypeError):
        weights_seasonal(alpha_seasonal=0.5, n=3.3, period_length=12)
    with pytest.raises(TypeError):
        weights_seasonal(alpha_seasonal=0.5, n="3", period_length=12)

def test_weights_seasonal_throws_error_on_n_less_than_one():
    with pytest.raises(ValueError):
        weights_seasonal(alpha_seasonal=0.5, n=0, period_length=12)
    with pytest.raises(ValueError):
        weights_seasonal(alpha_seasonal=0.5, n=-48, period_length=12)

def test_weights_seasonal_throws_error_on_period_length_not_int():
    with pytest.raises(TypeError):
        weights_seasonal(alpha_seasonal=0.5, n=3, period_length=12.)
    with pytest.raises(TypeError):
        weights_seasonal(alpha_seasonal=0.5, n=3, period_length="12")

def test_weights_seasonal_throws_error_on_n_less_than_one():
    with pytest.raises(ValueError):
        weights_seasonal(alpha_seasonal=0.5, n=3, period_length=0)
    with pytest.raises(ValueError):
        weights_seasonal(alpha_seasonal=0.5, n=3, period_length=-492)