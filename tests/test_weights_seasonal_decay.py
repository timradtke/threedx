import numpy as np
import numpy.testing as npt
import pytest
from threedx.weights import weights_seasonal_decay, weights_exponential

def test_weights_seasonal_returns_ndarray():
    assert isinstance(
        weights_seasonal_decay(alpha=0.5, n=5, period_length=12),
        np.ndarray
    )
    assert isinstance(
        weights_seasonal_decay(alpha=0.9, n=50, period_length=1),
        np.ndarray
    )
    assert isinstance(
        weights_seasonal_decay(alpha=0.1, n=493, period_length=7),
        np.ndarray
    )

def test_weights_seasonal_sum_up_to_one():
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal_decay(alpha=1., n=2749, period_length=12)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal_decay(alpha=0.3532, n=47, period_length=12)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal_decay(alpha=0.0, n=3, period_length=7)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal_decay(alpha=0.2, n=7, period_length=7)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal_decay(alpha=0.7, n=12, period_length=12)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal_decay(alpha=1/3, n=1, period_length=12)
        ),
        desired=np.float64(1.0)
    )
    npt.assert_almost_equal(
        actual=np.sum(
            weights_seasonal_decay(alpha=1.0, n=5, period_length=12)
        ),
        desired=np.float64(1.0)
    )

def test_returns_uniform_when_period_length_is_one_and_alpha_is_zero():
    npt.assert_almost_equal(
        actual=weights_seasonal_decay(alpha=0.0, n=23, period_length=1),
        desired=(np.ones(23) / 23.)
    )

def test_returns_exponential_when_period_length_is_one():
    npt.assert_almost_equal(
        actual=weights_seasonal_decay(alpha=0.292, n=23, period_length=1),
        desired=weights_exponential(alpha=0.292, n=23)
    )

def test_returns_uniform_when_n_less_than_or_equal_period_length():
    npt.assert_almost_equal(
        actual=weights_seasonal_decay(alpha=0.8746, n=23, period_length=24),
        desired=(np.ones(23) / 23.)
    )
    npt.assert_almost_equal(
        actual=weights_seasonal_decay(alpha=1./3., n=12, period_length=12),
        desired=(np.ones(12) / 12.)
    )
    npt.assert_almost_equal(
        actual=weights_seasonal_decay(alpha=0.1, n=1, period_length=7),
        desired=(np.ones(1) / 1.)
    )

def test_cross_period_is_exponential():
    def _assert(alpha, n, period_length):
        """Helper to assert on each cross-period slice."""
        x = weights_seasonal_decay(
            alpha=alpha,
            n=n,
            period_length=period_length
        )

        seq_along_x = np.arange(start=0, stop=n, step=1)

        for i in range(period_length):
            mask = ((seq_along_x - i) % period_length) == 0
            x_cross_period = (x[mask]/np.sum(x[mask]))

            npt.assert_almost_equal(
                actual=x_cross_period,
                desired=weights_exponential(
                    alpha=alpha,
                    n=x_cross_period.shape[0]
                )
            )

    _assert(alpha=0.1264, n=76, period_length=12)
    _assert(alpha=0.837, n=1840, period_length=7)
    _assert(alpha=1.0, n=50, period_length=24)
    _assert(alpha=0.0, n=50, period_length=24)
    
def test_throws_error_on_alpha_not_float():
    with pytest.raises(TypeError):
        weights_seasonal_decay(alpha=1, n=3, period_length=12)
    with pytest.raises(TypeError):
        weights_seasonal_decay(alpha="1.5", n=3, period_length=12)

def test_throws_error_on_alpha_not_in_range():
    with pytest.raises(ValueError):
        weights_seasonal_decay(alpha=-0.5, n=3, period_length=12)
    with pytest.raises(ValueError):
        weights_seasonal_decay(alpha=36., n=3, period_length=12)

def test_throws_error_on_n_not_int():
    with pytest.raises(TypeError):
        weights_seasonal_decay(alpha=0.5, n=3.3, period_length=12)
    with pytest.raises(TypeError):
        weights_seasonal_decay(alpha=0.5, n="3", period_length=12)

def test_throws_error_on_n_less_than_one():
    with pytest.raises(ValueError):
        weights_seasonal_decay(alpha=0.5, n=0, period_length=12)
    with pytest.raises(ValueError):
        weights_seasonal_decay(alpha=0.5, n=-48, period_length=12)

def test_throws_error_on_period_length_not_int():
    with pytest.raises(TypeError):
        weights_seasonal_decay(alpha=0.5, n=3, period_length=12.)
    with pytest.raises(TypeError):
        weights_seasonal_decay(alpha=0.5, n=3, period_length="12")

def test_throws_error_on_n_less_than_one():
    with pytest.raises(ValueError):
        weights_seasonal_decay(alpha=0.5, n=3, period_length=0)
    with pytest.raises(ValueError):
        weights_seasonal_decay(alpha=0.5, n=3, period_length=-492)