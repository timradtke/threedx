import numpy as np
import numpy.testing as npt
import pytest
from threedx.weights import (
    weights_threedx,
    weights_exponential,
    weights_seasonal,
    weights_seasonal_decay
)

def test_weights_seasonal_returns_ndarray():
    assert isinstance(
        weights_threedx(
            alpha=0.5,
            alpha_seasonal=0.1,
            alpha_seasonal_decay=0.1,
            n=20,
            period_length=12
        ),
        np.ndarray
    )

def test_weights_seasonal_sum_up_to_one():
    npt.assert_almost_equal(
        actual=np.sum(
            weights_threedx(
                alpha=0.5,
                alpha_seasonal=0.1,
                alpha_seasonal_decay=0.1,
                n=3829,
                period_length=12
            )
        ),
        desired=np.float64(1.0)
    )

    npt.assert_almost_equal(
        actual=np.sum(
            weights_threedx(
                alpha=0.01,
                alpha_seasonal=0.01,
                alpha_seasonal_decay=0.2,
                n=10,
                period_length=12
            )
        ),
        desired=np.float64(1.0)
    )

    npt.assert_almost_equal(
        actual=np.sum(
            weights_threedx(
                alpha=1.0,
                alpha_seasonal=1.0,
                alpha_seasonal_decay=0.2,
                n=4,
                period_length=7
            )
        ),
        desired=np.float64(1.0)
    )

def test_returns_random_walk_when_1_0_0():
    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=1.0,
            alpha_seasonal=0.0,
            alpha_seasonal_decay=0.0,
            n=70,
            period_length=12
        ),
        desired=weights_exponential(alpha=1.0, n=70)
    )

def test_returns_random_walk_when_1_1_0():
    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=1.0,
            alpha_seasonal=1.0,
            alpha_seasonal_decay=0.0,
            n=70,
            period_length=12
        ),
        desired=weights_exponential(alpha=1.0, n=70)
    )

def test_returns_random_walk_when_1_1_1():
    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=1.0,
            alpha_seasonal=1.0,
            alpha_seasonal_decay=1.0,
            n=70,
            period_length=12
        ),
        desired=weights_exponential(alpha=1.0, n=70)
    )

def test_returns_random_walk_when_1_0_1():
    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=1.0,
            alpha_seasonal=0.0,
            alpha_seasonal_decay=1.0,
            n=70,
            period_length=12
        ),
        desired=weights_exponential(alpha=1.0, n=70)
    )

def test_returns_average_when_0_0_0():
    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=0.0,
            alpha_seasonal=0.0,
            alpha_seasonal_decay=0.0,
            n=70,
            period_length=12
        ),
        desired=(np.ones(70) / 70.0)
    )

def test_returns_seasonal_average_when_0_1_0():
    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=0.0,
            alpha_seasonal=1.0,
            alpha_seasonal_decay=0.0,
            n=70,
            period_length=12
        ),
        desired=weights_seasonal(alpha=1.0, n=70, period_length=12)
    )

def test_returns_seasonal_naive_when_0_1_1():
    desired = np.zeros(70, dtype=np.float64)
    desired[-12] = 1.0

    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=0.0,
            alpha_seasonal=1.0,
            alpha_seasonal_decay=1.0,
            n=70,
            period_length=12
        ),
        desired=desired
    )

def test_returns_latest_period_average_when_0_0_1():
    npt.assert_almost_equal(
        actual=weights_threedx(
            alpha=0.0,
            alpha_seasonal=0.0,
            alpha_seasonal_decay=1.0,
            n=70,
            period_length=12
        ),
        desired=weights_seasonal_decay(alpha=1.0, n=70, period_length=12)
    )