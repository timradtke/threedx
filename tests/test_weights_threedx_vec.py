import numpy as np
import numpy.testing as npt
from threedx.weights import weights_threedx, _weights_threedx_vec

def test_returns_matrix_where_rows_are_like_weights_threedx():
    def assert_(
        alphas_list,
        alphas_seasonal_list,
        alphas_seasonal_decay_list,
        n,
        period_length
    ):
        m_ = _weights_threedx_vec(
            alphas=np.array(alphas_list, dtype=np.float64),
            alphas_seasonal=np.array(alphas_seasonal_list, dtype=np.float64),
            alphas_seasonal_decay=np.array(alphas_seasonal_decay_list, dtype=np.float64),
            n=n,
            period_length=period_length
        )
        for i in range(len(alphas_list)):
            npt.assert_almost_equal(
                actual=m_[i, ],
                desired=weights_threedx(
                    alpha=alphas_list[i],
                    alpha_seasonal=alphas_seasonal_list[i],
                    alpha_seasonal_decay=alphas_seasonal_decay_list[i],
                    n=n,
                    period_length=period_length
                )
            )

    alphas_list = [
        0.0, 0.0000000001, 0.1, 0.2, 0.284, 0.40003884, 0.8233, 0.999999, 1.0,
        1.0, 0.2, 0.583, 0.1109, 0.99919375, 1.0,
        1./3., 1.0, 1.0, 1.0, 0.0, 0.0, 0.0,
    ]
    alphas_seasonal_list = [
        0.0, 0.0000000001, 0.1, 0.2, 0.284, 0.40003884, 0.8233, 0.999999, 1.0,
        0.2, 0.839, 0.333, 0.1111, 0.000000001, 0.0,
        1./3., 1.0, 0.000000001, 0.999999999, 0.0, 0.48294, 1./3.,
    ]
    alphas_seasonal_decay_list = [
        0.0, 0.0000000001, 0.1, 0.2, 0.284, 0.40003884, 0.8233, 0.999999, 1.0,
        1./3., 1.0, 0.000000001, 0.999999999, 0.0, 0.48294,
        1./3., 0.2, 0.839, 0.333, 0.1111, 0.000000001, 0.0,

    ]
    for pl in [1, 7, 12, 24, 365]:
        assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=1, period_length=pl)
        assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=5, period_length=pl)
        assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=7, period_length=pl)
        assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=12, period_length=pl)
        assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=14, period_length=pl)
        assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=24, period_length=pl)
        assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=365, period_length=pl)

        # # Turning large `n` on will make tests noticeably slower due to poor
        # # scaling of the above `for` loop calling `weights_threedx()`.
        # assert_(alphas_list=alphas_list, alphas_seasonal_list=alphas_seasonal_list, alphas_seasonal_decay_list=alphas_seasonal_decay_list, n=18474, period_length=pl)
