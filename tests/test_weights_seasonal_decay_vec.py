import numpy as np
import numpy.testing as npt
from threedx.weights import weights_seasonal_decay, _weights_seasonal_decay_vec

def test_returns_matrix_where_rows_are_like_weights_seasonal_decay():
    def assert_(alphas_list, n, period_length):
        alphas = np.array(alphas_list, dtype=np.float64)
        m_ = _weights_seasonal_decay_vec(
            alphas=alphas,
            n=n,
            period_length=period_length
        )
        for i in range(len(alphas_list)):
            npt.assert_almost_equal(
                actual=m_[i, ],
                desired=weights_seasonal_decay(
                    alpha=alphas_list[i],
                    n=n,
                    period_length=period_length
                )
            )

    alphas_list = [
        0.0, 0.0000000001, 0.1, 0.2, 0.284, 0.40003884, 0.8233, 0.999999, 1.0
    ]
    for pl in [1, 7, 12, 24, 365]:
        assert_(alphas_list=alphas_list, n=1, period_length=pl)
        assert_(alphas_list=alphas_list, n=5, period_length=pl)
        assert_(alphas_list=alphas_list, n=7, period_length=pl)
        assert_(alphas_list=alphas_list, n=12, period_length=pl)
        assert_(alphas_list=alphas_list, n=14, period_length=pl)
        assert_(alphas_list=alphas_list, n=24, period_length=pl)
        assert_(alphas_list=alphas_list, n=365, period_length=pl)
        assert_(alphas_list=alphas_list, n=18474, period_length=pl)
