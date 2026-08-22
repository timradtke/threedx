import numpy as np
import numpy.testing as npt
from threedx.weights import weights_exponential, _weights_exponential_vec

def test_returns_matrix_where_rows_are_like_weights_exponential():
    def assert_(alphas_list, n):
        alphas = np.array(alphas_list, dtype=np.float64)
        m_ = _weights_exponential_vec(alphas=alphas, n=n)
        for i in range(len(alphas_list)):
            npt.assert_almost_equal(
                actual=m_[i, ],
                desired=weights_exponential(alpha=alphas_list[i], n=n)
            )

    alphas_list = [
        0.0, 0.0000000001, 0.1, 0.2, 0.284, 0.40003884, 0.8233, 0.999999, 1.0
    ]
    assert_(alphas_list=alphas_list, n=1)
    assert_(alphas_list=alphas_list, n=5)
    assert_(alphas_list=alphas_list, n=7)
    assert_(alphas_list=alphas_list, n=12)
    assert_(alphas_list=alphas_list, n=14)
    assert_(alphas_list=alphas_list, n=24)
    assert_(alphas_list=alphas_list, n=365)
    assert_(alphas_list=alphas_list, n=18474)
