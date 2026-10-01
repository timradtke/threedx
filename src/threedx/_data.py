import numpy as np

def _load_y(): # type: ignore
    """
    Create example time series for documentation.
    """
    n_obs = 65
    period_length = 12
    rng = np.random.default_rng(seed=512)

    _y = rng.poisson(
        lam=np.maximum(
            0.1,
            1 + 10 * np.sin(2 * np.pi * np.arange(6, 6 + n_obs) / period_length)
        ),
        size=n_obs
    )

    y = _y[:(n_obs-period_length)]
    y_future = _y[(n_obs-period_length):]

    return y, y_future
