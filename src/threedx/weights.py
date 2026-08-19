import numpy as np

def _validate_alpha(alpha):
    if not isinstance(alpha, float):
        raise TypeError("`alpha` must have type float.")
    if alpha < 0.0 or alpha > 1.0:
        raise ValueError(
            f"`alpha` must be a float in the range of [0., 1.], got {alpha}."
        )

def _validate_n(n):
    if not isinstance(n, int):
        raise TypeError("`alpha` must have type int.")
    if n <= 0:
        raise ValueError("`n` must be larger than zero.")

def weights_exponential(
    alpha: float,
    n: int
) -> np.ndarray[tuple[int], np.dtype[np.float64]]:
    """
    Derive exponential weights

    A larger value of `alpha` will assign larger weights to more recent
    observations.

    Parameters
    ----------
    alpha
        A scalar float between 0 and 1 that determines how quickly the weights
        decay.
    n
        The number of weights to create, usually the number of observations in
        a time series with which the weights are aligned.
    
    Returns
    -------
    A monotonically increasing numpy array (vector) of `n` values between 0 and
    1 that sum up to 1.

    See Also
    --------
    weights_seasonal, weights_seasonal_decay, weights_threedx

    Examples
    --------
    >>> weights_exponential(alpha=0.25, n=5)
    array([0.10371319, 0.13828425, 0.184379, 0.24583867, 0.32778489])

    >>> weights_exponential(alpha=1, n=7)
    array([0., 0., 0., 0., 0., 0., 1.])

    >>> weights_exponential(alpha=0, n=4)
    array([0.25, 0.25, 0.25, 0.25])
    """
    _validate_alpha(alpha=alpha)
    _validate_n(n=n)

    if alpha == 0:
        return np.repeat(1. / n, n)

    weights = ((1 - alpha) ** np.arange(n)[::-1]) * alpha # in reverse order
    weights = weights / weights.sum()
    return weights