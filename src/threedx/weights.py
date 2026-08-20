import numpy as np

def _validate_alpha(alpha):
    if not isinstance(alpha, float):
        raise TypeError("`alpha` must have type float.")
    if alpha < 0.0 or alpha > 1.0:
        raise ValueError(
            f"`alpha` must be a float in the range of [0., 1.], got {alpha}."
        )

def _validate_positive_int(n):
    if not isinstance(n, int):
        raise TypeError("`n` must have type int.")
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
    _validate_positive_int(n=n)

    if alpha == 0:
        return np.repeat(1. / n, n)

    weights = ((1 - alpha) ** np.arange(n)[::-1]) * alpha # in reverse order
    weights = weights / weights.sum()
    return weights

def weights_seasonal(
    alpha: float,
    n: int,
    period_length: int
) -> np.ndarray[tuple[int], np.dtype[np.float64]]:
    """
    Derive within-season exponential weights

    A larger value of `alpha` will assign larger weights to
    observations closer to the most recent observation in terms of its relative
    position in the seasonal period.

    For example, if `period_length=12`, then the 12th most recent observation
    has the highest possible weight, and the same weight as the 24th most recent
    observation, and so on. The most recent observation has the second highest
    weight and the same weight as the 11th and 13th most recent observations.

    The weights are symmetric within a period, and each period is equal.

    Parameters
    ----------
    alpha
        A scalar float between 0 and 1 that determines how quickly the weights
        decay.
    n
        The number of weights to create, usually the number of observations in
        a time series with which the weights are aligned.
    period_length
        Determines the length of the seasonal pattern. For annual seasonality in
        monthly observations, use 12. For weekly seasonality in daily
        observations, use 7. And so on.
    
    Returns
    -------
    A numpy array (vector) of `n` values between 0 and 1 that sum up to 1.

    See Also
    --------
    weights_exponential, weights_seasonal_decay, weights_threedx

    Examples
    --------
    >>> weights_seasonal(alpha=0.5, n=7, period_length=7)
    array([0.36363636, 0.18181818, 0.09090909, 0.04545455, 0.04545455,
           0.09090909, 0.18181818])

    >>> import numpy as np
    >>> np.round(weights_seasonal(alpha=0.9, n=16, period_length=7), 3)
    array([0.004, 0.039,
           0.392, 0.039, 0.004, 0.   , 0.   , 0.004, 0.039,
           0.392, 0.039, 0.004, 0.   , 0.   , 0.004, 0.039])

    >>> weights_seasonal(alpha=1.0, n=4, period_length=4)
    array([1., 0., 0., 0.])

    >>> weights_seasonal(alpha=0.0, n=4, period_length=4)
    array([0.25, 0.25, 0.25, 0.25])
    """
    _validate_alpha(alpha=alpha)
    _validate_positive_int(n=n)
    _validate_positive_int(n=period_length)

    if alpha == 1.0 and n < period_length:
        return np.zeros(shape=(n,), dtype=np.float64)

    n_left = (
        np.ceil(period_length / 2) + (1 - np.ceil(period_length % 2))
    ).astype(int).item() # convert the resulting np int scalar to python int

    length_needed_right = period_length - n_left

    weights_left = weights_exponential(alpha = alpha, n = n_left)[::-1] # in reverse order
    weights_right = weights_exponential(alpha = alpha, n = n_left)[:-1] # not the last value

    length_right = weights_right.shape[0]

    weights___period = np.concatenate((
        weights_left,
        weights_right[(length_right - length_needed_right):length_right]
    ))

    weights = np.resize(weights___period[::-1], n)[::-1]

    weights = weights / weights.sum()
    return weights

def weights_seasonal_decay(
    alpha: float,
    n: int,
    period_length: int
) -> np.ndarray[tuple[int], np.dtype[np.float64]]:
    _validate_alpha(alpha=alpha)
    _validate_positive_int(n=n)
    _validate_positive_int(n=period_length)

    # Get as many weights as there are periods (n/period_length)
    weights__seasons = weights_exponential(
        alpha = alpha,
        n = np.ceil(n / period_length).astype(int).item()
    )

    # Assign each weight to one of the periods, fill each period with it
    weights = np.repeat(weights__seasons, period_length)
    weights = weights[(weights.size - n):(weights.size)]
    weights = weights / weights.sum()
    return weights