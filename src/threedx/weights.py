import numpy as np

def _validate_positive_int(n):
    if not isinstance(n, int):
        raise TypeError("`n` must have type int.")
    if n <= 0:
        raise ValueError("`n` must be larger than zero.")

def _validate_alpha(alpha):
    if not isinstance(alpha, float):
        raise TypeError("`alpha` must have type float.")
    if alpha < 0.0 or alpha > 1.0:
        raise ValueError(
            f"`alpha` must be a float in the range of [0., 1.], got {alpha}."
        )

def _validate_alphas(alphas):
    if not isinstance(alphas, np.ndarray[tuple[int], np.dtype[np.float64]]):
        raise TypeError("`n` must be an float64 ndarray of shape (int, ).")
    if (np.sum(alphas < 0.) > 0) | (np.sum(alphas > 1.) > 0):
        raise ValueError(
            f"Each `alpha` in `alphas` must be a float in the range of [0., 1.]."
        )


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
    Derive within-period exponential weights

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

    if n < period_length and alpha == 1.0:
        # Instead of returning an all-zero vector, return something reasonable
        # that won't result in np.nan values later.
        return np.ones(n, dtype=np.float64) / n

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
    """
    Derive cross-period exponential weights

    Returns a numpy vector for which each value within a period has the same
    weight and weights increase exponentially across periods.

    In a sense, this function repeats every value returned by
    `weights_exponential()` each `period_length` times.

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
    weights_exponential, weights_seasonal, weights_threedx

    Examples
    --------
    >>> import numpy as np
    >>> np.round(weights_seasonal_decay(alpha=0.5, n=20, period_length=7), 4)
    array([
                0.0208, 0.0208, 0.0208, 0.0208, 0.0208, 0.0208,
        0.0417, 0.0417, 0.0417, 0.0417, 0.0417, 0.0417, 0.0417,
        0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833, 0.0833
    ])

    >>> np.round(weights_seasonal_decay(alpha=1.0, n=30, period_length=12), 3)
    array([
        0.   , 0.   , 0.   , 0.   , 0.   , 0.   ,
        0.   , 0.   , 0.   , 0.   , 0.   , 0.   ,
        0.   , 0.   , 0.   , 0.   , 0.   , 0.   ,
        0.083, 0.083, 0.083, 0.083, 0.083, 0.083,
        0.083, 0.083, 0.083, 0.083, 0.083, 0.083
    ])

    >>> weights_seasonal_decay(alpha=1.0, n=4, period_length=12)
    array([0.25, 0.25, 0.25, 0.25])

    >>> weights_seasonal_decay(alpha=0.0, n=4, period_length=12)
    array([0.25, 0.25, 0.25, 0.25])

    >>> weights_seasonal_decay(alpha=0.0, n=10, period_length=4)
    array([0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1])
    """
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

def weights_threedx(
    alpha: float,
    alpha_seasonal: float,
    alpha_seasonal_decay: float,
    n: int,
    period_length: int
) -> np.ndarray[tuple[int], np.dtype[np.float64]]:
    """
    Derive three-dimensional exponential weights

    Parameters
    ----------
    alpha
        A scalar float between 0 and 1 that determines how quickly the weights
        decay.
    alpha_seasonal
        A scalar float between 0 and 1 that determines the within-period
        weights.
    alpha_seasonal_decay
        A scalar float between 0 and 1 that determines how quickly the
        cross-period weights decay.
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
    weights_exponential, weights_seasonal, weights_seasonal_decay

    Examples
    --------
    >>> import numpy as np
    >>> np.round(weights_threedx(
            alpha=0.0,
            alpha_seasonal=0.5,
            alpha_seasonal_decay=0.1,
            n=21,
            period_length=7
        ), 4)
    array([
        0.1087, 0.0543, 0.0272, 0.0136, 0.0136, 0.0272, 0.0543,
        0.1208, 0.0604, 0.0302, 0.0151, 0.0151, 0.0302, 0.0604,
        0.1342, 0.0671, 0.0335, 0.0168, 0.0168, 0.0335, 0.0671
    ])

    >>> np.round(weights_threedx(
            alpha=0.1,
            alpha_seasonal=0.8,
            alpha_seasonal_decay=0.0,
            n=21,
            period_length=7
        ), 4)
    array([
        0.0771, 0.0171, 0.0038, 0.0008, 0.0009, 0.0052, 0.029 ,
        0.1611, 0.0358, 0.008 , 0.0018, 0.002 , 0.0109, 0.0606,
        0.3369, 0.0749, 0.0166, 0.0037, 0.0041, 0.0228, 0.1268
    ])

    >>> weights_threedx(
            alpha=0.0,
            alpha_seasonal=0.0,
            alpha_seasonal_decay=0.0,
            n=4,
            period_length=12
        )
    array([0.25, 0.25, 0.25, 0.25])

    >>> weights_threedx(
            alpha=0.0,
            alpha_seasonal=1.0,
            alpha_seasonal_decay=1.0,
            n=12,
            period_length=4
        )
    array([
        0., 0., 0., 0.,
        0., 0., 0., 0.,
        1., 0., 0., 0.
    ])

    >>> np.round(weights_threedx(
            alpha=0.0,
            alpha_seasonal=1.0,
            alpha_seasonal_decay=0.0,
            n=12,
            period_length=4
        ), 3)
    array([
        0.333, 0.   , 0.   , 0.   ,
        0.333, 0.   , 0.   , 0.   ,
        0.333, 0.   , 0.   , 0.   
    ])

    >>> np.round(weights_threedx(
            alpha=0.0,
            alpha_seasonal=0.0,
            alpha_seasonal_decay=0.5,
            n=12,
            period_length=4
        ), 3)
    array([
        0.036, 0.036, 0.036, 0.036,
        0.071, 0.071, 0.071, 0.071,
        0.143, 0.143, 0.143, 0.143
    ])
    """
    _validate_alpha(alpha=alpha)
    if alpha == 1.0:
        return weights_exponential(alpha=1.0, n=n)

    weights = \
        weights_exponential(
            alpha = alpha,
            n = n
        ) * \
        weights_seasonal(
            alpha = alpha_seasonal,
            n = n,
            period_length = period_length
        ) * \
        weights_seasonal_decay(
            alpha = alpha_seasonal_decay,
            n = n,
            period_length = period_length
        )
    
    weights = weights / weights.sum()
    return weights

def _sum_each_row_to_one(
    weights: np.ndarray[tuple[int, int], np.dtype[np.float64]]
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Standardize rows to sum to one

    Takes a two-dimensional numpy array, ensures the rows each sum to
    one, and returns it.
    """
    row_sums = weights.sum(axis = 1)
    weights = weights / np.tile(row_sums, (weights.shape[1], 1)).T
    return weights

def _weights_exponential_vec(
    alphas: np.ndarray[tuple[int], np.dtype[np.float64]],
    n: int
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Returns a matrix where each row consists of exponential weights, and rows
    differ by their smoothing factor.

    Parameters
    ----------
    alphas
        A one-dimensional numpy array of floats in the range from 0 to 1. Its
        shape determines the number of rows of the returned matrix.
    n
        The number of weights to create, usually the number of observations in
        a time series with which the weights are aligned.
        Corresponds to the number of columns.
    
    Returns
    -------
    A numpy array of shape `(alphas.size, n)` with float values between 0 and 1
    that sum up to 1.
    """
    # m_an_: A matrix with a rows and n columns, where a=alphas.size and n=n
    
    m_an_alphas = np.tile(alphas, (n, 1)).T
    m_an_one_minus_alphas = np.tile((1-alphas), (n, 1)).T
    m_an_exponents = np.tile(np.arange(n)[::-1], (alphas.size, 1))

    m_an_weights = (m_an_one_minus_alphas ** m_an_exponents) * m_an_alphas
    m_an_weights[m_an_alphas == 0, ] = 1.0 / n

    m_an_weights = _sum_each_row_to_one(m_an_weights)
    return m_an_weights

def _weights_seasonal_vec(
    alphas: np.ndarray[tuple[int], np.dtype[np.float64]],
    n: int,
    period_length: int
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Returns a matrix where each row consists of seasonal exponential weights,
    and rows differ by their smoothing factor.

    Parameters
    ----------
    alphas
        A one-dimensional numpy array of floats in the range from 0 to 1. Its
        shape determines the number of rows of the returned matrix.
    n
        The number of weights to create, usually the number of observations in
        a time series with which the weights are aligned.
        Corresponds to the number of columns.
    
    Returns
    -------
    A numpy array of shape `(alphas.size, n)` with float values between 0 and 1
    that sum up to 1.
    """
    seasons = np.ceil(n / period_length).astype(int)

    # The construction of weights works by first constructing weights for a
    # single period and then repeating that period to get to `n` weights.

    # To construct a single period, we need to know how many observations are on
    # the left half and how many are on the right half. The left half breaks
    # ties in the middle when the `period_length` is odd.
    n_left = (
        np.ceil(period_length / 2) + # 3 when period_length is 7, 6 when 12, ...
        (1 - np.ceil(period_length % 2)) # 0 when 7, 1 when 12, ...
    ).astype(int).item()

    # 4 when period_length is 7, 5 when period_length is 12, ...
    length_needed_right = period_length - n_left

    weights_base = _weights_exponential_vec(alphas=alphas, n=n_left)
    # The maximum weight is always at first position in the period (i.e., it's
    # `period_length` away from the most recent observation). Thus drop the
    # maximum for `weights_right`, but keep it for `weights_left`.
    weights_right = weights_base[::, :-1] # all but last
    weights_left = weights_base[::, ::-1] # reverse order

    length_right = weights_right.shape[1]
    weights = np.concatenate((
        weights_left,
        weights_right[::, (length_right - length_needed_right):length_right]
    ), axis = 1)

    # The `dummy` matrix is used to repeat the individual period matrix
    # `weights` as often as necessary to return `n` columns.
    dummy = np.tile(np.eye(period_length), reps = (1, seasons))
    dummy = dummy[::, (seasons * period_length - n):(seasons * period_length)]

    weights = weights @ dummy # Perhaps more efficient than a for loop?

    # If less than an entire period, and any alpha in alphas is equal to one,
    # overwrite the weights so that they turn out to be uniform. This follows
    # the behavior of `weights_seasonal()` and avoids np.nan when resulting 
    # weights are combined with weights from `_weights_exponential_vec()`.
    if n < period_length and (np.sum(alphas == 1.) > 0):
        weights[alphas == 1., ::] = 1. # will be standardized in the next line

    weights = _sum_each_row_to_one(weights)

    return weights