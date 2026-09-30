import numpy as np
from .weights import _validate_positive_int

def initialize_edge_case_parameters(
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Initialize a `threedx` parameter grid to be searched during model training.
    
    Returns
    -------
    array_like
        A numpy two-dimensional array with three columns and `size` rows, filled
        with floating values in the range [0,1].

    See Also
    --------
    initialize_parameters_at_random, initialize_parameters_in_grid

    Examples
    --------
    >>> initialize_edge_case_parameters()
    array([[1., 0., 0.],  # Naive
           [0., 0., 0.],  # Mean
           [0., 1., 0.],  # Seasonal Average
           [0., 1., 1.],  # Seasonal Naive
           [0., 0., 1.]]) # Latest Period Average
    """
    # 1) Naive
    # 2) Mean
    # 3) Seasonal Average
    # 4) Seasonal Naive
    # 5) Latest Period Average
    a_edge = np.array([1.0, 0.0, 0.0, 0.0, 0.0], dtype=np.float64)
    b_edge = np.array([0.0, 0.0, 1.0, 1.0, 0.0], dtype=np.float64)
    c_edge = np.array([0.0, 0.0, 0.0, 1.0, 1.0], dtype=np.float64)

    grid = np.stack((a_edge, b_edge, c_edge), axis=0).T

    return grid

def initialize_parameters_at_random(
    size: int = 1000,
    seed: int = None,
    include_edge_cases: bool = True,
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Initialize a `threedx` parameter grid to be searched during model training.

    Parameters
    ----------
    size
        An integer number of parameter combinations to generate.
    seed
        An integer seed used during random number generation, default `None`.
    include_edge_cases
        A boolean indicating whether the parameter grid should include the
        edge cases where parameters equal 0 or 1 and result in Naive, Mean,
        Seasonal Naive, or the Latest Period Average methods. `True` by
        default.
    
    Returns
    -------
    array_like
        A numpy two-dimensional array with three columns and `size` rows, filled
        with floating values in the range [0,1].

    See Also
    --------
    initialize_edge_case_parameters, initialize_parameters_in_grid

    Examples
    --------
    >>> import numpy as np
    >>> np.round(
            initialize_parameters_at_random(size=6, include_edge_cases=False)
        , 2)
    array([[0.87, 0.56, 0.06],
           [0.73, 0.49, 0.25],
           [0.03, 0.24, 0.85],
           [0.38, 0.59, 0.43],
           [0.04, 0.33, 0.17],
           [0.7 , 0.14, 0.35]])

    >>> np.round(
            initialize_parameters_at_random(size=6, include_edge_cases=True)
        , 2)
    array([[1.  , 0.  , 0.  ],
           [0.  , 0.  , 0.  ],
           [0.  , 1.  , 0.  ],
           [0.  , 1.  , 1.  ],
           [0.  , 0.  , 1.  ],
           [0.46, 0.27, 0.01]])
    """
    _validate_positive_int(n=size, name="size")

    grid_of_edge_cases = initialize_edge_case_parameters()
    size_fits_edge_cases = size >= grid_of_edge_cases.shape[0]

    if include_edge_cases and size_fits_edge_cases:
        size = size - grid_of_edge_cases.shape[0]

    rng = np.random.default_rng(seed=seed)
    alphas = rng.beta(a=1, b=2, size=size)
    alphas_seasonal = rng.beta(a=1, b=1, size=size)
    alphas_seasonal_decay = rng.beta(a=1, b=1, size=size)

    grid = np.stack((alphas, alphas_seasonal, alphas_seasonal_decay), axis=0).T

    if include_edge_cases and size_fits_edge_cases:
        grid = np.vstack((grid_of_edge_cases, grid))

    return grid

def initialize_parameters_in_grid(
    base_size: int = 10
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Initialize a `threedx` parameter grid to be searched during model training.

    Parameters
    ----------
    base_size
        Defines an equally spaced grid of three parameters. The number of
        parameter combinations will be `(base_size**3)`, i.e. if `base_size=3`,
        the returned array will have 27 rows.
        Choosing `base_size=1_000` would result in 1,000,000,000 rows.
    
    Returns
    -------
    array_like
        A numpy two-dimensional array with three columns and `base_size**3`
        rows, filled with floating values in the range [0,1].

    See Also
    --------
    initialize_edge_case_parameters, initialize_parameters_at_random

    Examples
    --------
    >>> initialize_parameters_in_grid(base_size=3)
    array([[1. , 0. , 0. ],
           [0. , 0. , 0. ],
           [0. , 1. , 0. ],
           [0. , 1. , 1. ],
           [0. , 0. , 1. ],
           [0. , 0. , 0.5],
           [0. , 0.5, 0. ],
           [0. , 0.5, 0.5],
           [0. , 0.5, 1. ],
           [0. , 1. , 0.5],
           [0.5, 0. , 0. ],
           [0.5, 0. , 0.5],
           [0.5, 0. , 1. ],
           [0.5, 0.5, 0. ],
           [0.5, 0.5, 0.5],
           [0.5, 0.5, 1. ],
           [0.5, 1. , 0. ],
           [0.5, 1. , 0.5],
           [0.5, 1. , 1. ],
           [1. , 0. , 0.5],
           [1. , 0. , 1. ],
           [1. , 0.5, 0. ],
           [1. , 0.5, 0.5],
           [1. , 0.5, 1. ],
           [1. , 1. , 0. ],
           [1. , 1. , 0.5],
           [1. , 1. , 1. ]])
    """
    _validate_positive_int(n=base_size, name="base_size")

    alphas_base = np.linspace(
        start=0.0,
        stop=1.0,
        num=base_size,
        endpoint=True,
        dtype=np.float64
    )

    alphas = np.tile(alphas_base, reps=base_size)
    alphas_seasonal = np.repeat(alphas_base, repeats=base_size)

    grid = np.column_stack(
        (
            np.tile(alphas, reps=base_size),
            np.tile(alphas_seasonal, reps=base_size),
            np.repeat(alphas_base, repeats=base_size*base_size)
        )
    )

    if base_size >= 2:
        return _shuffle_edge_cases_to_top(grid=grid)

    return grid

def _shuffle_edge_cases_to_top(
        grid: np.ndarray[tuple[int, int], np.dtype[np.float64]]
    ) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Shuffle edge cases to top of parameter grid.

    This ensures that the edge cases defined by
    `initialize_edge_case_parameters()` are included in the result grid and
    appear in the first rows. Edge cases are not duplicated if they already
    existed.

    Note: Do not use this function if `grid` can contain duplicates.
    Note: Do not use this function if `grid` does not contain all edge cases but
          you require that the output shape is identical to the input shape.
    """
    # If np.unique() didn't sort, we could simply stack the edge cases on top
    # and run unique to avoid creating duplicates. But even with `sorted=False`,
    # np.unique() can change the order of the rows compared to the original
    # array.

    # Implement a workaround: First, stack and count to identify the rows that
    # were duplicates. Then, stack again, dropping the rows that were duplicates
    # and thus allowing a safe stacking of edge cases that won't create new
    # duplicates and thus does not require np.unique().

    # Note: This approach makes sense only if `grid` is guaranteed to not
    # contain any duplicates beforehand. Otherwise the result won't contain all
    # rows that existed in `grid`.

    grid_of_edge_cases = initialize_edge_case_parameters()
    
    grid_unique, counts = np.unique(
        np.vstack((grid_of_edge_cases, grid)),
        axis=0,
        return_counts=True
    )

    grid_with_edge_cases_atop = np.vstack((
        grid_of_edge_cases,
        grid_unique[counts == 1, :]
    ))

    return grid_with_edge_cases_atop

def _validate_parameter_grid(
        grid: np.ndarray[tuple[int, int], np.dtype[np.float64]]
    ) -> None:
    """
    Validate that a parameter grid fulfills Threedx expectations.
    """
    if not isinstance(grid, np.ndarray):
        raise TypeError("The parameter grid needs to be a numpy ndarray.")
    if len(grid.shape) != 2:
        raise ValueError("The parameter grid has to have a two dimensions.")
    if grid.shape[0] < 1:
        raise ValueError("The parameter grid has to have at least one row.")
    if grid.shape[1] != 3:
        raise ValueError("The parameter grid has to have three columns.")
    if not np.all(grid <= 1.0):
        raise ValueError(
            "All parameters in the parameter grid need to less than "
            "or equal to 1."
        )
    if not np.all(grid >= 0.0):
        raise ValueError(
            "All parameters in the parameter grid need to greater than "
            "or equal to 0."
        )

def _grid_contains_triple_vector(
    triple: np.ndarray[tuple[int], np.dtype[np.float64]],
    grid: np.ndarray[tuple[int, int], np.dtype[np.float64]]
) -> bool:
    """
    Does the parameter grid contain a specific parameter triple?

    Helper function that is useful during testing.
    """
    counter = 0
    for i in range(grid.shape[0]):
        counter += np.array_equal(triple, grid[i,:])
    return counter == 1