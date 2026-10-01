"""
The code in this module is provided as examples of how Threedx can be applied
onto fev-bench tasks. It is not tested. Consider it a starting point for your
own code, not as code supported as part of Threedx. It is only included in
Threedx to reduce the complexity of some tutorials in the Threedx documentation.
"""

import numpy as np
from fev import Task # type: ignore
from datasets import Dataset, DatasetDict # type: ignore
from typing import Callable
from .loss import Loss
from .threedx import Threedx

def _forecast_task_as_dataset_dict_per_window(
    using: Callable,
    task: Task,
    parameter_grid: None | np.ndarray = None,
    loss: None | Loss = None,
) -> list[DatasetDict]:
    """
    Forecast a fev-bench task.

    Iterate over data windows provided by the fev `Task`, forecast each time
    series and window provided by the `Task`, and return the set of quantile
    forecasts as a list of `datasets.DatasetDict` per `Task` window.

    The returned object has the structure expected by
    `Task.evaluation_summary()` and its `predictions_per_window` parameter.

    Parameters
    ----------
    using
        A function that can predict a numpy one-dimensional array and return
        an array of quantile forecasts, as for example
        `_predict_fev_quantiles_using_threedx()`.
    task
        A `fev.Task` with one target column.
    parameter_grid
        A parameter grid passed through to `Threedx`.
    loss
        A `Loss` passed through to `Threedx`.
    
    Returns
    -------
    DatasetDict
        A list of `datasets.DatasetDict`, each containing the quantile forecasts
        for all of the `Task` time series for one of the windows defined by
        `Task`.
        Can be passed to `Task.evaluation_summary()`.

    See Also
    --------
    _predict_fev_quantiles_using_threedx,
    _predict_fev_quantiles_using_latest_period
    """
    if len(task.target_columns) != 1:
        raise NotImplementedError(
            "Only tasks with one target column are supported."
        )
    if task.target_columns[0] != task.target:
        raise ValueError(
            "The `task.target` should be the column specified in" \
            "`task.target_columns`."
        )

    forecast_dataset_dict_per_window = []
    for window in task.iter_windows():
        past_data, _ = window.get_input_data()
        list_of_quantile_forecast_arrays = [
            using(
                # Some `fev` data sets contain missing values
                y=np.nan_to_num(ts[task.target], nan=0.0),
                quantile_levels=task.quantile_levels,
                horizon=task.horizon,
                period_length=task.seasonality,
                parameter_grid=parameter_grid,
                loss=loss

            ) for ts in past_data
        ]
        forecast_dataset_dict = \
            _convert_list_of_quantile_arrays_to_datasetdict(
                forecast_list=list_of_quantile_forecast_arrays,
                horizon=task.horizon,
                quantile_levels=task.quantile_levels,
                target_columns=task.target_columns,
            )
        forecast_dataset_dict_per_window.append(forecast_dataset_dict)

    return forecast_dataset_dict_per_window

def _convert_list_of_quantile_arrays_to_datasetdict(
    forecast_list: list[np.ndarray],
    horizon: int,
    quantile_levels: list[float],
    target_columns: list[str],
) -> DatasetDict:
    """
    Convert a list of quantile forecast arrays to a DatasetDict expected by fev.

    Reshapes numpy arrays representing quantile forecasts into the DatasetDict
    data structure expected by fev. The template for this construction is
    `fev.utils.convert_forecast_df_to_predictions()` which returns the same
    data structure but starts from a pandas DataFrame instead of from a list
    of numpy arrays.

    Parameters
    ----------
    forecast_list
        A list of two-dimensional numpy arrays. Each array represents quantile
        forecasts for a single time series. Each array has `horizon` columns
        and `len(quantile_levels)+1` rows. The first row is interpreted as point
        predictions, whereas the subsequent rows are interpreted as the
        quantiles listed in `quantile_levels`.
    horizon
        An integer number of steps to predict into the future. Should match the
        `Task.horizon`.
    quantile_levels
        The quantiles to be predicted as defined in `Task.quantile_levels`.
    target_columns
        All possible target columns to be predicted as defined in
        `Task.target_columns`.

    Returns
    -------
    DatasetDict
        A DatasetDict of quantile forecasts, as expected by fev during
        evaluation of forecasts for a forecast task. See
        `fev.Task.evaluation_summary()`.
    """
    quantile_names = ["predictions"] + [str(q) for q in quantile_levels]

    if forecast_list[0].shape[0] != len(quantile_names):
        raise ValueError(
            f"Arrays in `forecast_list` must have {len(quantile_levels)+1=}"
            f" rows. Got {forecast_list[0].shape[0]=}."
        )
    if forecast_list[0].shape[1] != horizon:
        raise ValueError(
            f"Arrays in `forecast_list` must have {horizon=}"
            f" columns. Got {forecast_list[0].shape[1]=}."
        )

    n_targets = len(target_columns)
    n_series = len(forecast_list)

    forecast_array = np.concatenate(forecast_list, axis=1)

    prediction_dict = {}
    for target_idx, target_name in enumerate(target_columns):
        col_data = {}
        for quantile_idx, quantile_name in enumerate(quantile_names):
            arr = forecast_array[quantile_idx, :].reshape(n_series, horizon)
            col_data[quantile_name] = arr[target_idx::n_targets]
        prediction_dict[target_name] = Dataset.from_dict(col_data)
    
    return DatasetDict(prediction_dict)

def _predict_fev_quantiles_using_latest_value(
    y: np.ndarray[tuple[int,], np.dtype[np.float64]],
    quantile_levels: list[float],
    horizon: int,
    period_length: int,
    parameter_grid: None | np.ndarray = None,
    loss: None | Loss = None,
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Predict a single time series using its latest observation (NAIVE)
    and return point predictions as quantiles as required by fev tasks.
    This function can be used as a fallback for other forecast methods when
    time series are too short for them.
    """
    y_pred = np.tile(y[-1], reps=horizon)
    return np.quantile(
        np.tile(y_pred, reps=100).reshape(100, horizon),
        q=[0.5]+quantile_levels,
        axis=0
    )

def _predict_fev_quantiles_using_latest_period(
    y: np.ndarray[tuple[int,], np.dtype[np.float64]],
    quantile_levels: list[float],
    horizon: int,
    period_length: int,
    parameter_grid: None | np.ndarray = None,
    loss: None | Loss = None,
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Predict a single time series using its latest period's observations (SNAIVE)
    and return point predictions as quantiles as required by fev tasks.
    This function can be used as a fallback for other forecast methods when
    time series are too short for them.
    """
    if y.size < period_length:
        return _predict_fev_quantiles_using_latest_value(
            y=y,
            horizon=horizon,
            period_length=period_length,
            quantile_levels=quantile_levels,
        )

    next_multiple_of_period_length = (horizon // period_length) + 1

    y_pred = np.tile(
        y[-period_length:],
        reps=next_multiple_of_period_length
    )[:horizon]

    return np.quantile(
        np.tile(y_pred, reps=100).reshape(100, horizon),
        q=[0.5]+quantile_levels,
        axis=0
    )

def _predict_fev_quantiles_using_threedx(
    y: np.ndarray[tuple[int,], np.dtype[np.float64]],
    quantile_levels: list[float],
    horizon: int,
    period_length: int,
    parameter_grid: np.ndarray[tuple[int, int], np.dtype[np.float64]],
    loss: Loss
) -> np.ndarray[tuple[int, int], np.dtype[np.float64]]:
    """
    Wrapper around `Threedx`, predicting a single time series and returning
    only an array of quantile forecasts as required by fev tasks.
    """
    if y.size <= 2*period_length:
        return _predict_fev_quantiles_using_latest_period(
            y=y,
            horizon=horizon,
            period_length=period_length,
            quantile_levels=quantile_levels,
        )

    # The amount by which the context length is shortened is a modeling choice.
    context_length = min(period_length*10, y.size)

    model = Threedx(
        period_length=period_length,
        parameter_grid=parameter_grid
    )
    model = model.fit(
        y=y[-context_length:],
        loss=loss
    )
    sample_paths = model.predict(
        horizon=horizon,
        n_samples=2501,
        observation_driven=True,
        draw=None,
    )

    return np.quantile(
        sample_paths,
        q=[0.5]+quantile_levels,
        axis=0
    )
