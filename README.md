# threedx

Threedx provides an interface for interpretable probabilistic forecasts that puts you in control.

If you believe in choosing the right model for the job, you'll love that Threedx let's you pick the loss function at training time. And at prediction time, Threedx either forecasts by sampling from past observations, or via an innovations generating process you define.

Threedx could be the right fit for you, if...

- your users require interpretability,
- your use case benefits from a custom loss function,
- your data is dominated by seasonality while trends are weak,
- your data is intermittent, non-negative, or otherwise non-normal.

## Install threedx

Install Threedx from Github. Use either `pip`...

```bash
uv pip install "git+https://github.com/timradtke/threedxpy"
```

... or use [`uv`](https://docs.astral.sh/uv/):

```bash
uv pip install "git+https://github.com/timradtke/threedxpy"
```

From within a [python project managed with `uv`](https://docs.astral.sh/uv/guides/projects/), you can add `threedxpy` (showing
SSH as an alternative to HTTPS authentication):

```bash
uv add git+ssh://git@github.com/timradtke/threedxpy
```

The following syntax lets you install Threedx along with optional dependencies (e.g. `matplotlib` for graphics):

```bash
uv add "threedx[graphics] @ git+ssh://git@github.com/timradtke/threedxpy"
```

The same works when using pip.