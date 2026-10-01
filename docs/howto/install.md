# Install Threedx

Install Threedx from Github. Use either `pip`...

```bash
uv pip install "git+https://github.com/timradtke/threedx"
```

... or use [uv](https://docs.astral.sh/uv):

```bash
uv pip install "git+https://github.com/timradtke/threedx"
```

From within a [python project managed with uv](https://docs.astral.sh/uv/guides/projects/), you can add `threedx` (showing
SSH as an alternative to HTTPS authentication):

```bash
uv add git+ssh://git@github.com/timradtke/threedx
```

The following syntax lets you install Threedx along with optional dependencies (e.g. `matplotlib` for graphics):

```bash
uv add "threedx[graphics] @ git+ssh://git@github.com/timradtke/threedx"
```

The same works when using pip.