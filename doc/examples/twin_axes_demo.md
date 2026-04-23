title: Twin Axes Demo
---

# Twin Axes Demo

Source: [twin_axes_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/twin_axes_demo/twin_axes_demo.f90)

Demonstrates `twinx` and `twiny` for multiple axes on one figure.

The demo plots three synthetic time series and assigns each to a different axis:

- Left y-axis: primary signal (temperature-like)
- Right y-axis: secondary signal with log scaling
- Top x-axis: cumulative index with log scaling

Run the example via:

Outputs are written to `output/example/fortran/twin_axes_demo/` in PNG and ASCII
formats.

## Files

- `twin_axes_demo.f90` - Source code
- Run the example to populate `output/example/fortran/twin_axes_demo/`

## Running

```bash
make example ARGS="twin_axes_demo"
```

## Output

Run this example to generate plots and other media assets.

