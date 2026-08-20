import numpy as np
from .weights import (
    weights_exponential,
    weights_seasonal,
    weights_seasonal_decay,
    weights_threedx
)

def main() -> None:
    print(f"{weights_exponential(alpha=0.5, n=5)=}")
    print(f"{weights_seasonal(alpha=0.5, n=12, period_length=12)=}")
    print(f"{weights_seasonal_decay(alpha=0.5, n=17, period_length=7)=}")
    print(
        f"""
        {np.round(weights_threedx(
            alpha=0.0,
            alpha_seasonal=0.5,
            alpha_seasonal_decay=0.1,
            n=17,
            period_length=7
        ), 4)=}
        """
    )
