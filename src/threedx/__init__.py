from .weights import (
    weights_exponential,
    weights_seasonal,
    weights_seasonal_decay
)

def main() -> None:
    print(f"{weights_exponential(alpha=0.5, n=5)=}")
    print(f"{weights_seasonal(alpha=0.5, n=12, period_length=12)=}")
    print(f"{weights_seasonal_decay(alpha=0.5, n=17, period_length=7)=}")
