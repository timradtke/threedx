from .weights import weights_exponential, weights_seasonal

def main() -> None:
    print(f"{weights_exponential(alpha=0.5, n=5)=}")
    print(f"{weights_seasonal(alpha_seasonal=0.5, n=12, period_length=12)=}")
