from .weights import weights_exponential

def main() -> None:
    print(f"{weights_exponential(alpha=0.5, n=5)=}")
