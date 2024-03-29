import numpy as np
import pandas as pd

from munu_var import random_vecs_cv


if __name__ == "__main__":
	import argparse

	parser = argparse.ArgumentParser(description="Generate a set of random vectors and get the frequency of each value.")
	parser.add_argument("n", type=int, help="Vector length")
	parser.add_argument("N", type=int, help="Sum of elements")
	parser.add_argument("CV", type=float, help="Coefficient of variation of elements")
	parser.add_argument("eps", type=float, help="Allowed error on CV")
	parser.add_argument("alpha", type=int, help="Minimum value of each element")
	parser.add_argument("beta", type=int, help="Maximum value of each element")
	parser.add_argument("input", type=str, help="Pre-calculation table")
	parser.add_argument("output", type=str, help="Data output file")
	parser.add_argument("--count", dest="count", type=int, default=1)
	parser.add_argument("--seed", dest="seed", type=int, default=1)
	parser.add_argument("--verbose", dest="verbose", action="store_true")
	args = parser.parse_args()

	data = random_vecs_cv(np.load(args.input), args.n, args.N, args.CV, args.eps, args.eps, args.alpha, args.beta, args.count, args.seed)
	values = np.arange(args.N + 1, dtype=np.int64)
	freqs = np.bincount(data.flatten(), minlength=args.N + 1)
	norm_freqs = freqs / freqs.max()
	
	df = pd.DataFrame({
		"n": args.n,
		"N": args.N,
		"CV": args.CV,
		"eps": args.eps,
		"alpha": args.alpha,
		"beta": args.beta,
		"value": values,
		"frequency": freqs,
		"norm_frequency": norm_freqs})

	df.to_csv(args.output, index=False)
	
	if args.verbose:
		print("Written to", args.output)

