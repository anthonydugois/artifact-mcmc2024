import numpy as np
import pandas as pd


def distance_nonzero(arr):
	nz = np.nonzero(arr)[0]
	dist = np.full(arr.size, np.NaN)
	dist[nz[:-1]] = np.diff(nz) - 1
	
	return dist


if __name__ == "__main__":
	import argparse

	parser = argparse.ArgumentParser(description="Transform a pre-calculation table in usable tabular data.")
	parser.add_argument("n", type=int, help="Vector length")
	parser.add_argument("input", type=str, help="Pre-calculation table")
	parser.add_argument("output", type=str, help="Data output file")
	parser.add_argument("--verbose", dest="verbose", action="store_true")
	args = parser.parse_args()

	data = np.load(args.input)

	dist = pd.DataFrame([distance_nonzero(data[args.n, j]) for j in range(0, data.shape[1])])
	dist.reset_index(inplace=True)
	dist = dist.melt("index")
	dist = dist.rename(columns={"index": "N", "variable": "S", "value": "dist"})

	df = pd.DataFrame(data[args.n])
	df.reset_index(inplace=True)
	df = df.melt("index")
	df = df.rename(columns={"index": "N", "variable": "S", "value": "count"})
	
	df = df.merge(dist, how="left", left_on=["N", "S"], right_on=["N", "S"])

	df.to_csv(args.output, index=False)

	if args.verbose:
		print("Written to", args.output)

