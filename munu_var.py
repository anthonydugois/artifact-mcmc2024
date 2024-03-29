import numpy as np
import numba as nb
import pandas as pd
import math


@nb.jit(nopython=True, parallel=True, cache=True)
def mem_table_cv(n, _N, alpha, beta):
	"""Compute the pre-calculation table with constraint on the coefficient of variation

	Compute the full pre-calculation table needed for the random vector generation with the
	following constraints: `n` is the vector length, `N` is the sum of elements, `N^2` is
	the sum of squared elements and `alpha` (resp. `beta`) is the lower bound (resp. upper
	bound) of each element.

	**Warning**: the entire table may not fit in memory. Be careful about `n` and `N`, as
	the needed space in bits is of order `O(n * N^3)`.
	"""

	N = _N - n * alpha  # Needed for translation
	S = N * N  # Compute up to the maximum sum of squares

	# Initialize the table. There are n + 1 elements on the
	# first dimension in order to ensure a convenient way to access
	# each element (e.g., g_{1,0,0} corresponds to `T[1,0,0]`,
	# not `T[0,0,0]`).
	T = np.zeros((n + 1, N + 1, S + 1), np.float64)

	# Initial conditions
	for j in range(0, min(beta - alpha, N) + 1):
		if S >= j * j:
			T[1, j, j * j] = 1

	for i in range(2, n + 1):  # i = 1 has already been computed
		for j in nb.prange(0, N + 1):  # Parallel loop; needed to help Numba
			for k in range(0, S + 1):
				for x in range(0, min(beta - alpha, j) + 1):
					if k >= x * x:
						T[i, j, k] += T[i - 1, j - x, k - x * x]

	return T


def random_vec_cv(rng, data, n, _N, CV, eps_left, eps_right, alpha, beta):
	"""Generate a random uniform vector with constraint on the coefficient of variation"""

	N = _N - n * alpha  # Needed for translation

	# First, let's transform the coefficient of variation and allowed
	# error into an usable sum of squares.
	CVmin = (1 - eps_left) * CV
	CVmax = (1 + eps_right) * CV
	Smin = math.ceil(_N * _N / n * CVmin * CVmin + N * N / n)
	Smax = math.floor(_N * _N / n * CVmax * CVmax + N * N / n)

	sigma = np.arange(Smin, Smax + 1, dtype=np.int64)
	num = data[n, N, sigma]
	total = num.sum()

	if total <= 0:
		raise Exception(f"The provided coefficient of variation (`CV`={CV}) and allowed error (`eps`={eps_left}) are not compatible with other parameters.")

	S = rng.choice(sigma, p=num / total)  # Pick a random uniform sum of squares

	# Second, generate the random vector.
	vec = np.zeros(n, dtype=np.int64)
	x, y = 0, 0
	for i in range(1, n):
		k = np.arange(0, min(beta - alpha, N - x) + 1, dtype=np.int64)
		num = data[n - i, N - x - k, S - y - k * k]
		total = data[n - i + 1, N - x, S - y]

		j = rng.choice(k, p=num / total)
		vec[i - 1] = j
		x += j
		y += j * j

	if N - x > beta - alpha:
		raise Exception(f"The provided upper bound (`beta`={beta}) is not compatible with other parameters.")

	vec[n - 1] = N - x  # Complete the vector

	return vec + alpha  # Apply translation


def random_vecs_cv(data, n, N, CV, eps_left, eps_right, alpha, beta, count=1, seed=1):
	"""Generate a set of random uniform vectors with constraint on the coefficient of variation

	Generate a set of `count` random uniform vectors according to parameters `n`, `N`, `CV`, `eps`,
	`alpha`, `beta`. `n` is the vector length, `N` is the sum of elements, `CV` is the coefficient
	of variation, and `alpha` (resp. `beta`) is the lower bound (resp. upper bound) of each element.
	As there may be no solution for the wanted coefficient of variation, `eps_left` and `eps_right`
	are here to control the allowed error on `CV`.

	A valid and compatible pre-calculation table should be passed in `data`.

	Examples:
		> data = mem_table_cv(3, 5, 1, 5)
		> random_vecs_cv(data, n = 3, N = 5, CV = 1.0, eps_left = 0.1, eps_right = 0.1, alpha = 1, beta = 5, count = 10, seed = 123456)
	"""

	rng = np.random.default_rng(seed)

	return np.array([random_vec_cv(rng, data, n, N, CV, eps_left, eps_right, alpha, beta)
		for _ in range(count)])


def random_vecs(data, n, N, alpha, beta, count=1, seed=1):
	"""Generate a set of random uniform vectors

	Helper function for generating random uniform vectors without any constraint on CV. This is
	made possible by setting `eps_left` to 1 and `eps_right` to `sqrt(n - 1) / CV - 1`.
	"""

	return random_vecs_cv(data, n, N, 1, 1, np.sqrt(n - 1) - 1, alpha, beta, count, seed)


if __name__ == "__main__":
	import time
	import argparse

	parser = argparse.ArgumentParser(description="Compute a pre-calculation table and save it on disk.")
	parser.add_argument("n", type=int, help="Vector length")
	parser.add_argument("N", type=int, help="Sum of elements")
	parser.add_argument("alpha", type=int, help="Minimum value of each element")
	parser.add_argument("beta", type=int, help="Maximum value of each element")
	parser.add_argument("output", type=str, help="Data output file (without extension)")
	parser.add_argument("--verbose", dest="verbose", action="store_true")
	args = parser.parse_args()

	start_time = time.time()
	data = mem_table_cv(args.n, args.N, args.alpha, args.beta)
	end_time = time.time()

	#names = ['n', 'N', 'S']
	#index = pd.MultiIndex.from_product([range(s)for s in data.shape], names=names)
	#df = pd.DataFrame({'count': data.flatten()}, index=index).reset_index()
	#df = df[df['count'] != 0]
	#df.to_csv(args.output + '.gz', index = False)

	np.save(args.output, data)

	if args.verbose:
		print("Took", end_time - start_time, "seconds")
		print("Memory usage", data.nbytes, "bytes")
		print("Written to", f"{args.output}.npy")
