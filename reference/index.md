# Package index

## Solve a problem

The one verb, its readable aliases, and the typed problem contract.

- [`optimix()`](https://max578.github.io/optimix/reference/optimix.md)
  [`minimise()`](https://max578.github.io/optimix/reference/optimix.md)
  [`maximise()`](https://max578.github.io/optimix/reference/optimix.md)
  : Optimise a function
- [`optim_problem`](https://max578.github.io/optimix/reference/optim_problem.md)
  : A unified optimisation problem

## Design spaces

Where the search lives.

- [`space_box()`](https://max578.github.io/optimix/reference/space_box.md)
  : Continuous box-constrained design space
- [`space_permutation()`](https://max578.github.io/optimix/reference/space_permutation.md)
  : Permutation design space
- [`opt_space()`](https://max578.github.io/optimix/reference/opt_space.md)
  : Design-space base class

## Objective declarations

What the function costs and how it misbehaves.

- [`objective_deterministic()`](https://max578.github.io/optimix/reference/objective_deterministic.md)
  : Declare a deterministic objective
- [`objective_noisy()`](https://max578.github.io/optimix/reference/objective_noisy.md)
  : Declare a noisy objective
- [`objective_expensive()`](https://max578.github.io/optimix/reference/objective_expensive.md)
  : Declare an expensive objective
- [`objective_spec()`](https://max578.github.io/optimix/reference/objective_spec.md)
  : Objective specification

## Engines and the registry

The curated portfolio and how to extend it.

- [`optim_engine()`](https://max578.github.io/optimix/reference/optim_engine.md)
  : An optimiser engine specification
- [`register_optimiser()`](https://max578.github.io/optimix/reference/register_optimiser.md)
  : Register an optimiser engine
- [`list_optimisers()`](https://max578.github.io/optimix/reference/list_optimisers.md)
  : List the registered optimisers

## Results

The optim()-compatible result and the optima map.

- [`optimix_map()`](https://max578.github.io/optimix/reference/optimix_map.md)
  : Map the optima of a function
- [`as_optim()`](https://max578.github.io/optimix/reference/as_optim.md)
  : Coerce an optimisation result to a base optim() list

## Orchestra manifest

The typed hand-off contract for downstream consumers.

- [`orchestra_manifest`](https://max578.github.io/optimix/reference/orchestra_manifest.md)
  : The orchestra ensemble-manifest contract (optimix-side
  implementation)
- [`as_orchestra_manifest()`](https://max578.github.io/optimix/reference/as_orchestra_manifest.md)
  : Emit an optimix result as an orchestra manifest
- [`verify_manifest()`](https://max578.github.io/optimix/reference/verify_manifest.md)
  : Verify an orchestra manifest's payload integrity
