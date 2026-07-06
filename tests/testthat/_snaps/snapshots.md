# print and summary of a deterministic base_optim result are stable

    Code
      print(res)
    Output
      <optimix_result>
        optimiser : base_optim
        value     : 0
        par       : 1, 1
        evals     : 20
        converged : yes

---

    Code
      summary(res)
    Output
      Optimisation result
        Chosen optimiser : base_optim
        Reason           : user-specified
        Best value       : 0
        Best parameters  : 1, 1
        Evaluations used : 20
        Converged        : yes

# print of a deterministic native optimix_map result is stable

    Code
      print(map_res)
    Output
      <optimix_map> 4 optima found (native)
        [1] value -8 at -2, -2
        [2] value -8 at 2, 2
        [3] value -8 at -2,  2
        [4] value -8 at  2, -2

