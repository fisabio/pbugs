
# pbugs

`pbugs` is an `R` package devised to run `WinBUGS` or `OpenBUGS` models
in parallel. Each chain is run in a different `BUGS` instance so it is
computed in a different core of your machine. Ideally, if `n.chains` are
simulated (and your simulation is long enough) this should yield
computing time improvements close to `1 / n.chains`.

The `pbugs` package heavily depends on the
[`R2WinBUGS`](https://cran.r-project.org/web/packages/R2WinBUGS/) and
[`OpenBUGS`](https://cran.r-project.org/web/packages/OpenBUGS/)
libraries. Indeed, any `R` syntax calling to any `R2WinBUGS` or
`R2OpenBUGS` should work equally when using the `pbugs` package by
simply replacing any `bugs(...)` sentence by the corresponding
`pbugs(...)` sentence. We have tried to preserve the original syntax so
that your old code using this package works also when the `pbugs`
package is used instead, by simply changing `bugs` by `pbugs`. Anyway,
when `pbugs` is used that same code should run substantially faster.

Aditionally, the `pbugs` package contains also some improvements over
`R2WinBUGS` and `R2OpenBUGS` that we have thought interesting to
implement as former `R2WinBUGS` users. These main improvements could be
summarized as:

  - `pbugs` objects keep the structure and functionality of `bugs`
    objects with some enhancements, so anything that you did with your
    `bugs` will be also possible to do with your `pbugs` object.
  - `pbugs` automatically saves the computing time taken to run the
    sentence generating that object. This is saved at the `exec.time`
    component of any `pbugs` object.
  - `pbugs` makes it possible to change and control the default sampling
    methods when using `WinBUGS` (as described at the ‘Changing MCMC
    defaults’ section of the `WinBUGS` help) automatically.
  - If some of the `WinBUGS` chains crashes during the MCMC simulation
    process for some reason, `pbugs` will retrieve the results of the
    rest of chains and will build the corresponding `pbugs` object. In
    this manner you will not lose the results for the rest of chains.
  - Improved functionality for `print` and `summary` methods for `pbugs`
    objects, allowing for example ordering variables in summaries by
    their values of their convergence statistics.
  - An enhanced history plot function for `pbugs` objects (`traceplot`)
    has been included into the package. This function automatically
    plots those parameters with deficient convergence criteria.

`pbugs` has been substantially tested on Linux workstations, we have
also substantially tested the package on Windows machines, but although
tested, our experience on ‘Mac’ systems is much lower. Thus, if you
found some issue on the running of this package on some of these
different environments please do not doubt to [let us
know\*](https://github.com/fisabio/pbugs/issues).
