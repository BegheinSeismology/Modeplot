modeplot.f is John Woodhouse's old Unix code to compute sensitivity kernels based on eigenfunctions calculated for PREM or AK135.
Probably obsolete now but a good reference to have.

Options are for:
- isotropic kernels for relative perturbations in vp, vs, and rho
- isotropic kernels for relative perturbations in rho, mu and kappa (shear and bulk moduli)
- radial anisotropy for relative perturbations in vph, vpv, vsh, vsv, rho, and eta

modeplot.notrel.f is the same code modified by C.B. for non-relative relative perturbations


Compilation:
**!!! The codes run on Unix only, using the libio.a library**
Compilation using: f77 modeplot.f libio.a -o modeplot

They need to read files foanis05.222 and PREM222.BIN to use PREM or ak135f.222 and AK135F222.BIN to use AK135


modeplot_library/ contains all the codes that were initially used to build libio.a
