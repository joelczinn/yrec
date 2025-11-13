Release Paper Test Suite
========================

This folder contains the published test suites from "The YREC Stellar Evolution Code: Public Data Release", Pinsonneault et al. 2025.

While YREC is a pure Fortran application, pytest was chosen to be the test execution framework due to its feature set and ease of installation and use. A standardized set of test cases is provided in this directory. Instructions for how to run all or a subset of the available preselected test cases may be found in this readme.

## Environment

The test suite is implemented using pytest, which in turn requires python. A python virtualenv may be useful.

```
python -m venv ./yrec
source ./yrec/bin/activate
pip install pytest pytest-xdist
```

If a suitable python is not available, one may be installed using mamba or conda.

```
mamba create -n yrec python=3.10
mamba activate yrec
pip install pytest pytest-xdist
```

## Running Tests

1. Activate environment containing pytest
2. Edit configuration file to specify various parameters, such as:
   a. Location of yrec executable
   b. Subset of test cases to run, if desired
   c. Numerical tolerances for comparisons with reference standard outputs
3. $ cd yrec/examples
4. Run tests
   a. Sequentially
      1. `pytest`
   b. In parallel, using X concurrent workers
      1. `pytest -nX`

The test runner will produce a summary of pass/fail results and indicate any differences from reference outputs indentified.


Namelists
================

## Solar

This directory contains solar models with a variety of different rotation & microphysics parameters. Cases include:

- `Test_solar_base`: a base, no diffusion, nonrotating case with a gray atmosphere
- `Test_solar_allard`: base case + Allard model atmosphere
- `Test_solar_kurucz`: base case + Kurucz model atmosphere
- `Test_solar_dif`: base case + diffusion
- `Test_solar_dif_rot`: base case + diffusion + rotation
- `Test_solar_dif_rot_fast`: base case + diffusion + rapid rotation
- `Test_solar_dif_rot_solid`: base case + diffusion + solid body rotation
- `Test_solar_SF3`: base case + solar fusion III nuclear reaction rates
- `Test_solar_yaleeos`: base case + Yale equation of state

## Brown Dwarf

This directory contains brown dwarf models in three mass ranges: 0.03, 0.05, and 0.08 solar masses. These models use the Allard model atmospheres and run to 15 Gyr, without rotation or diffusion.

## ZAMS

This directory contains runs to the zero age main sequence at 0.1, 0.3, and 1.0 solar masses. For each mass, a base model assuming a gray atmosphere with no diffusion or rotation, a spotted model on top of a gray atmosphere model, and a model using an Allard model atmosphere.

## TAMS

This directory contains runs to the terminal age main sequence at 1.0 and 1.4 solar masses. For each mass, a run with diffusion and rotational mixing are included.

## Evolution

This directory contains runs to relevant evolutionary stages for 0.3, 1.0, 3.0, and 9.0 solar mass base cases with no rotation or diffusion.
