# 2hdm_constraint

## Purpose

`2hdm_constraint` is a Wolfram/Mathematica-based project for studying constraints on parameters of the Two-Higgs-Doublet Model (2HDM) through loop-induced anomalous electroweak vertices.

The main focus is the calculation and analysis of anomalous contributions to

- `ZZZ` vertex functions;
- `ZWW` vertex functions;
- related coefficient functions such as `f4`;
- hadronic cross sections using parton distribution functions.

The project combines symbolic one-loop calculations in Wolfram Language with numerical evaluation, CSV export, plotting, and optional Telegram notifications for long scans.

## Physics workflow

The intended workflow is:

1. Define the 2HDM parameter point and analytic substitutions.
2. Generate and manipulate amplitudes using FeynArts and FeynCalc.
3. Reduce and evaluate loop structures with Passarino-Veltman functions and LoopTools.
4. Extract anomalous vertex coefficients for `ZZZ` and `ZWW`.
5. Scan over mass and energy grids.
6. Save intermediate expressions and numerical results to cache files and CSV tables.
7. Build plots and compare the resulting constraints.

The current main physics targets are:

- calculation of `f4` coefficients for `ZZZ` and `ZWW`;
- extension to `f6` coefficients;
- cross-section calculations for hadronic initial states;
- validation on auxiliary processes such as `pp -> H -> gg`.

## Project layout

```text
2hdm/
├── ZZZ_ZWW/
├── articles/
├── modules/
├── ppHgg/
├── setup/
├── tg/
├── README.md
└── requirements.txt
```

## `modules/`

Core Wolfram modules used by the calculations.

Important files:

- `setup.m` — initializes paths, FeynCalc/FeynArts, LoopTools, model files, Python/PDF hooks.
- `FunctionalModules.wl` — utility functions for timing, progress reporting, execution helpers, and Telegram messages.
- `ModelParams.wl` — 2HDM parameter definitions and replacement rules.
- `SaveToCSVmodule.wl` — CSV export helpers.
- `pdfcode/` — Python-side PDF utilities used from Wolfram code.

The module layer should contain reusable logic. Long process-specific scripts should remain in the corresponding process directory.

## `ZZZ_ZWW/`

Main directory for anomalous vertex calculations.

Typical file groups:

### Coefficient extraction

- `f4calc_ZZZ.wl` — extraction/calculation logic for the `ZZZ` `f4` coefficient.
- `f4calc_ZWW.wl` — extraction/calculation logic for the `ZWW` `f4` coefficient.

### Executable scripts

- `ORCHESTRATOR.wls` — high-level orchestration of scans.
- `SCRIPTqqZZZ.wls` — executable script for the `q qbar -> Z -> ZZ` chain.
- `SCRIPTcross.wls` — cross-section calculation script.
- `SCRIPT_f4meanvalue.wls` — mean-value calculations for `f4` coefficients.

### Plotting and tests

- `GRAPHs_Metod1.wl`, `GRAPHs_Metod2.wl`, `GRAPHs_Metod3.wl` — plotting methods.
- `TESTqqZZZ.wl` — development and debugging tests.
- `TestPVphysicalError.wl` — checks related to physical Passarino-Veltman behavior.

### Runtime subdirectories

- `buffer/` — cache directory for `.mx`, `.csv`, and intermediate symbolic/numeric results.
- `subgraphs/` — generated plots and auxiliary plotting output.

`buffer/` is important for performance. Large symbolic expressions should be cached there rather than recomputed unnecessarily.

## `ppHgg/`

Auxiliary validation process for cross-section and PDF-related logic.

This directory is not the main physics target, but it is useful for validating the cross-section machinery.

## `setup/`

Physics/model setup files used by the Wolfram code.

Important files may include:

- `THDM.nb` — Wolfram notebook for model preparation.
- `THDMCPV.mod` — FeynArts model file.
- `THDMCPV.pars` — FeynArts model parameters.
- auxiliary platform-specific files used by older local installations.

For the Docker workflow, model deployment into the FeynArts model directory is handled by the Docker environment. The project code should refer to model paths through `setup.m`, not through hard-coded machine-specific paths.

## `tg/`

Telegram notification utilities for long computations.

Typical files:

- `tg_bot.py`
- `notifier_console.py`

The Wolfram side can call Telegram helpers through functions such as `PrintTG[...]`, depending on the current implementation in `FunctionalModules.wl`.

## Basic usage

From inside the configured environment:

```bash
cd ~/2hdm
```

Run a Wolfram script:

```bash
wolframscript -file ZZZ_ZWW/SCRIPTqqZZZ.wls
```

Run an orchestrated scan:

```bash
wolframscript -file ZZZ_ZWW/ORCHESTRATOR.wls
```

Open the project in Mathematica GUI and evaluate notebooks/scripts interactively when needed.

## Development notes

- Avoid hard-coded absolute paths tied to one machine.
- Prefer `TWOHDM_ROOT` or paths derived from `$HomeDirectory`.
- Keep reusable functions in `modules/`.
- Keep large generated files out of git.
- Cache expensive symbolic expressions in `ZZZ_ZWW/buffer/`.
- Use CSV output for post-processing and plotting.
