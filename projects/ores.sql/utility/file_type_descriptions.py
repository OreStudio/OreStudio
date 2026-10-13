# -*- mode: python; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
"""What each ORE file is for, written once and used by the file type seed.

A file type's description has three parts. The first says what the file holds. It is
keyed by the well known name, so every file type that uses the name shares it. The
second says what the analytic does with the file, and it is keyed by the analytic.
The third says how this file type reaches the run: which parameter names it, who
writes it and who reads it. The seed script builds the third part from the row.

generate_file_types_populate.py fails when a file type has no entry in CONTENT or
ANALYTICS, so a new kind of file cannot be added without saying what it is for.
"""

# What the file holds, keyed by the canonical name. The official guide's description
# of a document is used first; this text is for the files the guide does not describe.
CONTENT = {
    "additional_results.csv":
        "The additional results report. It holds the extra figures that the pricing engines "
        "produce for a trade besides its value, such as model specific outputs.",
    "amcBuffer.dat":
        "The path data of an American Monte Carlo run, in binary form. The guide lists the "
        "parameter but does not describe the file.",
    "calendaradjustment.xml":
        "The calendar adjustments. It lists, for named calendars, the holidays to add and the "
        "days to treat as business days, so a run can correct or extend the built-in holiday "
        "calendars without a new release of the library. Schedule generation, payment date "
        "adjustment and the fixing date of every trade go through these calendars, so a wrong "
        "adjustment moves cash flows.",
    "calibration.csv":
        "The calibration report. It gives the calibrated model parameters, for example the "
        "volatility and mean reversion of each currency, with the calibration instruments and the "
        "fit error.",
    "collateralbalances.xml":
        "The collateral balances. For each netting set it holds the variation margin and the "
        "initial margin currently held or posted, with the currency. The exposure methods "
        "subtract these balances from the portfolio value to get the net exposure.",
    "conventions.xml":
        "The market conventions. It defines, by id, how each kind of quote is turned into an "
        "instrument: the day counters, calendars, business day conventions, fixing and settlement "
        "lags and index names of deposits, FRAs, swaps, OIS swaps, FX swaps, credit default "
        "swaps, zero rates and the rest. The curve configuration refers to these ids, so a curve "
        "segment cannot be built without the convention it names.",
    "correlation.csv":
        "The correlation matrix of the risk factors. The XVA analytic reads it to correlate the "
        "simulated factors, and the correlation analytic writes it from the historical scenarios.",
    "counterparty.xml":
        "The counterparty information. For each counterparty it holds the attributes the "
        "regulatory capital and credit valuation adjustment methods need, such as the credit "
        "quality, the sector, the credit risk weight and whether the counterparty is a financial "
        "institution. The standardised methods read it to assign risk weights and supervisory "
        "factors to the exposures.",
    "covariance.csv":
        "The covariance data for the parametric value at risk. ORE does not calculate "
        "covariances, so the file is provided from outside. It is a blank, tab or comma separated "
        "file with three columns: the two risk factors and their covariance.",
    "cptyCube.csv.gz":
        "The NPV cube aggregated by counterparty, compressed. The simulation writes it together "
        "with the survival probabilities and the credit state NPVs, which the dynamic credit XVA "
        "calculation needs.",
    "credit_migration_bond1":
        "The credit migration output. The stem of the files that the credit migration run writes "
        "for a bond, one for each reported quantity.",
    "creditcurves.csv":
        "The credit curve report. It lists the survival probabilities of the credit curves on a "
        "grid of dates, so the default curves the run built can be checked.",
    "creditsimulation.xml":
        "The credit simulation configuration. It defines the credit migration model, the rating "
        "states and the transition matrices that the XVA analytic uses to simulate defaults and "
        "rating changes along the paths.",
    "crif.csv":
        "The CRIF, the sensitivities in the Common Risk Interchange Format defined by ISDA. The "
        "crif analytic generates it from a par sensitivity analysis with a SIMM specific "
        "sensitivity configuration, after labelling the sensitivities so the file meets the CRIF "
        "format. The simm analytic reads it to compute the initial margin. ORE's own CRIF "
        "generation covers interest rate and FX risks only, while the SIMM analytic can process a "
        "full CRIF across interest rate, FX, inflation, equity, credit and commodity risks.",
    "crif_schedule.csv":
        "The CRIF for the IM Schedule method. The imschedule analytic computes the basic initial "
        "margin from minimal trade information. The file has two lines for each trade, one with "
        "the risk class PV, whose amount is the NPV, and one with the risk class Notional, whose "
        "amount is the notional. It also needs the product class and the end date of each trade.",
    "crifmarket.xml":
        "The market description of the CRIF analytic. It lists the market objects needed to "
        "compute the ISDA SIMM sensitivities that the CRIF report contains.",
    "crossgamma.csv":
        "The cross gamma report. For pairs of risk factors it gives the second order change in "
        "value when both are bumped, the term a first order sensitivity misses.",
    "cube.csv.gz":
        "The valuation cube of a simulation, compressed. It holds the simulated value of each "
        "trade for every date and sample of the time grid. The XVA and exposure analytics read it "
        "to compute the exposure profiles.",
    "cube.dat":
        "The valuation cube of a simulation in binary form. It holds the simulated value of each "
        "trade for every date and sample.",
    "currencies.xml":
        "The currency configuration. It describes each currency the run may meet: its ISO code "
        "and number, the number of minor units, the rounding rule, the symbol and the formatting. "
        "Amounts are rounded and reported according to it, and a currency that is missing from it "
        "cannot be priced.",
    "curveconfig.xml":
        "The curve configuration. It says how every curve the run needs is built from market "
        "quotes: the curve id, the interpolation and extrapolation, the instruments or segments "
        "the curve is bootstrapped from, and the quote names it reads. Yield, default, FX and "
        "volatility curves, inflation curves and the rest are all described here, and today's "
        "market refers to them by id.",
    "curves.csv":
        "The curve report. It lists the discount factors of the requested curves on a grid of "
        "dates, so the curves the run built can be checked against the market.",
    "dim_evolution.csv":
        "The evolution of the dynamic initial margin: a deterministic initial margin for each "
        "date, read by the xva analytic when it is given the initial margin as a file.",
    "dim_evolution.txt":
        "The evolution of the dynamic initial margin. The xva analytic writes the evolution of "
        "the zero order DIM and the average of the nth order DIM through time.",
    "dim_regression.csv":
        "The regression coefficients of the dynamic initial margin. They are the fit of the "
        "margin against the risk factors that the dynamic initial margin model uses to estimate "
        "the margin on each path.",
    "dim_regression.txt":
        "A snapshot of the regression that the dynamic initial margin model fits. The xva "
        "analytic writes it, and a list of file names can be given when more than one snapshot is "
        "needed.",
    "fixings.txt":
        "The historical fixings. A plain text file with one line for each date, index and fixing "
        "value. Trades with floating legs read the fixings that were set before the as-of date "
        "from here, so a missing fixing makes a coupon impossible to value.",
    "flows.csv":
        "The cash flow report. One row for each future cash flow of each trade with the pay date, "
        "amount, currency, the legs and the flow type, with the discounted present value. It "
        "shows how the NPV is made up.",
    "iborFallbackConfig.xml":
        "The IBOR fallback configuration. It represents the rules for replacing IBOR reference "
        "rates by risk free rates. If no configuration is given, ORE uses a standard one.",
    "ibor_fallback.xml":
        "The IBOR fallback configuration. It names the rules that replace a discontinued IBOR "
        "index with its risk free rate, the spread added and the date the fallback starts. Trades "
        "that reference a retired index are priced through the rule.",
    "jacobi.csv":
        "The Jacobian. The matrix that converts a shift of zero rates into the shift of par "
        "rates, used to turn zero rate sensitivities into par rate sensitivities.",
    "jacobi_inverse.csv":
        "The inverse Jacobian. The matrix that converts a shift of par rates back into the shift "
        "of zero rates.",
    "market.txt":
        "The market data. A plain text file of dated quotes, one per line: the date, the quote "
        "name and the value. The curve configuration names the quotes a curve needs, and the run "
        "reads their values for the as-of date from here.",
    "marketObjects.csv":
        "The market objects of the portfolio, written by the portfolioDetails analytic: the "
        "curves, surfaces and other market objects that the trades of the portfolio need for "
        "their valuation. The guide does not describe the columns.",
    "meanReversion.csv":
        "The mean reversion report of a calibration. It gives the mean reversion values that the "
        "calibration found for the model, so they can be compared between runs.",
    "mporportfolio.xml":
        "The portfolio as it stands after the margin period of risk. It holds the trades valued "
        "at the later date, so the profit and loss over that period can be computed from the two "
        "portfolios.",
    "netcube.csv":
        "The netted cube. It holds the exposure of each netting set on every date and sample, "
        "after the netting and the collateral. The exposure and XVA figures are taken from it.",
    "netting.xml":
        "The netting set definitions. For each netting set it holds the counterparty, whether it "
        "is covered by a credit support annex and the terms of that annex: the thresholds, the "
        "minimum transfer amounts, the independent amount, the margin period of risk and the "
        "margining frequency. Exposure at default and the credit valuation adjustment depend on "
        "these terms.",
    "nettingSetCube.csv.gz":
        "The NPV cube aggregated by netting set, compressed, with the NPVs converted into the "
        "base currency of the simulation. The simulation writes it next to the trade level cube, "
        "so the post processing can start from netting set values without aggregating the trades "
        "again.",
    "npv.csv":
        "The NPV report. One row for each trade with its id, type, maturity, net present value in "
        "the trade and base currencies, the notional and the netting set. It is the basic "
        "valuation result that most other reports are checked against.",
    "parconversion_sensitivity.csv":
        "The converted sensitivities. It gives the sensitivities to par rates that the conversion "
        "computed from the zero rate sensitivities.",
    "parsensi.csv":
        "The par rate sensitivity report. It gives the sensitivities of the portfolio to par "
        "instrument quotes instead of zero rates.",
    "parsensitivity.csv":
        "The par sensitivity report. It gives the sensitivities to par rates, such as swap rates, "
        "that the zero rate sensitivities convert to through the Jacobian.",
    "parshifts.csv":
        "The par shifts that the zeroToParShift analytic writes. ORE's scenario machinery works "
        "in the raw (zero rate) domain, and the pnlExplain analytic, when it works on par "
        "sensitivities, needs the market moves between two dates as par rate shifts. This file "
        "holds the raw moves of a stress scenario converted into the equivalent par moves.",
    "parstresstest.xml":
        "The par stress test configuration. It defines stress scenarios as shifts of par rates, "
        "such as swap rates and deposit rates, which the par stress conversion turns into the "
        "equivalent shifts of zero rates before the portfolio is revalued.",
    "pca.csv":
        "The principal component report. It gives the eigenvectors and eigenvalues of the curve "
        "moves, from which the model volatility structure is derived.",
    "pca_{{CURRENCY}}.csv":
        "The principal component input for one currency. Each currency has its own file holding "
        "the history of curve moves from which the components are computed. The name is a pattern "
        "because the calibration reads one file for each currency.",
    "pnl.csv":
        "The profit and loss report. For each trade it gives the value at the as-of date, the "
        "value after the margin period of risk and the change between them.",
    "pnl_explain.csv":
        "The profit and loss explain report. It splits the change in value over the margin period "
        "of risk into the parts caused by the movement of each risk factor and the unexplained "
        "rest.",
    "portfolio.xml":
        "The portfolio. It lists the trades to be valued, each with an id, a trade type, the "
        "netting set it belongs to, the counterparty and the full instrument data. It is the "
        "object every analytic runs over, so the trades in it decide what the reports contain.",
    "pricingengine.xml":
        "The pricing engines. It maps each product type to the model and the engine that prices "
        "it, with the model and engine parameters, for example the analytic or Monte Carlo engine "
        "of a swaption and its tolerances. A trade whose product has no entry cannot be priced, "
        "and a different entry changes its value.",
    "pricingengine_amc.xml":
        "The pricing engines for American Monte Carlo. It maps the products that are valued by "
        "regression on the simulated paths to their AMC engines, so that exposure simulation can "
        "value early exercise and path dependent trades.",
    "pricingengine_amccg.xml":
        "The pricing engines for American Monte Carlo on a computation graph. It maps the "
        "products to engines that record their payoff as a graph, which allows exposure and "
        "sensitivities by algorithmic differentiation.",
    "pricingengine_dg.xml":
        "The pricing engines for the discounting and the grid based run.",
    "productclass_mapping.csv":
        "The product class mapping that the sensitivity analytic can write. It maps the trades to "
        "the product classes of the sensitivity report. The guide does not describe the columns.",
    "rawcube.csv":
        "The raw cube. It holds the value of each trade on every date and sample before the "
        "netting and the collateral, so the netting set exposures can be rebuilt.",
    "referencedata.xml":
        "The reference data. It holds the static data that trades refer to by id rather than "
        "carry inline, such as the structure of a bond, a credit index or a basket and the "
        "constituents of an index. A trade resolves its reference data id against it when the "
        "portfolio is built.",
    "results.xml":
        "The zero stress scenario data that the par stress conversion writes and the stress "
        "analytic reads. It holds the stress scenarios expressed as shifts of zero rates.",
    "riskFactors.csv":
        "The risk factors of the portfolio, written by the portfolioDetails analytic: the risk "
        "factors that the trades of the portfolio depend on. The guide does not describe the "
        "columns.",
    "sacva_sensitivity.csv":
        "The net CVA sensitivities of the sacva analytic, the figures its SA-CVA capital "
        "calculation works on. The guide does not describe the columns.",
    "scenario.csv":
        "A scenario file in the ORE csv format. The scenario analytic writes the base scenario of "
        "the simulation market to it. The sensitivity analytic writes the base scenario and the "
        "shifted scenarios to it. Other analytics read the file as the historical discount "
        "factors and FX spot rates they work from.",
    "scenariodata.csv":
        "The aggregation scenario data in plain text, the uncompressed form of the aggregation "
        "scenario data that the simulation writes. It holds the simulated FX rates and index "
        "fixings that the XVA post processor needs for the variation margin.",
    "scenariodata.csv.gz":
        "The aggregation scenario data written by a simulation. For every date and sample of the "
        "time grid it gives the simulated values of the market factors, compressed. The XVA post "
        "processing reads it to aggregate the exposures consistently with the paths.",
    "scenariodata.dat":
        "The scenario data of a simulation in binary form. It holds the simulated market factors "
        "for every date and sample.",
    "scenariodump.csv":
        "The full raw market scenarios that the simulation market generates, for example the "
        "discount factors of the yield curves on every date and sample, written for later "
        "inspection or reuse. ORE writes the file only when the parameter is given.",
    "scenarios.csv":
        "The historical scenarios. Each row is the move of a risk factor between two dates. The "
        "historical simulation takes its scenarios from here.",
    "scriptlibrary.xml":
        "The script library. It holds the scripts that scripted trades run, written in the ORE "
        "scripting language, so a payoff that no built-in product covers can be priced. A "
        "scripted trade names a script by id.",
    "sensimarket.xml":
        "The market description of the sensitivity analytic. It lists the market objects that the "
        "sensitivity run may bump.",
    "sensitivity.csv":
        "The sensitivity report. For each trade and risk factor it gives the base value, the "
        "shift, the shifted value and the sensitivity, which is the change in value for the bump. "
        "It is the input of the value at risk and the margin analytics.",
    "sensitivity.xml":
        "The sensitivity configuration. For each risk factor group it gives the shift type and "
        "size, the tenors and strikes to bump and, for the second order, the cross gamma pairs. "
        "It also holds the conversion from zero rate to par rate sensitivities. The sensitivity "
        "analytic bumps exactly the factors it lists.",
    "sensitivity_stress.xml":
        "A sensitivity configuration for the stressed runs. It lists the risk factors to bump in "
        "the stressed market and uses a shift size that suits the stress, so the stressed "
        "sensitivities are computed on the stressed market.",
    "sensitivitystress.xml":
        "The sensitivity configuration of the sensitivity stress analytic. It lists the risk "
        "factors whose sensitivities are taken in the stressed market.",
    "sensitivitystressmarket.xml":
        "The market description of the sensitivity stress analytic. It lists the market objects "
        "that are stressed and then bumped.",
    "simm_calibration.xml":
        "The SIMM calibration. It holds the risk weights, the correlations and the concentration "
        "thresholds of a SIMM version, so the simm analytic can add a version or override one of "
        "the versions ORE supports. When it is given it overrides the version parameter.",
    "simulation.xml":
        "The simulation configuration. It defines the model of the risk factors, such as the "
        "Linear Gauss Markov model for interest rates and the log normal model for FX, with the "
        "calibration, the dates of the time grid, the number of samples, the sequence type and "
        "the market objects that are simulated. When a sensitivity or stress analytic uses it as "
        "its market file, it serves as the description of the risk factors that can be shifted.",
    "simulation_B.xml":
        "A second simulation configuration, used by a run that simulates twice.",
    "simulation_ccs.xml":
        "A simulation configuration for cross currency swaps. It has the same structure as the "
        "usual simulation configuration but adds the cross currency basis factors the simulation "
        "needs for these trades.",
    "simulationmarket.xml":
        "The market part of a simulation configuration. It lists the simulated market objects, "
        "which curves, volatilities and FX rates are evolved, and the tenors and strikes at which "
        "each is represented. It is kept apart so that several simulations can share one market "
        "definition.",
    "simulationmodels.xml":
        "The model part of a simulation configuration. It holds the models and their calibration "
        "settings, so that the model choice can change without touching the market or the "
        "parameters.",
    "simulationparams.xml":
        "The parameter part of a simulation configuration. It holds the time grid, the number of "
        "samples and the other settings that control the run, apart from the models and the "
        "market.",
    "stress_scenarios.csv":
        "The stress scenarios that the scenarioGeneration analytic writes, one row for each "
        "scenario and risk factor, so that the stress analytic can read them.",
    "stressmarket.xml":
        "The market description of the stress scenario generation. It lists the market objects "
        "that the generated stress scenarios may shift.",
    "stresstest.csv":
        "The stress test report. For each trade or netting set and each scenario it gives the "
        "base value, the stressed value and the change between them.",
    "stresstest.xml":
        "The stress test configuration. It defines named stress scenarios as shifts of market "
        "factors, for example a parallel shift of a yield curve or a relative move of an FX rate. "
        "The stress analytic revalues the portfolio under each scenario and reports the change "
        "against the base value.",
    "stresstest_flows.csv":
        "The stress test cash flow report. It lists the cash flows of each trade under each "
        "stress scenario, so the stressed values can be traced to their flows.",
    "stresstest_sensi.xml":
        "The stress test configuration of the sensitivity stress analytic. It lists the scenarios "
        "under which the sensitivities are recomputed.",
    "timeaveragedexposure.csv":
        "The time averaged netted exposure that the xva analytic writes, one figure for each "
        "netting set. The guide does not describe the columns.",
    "todaysmarket.xml":
        "Today's market. It gathers the curves of the curve configuration into named market "
        "configurations: which discount curve, forward curve, FX spot, volatility surface and "
        "credit curve a trade, a currency or an index uses. The Markets block of the run document "
        "picks one of these configurations for each purpose, such as pricing, simulation or "
        "calibration.",
    "var.csv":
        "The value at risk report. It gives the value at risk of the portfolio and its breakdown "
        "by risk class, portfolio and trade at the requested quantiles.",
    "xva_par_sensitivity_cva.csv":
        "The par sensitivities of the credit valuation adjustment. The sacva analytic computes "
        "the SA-CVA capital charge from CVA sensitivities. These can be computed on the fly, by "
        "running the xvaSensitivity analytic first, or they can be provided externally, and this "
        "file is the external form.",
    "xva_sensitivity_jacobi.csv":
        "The Jacobian of the XVA sensitivity run: the matrix that turns sensitivities to zero "
        "rates into sensitivities to par rates.",
    "xva_sensitivity_jacobi_inverse.csv":
        "The inverse of the XVA sensitivity Jacobian, used to convert par rate sensitivities back "
        "to zero rate sensitivities.",
    "xvasensiconfig.xml":
        "The sensitivity configuration of the computation graph XVA run. It lists the risk "
        "factors for which the XVA sensitivities are taken by differentiating the recorded graph.",
    "xvasensimarket.xml":
        "The market description of the XVA sensitivity analytic. It lists the market objects that "
        "the XVA sensitivity run bumps.",
    "zeroStressScenarioData.xml":
        "The zero stress scenario data. It holds the stress scenarios already expressed as shifts "
        "of zero rates, which the stress analytic applies directly to the market.",
}

# What each analytic does, for the sentence that places the file in its run.
ANALYTICS = {
    "additionalResults":
        "writes a report of the additional results that the pricing engines produce, which are "
        "engine specific",
    "amc":
        "values the portfolio with American Monte Carlo",
    "bacva":
        "computes the BA-CVA capital from a netting set file and the collateral balances, using "
        "saccr for the exposures",
    "calibration":
        "calibrates the simulation model to the market",
    "cashflow":
        "lists the future cash flows of every trade",
    "correlation":
        "estimates the correlation of the risk factors from historical scenarios",
    "creditcurves":
        "reports the credit curves the run built",
    "crif":
        "generates the CRIF, the input of the ISDA SIMM calculation, from a par sensitivity "
        "analysis",
    "curves":
        "reports the curves the run built",
    "historicalSimulationVar":
        "computes value at risk from historical scenarios",
    "imschedule":
        "computes the initial margin with the IM Schedule method, from minimal trade information "
        "in a CRIF",
    "initialMargin":
        "computes the initial margin",
    "npv":
        "values every trade in the portfolio",
    "parStressConversion":
        "converts a stress given on par rates into a stress on zero rates",
    "parametricVar":
        "computes value at risk from sensitivities and a covariance matrix",
    "pfe":
        "computes the potential future exposure profiles",
    "pnl":
        "computes a single period profit and loss between the as-of date and a later date",
    "pnlExplain":
        "explains that profit and loss in terms of raw or par sensitivities",
    "portfolioDetails":
        "describes the portfolio and what its valuation depends on",
    "saccr":
        "computes the SA-CCR capital, from a netting set file and the collateral balances",
    "sacva":
        "computes the SA-CVA capital from CVA sensitivities, computed on the fly or provided",
    "scenario":
        "exports the base scenario of the simulation market as a file",
    "scenarioGeneration":
        "generates market scenarios, for example stress scenarios",
    "sensitivity":
        "bumps the risk factors and reports the sensitivities of the portfolio",
    "sensitivityStress":
        "computes sensitivities in a stressed market",
    "simm":
        "computes initial margin with ISDA's Standard Initial Margin Model from a CRIF",
    "simulation":
        "simulates the risk factors and values the portfolio on every path",
    "smrc":
        "computes the basic SMRC market risk capital",
    "stress":
        "revalues the portfolio under stress scenarios",
    "xva":
        "computes the exposure profiles and the valuation adjustments",
    "xvaExplain":
        "computes the market implied change of the value adjustments between two evaluation dates",
    "xvaSensitivity":
        "computes bump and revalue sensitivities of the value adjustments",
    "xvaStress":
        "computes the change of the value adjustments under user defined stress scenarios",
    "zeroToParSensiConversion":
        "converts raw (zero rate) sensitivities into par sensitivities",
    "zeroToParShift":
        "converts the raw market moves of a stress scenario into the equivalent par moves",
}
