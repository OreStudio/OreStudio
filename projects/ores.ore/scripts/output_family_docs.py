"""What each output family states, and the units of its figures.

The catalogue says which columns a family has and what kind each is; it cannot
say what a figure means. This table says it, once for each family, and the
generator renders it beside the catalogue's columns into the output formats
page. The generator refuses a family this table does not describe, so a new
family cannot reach the page without a sentence.

Each entry is (what the file states, the units of its figures). Money is in the
currency the row states, or the run's base currency where a column says Base.
"""

MONEY = "Money, in the currency the row states; a (Base) column is in the run's base currency."

FAMILY_DOCS = {
    "additional_results": (
        "The extra results a pricer reports for each trade beyond its NPV: the engine and model it used, intermediate values and projected cashflows.",
        "Each value states its own type: string, double or cashflows; a double is in the trade's currency unless its name says otherwise."),
    "bacva": (
        "The basic approach CVA capital charge for each netting set.",
        MONEY),
    "calibration": (
        "The calibrated cross-asset model, as one XML document.",
        "None: the document is configuration, not figures."),
    "capital_crif": (
        "The capital sensitivities in CRIF form, one row for each trade, risk type and label, with the counterparty and netting set they belong to.",
        "Amount is in AmountCurrency; AmountUSD is in US dollars."),
    "colva-nettingset": (
        "The collateral valuation adjustment of one netting set along the exposure dates, with the collateral balance and floor behind it.",
        MONEY),
    "correlation": (
        "The correlation between two risk factors, estimated from history.",
        "A correlation between -1 and 1."),
    "counterparties": (
        "The counterparties a portfolio's trades name, with how many trades each has.",
        "A count of trades."),
    "credit-migration": (
        "The distribution of a credit portfolio's value under credit migration, bucket by bucket.",
        "upperBucketBound is money in the base currency; pdf and cdf are probabilities."),
    "crif": (
        "The SIMM sensitivities in CRIF form, one row for each trade, risk type, qualifier, bucket and label.",
        "Amount is in AmountCurrency; AmountUSD is in US dollars."),
    "curves": (
        "The discount factors of each curve the run built, one column for each curve, at each pillar date.",
        "A discount factor, without unit; the columns are the run's configured curves."),
    "cva_sensitivities": (
        "The CVA of each netting set and its change when one risk factor is shifted.",
        "BaseCva and Delta are money in the base currency; ShiftSize is in the factor's own unit."),
    "dim_evolution": (
        "The expected dynamic initial margin of each netting set along the exposure dates.",
        "Money in the base currency; DaysInPeriod is a count of days."),
    "dim_regression": (
        "The dynamic initial margin of each simulation sample and the regressors it was estimated from.",
        "Money in the base currency; the regressor columns are the run's configured regressors."),
    "dividends": (
        "The dividends the run loaded, one row for each equity and ex date.",
        "dividendRate is the dividend per share, in the equity's currency."),
    "eigenvalues": (
        "The eigenvalues of a principal component analysis of historical moves.",
        "An eigenvalue, without unit."),
    "eigenvectors": (
        "The eigenvectors of a principal component analysis, one row for each tenor.",
        "A loading, without unit."),
    "exposure-nettingset": (
        "The exposure profile of one netting set: expected positive and negative exposure, potential future exposure and the Basel measures, at each date.",
        MONEY),
    "exposure-trade": (
        "The exposure profile of one trade, with the share of its netting set's exposure allocated to it, at each date.",
        MONEY),
    "fixings": (
        "The index fixings the run loaded, one row for each index and date.",
        "A fixing, in the index's own unit: a rate, a price or a level."),
    "flows": (
        "The cashflows of each trade, one row for each payment of each leg, with its dates, rates and present value.",
        MONEY + " Coupon is a rate; Accrual is a year fraction; DiscountFactor has no unit."),
    "historical_PnL": (
        "The historical profit and loss of a portfolio between two dates, one row for each historical scenario.",
        MONEY),
    "im_schedule": (
        "The initial margin of each portfolio under the regulatory schedule approach, with the replacement costs and net-to-gross ratio.",
        MONEY + " NetToGrossRatio has no unit."),
    "im_schedule_trade": (
        "The schedule initial margin of each trade, with the notional and value it is computed from.",
        MONEY + " Maturity is a year fraction; Multiplier has no unit."),
    "jacobi": (
        "The conversion matrix from zero-rate to par-rate sensitivities: how much one par factor moves with one raw factor.",
        "A sensitivity ratio, without unit."),
    "jacobi_inverse": (
        "The inverse conversion matrix: how much one raw factor moves with one par factor.",
        "A sensitivity ratio, without unit."),
    "marketObjects": (
        "The market objects a portfolio depends on, one row for each kind, with the objects of that kind.",
        "None: the values are names."),
    "marketdata": (
        "The market quotes the run loaded, one row for each quote and date.",
        "A quote, in the quote's own unit: a rate, a price, a spread or a volatility."),
    "mean-reversion": (
        "The mean reversion and volatility parameters calibrated for a Hull-White model, one file for each currency.",
        "A parameter of the model, without unit."),
    "netcube": (
        "The value of each netting set in each simulated scenario at each date: the NPV cube after netting.",
        "Money in the base currency."),
    "netting_sets": (
        "The netting sets a portfolio's trades fall in, with how many trades each has.",
        "A count of trades."),
    "npv": (
        "The net present value and notional of each trade.",
        MONEY),
    "parconversion_sensitivity": (
        "The par-rate sensitivities of each trade converted from zero-rate sensitivities.",
        MONEY + " A (Trade) column is in the trade's currency."),
    "parsensitivity": (
        "The par-rate sensitivities of each trade: delta and gamma for each par factor.",
        MONEY + " A (Trade) column is in the trade's currency."),
    "parshifts": (
        "The par-rate shift a zero-rate stress scenario implies for each par instrument.",
        "A shift in the instrument's quote unit, usually a rate."),
    "pca": (
        "The principal components of historical curve moves, one file for each currency.",
        "Eigenvalues and loadings, without unit."),
    "pnl": (
        "The profit and loss of each trade between two dates, split into its sources.",
        MONEY),
    "pnl_explain": (
        "The profit and loss of each trade between two dates, explained factor by factor.",
        MONEY),
    "pricingstats": (
        "How many times each trade was priced and how long it took.",
        "NumberOfPricings is a count; the timings are in microseconds."),
    "rawcube": (
        "The value of each trade in each simulated scenario at each date: the NPV cube before netting.",
        "Money in the base currency."),
    "regulation_breakdown_simm": (
        "The SIMM initial margin of each portfolio broken down by regulation.",
        MONEY),
    "riskFactors": (
        "The risk factors a portfolio depends on, one row for each kind, with the factors of that kind.",
        "None: the values are names."),
    "runtimes": (
        "How long each part of the run took.",
        "Count is a number of calls; the others are microseconds."),
    "saccr": (
        "The SA-CCR exposure at default of each netting set, with the add-on, replacement cost and margin terms behind it.",
        MONEY + " Multiplier and RW have no unit."),
    "saccr_detail": (
        "The SA-CCR terms of each trade: its hedging set, supervisory delta, maturity factor and adjusted notional.",
        MONEY + " delta, MF and the supervisory factors have no unit; M, S, E and T are year fractions."),
    "sacva": (
        "The standardised approach CVA capital charge of each netting set.",
        MONEY),
    "sacva_sensitivities": (
        "The SA-CVA sensitivities of each netting set to each risk factor.",
        MONEY),
    "sacva_sensitivity": (
        "The net SA-CVA sensitivities of each netting set, by risk type, CVA type and margin type.",
        MONEY),
    "sacvadetail": (
        "The SA-CVA sensitivities of each netting set with the risk weight applied to each.",
        "Sensitivity is money in the base currency; RiskWeight has no unit."),
    "scenario": (
        "The value of each trade under each shifted factor of a sensitivity run.",
        MONEY),
    "scenario_fullSurface": (
        "The value of each trade under each shifted factor of a full volatility surface sensitivity run.",
        MONEY),
    "scenario_par_rates": (
        "The par rates of each par instrument in the base and in the shifted market.",
        "A rate."),
    "scenariodump": (
        "The simulated market: the value of each risk factor in each scenario at each date.",
        "A risk factor in its own unit: a discount factor, a rate, a spot or a volatility; the columns are the run's configured factors."),
    "scenariostatistics": (
        "Statistics of the simulated market across scenarios, for each risk factor at each date.",
        "A risk factor in its own unit; the columns are the run's configured factors."),
    "sensitivity": (
        "The zero-rate sensitivities of each trade: delta and gamma for each factor, and cross gammas for factor pairs.",
        MONEY + " A (Trade) column is in the trade's currency; a shift size is in the factor's unit."),
    "sensitivity_atmOnly": (
        "The sensitivities of each trade when only the at-the-money volatilities are shifted.",
        MONEY),
    "sensitivity_fullSurface": (
        "The sensitivities of each trade when the full volatility surface is shifted.",
        MONEY),
    "sensitivity_scenario": (
        "The value of each trade under each scenario of a sensitivity run, with the factor the scenario shifts.",
        MONEY),
    "simm": (
        "The SIMM initial margin of each portfolio.",
        MONEY),
    "simm_data": (
        "The SIMM margin of each portfolio broken down by product class, risk class and bucket.",
        MONEY),
    "smrc": (
        "The standardised market risk charge of each trade type.",
        MONEY + " RiskWeight has no unit."),
    "smrcdetail": (
        "The standardised market risk charge terms of each trade.",
        MONEY + " Maturity is a year fraction; RiskWeight has no unit."),
    "stress": (
        "The value of each trade in each stress scenario and the change from the base.",
        MONEY),
    "stress_scenario_par_rates": (
        "The par rates of each par instrument in the base and in each stress scenario.",
        "A rate."),
    "stress_scenarios": (
        "The stressed market: the value of each risk factor in each stress scenario.",
        "A risk factor in its own unit; the columns are the run's configured factors."),
    "stresstest": (
        "The value of each trade in each stress scenario and the change from the base.",
        MONEY),
    "stresstest_flows": (
        "The cashflows of each trade in each stress scenario, each figure stated for the base, the scenario and the difference.",
        MONEY + " Coupon is a rate; Accrual is a year fraction."),
    "swap_indices": (
        "The swap indices a portfolio references.",
        "None: the values are names."),
    "swaption_npv": (
        "The net present value and notional of each swaption.",
        MONEY),
    "timeaveragedexposure": (
        "The time-averaged netted exposure of each netting set in each simulation sample, with and without collateral.",
        "Money in the base currency."),
    "todaysmarketcalibration": (
        "What the market the run built came out as: one value for each result of each curve, surface or model, such as a pillar's rate or time.",
        "Each value states its type, double or string; a double is in the object's own unit."),
    "todaysmarketcalibration_cashflows": (
        "The cashflows of the instruments each curve was calibrated to, one row for each payment of each pillar's instrument.",
        MONEY + " Coupon is a rate; Accrual is a year fraction."),
    "trade_types": (
        "The trade types a portfolio holds, with how many trades of each.",
        "A count of trades."),
    "underlying_indices": (
        "The underlying indices a portfolio references, one row for each asset type.",
        "None: the values are names."),
    "var": (
        "The value at risk and expected shortfall of each portfolio at each configured quantile.",
        "Money in the base currency; the columns are the run's configured quantiles."),
    "xva": (
        "The valuation adjustments of each trade and netting set: CVA, DVA, funding, collateral, margin and capital, with the Basel exposures.",
        "Money in the base currency."),
    "xvaExplain": (
        "The change in CVA of each trade and netting set explained by each risk factor.",
        "Money in the base currency."),
    "xvaExplain_details": (
        "The valuation adjustments of each trade and netting set under each explain scenario.",
        "Money in the base currency."),
    "xva_par_sensitivity": (
        "The par-rate sensitivities of one valuation adjustment, one file for each adjustment.",
        "Money in the base currency."),
    "xva_zero_sensitivity": (
        "The zero-rate sensitivities of one valuation adjustment, one file for each adjustment.",
        "Money in the base currency."),
    "xvacg-cva-sensi-scenario": (
        "The CVA of each trade under each shifted factor, from the computation-graph XVA engine.",
        "Money in the base currency."),
    "xvacg-exposure": (
        "The expected positive and negative exposure at each date, from the computation-graph XVA engine.",
        "Money in the base currency."),
}
