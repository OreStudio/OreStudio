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
"""What each ORE file and each category of file is for, written by hand.

A screen shows these texts to a person who opens a file of a run, so each says
what the file is for, what it holds and how it is laid out, which part of a run
writes it and which reads it, and how a run names it. They are written from the
ORE user guide, the output catalogue of ores.ore and the file types of the
reporting seed, and kept here so that every seed that names a file shares them.
"""

CATEGORIES = {
    "master_input":
        "The master input of a run. It is the one file the ORE engine is started with, and "
        "every other file a run reads is named in it. A person opens it to see which analytics "
        "a run asked for, the as-of date and the base currency, and which configuration, trade "
        "and market data files it used.",
    "configuration":
        "The configuration documents of a run. Each is an XML document that ORE validates "
        "against its own schema, and each describes one part of how the run works: how the "
        "curves are built, which market objects exist, how each product is priced, how the "
        "market is simulated, which risk factors are shifted. ORE Studio holds their content "
        "as configuration entities and writes the documents when it packages a run, so the "
        "documents of a run show exactly which configuration produced its figures.",
    "trade_data":
        "The trade data of a run: the portfolio of trades it values, the netting sets the "
        "trades belong to with the terms of their collateral agreements, and the facts about "
        "the counterparties and collateral that the exposure and capital methods need. Every "
        "analytic runs over these trades, so they decide which rows the reports contain.",
    "market_data":
        "The market data of a run: the quotes the curves and surfaces are built from, the "
        "historical fixings of the indices the trades reference, and the historical scenarios, "
        "covariances and stress shifts that some risk analytics are given. These are plain data "
        "files, not configuration: they change every day while the configuration stays the "
        "same.",
    "calibration_report":
        "The reports of the first step of a run: the term structures ORE built from the market "
        "data and the models it calibrated. A person reads them to check that the curves and "
        "the model parameters the rest of the run relies on are sensible, before trusting any "
        "valuation or risk figure.",
    "valuation_report":
        "The valuation reports: the value of each trade today, its cashflows and the extra "
        "results its pricing engine reported. These are the basic figures of a run. Most other "
        "reports are checked against them.",
    "risk_report":
        "The market risk reports: how the value of the portfolio changes when the market moves. "
        "They hold the sensitivities to each risk factor, the values under stress scenarios, "
        "the value at risk and the profit and loss between two dates with its explanation.",
    "capital_report":
        "The regulatory capital and initial margin reports: the standardised methods regulators "
        "prescribe, such as SA-CCR, SA-CVA, BA-CVA and the standardised market risk charge, and "
        "the initial margin under ISDA SIMM or the regulatory schedule, with the CRIF "
        "sensitivities the margin is computed from.",
    "xva_report":
        "The XVA and exposure reports: the third step of a run, which post-processes the "
        "simulated values of the trades. They hold the exposure profiles of each trade and "
        "netting set through time, the valuation adjustments such as CVA, DVA, FVA, COLVA and "
        "MVA, the dynamic initial margin, and the explanation and sensitivities of the "
        "adjustments.",
    "portfolio_details":
        "Lists of what a portfolio contains and depends on: its counterparties, netting sets "
        "and trade types with how many trades each has, and the indices, market objects and "
        "risk factors its trades need. A person reads them to see what a run must be given "
        "before it can value the portfolio.",
    "intermediate_data":
        "The data files one analytic of a run writes for another to read: the simulated value "
        "cubes, the simulated market scenarios, the conversion matrices between zero and par "
        "rates. They are large and meant for machines rather than people, but they are kept "
        "so that a later run can reuse them and so that a figure in a report can be traced "
        "back to the paths it came from.",
    "run_diagnostics":
        "What the engine reports about the run itself: its log, how long each part took, how "
        "often each trade was priced, and the market data, fixings and dividends it actually "
        "loaded. A person reads them when a run fails or when a figure needs to be traced to "
        "the input it was computed from.",
    "packaging":
        "The archives ORE Studio makes to carry a run to the compute grid and back. They are "
        "not part of ORE: the input package holds every file the engine is given, the output "
        "archive holds every file the engine wrote, and the manifest lists the outputs one by "
        "one so that each can be read without unpacking the archive. They are the complete "
        "record of what a run was given and what it produced.",
}

FILES = {
    # The master input.
    "master_input":
        "The master input file of an ORE run, ore.xml. It is the single file the engine is "
        "started with, as ore ore.xml, and every other file of the run is named in it.\n\n"
        "It is an XML document with three blocks. The Setup block gives the as-of date, the "
        "input and output paths, and the names of the files that hold the trades, the market "
        "data, the fixings, the curve configuration, the conventions, today's market, the "
        "pricing engines and the reference data. The Markets block chooses which market "
        "configuration of today's market is used for each purpose, such as pricing, "
        "simulation or calibration. The Analytics block lists the analytics to run, each with "
        "an active flag and its own parameters, including the names of the reports it "
        "writes.\n\n"
        "ORE Studio writes this file when it packages a run, from the report definition and "
        "the configuration it is bound to. Reading it shows exactly what a run was asked to "
        "do.",

    # The configuration documents.
    "todays_market":
        "Today's market configuration, normally todaysmarket.xml. It decides which part of "
        "the market universe ORE builds for a run, and under which names.\n\n"
        "It is an XML document with the root element TodaysMarket. It holds blocks of "
        "discounting curves, index curves, yield curves, FX spots, FX volatilities, swaption "
        "and cap/floor volatilities, default curves, inflation curves, equity curves and the "
        "rest, each mapping a name a trade uses, such as a currency or an index, to a curve "
        "defined in the curve configuration. These blocks are gathered into named market "
        "configurations, and the Markets block of the master input picks one for each "
        "purpose, such as pricing or simulation.\n\n"
        "If the configuration does not cover what the portfolio needs, the run stops at the "
        "point where a trade asks for a market object that was not built.",
    "pricing_engines":
        "The pricing engine configuration, normally pricingengine.xml. It says how each "
        "product is priced: for every product type, the model and the pricing engine to use, "
        "with their parameters, for example an analytic engine for a European swaption or a "
        "Monte Carlo engine with its number of paths and tolerances.\n\n"
        "It is an XML document with the root element PricingEngines, validated against "
        "pricingengines.xsd. A run may be given several of them: the main one, and variants "
        "for American Monte Carlo exposure simulation (pricingengine_amc.xml), for American "
        "Monte Carlo on a computation graph, which allows sensitivities by algorithmic "
        "differentiation (pricingengine_amccg.xml), and for other special runs.\n\n"
        "A trade whose product type has no entry cannot be priced, and changing an entry "
        "changes the value of every trade of that type.",
    "simulation":
        "The simulation configuration, normally simulation.xml. It defines how the market is "
        "simulated into the future for the exposure and XVA analytics, and it describes the "
        "risk factors that the sensitivity and stress analytics may shift.\n\n"
        "It is an XML document with the root element Simulation, validated against "
        "simulation.xsd. Its Parameters block gives the time grid, such as 80 quarterly "
        "steps, the number of samples, the calendar and the random sequence. Its "
        "CrossAssetModel block gives the model of each risk factor, such as a Linear Gauss "
        "Markov model for interest rates and a lognormal model for FX, with how each is "
        "calibrated and how the factors are correlated. Its Market block lists the market "
        "objects that are simulated, with the tenors and strikes each is represented at.\n\n"
        "A run may split it into separate market, model and parameter files "
        "(simulationmarket.xml, simulationmodels.xml, simulationparams.xml), or give a "
        "market-only document of the same schema to a sensitivity or stress analytic "
        "(sensimarket.xml, stressmarket.xml and similar).",
    "sensitivity":
        "The sensitivity analysis configuration, normally sensitivity.xml. It says which risk "
        "factors the sensitivity analytic shifts and how.\n\n"
        "It is an XML document with the root element SensitivityAnalysis, validated against "
        "sensitivity.xsd. For each group of risk factors, such as discount curves, index "
        "curves, FX spots, swaption volatilities, default curves or equity spots, it gives "
        "the shift type, absolute or relative, the shift size, and the tenors, expiries and "
        "strikes to shift. It also lists the pairs of factors for which cross gammas are "
        "computed, and the par instruments used to convert zero rate sensitivities into par "
        "rate sensitivities.\n\n"
        "The sensitivity analytic shifts exactly the factors listed here and no others. "
        "Variants of the same schema configure the sensitivities taken in a stressed market "
        "(sensitivity_stress.xml) and the XVA sensitivities of a computation graph run "
        "(xvasensiconfig.xml).",
    "stress_test":
        "The stress scenario configuration, normally stresstest.xml; the guide calls it "
        "stressconfig.xml. It defines the named stress scenarios that the stress analytic "
        "applies to the market.\n\n"
        "It is an XML document with the root element StressTesting, validated against "
        "stress.xsd. Each scenario lists the market components it moves, using the same "
        "syntax as the sensitivity configuration, for example a parallel shift of a yield "
        "curve or a relative move of an FX rate. Besides absolute and relative shifts, a "
        "stress scenario may set a factor equal to a given value. Scenarios can be stated "
        "in zero rates or in par rates.\n\n"
        "The stress analytic revalues the portfolio under each scenario and reports the "
        "change from the base value in the stress test report.",
    "calendar_adjustment":
        "The calendar adjustments, calendaradjustment.xml. It adds holidays and business "
        "days to the calendars built into ORE, so a run can correct or extend a calendar "
        "without a new release of the engine.\n\n"
        "It is an XML document with the root element CalendarAdjustments, validated against "
        "calendaradjustment.xsd. For each named calendar, such as Japan, it lists the dates "
        "to add as holidays and the dates to treat as business days. It can also create a "
        "new calendar from a base calendar. Adding a date that is already a holiday does no "
        "harm.\n\n"
        "Every schedule, payment date and fixing date of every trade goes through these "
        "calendars, so a wrong adjustment moves cashflows and changes values.",
    "curve_configuration":
        "The curve configuration, normally curveconfig.xml. It says how every term structure "
        "the run needs is built from market quotes.\n\n"
        "It is an XML document with the root element CurveConfiguration, validated against "
        "curveconfig.xsd. It describes yield curves, default curves, inflation curves, "
        "equity forward curves, swaption, cap/floor, FX option, CDS and equity volatility "
        "structures, inflation cap/floor price surfaces, security spreads and recovery rates, "
        "base correlation and correlation curves, and more. Each has an id, and for each the "
        "file gives the instruments or segments it is built from, the quotes it reads, the "
        "conventions those quotes follow, and the interpolation and extrapolation.\n\n"
        "Today's market refers to these curves by id, and the market data must hold every "
        "quote a curve names for the as-of date.",
    "reference_data":
        "The reference data, referencedata.xml. It holds static data that trades refer to by "
        "id instead of carrying it inline, which keeps the portfolio small.\n\n"
        "It is an XML document with the root element ReferenceData, validated against "
        "referencedata.xsd. Each entry has an id and a type, such as Bond, with the static "
        "data of that type: for a bond, the data of its legs; for a credit or equity index, "
        "its constituents. Bond reference data is also used to build yield curves fitted to "
        "liquid bond prices.\n\n"
        "A trade that names a reference data id is completed from this file when the "
        "portfolio is built.",
    "ibor_fallback":
        "The IBOR fallback configuration, normally iborFallbackConfig.xml. It states the "
        "rules for replacing an IBOR reference rate by a risk-free rate once the IBOR ceases "
        "to be published: which risk-free index replaces each IBOR, and on what terms.\n\n"
        "It is an XML document with the root element IborFallbackConfig, validated against "
        "iborfallbackconfig.xsd. When a run is not given one, ORE uses its own standard "
        "configuration.",
    "simm_calibration":
        "The SIMM calibration, simm_calibration.xml. It holds the risk weights, correlations "
        "and concentration thresholds of a version of the ISDA Standard Initial Margin Model, "
        "so the SIMM analytic can use a version ORE does not ship or override one it does.\n\n"
        "It is an XML document with the root element SIMMCalibrationData, validated against "
        "simmcalibration.xsd. When it is given, it takes precedence over the SIMM version "
        "named in the analytic's parameters.",
    "conventions":
        "The market conventions, normally conventions.xml. It defines how each kind of "
        "market quote is turned into an instrument when a curve is built.\n\n"
        "It is an XML document with the root element Conventions, validated against "
        "conventions.xsd. Each child node is one set of conventions with a unique Id, and its "
        "node name says what kind it is: Zero, Deposit, FRA, Future, OIS, Swap, "
        "AverageOIS, TenorBasisSwap, FXSwap, CrossCurrencyBasis, CDS, Inflation and others. "
        "Each gives the day counters, calendars, business day conventions, fixing and "
        "settlement lags and index names that the quote implies.\n\n"
        "The curve configuration names a convention id for each segment, so a curve cannot "
        "be built if the convention it names is missing.",
    "currency_configuration":
        "The currency configuration, normally currencies.xml. It describes each currency a "
        "run may meet: its ISO code and number, the number of minor units, the rounding rule, "
        "the symbol and the formatting.\n\n"
        "It is an XML document with the root element CurrencyConfig, validated against "
        "currencyconfig.xsd. Amounts are rounded and reported according to it, and a currency "
        "missing from both this file and ORE's built-in list cannot be priced.",
    "credit_simulation":
        "The credit simulation configuration, creditsimulation.xml. It defines the credit "
        "migration model the XVA analytic uses to simulate defaults and rating changes along "
        "the paths: the rating states and the transition matrices between them.\n\n"
        "It is an XML document with the root element CreditSimulation, validated against "
        "creditsimulation.xsd. It is only read when the XVA analytic is asked for credit "
        "migration.",
    "crif_market":
        "The market description of the CRIF analytic, normally crifmarket.xml. It lists the "
        "market objects, with their tenors and strikes, that are needed to compute the ISDA "
        "SIMM sensitivities written to the CRIF.\n\n"
        "It is an XML document with the same schema as the market part of a simulation "
        "configuration, simulation.xsd. The CRIF analytic reads it in place of a simulation "
        "configuration.",
    "script_library":
        "The script library, scriptlibrary.xml. It holds the payoff scripts that scripted "
        "trades run, written in ORE's payoff scripting language, so a product that no "
        "built-in trade type covers can still be priced.\n\n"
        "It is an XML document with the root element ScriptLibrary, validated against "
        "scriptlibrary.xsd. Each script has a name and the code of the payoff. A scripted "
        "trade names the script it runs.",

    # The trade data.
    "trade_data":
        "The portfolio, normally portfolio.xml. It lists the trades a run values, and every "
        "analytic runs over them, so it decides which rows every report contains.\n\n"
        "It is an XML document with the root element Portfolio, validated against input.xsd. "
        "Each Trade has an id, a trade type such as Swap, FxForward or EuropeanSwaption, and "
        "an Envelope with the counterparty, the netting set it belongs to and any "
        "additional fields, followed by the full data of the instrument: its legs, "
        "schedules, notionals, rates, options and underlyings. The ORE product catalogue "
        "describes the data of every trade type.\n\n"
        "A profit and loss run is also given the portfolio as it stands after the margin "
        "period of risk (mporportfolio.xml), so it can value the trades at both dates.",
    "netting_set_definitions":
        "The netting set definitions, normally netting.xml. For each netting set it states "
        "the ISDA agreement the trades are netted under and, if the agreement has a credit "
        "support annex, the terms of the collateral.\n\n"
        "It is an XML document with the root element NettingSetDefinitions, validated "
        "against nettingsetdefinitions.xsd. Each NettingSet has an id and either says it is "
        "uncollateralised or gives the CSA terms: the thresholds and minimum transfer "
        "amounts of both parties, the independent amount, the margining frequency, the "
        "margin period of risk, the collateral currency and the eligible collateral.\n\n"
        "Exposure, XVA, SA-CCR and BA-CVA depend on these terms. A trade names its netting "
        "set in its envelope.",
    "counterparty_information":
        "The counterparty information, normally counterparty.xml. For each counterparty it "
        "holds the attributes that the regulatory capital and CVA methods need.\n\n"
        "It is an XML document with the root element CounterpartyInformation, validated "
        "against counterparty.xsd. Its Counterparties node lists each counterparty with its "
        "id and, where an analytic needs them, whether it is a clearing counterparty, its "
        "credit quality, its supervisory risk weights for BA-CVA and SA-CCR, and its SA-CVA "
        "risk bucket. Its Correlations node gives the correlations between counterparties.\n\n"
        "The standardised methods read it to assign risk weights and supervisory factors to "
        "the exposures of each counterparty.",
    "collateral_balances":
        "The collateral balances, normally collateralbalances.xml. For each netting set it "
        "holds the collateral currently exchanged under its credit support annex: the "
        "variation margin, the initial margin and the independent amount, with their "
        "currency.\n\n"
        "It is an XML document with the root element CollateralBalances, validated against "
        "collateralbalance.xsd, with one CollateralBalance node for each netting set. The "
        "XVA, SA-CCR and BA-CVA analytics subtract these balances from the value of the "
        "netting set to get the net exposure today.",
    "crif_schedule":
        "The trade data for the regulatory schedule initial margin, normally "
        "crif_schedule.csv. The IM Schedule method computes initial margin from very little "
        "information about each trade, and this file gives it that information in CRIF form.\n\n"
        "It is a comma separated file with a header line. Each trade has two rows: one with "
        "the risk type PV, whose amount is the trade's value, and one with the risk type "
        "Notional, whose amount is its notional. Each row also states the product class and "
        "the end date of the trade. The imschedule analytic reads it.",

    # The market data.
    "market_data":
        "The market data, normally market.txt. It holds the quotes that every curve, "
        "surface and model of the run is built from.\n\n"
        "It is a plain text file with one quote on each line, in three columns: the date, the "
        "quote name and the value, as in 2011-01-31 IR_SWAP/RATE/EUR/2D/3M/1Y 0.014400. A "
        "quote name is a sequence of fields separated by slashes. It starts with the "
        "instrument type and the quote type, such as IR_SWAP/RATE or FX/RATE, and the fields "
        "after them identify the instrument: for a swap rate, the currency, the start lag, the "
        "index tenor and the term; for an FX rate, the two currencies. Values are plain "
        "numbers, not percentages.\n\n"
        "The curve configuration names the quotes each curve needs, and the run reads their "
        "values for the as-of date from this file. A quote that is named but missing makes "
        "the curve fail to build.",
    "fixing_history":
        "The fixing history, normally fixings.txt. It holds the past values of the indices "
        "the trades reference.\n\n"
        "It is a plain text file with one fixing on each line, in three columns separated by "
        "semicolons or blanks: the index name, such as EUR-EURIBOR-6M or USD-SOFR, the "
        "fixing date and the value. Values are plain numbers, not percentages or basis "
        "points.\n\n"
        "A trade reads a fixing when a coupon was fixed before the as-of date, or when it has "
        "another feature that depends on the path of an index. A missing fixing makes such a "
        "coupon impossible to value.",
    "covariance":
        "The covariance data for the parametric value at risk, normally covariance.csv. ORE "
        "does not estimate covariances, so a parametric VaR run is given them from outside.\n\n"
        "It is a file with three columns separated by blanks, tabs or commas: the first risk "
        "factor, the second risk factor and their covariance. The risk factors are named as "
        "in the sensitivity report, for example DiscountCurve/EUR/0/6M.",
    "historical_scenarios":
        "The historical scenarios, normally scenarios.csv. It holds the past values of the "
        "risk factors, from which the historical simulation VaR, the correlation analytic and "
        "historical XVA runs take their scenarios.\n\n"
        "It is a comma separated file with a header line. The first columns are Date, Scenario "
        "and Numeraire, and every other column is one risk factor named as in the simulation "
        "market, such as DiscountCurve/EUR/0. Each row gives the value of every factor on one "
        "date. The factors must match the simulation configuration the analytic is given, and "
        "the moves between two dates of the history become the scenarios applied to today's "
        "market.",
    "zero_stress_scenario_data":
        "Stress scenarios stated directly as shifts of zero rates, normally "
        "zeroStressScenarioData.xml. The stress and sensitivity analytics can apply these "
        "shifts to the market without converting them from par rates first.\n\n"
        "It is an XML document listing, for each scenario, the zero rate shift of each "
        "curve at each tenor. It is often the output of a par stress conversion, kept so that "
        "later runs can reuse it.",
}

FILES.update({
    # The calibration reports.
    "curves_report":
        "The curves report, normally curves.csv. It shows the discount factors of the yield "
        "curves the run built, so a person can check the curves against the market before "
        "trusting any value computed from them.\n\n"
        "It is a comma separated file with a header line. The first columns are Date and "
        "Tenor, one row for each date of the grid the curves analytic is given, and every "
        "other column is one curve, named as in today's market, such as USD, EUR-EONIA or "
        "EUR-EURIBOR-6M. Each value is a discount factor, without unit; which curves appear "
        "depends on the run's configuration. The curves analytic writes it, under the name "
        "its outputFileName parameter gives.",
    "credit_curves":
        "The credit curves report, normally creditcurves.csv. It lists the survival "
        "probabilities of the default curves the run built on a grid of dates, so the credit "
        "curves can be checked the way the curves report checks the yield curves.\n\n"
        "It is a comma separated file with a header line. ORE 1.8.17 has no writer for the "
        "parameter that names it, so a run of that version does not produce it.",
    "todays_market_calibration":
        "The calibration report of today's market, normally todaysmarketcalibration.csv. It "
        "states what each curve, surface and model of the market came out as after it was "
        "built: the pillar dates, times, rates, discount factors, volatilities and the other "
        "results each one reports.\n\n"
        "It is a comma separated file with a header line. Each row is one result of one market "
        "object: MarketObjectType and MarketObjectId name the object, ResultId and up to three "
        "keys name the result, such as a pillar, and ResultType says whether ResultValue is a "
        "number or text. A number is in the object's own unit. The todaysMarketCalibration "
        "analytic writes it.",
    "todays_market_calibration_cashflows":
        "The cashflows behind today's market calibration, normally "
        "todaysmarketcalibration_cashflows.csv. For each curve that was bootstrapped, it lists "
        "the cashflows of the instrument at each pillar, so a person can see exactly which "
        "deposit, swap or future fixed each point of the curve.\n\n"
        "It is a comma separated file with a header line, one row for each payment of each "
        "pillar's instrument. PillarDate, MarketObjectType and MarketObjectId name the curve "
        "and the pillar, RateHelperType and MdLabel name the instrument and its quote, and "
        "LegNo and CashflowNo name the payment. The figures are the same as in the cashflow "
        "report: the amount, coupon, accrual, fixing, notional, discount factor and present "
        "value, in the currency of the row, with the present value also in the base currency. "
        "The todaysMarketCalibration analytic writes it.",
    "calibration_report":
        "The calibration report, normally calibration.csv. It records the cross asset model "
        "that the calibration analytic fitted to the market: for each currency and asset the "
        "calibrated volatilities and mean reversions, so the model behind a simulation can be "
        "inspected and reused.\n\n"
        "It is a comma separated file whose CrossAssetModel column holds the calibrated model "
        "as one XML document, in the same form as the CrossAssetModel block of the simulation "
        "configuration. It is configuration, not figures, and has no unit. A later run can be "
        "given it to simulate without calibrating again.",
    "mean_reversion":
        "The mean reversion report, normally meanReversion.csv, or one file for each currency "
        "such as meanReversion_EUR.csv. It gives the mean reversion and volatility parameters "
        "the calibration analytic found for a Hull-White model of interest rates, so they can "
        "be compared between runs.\n\n"
        "It is a comma separated file with a header line, one row for each principal "
        "component, with the volatility and mean reversion parameters of the model in columns "
        "such as v_0, v_1, kappa_0 and kappa_1. The parameters have no unit. The calibration "
        "analytic writes it when its meanReversionOutputFileName parameter names it.",
    "pca":
        "The principal component report, normally pca.csv, or one file for each currency such "
        "as pca_EUR.csv. It holds the principal components of the historical moves of the "
        "yield curves, from which the volatility structure of the interest rate model is "
        "derived.\n\n"
        "It is a comma separated file with a header line, one row for each principal "
        "component, with its EigenValue and its loading on each tenor in the columns "
        "EigenVector_0, EigenVector_1 and so on. The figures have no unit. The calibration "
        "analytic writes it when its pcaOutputFileName parameter names it.",
    "eigenvalues":
        "The eigenvalues of a principal component analysis of historical curve moves, normally "
        "eigenvalues.csv. They say how much of the movement of the curves each component "
        "explains.\n\n"
        "It is a comma separated file with a header line and two columns: number, the index "
        "of the component, and value, its eigenvalue, without unit. The calibration analytic "
        "writes it next to the eigenvectors.",
    "eigenvectors":
        "The eigenvectors of a principal component analysis of historical curve moves, normally "
        "eigenvectors.csv. They give the shape of each component: how much each tenor of the "
        "curve moves when the component moves.\n\n"
        "It is a comma separated file with a header line, one row for each tenor, with the "
        "tenor in the first column and the loading of each component in the columns "
        "eigenvector_1, eigenvector_2 and so on. The loadings have no unit. The calibration "
        "analytic writes it next to the eigenvalues.",
    "correlations":
        "The correlations between risk factors, normally correlation.csv. The correlation "
        "analytic estimates them from historical scenarios, and the XVA analytic can be given "
        "them to correlate the factors it simulates.\n\n"
        "It is a comma separated file with a header line and one row for each pair of risk "
        "factors: RiskFactor1, RiskFactor2 and Correlation, a number between -1 and 1. The "
        "factors are named as in the simulation market, such as DiscountCurve/EUR/0.",

    # The valuation reports.
    "npv_report":
        "The NPV report, normally npv.csv. It holds the value of each trade at the as-of "
        "date. It is the basic result of a run, and most other reports are checked against "
        "it.\n\n"
        "It is a comma separated file with a header line and one row for each trade. TradeId, "
        "TradeType and NettingSet identify the trade, and Maturity, MaturityTime and "
        "CounterParty describe it. NPV is the value in the trade's own currency, given in "
        "NpvCurrency, and NPV(Base) is the same value in the run's base currency, given in "
        "BaseCurrency. Notional and Notional(Base) give the current notional in "
        "NotionalCurrency and in the base currency. A trade that failed to build or price "
        "still has a row, with the trade type Failed and a value of zero; the log says "
        "why.\n\n"
        "The npv analytic writes it, under the name its outputFileName parameter gives. ORE "
        "Studio stores its figures for every run, so they can be compared between runs.",
    "swaption_npv":
        "The NPV report of the swaptions of a run, normally swaption_npv.csv. It has the same "
        "columns as the NPV report, restricted to swaptions: TradeId, TradeType, NettingSet, "
        "the maturity and counterparty, NPV in NpvCurrency and NPV(Base) in the base currency, "
        "and the notional in its currency and in the base currency.\n\n"
        "It is a comma separated file with a header line and one row for each swaption. The "
        "npv analytic writes it when a run asks for it.",
    "cashflow_report":
        "The cashflow report, normally flows.csv. It lists every future payment of every "
        "trade, so a person can see how the value of a trade is made up and when its money "
        "moves.\n\n"
        "It is a comma separated file with a header line and one row for each payment of each "
        "leg of each trade. TradeId, LegNo and CashflowNo identify the payment. Type, "
        "PayDate, FlowType and Currency describe it, and AccrualStartDate, AccrualEndDate and "
        "fixingDate give its period and fixing. Amount is the payment in its currency, Coupon "
        "is the rate, Accrual the year fraction, AccruedAmount what has accrued so far, "
        "fixingValue the index fixing, Notional the notional, DiscountFactor the factor it is "
        "discounted with, PresentValue its value today, FXRate(Local-Base) the rate to the "
        "base currency, and PresentValue(Base) its value in the base currency. For capped or "
        "floored coupons the strikes and volatilities are given too.\n\n"
        "The cashflow analytic writes it, under the name its outputFileName parameter gives.",
    "additional_results_report":
        "The additional results report, normally additional_results.csv. Besides the value of "
        "a trade, a pricing engine reports further results: the model and engine it used, "
        "intermediate values such as the forward, the volatility or the strike it priced at, "
        "and projected cashflows. This report holds them, so a person can see how a value was "
        "obtained.\n\n"
        "It is a comma separated file with a header line and one row for each result of each "
        "trade: TradeId, ResultId, ResultType and ResultValue. ResultType says whether the "
        "value is text, a number or a list of cashflows. A number is in the trade's currency "
        "unless its name says otherwise. The additionalResults analytic writes it.",

    # The risk reports.
    "sensitivity_report":
        "The sensitivity report, normally sensitivity.csv. It says how the value of each trade "
        "changes when each risk factor is shifted by the amount the sensitivity configuration "
        "gives. It is the main market risk output of a run, and the value at risk analytics "
        "and the par conversion read it.\n\n"
        "It is a comma separated file with a header line and one row for each trade and risk "
        "factor, or pair of factors for a cross gamma. TradeId names the trade, and Factor_1 "
        "and ShiftSize_1, with Factor_2 and ShiftSize_2 for a cross gamma, name the factors and "
        "the shifts, a shift being in the factor's own unit. IsPar says whether the factor is "
        "a zero rate or a par rate. Base NPV is the value before the shift, Delta the first "
        "order change and Gamma the second order change, all in the currency given in "
        "Currency; the columns marked (Trade) give the same figures in the trade's own "
        "currency.\n\n"
        "The sensitivity analytic writes it, under the name its sensitivityOutputFile parameter "
        "gives.",
    "sensitivity_atm_only":
        "The sensitivity report when only the at-the-money volatilities are shifted, normally "
        "sensitivity_atmOnly.csv. It lets a person compare the vega of a trade to the "
        "at-the-money point alone with its vega to the full volatility surface.\n\n"
        "It has the same columns as the sensitivity report: TradeId, IsPar, the factors and "
        "shift sizes, Currency, Base NPV, Delta and Gamma, and the same figures in the trade's "
        "currency. The sensitivity analytic writes it.",
    "sensitivity_full_surface":
        "The sensitivity report when the full volatility surface is shifted, strike by "
        "strike, normally sensitivity_fullSurface.csv. It is the counterpart of the "
        "at-the-money-only report.\n\n"
        "It has the same columns as the sensitivity report: TradeId, IsPar, the factors and "
        "shift sizes, Currency, Base NPV, Delta and Gamma, and the same figures in the trade's "
        "currency. The sensitivity analytic writes it.",
    "sensitivity_scenario":
        "The scenario values behind a sensitivity run, normally sensitivity_scenario.csv. For "
        "each trade it gives its value in every shifted market the sensitivity analytic "
        "built, so a sensitivity can be recomputed from the two values it came from.\n\n"
        "It is a comma separated file with a header line and one row for each trade, scenario "
        "and factor. TradeId, Scenario and Factor name the row, Up/Down says the direction of "
        "the shift and ShiftSize_1 and ShiftSize_2 its size. Base NPV is the value in the base "
        "market, Scenario NPV the value in the shifted market and Difference the change, all "
        "in the currency given in Base Currency.",
    "scenario":
        "The scenario report, normally scenario.csv. The sensitivity analytic writes the value "
        "of each trade under each shifted factor to it, and the scenario analytic writes the "
        "base scenario of the simulation market. Some analytics read a file in this form as "
        "the scenario they work from.\n\n"
        "As a sensitivity output it is a comma separated file with a header line and one row "
        "for each trade and factor: TradeId, Factor, Up/Down, ShiftSize_1, ShiftSize_2, Base "
        "Currency, Base NPV, Scenario NPV and Difference, the values in the base currency.",
    "scenario_full_surface":
        "The scenario values of a sensitivity run that shifts the full volatility surface, "
        "normally scenario_fullSurface.csv. It is the scenario report for the full surface "
        "sensitivities.\n\n"
        "It is a comma separated file with a header line and one row for each trade and "
        "factor: TradeId, Factor, Up/Down, ShiftSize_1, ShiftSize_2, Base Currency, Base NPV, "
        "Scenario NPV and Difference.",
    "scenario_par_rates":
        "The par rates of a sensitivity run, normally scenario_par_rates.csv. For each par "
        "instrument it gives its par rate in the base market and in the shifted market, so "
        "the shifts can be checked in the par domain a person quotes in.\n\n"
        "It is a comma separated file with a header line and one row for each par instrument: "
        "ParKey names the instrument, BaseParRate and ScenarioParRate give the two rates.",
    "par_sensitivity":
        "The par sensitivity report, normally parsensitivity.csv. It says how the value of "
        "each trade changes when the par rate of each instrument, such as a swap rate or a "
        "deposit rate, moves, which is how traders quote and hedge risk. The sensitivity "
        "analytic computes it from the zero rate sensitivities through the Jacobian.\n\n"
        "It has the same columns as the sensitivity report: TradeId, IsPar, which is true "
        "here, the factors and shift sizes, Currency, Base NPV, Delta and Gamma, and the same "
        "figures in the trade's currency. The sensitivity analytic writes it under the name "
        "its parSensitivityOutputFile parameter gives.",
    "par_conversion_sensitivity":
        "The par sensitivities computed by the zero to par conversion, normally "
        "parconversion_sensitivity.csv. The zeroToParSensiConversion analytic reads zero rate "
        "sensitivities, from this run or from a file, and converts them into par rate "
        "sensitivities.\n\n"
        "It is a comma separated file with a header line and one row for each trade and par "
        "factor: TradeId, IsPar, the factors and shift sizes, Currency, Base NPV and Delta, and "
        "the same figures in the trade's currency. The analytic writes it under the name its "
        "outputFile parameter gives.",
    "par_shifts":
        "The par shifts of a stress scenario, normally parshifts.csv. ORE shifts the market in "
        "zero rates, but an explanation of profit and loss on par sensitivities needs the "
        "market moves as par rate shifts. The zeroToParShift analytic converts the zero rate "
        "moves of a scenario into the equivalent par rate moves and writes them here.\n\n"
        "It is a comma separated file with a header line and one row for each scenario and par "
        "instrument: ScenarioLabel, ParKey and ParShift, the shift in the instrument's quote "
        "unit, usually a rate.",
    "cross_gamma":
        "The cross gamma report, normally crossgamma.csv. For pairs of risk factors it gives "
        "the second order change in value when both are shifted together, the part of the "
        "change a first order sensitivity misses.\n\n"
        "It is a comma separated file with a header line. ORE 1.8.17 has no writer for the "
        "parameter that names it: the cross gammas appear in the sensitivity report instead, "
        "in the rows that name two factors.",
    "product_class_mapping":
        "The product class mapping, normally productclass_mapping.csv. It maps the trades of a "
        "run to the product classes of the sensitivity report.\n\n"
        "It is a comma separated file with a header line. ORE 1.8.17 has no writer for the "
        "parameter that names it, so a run of that version does not produce it.",
    "stress_test_report":
        "The stress test report, normally stresstest.csv; some runs name it stress.csv. For each "
        "trade and each stress scenario of the stress configuration it gives the value before "
        "and after the stress, so a person can see how the portfolio would fare in each "
        "scenario.\n\n"
        "It is a comma separated file with a header line and one row for each trade and "
        "scenario: TradeId, ScenarioLabel, Base NPV, the value in today's market, Scenario NPV, "
        "the value in the stressed market, and Sensitivity, the change between them, in the "
        "base currency. The stress analytic writes it, and so do the parStressConversion and "
        "zeroToParShift analytics, under the name their scenarioOutputFile parameter gives.",
    "stress_test_flows":
        "The stress test cashflow report, normally stresstest_flows.csv. It lists the "
        "cashflows of each trade in each stress scenario next to the same cashflows in today's "
        "market, so a stressed value can be traced to the payments that changed.\n\n"
        "It is a comma separated file with a header line and one row for each payment of each "
        "leg of each trade in each scenario. TradeId, ScenarioLabel, LegNo and CashflowNo "
        "identify the row, and the payment is described as in the cashflow report. Each "
        "figure is given three times: with the suffix _Base in today's market, _Scen in the "
        "stressed market and _Diff for the change, for the amount, coupon, accrued amount, "
        "fixing, notional, discount factor and present value. The stress analytic writes it "
        "under the name its scenarioCashflowOutputFile parameter gives.",
    "stress_scenario_par_rates":
        "The par rates of a stress run, normally stress_scenario_par_rates.csv. For each "
        "stress scenario and par instrument it gives the par rate in today's market and in "
        "the stressed market, so a stress stated in zero rates can be read in par terms.\n\n"
        "It is a comma separated file with a header line and one row for each scenario and par "
        "instrument: Scenario, ParKey, BaseParRate and ScenarioParRate.",
    "var_report":
        "The value at risk report, normally var.csv. It gives the value at risk and the "
        "expected shortfall of each portfolio at each quantile the run asks for, broken down "
        "by risk class and risk type when the run asks for a breakdown.\n\n"
        "It is a comma separated file with a header line and one row for each portfolio, risk "
        "class and risk type. There is one column for each quantile, such as Quantile_0.990000, "
        "and one for the expected shortfall at each, such as ExpectedShortfall_0.990000, both "
        "in the base currency; the quantiles come from the run's configuration. The "
        "historicalSimulationVar and parametricVar analytics write it, under the name their "
        "outputFile parameter gives.",
    "historical_pnl":
        "The historical profit and loss behind a historical simulation VaR, normally "
        "historical_PnL.csv. For each portfolio it gives the change in value the portfolio "
        "would have had over each historical period, the distribution the value at risk is "
        "read from.\n\n"
        "It is a comma separated file with a header line and one row for each portfolio, risk "
        "class, risk type and historical period: Portfolio, PLDate1 and PLDate2, the start and "
        "end of the period, and PLAmount, the profit or loss. The historicalSimulationVar "
        "analytic writes it.",
    "pnl_report":
        "The profit and loss report, normally pnl.csv. For each trade it gives the change in "
        "value between the as-of date and a later date, and splits that change into its "
        "sources: the passing of time, cashflows paid, new, matured and terminated trades, "
        "amendments, and the move of the market.\n\n"
        "It is a comma separated file with a header line and one row for each trade. TradeId "
        "and TradeType identify it, with its maturity, start and end dates and Currency. The "
        "NPV columns give the value under each combination of date, market and portfolio, "
        "such as NPV(t0_m0_p0) for today's trades in today's market and NPV(t1_m1_p1) for the "
        "later trades in the later market. The other columns give the parts of the profit and "
        "loss: Day1PnL, TradeChangePnL, PeriodCashFlow, New, Matured, Terminated, Amendments, "
        "Theta, HypotheticalCleanPnL, CleanPnL and DirtyPnL. All are money in the currency of "
        "the row. The pnl analytic writes it under the name its outputFileName parameter "
        "gives.",
    "pnl_explain_report":
        "The profit and loss explain report, normally pnl_explain.csv. It has every column of "
        "the profit and loss report and adds an explanation of the market part: how much of "
        "the change in value each kind of risk factor caused, computed from the "
        "sensitivities, and how much is left unexplained.\n\n"
        "It is a comma separated file with a header line and one row for each trade. After the "
        "columns of the profit and loss report come ScenarioPnl, TotalDelta, TotalGamma and "
        "TotalVega, and the delta, gamma and vega parts for each asset class, such as IrDelta, "
        "EqVega or FxDelta. All are money in the currency of the row. The pnlExplain analytic "
        "writes it under the name its outputFileName parameter gives.",
})

FILES.update({
    # The capital and margin reports.
    "simm_report":
        "The SIMM report, normally simm.csv. It gives the initial margin of each portfolio "
        "under the ISDA Standard Initial Margin Model, computed from the sensitivities in the "
        "CRIF, broken down level by level.\n\n"
        "It is a comma separated file with a header line. Portfolio names the portfolio, and "
        "ProductClass, RiskClass, MarginType and Bucket say which part of the margin a row "
        "is, with All marking a total. SimmSide says whether the margin is collected or "
        "posted, Regulation names the regulation it is computed under, and InitialMargin is "
        "the margin in the currency given in Currency, computed in CalculationCurrency. The "
        "simm analytic writes it.",
    "simm_data":
        "The data the SIMM margin was computed from, normally simm_data.csv. It lists the "
        "aggregated sensitivities of each portfolio as the SIMM analytic used them, so a "
        "margin figure can be traced to its inputs.\n\n"
        "It is a comma separated file with a header line and one row for each portfolio, risk "
        "type, product class, qualifier, bucket and label, in the form of a CRIF: Amount in "
        "AmountCurrency and AmountUSD in US dollars, with the regulations under which the "
        "margin is collected and posted. The simm analytic writes it.",
    "simm_regulation_breakdown":
        "The SIMM margin broken down by regulation, normally regulation_breakdown_simm.csv. "
        "When trades fall under more than one regulation, such as those of the US, the EU or "
        "Japan, the margin can differ by regulation, and this report states it for each.\n\n"
        "It has the same columns as the SIMM report: Portfolio, ProductClass, RiskClass, "
        "MarginType, Bucket, SimmSide, Regulation, InitialMargin, Currency and "
        "CalculationCurrency. The simm analytic writes it.",
    "crif":
        "The CRIF, normally crif.csv: the sensitivities of each trade in the Common Risk "
        "Interchange Format that ISDA defines for SIMM. It is the input of the SIMM "
        "calculation, and the standard way to exchange margin sensitivities with a "
        "counterparty.\n\n"
        "It is a comma separated file with a header line and one row for each trade, risk "
        "type, qualifier, bucket and label. TradeID, PortfolioID and TradeType identify the "
        "trade; ProductClass, RiskType, Qualifier, Bucket, Label1 and Label2 identify the "
        "sensitivity as SIMM defines it; Amount is the sensitivity in AmountCurrency and "
        "AmountUSD the same in US dollars; IMModel, the collect and post regulations and the "
        "end date complete the row.\n\n"
        "The crif analytic writes it from a par sensitivity run with a SIMM specific "
        "configuration, and the simm analytic reads it. ORE's own CRIF covers interest rate and "
        "FX risk; a CRIF given from outside may cover every risk class.",
    "capital_crif":
        "The capital CRIF, normally capital_crif.csv: the sensitivities of each trade that the "
        "regulatory capital methods use, in CRIF form, with the counterparty and netting set "
        "each belongs to.\n\n"
        "It is a comma separated file with a header line and one row for each trade, risk "
        "type and label. TradeID, PortfolioID and TradeType identify the trade, "
        "CounterpartyName, CounterpartyID and NettingSetNumber say whom it faces, RiskType, "
        "HedgingSet, Qualifier, Bucket and the labels identify the sensitivity, Regulation "
        "and Model say how it is used, and Amount is the sensitivity in AmountCurrency, with "
        "AmountUSD in US dollars.",
    "im_schedule":
        "The initial margin under the regulatory schedule, normally im_schedule.csv. The "
        "schedule approach computes the margin from the notional, product class and maturity "
        "of each trade, as a simpler alternative to SIMM, reduced by the net-to-gross "
        "ratio of the portfolio.\n\n"
        "It is a comma separated file with a header line and one row for each portfolio, "
        "product class, side and regulation: GrossIM, the margin before netting, "
        "GrossCurrentRC and NetCurrentRC, the replacement cost before and after netting, "
        "NetToGrossRatio, without unit, and ScheduleIM, the margin, in the currency given in "
        "Currency. The imschedule analytic writes it.",
    "im_schedule_trade":
        "The schedule initial margin of each trade, normally im_schedule_trade.csv. It shows "
        "how the schedule margin of a portfolio is built up from its trades.\n\n"
        "It is a comma separated file with a header line and one row for each trade: TradeId "
        "and Portfolio, the product class, end date and label, Maturity as a year fraction, "
        "the Multiplier the schedule applies, without unit, the Notional and PV in their own "
        "currencies and in the base currency, and GrossIM(Base), the trade's margin in the "
        "base currency. The imschedule analytic writes it.",
    "saccr_report":
        "The SA-CCR report, normally saccr.csv. It gives the exposure at default of each "
        "netting set under the standardised approach for counterparty credit risk, with the "
        "parts it is made of, and the capital charge that follows from it.\n\n"
        "It is a comma separated file with a header line and one row for each netting set, "
        "asset class and hedging set. AddOn is the potential future exposure add-on, NPV the "
        "value, IndependentAmountHeld, InitialMargin, VariationMargin, ThresholdAmount and "
        "MinimumTransferAmount the collateral terms, RC the replacement cost, Multiplier the "
        "multiplier, PFE the potential future exposure and EAD the exposure at default; RW "
        "is the risk weight and CC the capital charge. Multiplier and RW have no unit, the "
        "rest is money. The saccr analytic writes it.",
    "saccr_detail":
        "The SA-CCR terms of each trade, normally saccr_detail.csv. It shows how each trade "
        "enters the SA-CCR calculation: which hedging set it falls in and which supervisory "
        "terms apply to it.\n\n"
        "It is a comma separated file with a header line and one row for each trade: TradeId, "
        "NettingSet and TradeType, AssetClass, HedgingSet, HedgingSubset and Qualifier, the "
        "NPV and Currency, and the SA-CCR terms: delta, the supervisory delta, d, the adjusted "
        "notional, MF, the maturity factor, M, S, E and T, the maturity, start, end and "
        "exercise times as year fractions, SD, the supervisory duration, and the bucket, "
        "price, strike and volatility where they apply. The saccr analytic writes it.",
    "sacva_report":
        "The SA-CVA report, normally sacva.csv. It gives the capital charge for CVA risk of "
        "each netting set under the standardised approach, built from the sensitivities of the "
        "CVA to the risk factors.\n\n"
        "It is a comma separated file with a header line and one row for each netting set, "
        "risk type, margin type and bucket, with All marking a total: NettingSetId, RiskType, "
        "MarginType, Bucket, Analytic and Value, the charge. The sacva analytic writes it.",
    "sacva_detail":
        "The SA-CVA calculation in detail, normally sacvadetail.csv. It lists each sensitivity "
        "the SA-CVA charge is computed from, with the risk weight applied to it.\n\n"
        "It is a comma separated file with a header line and one row for each netting set and "
        "risk factor: NettingSetId, RiskFactor, RiskType, Bucket, MarginType, CvaType, "
        "Sensitivity, in the base currency, and RiskWeight, without unit. The sacva analytic "
        "writes it.",
    "sacva_sensitivity":
        "The net SA-CVA sensitivities, normally sacva_sensitivity.csv: the CVA sensitivities "
        "of each netting set after netting, by risk type, CVA type and margin type, which are "
        "the figures the SA-CVA charge is computed on.\n\n"
        "It is a comma separated file with a header line and one row for each netting set and "
        "risk factor: NettingSet, RiskFactor, RiskType, CvaType, MarginType, Bucket and Value. "
        "The sacva analytic writes it under the name its saCvaNetSensitivitiesFile parameter "
        "gives.",
    "sacva_sensitivities":
        "The SA-CVA sensitivities before netting, normally sacva_sensitivities.csv: the "
        "sensitivity of the CVA of each netting set to each risk factor, before netting and "
        "before any risk weight.\n\n"
        "It is a comma separated file with a header line and one row for each netting set and "
        "risk factor: NettingSet, RiskFactor, RiskType, CvaType, MarginType, Bucket and Value. "
        "The sacva analytic writes it.",
    "bacva_report":
        "The BA-CVA report, normally bacva.csv. It gives the capital charge for CVA risk of "
        "each netting set under the basic approach, which uses the supervisory risk weight of "
        "the counterparty and the exposure at default rather than sensitivities.\n\n"
        "It is a comma separated file with a header line and one row for each netting set and "
        "quantity: NettingSet, Counterparty, Analytic, which names the quantity, and Value. "
        "The bacva analytic writes it.",
    "smrc_report":
        "The standardised market risk charge report, normally smrc.csv. It gives the charge "
        "for each trade type and asset, computed from the signed notional and a supervisory "
        "risk weight.\n\n"
        "It is a comma separated file with a header line and one row for each trade type and "
        "asset: TradeType, Asset, RiskWeight, without unit, SignedNotional and SMRC, the "
        "charge. The smrc analytic writes it.",
    "smrc_detail":
        "The standardised market risk charge terms of each trade, normally smrcdetail.csv. It "
        "shows how each trade enters the charge.\n\n"
        "It is a comma separated file with a header line and one row for each trade: TradeId, "
        "NettingSet, TradeType, Asset, MaturityDate, Maturity as a year fraction, NPV, "
        "SignedNotional and RiskWeight, without unit. The smrc analytic writes it.",
    "credit_migration":
        "The credit migration report, one file for each quantity reported, such as "
        "credit_migration_bond1.csv. It gives the distribution of the value of a credit "
        "portfolio when the issuers may default or change rating, as a credit portfolio model "
        "computes it.\n\n"
        "It is a comma separated file with a header line and one row for each bucket of the "
        "distribution: upperBucketBound, the upper bound of the bucket in the base currency, "
        "pdf, the probability of the bucket, and cdf, the probability up to it.",

    # The XVA and exposure reports.
    "xva_report":
        "The XVA report, normally xva.csv. It gives the valuation adjustments of each netting "
        "set and of each trade within it: the price of the counterparty's and the bank's "
        "credit risk, of funding, of collateral, of initial margin and of capital.\n\n"
        "It is a comma separated file with a header line. Rows with a TradeId give the "
        "allocation to a trade; the netting set rows give the total of each NettingSetId. "
        "CVA and DVA are the credit and debit valuation adjustments; FBA and FCA the funding "
        "benefit and cost, also stated without the bank's own survival probability "
        "(exOwnSP) and without any (exAllSP); COLVA the collateral adjustment and "
        "CollateralFloor its floor; MVA the margin adjustment; OurKVACCR, TheirKVACCR, "
        "OurKVACVA and TheirKVACVA the capital adjustments; AllocatedCVA and AllocatedDVA "
        "the allocation to the trade under AllocationMethod; BaselEPE and BaselEEPE the "
        "Basel exposures. All are money in the base currency. The xva analytic writes it.",
    "trade_exposure_report":
        "The exposure profile of one trade, one file for each trade, named exposure_trade_ "
        "followed by the trade id. It shows how much the bank could lose on the trade if the "
        "counterparty defaulted at each future date, as the Monte Carlo simulation sees it.\n\n"
        "It is a comma separated file with a header line and one row for each date of the "
        "simulation grid: TradeId, Date and Time, the year fraction to the date, then EPE and "
        "ENE, the expected positive and negative exposure, AllocatedEPE and AllocatedENE, the "
        "trade's share of its netting set's exposure, PFE, the potential future exposure at "
        "the configured quantile, and the Basel measures BaselEE, BaselEEE, "
        "TimeWeightedBaselEPE and TimeWeightedBaselEEPE. All are money in the base currency. "
        "The xva analytic writes it.",
    "netting_set_exposure_report":
        "The exposure profile of one netting set, one file for each netting set, named "
        "exposure_nettingset_ followed by the netting set id. It shows how much the bank could "
        "lose on the netting set at each future date, after netting and collateral.\n\n"
        "It is a comma separated file with a header line and one row for each date of the "
        "simulation grid: NettingSet, Date and Time, then EPE and ENE, the expected positive "
        "and negative exposure, PFE, the potential future exposure, ExpectedCollateral, the "
        "collateral expected to be held, and the Basel measures BaselEE, BaselEEE, "
        "TimeWeightedBaselEPE and TimeWeightedBaselEEPE. All are money in the base currency. "
        "The xva analytic writes it.",
    "netting_set_colva_report":
        "The collateral valuation adjustment of one netting set through time, one file for "
        "each netting set, named colva_nettingset_ followed by the netting set id. It shows "
        "how the COLVA builds up date by date from the expected collateral balance.\n\n"
        "It is a comma separated file with a header line and one row for each date: "
        "NettingSet, Date and Time, CollateralBalance, COLVA Increment and COLVA, the "
        "adjustment so far, and CollateralFloor Increment and CollateralFloor, the value of "
        "the floor on the collateral rate. All are money in the base currency. The xva "
        "analytic writes it.",
    "time_averaged_exposure":
        "The time averaged netted exposure, normally timeaveragedexposure.csv. For each netting "
        "set and each simulated path it gives the exposure averaged over the life of the path, "
        "with and without collateral.\n\n"
        "It is a comma separated file with a header line and one row for each netting set and "
        "sample: NettingSetId, Sample, and PosExpNoColl, NegExpNoColl, PosExpWithColl and "
        "NegExpWithColl, the positive and negative exposure without and with collateral, in "
        "the base currency. The xva analytic writes it under the name its "
        "timeAveragedNettedExposureOutputFile parameter gives.",
    "dim_evolution":
        "The evolution of the dynamic initial margin, normally dim_evolution.csv or "
        "dim_evolution.txt. It gives the initial margin each netting set is expected to post "
        "at each future date, which the margin value adjustment is computed from.\n\n"
        "It is a table with a header line and one row for each netting set and date: "
        "NettingSet, Date, Time and TimeStep, DaysInPeriod, and the margin estimates "
        "ZeroOrderDIM, AverageDIM, SimpleDIM and AverageFLOW, in the base currency. The xva "
        "analytic writes it under the name its dimEvolutionFile parameter gives; it can also "
        "be given a file of this form as a deterministic initial margin.",
    "dim_regression":
        "A snapshot of the regression the dynamic initial margin model fits, normally "
        "dim_regression.csv or one file for each snapshot, such as dim_regression_1.csv. The "
        "model estimates the "
        "margin on each path by regressing it on the risk factors, and this report shows the "
        "fit at one date.\n\n"
        "It is a table with a header line and one row for each simulation sample: the "
        "regressors the run configured, in columns such as Regressor_0_USD, then "
        "RegressionDIM, LocalDIM, ExpectedDIM, ZeroOrderDIM, SimpleDIM and DeltaNPV, in the "
        "base currency. The xva analytic writes it under the names its dimRegressionFiles "
        "parameter gives.",
    "xva_explain_report":
        "The XVA explain report, normally xvaExplain.csv. It explains the change in CVA of "
        "each trade and netting set between two dates, risk factor by risk factor.\n\n"
        "It is a comma separated file with a header line and one row for each trade, netting "
        "set and risk factor: TradeId, NettingSetId, RiskFactor, CVA_Base, the CVA before, "
        "CVA, the CVA after the factor has moved, and Change, in the base currency. The "
        "xvaExplain analytic writes it.",
    "xva_explain_details":
        "The valuation adjustments under each scenario of an XVA explain run, normally "
        "xvaExplain_details.csv. It is the detail the XVA explain report is computed from.\n\n"
        "It is a comma separated file with a header line and one row for each trade, netting "
        "set and scenario, with the same adjustment columns as the XVA report: CVA, DVA, the "
        "funding, collateral, margin and capital adjustments, the allocations and the Basel "
        "exposures, in the base currency. The xvaExplain analytic writes it.",
    "xva_par_sensitivity":
        "The par rate sensitivities of one valuation adjustment, one file for each "
        "adjustment, named xva_par_sensitivity_ followed by the adjustment, such as "
        "xva_par_sensitivity_cva.csv. It says how the adjustment changes when each par rate "
        "moves. The SA-CVA charge can be computed from the CVA file, given here or produced "
        "by the run.\n\n"
        "It is a comma separated file with a header line and one row for each trade or "
        "netting set and factor: TradeId, NettingSetId, IsPar, the factors and shift sizes, "
        "Currency, Base XVA, the adjustment before the shift, Delta and Gamma, in the base "
        "currency. The xvaSensitivity analytic writes it.",
    "xva_zero_sensitivity":
        "The zero rate sensitivities of one valuation adjustment, one file for each "
        "adjustment, named xva_zero_sensitivity_ followed by the adjustment. It says how the "
        "adjustment changes when each zero rate risk factor moves.\n\n"
        "It is a comma separated file with a header line and one row for each trade or "
        "netting set and factor: TradeId, NettingSetId, IsPar, the factors and shift sizes, "
        "Currency, Base XVA, Delta and Gamma, in the base currency. The xvaSensitivity "
        "analytic writes it.",
    "xva_sensitivity_jacobi":
        "The Jacobian of an XVA sensitivity run, normally xva_sensitivity_jacobi.csv: the "
        "matrix that turns sensitivities to zero rates into sensitivities to par rates.\n\n"
        "It is a comma separated file with a header line, one row for each pair of par and "
        "zero rate factors, the entries without unit. The xvaSensitivity analytic writes it "
        "under the name its jacobiOutputFile parameter gives.",
    "xva_sensitivity_jacobi_inverse":
        "The inverse Jacobian of an XVA sensitivity run, normally "
        "xva_sensitivity_jacobi_inverse.csv: the matrix that turns sensitivities to par rates "
        "back into sensitivities to zero rates.\n\n"
        "It is a comma separated file with a header line, one row for each pair of zero and "
        "par rate factors, the entries without unit. The xvaSensitivity analytic writes it "
        "under the name its jacobiInverseOutputFile parameter gives.",
    "cva_sensitivities":
        "The sensitivities of the CVA of each netting set, normally cva_sensitivities.csv. "
        "For each risk factor it gives the CVA before the shift and the change when the "
        "factor is shifted.\n\n"
        "It is a comma separated file with a header line and one row for each netting set and "
        "risk factor: NettingSet, RiskFactor, ShiftType, Currency, ShiftSize, in the factor's "
        "own unit, BaseCva and Delta, in the base currency. The xvaSensitivity analytic writes "
        "it.",
    "xva_cg_exposure":
        "The exposure profile from the computation graph XVA engine, normally "
        "xvacg-exposure.csv. That engine records the payoffs of the trades as a graph so the "
        "XVA sensitivities can be taken by algorithmic differentiation, and it reports the "
        "exposure it computed on the way.\n\n"
        "It is a comma separated file with a header line and one row for each date: Date, EPE "
        "and ENE, the expected positive and negative exposure in the base currency. The xva "
        "analytic writes it when it runs on the computation graph.",
    "xva_cg_cva_sensitivity_scenario":
        "The CVA under each shifted factor from the computation graph XVA engine, normally "
        "xvacg-cva-sensi-scenario.csv. It lets the sensitivities taken by algorithmic "
        "differentiation be checked against shifted revaluations.\n\n"
        "It is a comma separated file with a header line and one row for each trade and "
        "factor: TradeId, Factor, Up/Down, ShiftSize_1, ShiftSize_2, Base Currency, Base NPV, "
        "Scenario NPV and Difference, in the base currency. The xva analytic writes it when "
        "it runs on the computation graph.",

    # The portfolio details.
    "market_objects":
        "The market objects a portfolio needs, normally marketObjects.csv: the curves, "
        "surfaces, FX rates and other objects the trades need to be valued. A person reads it "
        "to see what today's market must provide before the portfolio can be priced.\n\n"
        "It is a comma separated file with a header line and one row for each kind of market "
        "object: MarketObjectType, such as DiscountCurve, YieldCurve or IndexCurve, and "
        "MarketObjectName, the objects of that kind separated by |, such as EUR|GBP|USD. The portfolioDetails analytic writes it "
        "under the name its marketObjectFileName parameter gives.",
    "risk_factors":
        "The risk factors a portfolio depends on, normally riskFactors.csv. A person reads it "
        "to see which factors a sensitivity, stress or simulation must cover.\n\n"
        "It is a comma separated file with a header line and one row for each kind of risk "
        "factor: RiskFactorType and RiskFactorName, the factors of that kind separated by |. "
        "The "
        "portfolioDetails analytic writes it under the name its riskFactorFileName parameter "
        "gives.",
    "counterparties":
        "The counterparties a portfolio's trades face, normally counterparties.csv, with how "
        "many trades each has.\n\n"
        "It is a comma separated file with a header line and one row for each counterparty: "
        "Counterparty and Count, the number of trades.",
    "netting_sets":
        "The netting sets a portfolio's trades fall in, normally netting_sets.csv, with how "
        "many trades each holds.\n\n"
        "It is a comma separated file with a header line and one row for each netting set: "
        "NettingSets and Count, the number of trades.",
    "trade_types":
        "The trade types a portfolio holds, normally trade_types.csv, with how many trades of "
        "each.\n\n"
        "It is a comma separated file with a header line and one row for each trade type: "
        "TradeTypes and Count, the number of trades.",
    "swap_indices":
        "The swap indices a portfolio references, normally swap_indices.csv, such as the "
        "constant maturity swap rates its trades fix on.\n\n"
        "It is a comma separated file with a header line and one row for each index, in the "
        "column SwapIndices.",
    "underlying_indices":
        "The underlying indices a portfolio references, normally underlying_indices.csv, "
        "grouped by asset type: the interest rate, inflation, equity, FX and commodity "
        "indices its trades depend on.\n\n"
        "It is a comma separated file with a header line and one row for each asset type: "
        "AssetType and Indices, the indices of that type.",
})

FILES.update({
    # The intermediate data.
    "npv_cube":
        "The NPV cube, normally cube.csv.gz. It is the central result of a simulation run: "
        "the value of every trade on every date of the simulation grid in every simulated "
        "scenario. The guide calls it the cube because it has three dimensions, trades, dates "
        "and samples. The exposure and XVA analytics read it to compute the exposure "
        "profiles and the valuation adjustments.\n\n"
        "It is a compressed comma separated file, one row for each trade, date and sample, "
        "with the value in the base currency. It can be very large: a portfolio of a thousand "
        "trades on a grid of eighty dates with a thousand samples has eighty million values. "
        "The simulation analytic writes it, and the xva and pfe analytics read it, under the "
        "names their cubeFile parameters give. Keeping it lets a later run compute XVA "
        "without simulating again.",
    "npv_cube_binary":
        "The NPV cube in ORE's condensed binary form, normally cube.dat. It holds the same "
        "values as the compressed text cube, the value of every trade on every date in every "
        "scenario, in a form that is faster to read and write.\n\n"
        "It is a binary file that only ORE can read. A person who wants to look at the cube "
        "reads the raw cube or the text cube instead.",
    "raw_cube":
        "The raw cube, normally rawcube.csv: the NPV cube in readable text, before netting "
        "and collateral. It lets a person inspect, or rebuild, the exposures from the "
        "simulated trade values.\n\n"
        "It is a comma separated file with a header line and one row for each trade, date "
        "and sample: Id, the trade, NettingSet, DateIndex and Date, Sample, Depth and Value, "
        "the simulated value in the base currency. The xva and pfe analytics write it under "
        "the name their rawCubeOutputFile parameter gives.",
    "netting_set_cube":
        "The NPV cube aggregated by netting set, normally nettingSetCube.csv.gz. The "
        "simulation writes it next to the trade level cube, with the values already converted "
        "into the base currency, so the post-processing can start from netting set values "
        "without aggregating the trades again.\n\n"
        "It is a compressed comma separated file, one row for each netting set, date and "
        "sample, with the value in the base currency. The simulation analytic writes it and "
        "the xva analytic reads it, under the names their nettingSetCubeFile parameters give.",
    "net_cube":
        "The net cube, normally netcube.csv: the NPV cube after netting and collateral, in "
        "readable text. The exposure and XVA figures are computed from these values.\n\n"
        "It is a comma separated file with a header line and one row for each netting set, "
        "date and sample: Id, NettingSet, DateIndex and Date, Sample, Depth and Value, the "
        "netted and collateralised value in the base currency. The xva and pfe analytics "
        "write it under the name their netCubeOutputFile parameter gives.",
    "counterparty_cube":
        "The NPV cube aggregated by counterparty, normally cptyCube.csv.gz, written together "
        "with the survival probabilities and the values in each credit state. The dynamic "
        "credit XVA calculation needs it to account for the default of the counterparty "
        "along each path.\n\n"
        "It is a compressed comma separated file. The simulation analytic writes it and the "
        "xva analytic reads it, under the names their cptyCubeFile parameters give.",
    "aggregation_scenario_data":
        "The aggregation scenario data, normally scenariodata.csv.gz. For every date and "
        "sample of the simulation it gives the simulated FX rates and index fixings that the "
        "XVA post-processing needs, for example to compute the variation margin along each "
        "path consistently with the trade values in the cube.\n\n"
        "It is a compressed comma separated file; a run can also write it uncompressed, as "
        "scenariodata.csv, for inspection. The simulation analytic writes it, and the xva and "
        "pfe analytics read it.",
    "aggregation_scenario_data_binary":
        "The aggregation scenario data in ORE's binary form, normally scenariodata.dat. It "
        "holds the same simulated market factors as the compressed text file, for every date "
        "and sample, in a form only ORE reads. The simulation analytic writes it under the "
        "name its additionalScenarioDataFileName parameter gives.",
    "scenario_dump":
        "The simulated market, normally scenariodump.csv. It gives the value of every "
        "simulated risk factor on every date of every scenario, for example the discount "
        "factors of each yield curve, so the paths of a simulation can be inspected or reused. "
        "ORE writes it only when the run asks for it.\n\n"
        "It is a comma separated file with a header line and one row for each date and "
        "scenario: Date, Scenario and Numeraire, then one column for each risk factor, named "
        "as in the simulation market, such as DiscountCurve/EUR/0. Each value is in the "
        "factor's own unit: a discount factor, a rate, a spot or a volatility. The simulation "
        "and scenarioGeneration analytics write it under the name their scenariodump "
        "parameter gives.",
    "scenario_statistics":
        "Statistics of the simulated market across scenarios, normally "
        "scenariostatistics.csv. It summarises the values of every simulated risk factor at "
        "each date, so the behaviour of the simulation can be checked without reading every "
        "path.\n\n"
        "It is a comma separated file with a header line, with Date and Scenario columns, "
        "Numeraire, and one column for each risk factor, named as in the simulation market. "
        "The scenario analytic writes it.",
    "stress_scenarios":
        "The stressed markets of a scenario generation run, normally stress_scenarios.csv. "
        "For each stress scenario it gives the value of every risk factor, so the stress "
        "analytic can read the scenarios from a file instead of building them from the stress "
        "configuration.\n\n"
        "It is a comma separated file with a header line and one row for each scenario: Date, "
        "Scenario and Numeraire, then one column for each risk factor, named as in the "
        "simulation market, each in its own unit. The scenarioGeneration analytic writes it "
        "and the stress analytic reads it.",
    "zero_stress_scenario_results":
        "The stress scenarios converted into zero rates by a par stress conversion, normally "
        "results.xml. A stress stated in par rates must be converted into shifts of zero "
        "rates before ORE can apply it, and the parStressConversion analytic writes the "
        "converted scenarios here.\n\n"
        "It is an XML document with the same content as the zero stress scenario data: for "
        "each scenario, the zero rate shift of each curve at each tenor. The stress analytic "
        "can then read it as its zero stress scenario data.",
    "jacobi":
        "The Jacobian of a par conversion, normally jacobi.csv: the matrix that converts a "
        "shift of zero rates into the shift of par rates, used to turn zero rate "
        "sensitivities into par rate sensitivities.\n\n"
        "It is a comma separated file with a header line and one row for each pair of "
        "factors: ParFactor, RawFactor and ParSensitivity, how much the par factor moves with "
        "the raw one, without unit. The sensitivity and zeroToParSensiConversion analytics "
        "write it under the name their jacobiOutputFile parameter gives.",
    "jacobi_inverse":
        "The inverse Jacobian of a par conversion, normally jacobi_inverse.csv: the matrix "
        "that converts a shift of par rates back into the shift of zero rates.\n\n"
        "It is a comma separated file with a header line and one row for each pair of "
        "factors: RawFactor(z), ParFactor(c) and dz/dc, how much the raw factor moves with "
        "the par one, without unit. The sensitivity and zeroToParSensiConversion analytics "
        "write it under the name their jacobiInverseOutputFile parameter gives.",
    "amc_buffer":
        "The path data of an American Monte Carlo run, normally amcBuffer.dat. American Monte "
        "Carlo values trades with early exercise or path dependence by regression on the "
        "simulated paths, and this file keeps the paths so a later step can reuse them.\n\n"
        "It is a binary file that only ORE reads. The simulation and scenarioGeneration "
        "analytics write it under the name their amcPathDataOutput parameter gives; the guide "
        "names the parameter but does not describe the file.",

    # The run diagnostics.
    "market_data_report":
        "The market data the run actually loaded, normally marketdata.csv. Where the market "
        "data file may hold many dates and quotes, this report lists the quotes the run "
        "read, so a value can be traced to the exact market it was computed from.\n\n"
        "It is a comma separated file with a header line starting with #: datumDate, datumId, "
        "the quote name such as FX/RATE/EUR/USD, and datumValue, the quote in its own unit. "
        "ORE Studio stores these rows for every run as the market the run read.",
    "fixings_report":
        "The fixings the run actually loaded, normally fixings.csv: the past index values its "
        "trades used.\n\n"
        "It is a comma separated file with a header line starting with #: fixingDate, "
        "fixingId, the index name, and fixingValue, in the index's own unit.",
    "dividends_report":
        "The dividends the run actually loaded, normally dividends.csv: the past dividends of "
        "the equities its trades reference.\n\n"
        "It is a comma separated file with a header line starting with #: dividendExDate, "
        "equityId, dividendPaymentDate and dividendRate, the dividend per share in the "
        "equity's currency.",
    "pricing_stats":
        "The pricing statistics of a run, normally pricingstats.csv: how many times each "
        "trade was priced and how long it took. A person reads it to find the trades that "
        "make a run slow.\n\n"
        "It is a comma separated file with a header line and one row for each trade: TradeId, "
        "TradeType, NumberOfPricings, CumulativeTiming and AverageTiming, the timings in "
        "microseconds.",
    "runtimes":
        "The run times of a run, normally runtimes.csv: how long each part of the run took.\n\n"
        "It is a comma separated file with a header line starting with # and one row for each "
        "timed step: Key, which names the analytic and the step separated by |, such as "
        "PRICING|buildMarket(), then Total, Count, the number of times the step ran, Max, Min "
        "and Average. The timings are in microseconds.",
    "log":
        "The log of the engine, normally log.txt. It records what the engine did and what went "
        "wrong: the parameters it read, the market objects it built, the trades it could not "
        "build or price and why. It is the first place to look when a run fails or a trade is "
        "missing from a report.\n\n"
        "It is a plain text file with one message on each line: the level, such as NOTICE, "
        "WARNING or ERROR, the time, the source file and line of the engine, and the message. "
        "The logFile and logMask parameters of the master input say where it is written and "
        "which levels it holds.",
    "structured_log":
        "The structured log of the engine, normally log_structured.json. It holds the errors "
        "and warnings of the run as data rather than text, so they can be read by a program. "
        "ORE Studio records them as the errors of the run.\n\n"
        "It is a file with one JSON object on each line. Each object has a category, such as "
        "Error or Warning, a group, such as Trade, Curve or Analytics, a message, and "
        "sub_fields, a list of name and value pairs that identify what the message is about, "
        "such as the trade id, the trade type and the exception type.",

    # ORE Studio's packaging.
    "input_package":
        "The input package of a run, normally ore_package.tar.gz. ORE Studio builds it when it "
        "dispatches a run to the compute grid: it holds every file the engine is given, so a "
        "compute node can run ORE with nothing else.\n\n"
        "It is a gzip compressed tar archive. Its Input folder holds the master input "
        "ore.xml and every document and data file the master input names: today's market, "
        "the curve configuration, the conventions, the pricing engines, the portfolio, the "
        "market data, the fixings and the rest. ORE Studio writes these files from its own "
        "configuration and data when it packages the run, so the package is the exact record "
        "of what the run was given. Each file in it can be downloaded on its own.",
    "output_archive":
        "The output archive of a run, named by the id of its compute result, such as "
        "a131dbb8-7d6e-413e-96bd-c2f681f9af80.tar.gz; the copy kept with the run is "
        "output.tar.gz. The compute node packs everything ORE wrote into it when the run "
        "ends, modelled by ORE Studio or not, so nothing the engine produced is lost.\n\n"
        "It is a gzip compressed tar archive of the engine's output folder: the reports, the "
        "data files and the logs. The node also uploads each file on its own with a manifest, "
        "so a single report can be read without unpacking the archive.",
    "output_manifest":
        "The manifest of a run's outputs, manifest.json.gz. The compute node writes it next "
        "to the files it uploads, and ORE Studio reads it to find and check each output "
        "without unpacking the archive.\n\n"
        "It is a gzip compressed JSON document with the id of the result and a list of "
        "files. Each file gives its name, the output it belongs to, its size as ORE wrote "
        "it and as uploaded compressed, its SHA-256 digest, and the storage key it was "
        "uploaded under. ORE Studio checks each file it reads against its digest.",
})
