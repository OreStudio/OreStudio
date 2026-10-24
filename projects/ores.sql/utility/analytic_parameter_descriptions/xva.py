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
"""The XVA and PFE analytics, which post-process an NPV cube."""

from ._entry import COUNT, FILE, FLAG, INPUT_FILE, REAL, entry

IGNORED = "ORE ignores the parameter, so a value has no effect."
RESULTS_FILE = "A file name. ORE looks for the file in the output path of the Setup block."
LIST_REAL = "A comma-separated list of numbers."
LIST_INT = "A comma-separated list of whole numbers."
CORRELATION = (
    " The correlation calculation reads this parameter from the xva block when generate correlations is on."
)
KVA = " ORE uses it only when KVA is on."
DIM = " ORE uses it only when DIM is on."


def exposure(kind, name):
    """The parameters that the XVA and the PFE analytic share, worded for one of them."""
    both = f" The {name} analytic has the same parameter." if kind == "xva" else " The XVA analytic has the same parameter."
    return {
        "active": entry(
            f"Run the {kind.upper()} analytic",
            "A flag. Y runs the analytic.",
            f"Switches the {name} analytic on or off. When it is on, the run post-processes an NPV cube: it "
            "nets the trades, applies the collateral agreement and computes the exposure profiles"
            + (", and then the value adjustments that the flags below select." if kind == "xva" else
               " and the potential future exposure.")
            + " The cube comes from the simulation analytic of the same run or from a file.",
            "No. The analytic does not run.",
            FLAG, required=True),
        "csaFile": entry(
            "Netting set definitions file",
            "A file name.",
            "Names the XML file with the netting set definitions: the CSA details such as the margining "
            "frequency, the thresholds, the minimum transfer amounts and the margin period of risk. The "
            "post-processing reads the file, so you can change the CSA terms and see their effect on the results "
            "without a new simulation. ORE looks for the file name in the xva block, then in the pfe block and "
            "last in the Setup block. For example, netting.xml.",
            "The netting set definitions file of the Setup block.",
            INPUT_FILE),
        "cubeFile": entry(
            "Cube file",
            "A file name.",
            f"Names the NPV cube file that the {name} analytic reads, if the run has no simulation. The "
            "file is usually the cube that an earlier simulation wrote, so you can run the post-processing again "
            "without a new simulation. ORE looks for the file in the output path of the Setup block. For "
            "example, cube.csv.gz.",
            "None. When the run has a simulation, the analytic uses its cube.",
            RESULTS_FILE),
        "scenarioFile": entry(
            "Scenario data file",
            "A file name.",
            "Names the file with the scenario data that the simulation wrote for the post-processing, such as "
            "simulated index fixings and FX rates. Variation margin and dynamic initial margin need it, because "
            "they must recompute a margin amount on each path and date. Use it when the post-processing runs "
            "apart from the simulation, together with the cube file. For example, scenariodata.csv.gz.",
            "None. When the run has a simulation, the analytic uses its scenario data.",
            RESULTS_FILE),
        "useDoublePrecisionCubes": entry(
            "Double precision cubes",
            "A flag. Y uses double precision.",
            f"Decides whether the {name} analytic builds its cubes with double precision numbers. Single "
            "precision uses half the memory. Switch to double precision when the single precision rounding "
            "is too large for your netting sets." + both,
            "No. The cubes use single precision.",
            FLAG),
        "baseCurrency": entry(
            "Base currency",
            "A currency code, for example EUR.",
            "Names the currency in which the analytic states every NPV, exposure and value adjustment. ORE "
            "converts an amount in another currency at the spot rate of the valuation date. Use the currency in "
            "which the desk or the capital report measures its risk. For example, EUR states all values in euro. "
            "The Setup block has a parameter of the same name, which this one overrides for this analytic.",
            "The base currency of the Setup block.",
            "A currency code of three letters, for example EUR."),
        "exposureProfiles": entry(
            "Write exposure profiles",
            "A flag. Y writes the profiles for each netting set.",
            "Decides whether the analytic writes an exposure report for each netting set: the expected "
            "positive and negative exposure and the potential future exposure, for each date. The profiles "
            "show how the exposure builds up and runs off over the life of the trades, so they are the main "
            "result to read for a credit limit or a capital check." + both,
            "Yes.",
            FLAG),
        "exposureProfilesByTrade": entry(
            "Write exposure profiles by trade",
            "A flag. Y writes the stand-alone profiles for each trade.",
            "Decides whether the analytic writes an exposure report for each trade on its own, without the "
            "netting. With many trades this makes many files, unless Write individual exposure reports is off." + both,
            "Yes.",
            FLAG),
        "exposureProfilesUseCloseOutValues": entry(
            "Exposure profiles use close-out values",
            "A flag. Y uses the close-out values.",
            "Decides whether the exposure profiles use the close-out value of a trade, which is its value at "
            "the end of the margin period of risk, instead of its value at the default date. It needs a "
            "simulation with a close-out grid." + both,
            "No. The profiles use the values at the default date.",
            FLAG),
        "writeIndividualExposureReports": entry(
            "Write individual exposure reports",
            "A flag. Y writes one report for each trade and netting set.",
            "Decides whether the analytic writes one exposure report for each trade and for each netting set. "
            "When it is N, the analytic writes one report for all trades and one for all netting sets. Switch "
            "it off for a large portfolio to avoid a very large number of files." + both,
            "Yes.",
            FLAG),
        "quantile": entry(
            "PFE confidence level",
            "A number between 0 and 1.",
            "Sets the confidence level of the potential future exposure (PFE) in the exposure reports. The PFE "
            "is the quantile of the positive exposure distribution on each date, so a higher level gives a "
            "larger number. For example, 0.95 reports the 95th percentile, and 0.99 reports the 99th, which a "
            "limit system often uses." + both,
            "0.95",
            REAL),
        "calculationType": entry(
            "Margin settlement type",
            "NoLag, Symmetric, AsymmetricCVA or AsymmetricDVA.",
            "Decides how ORE settles margin calls. Symmetric settles the margin of both parties after the "
            "margin period of risk. AsymmetricCVA settles the margin that we request from the counterparty with "
            "the delay and the margin that the counterparty requests from us at once. AsymmetricDVA is the "
            "reverse. NoLag turns the delay off and works together with a close-out grid in the simulation. "
            "Without a close-out grid, use one of the three others. With one, use NoLag. When "
            "there is no close-out grid, the results are correct only if the simulation grid is equally "
            "spaced with steps equal to the margin period of risk." + both,
            "NoLag",
            "One of NoLag, Symmetric, AsymmetricCVA or AsymmetricDVA."),
        "allocationMethod": entry(
            "Exposure allocation method",
            "None, Marginal, RelativeXVA, RelativeFairValueGross or RelativeFairValueNet.",
            "Selects how ORE allocates the netting set exposure and the value adjustments to the trades in "
            "the netting set. None gives no allocation. Marginal uses the marginal method of Pykhtin and Rosen. "
            "The other three split the amount in proportion to the XVA of the trade, to its gross fair "
            "value or to its net fair value." + both,
            "None",
            "One of None, Marginal, RelativeXVA, RelativeFairValueGross or RelativeFairValueNet."),
        "marginalAllocationLimit": entry(
            "Marginal allocation limit",
            "A number.",
            "Sets a cutoff for the marginal allocation. The marginal method breaks down when the value of the "
            "netting set is close to zero while its exposure is not. Below this absolute value, ORE splits "
            "the exposure equally among the trades. The parameter has an effect only when the allocation "
            "method is Marginal." + both,
            "1.0",
            REAL),
        "exerciseNextBreak": entry(
            "Exercise at the next break",
            "A flag. Y ends each trade at its next break date.",
            "Decides whether ORE ends all trades at their next break date before it aggregates them. Use it "
            "to model a counterparty relationship in which each party can end a trade at its break dates. A "
            "trade that ends early adds no exposure after its break, so the profiles are lower than with the "
            "full maturity. The default is No, so every trade runs to maturity." + both,
            "No.",
            FLAG),
        "rawCubeOutputFile": entry(
            "Raw cube report file",
            "A file name.",
            "Names the file for the trade NPV cube as a text file with one row for each trade, date and sample. "
            "Setting it switches the report on. Leave it out to skip the file. The file is for further "
            "analysis and can be very large." + both,
            "None. The analytic does not write the file.",
            FILE),
        "netCubeOutputFile": entry(
            "Net cube report file",
            "A file name.",
            "Names the file for the aggregated NPV cube as a text file with one row for each netting set, "
            "date and sample, after the collateral. Setting it switches the report on. Leave it out to skip "
            "the file." + both,
            "None. The analytic does not write the file.",
            FILE),
    }


XVA = exposure("xva", "XVA")
PFE = exposure("pfe", "PFE")
PFE["useDoublePrecisionCubes"] = entry(
    "Double precision cubes",
    "A flag. ORE ignores it in the pfe block.",
    "The user guide shows this parameter in the pfe block, but ORE reads it only from the xva block. A value "
    "in the pfe block has no effect in release 1.8.17.0.",
    "No. " + IGNORED,
    FLAG)
PFE["collateralBalancesFile"] = entry(
    "Collateral balances file",
    "A file name.",
    "Names the XML file with the current variation margin and initial margin balances for each netting set. "
    "The analytic uses them as the margin at the valuation date. ORE looks for the file name in the xva block, "
    "then in the pfe block and last in the Setup block. For example, collateralbalances.xml.",
    "The collateral balances file of the Setup block.",
    INPUT_FILE)

XVA.update({
    "nettingSetCubeFile": entry(
        "Netting set cube file",
        "A file name.",
        "Names the netting set cube file that the XVA analytic reads when the run has no simulation of its own. "
        "It is the file that an earlier simulation wrote, so the XVA step can run again without the cost of the "
        "simulation. Use it to try other XVA settings on the same exposures, for example another set of CVA "
        "flags or another DIM quantile. For example, nettingSetCube.csv.gz reads the cube from that file in the "
        "output path. The cube must come from the same portfolio and the same valuation date.",
        "None.",
        RESULTS_FILE),
    "cptyCubeFile": entry(
        "Counterparty cube file",
        "A file name.",
        "Names the counterparty cube file that the XVA analytic reads when the run has no simulation. The "
        "cube holds the survival probabilities for the dynamic credit calculation. It is the file that an "
        "earlier simulation wrote. For example, cptyCube.csv.gz.",
        "None.",
        RESULTS_FILE),
    "collateralBalancesFile": entry(
        "Collateral balances file",
        "A file name.",
        "Names the XML file with the current variation margin and initial margin balances for each netting set. "
        "The analytic uses them as the margin at the valuation date, and the flat DIM model and the scaling of "
        "the DIM model use the initial margin. ORE looks for the file name in the xva block, then in the pfe "
        "block and last in the Setup block. For example, collateralbalances.xml.",
        "The collateral balances file of the Setup block.",
        INPUT_FILE),
    "cva": entry(
        "Compute CVA",
        "A flag. Y computes the credit value adjustment.",
        "Switches the credit value adjustment (CVA) on or off. CVA prices the loss that the default of the "
        "counterparty causes on the positive exposure, so it is the main charge of the analytic. The analytic "
        "needs the default curve of each counterparty in the market data. Set it to N when the run is for "
        "another adjustment only, for example the funding value adjustment, because the CVA takes time to "
        "compute for a large portfolio.",
        "Yes.",
        FLAG),
    "dva": entry(
        "Compute DVA",
        "A flag. Y computes the debit value adjustment.",
        "Switches the debit value adjustment (DVA) on or off. DVA is the gain from our own default on the "
        "negative exposure: the counterparty may lose when we default. Many firms report it, and some leave it "
        "out of the price of a trade. Set the DVA name too, so that ORE can find our own default curve and "
        "recovery rate. The DVA calculation needs that name.",
        "No.",
        FLAG),
    "dvaName": entry(
        "DVA name",
        "A credit name.",
        "Names the credit entity that ORE uses to look up our own default probability curve and recovery rate "
        "for the DVA. The name must be in the market data, in the same form as the name of a counterparty. For "
        "example, BANK uses the default curve of the entity BANK. The DVA needs the name.",
        "None. A DVA calculation needs a name.",
        "A credit name as it appears in the market data, for example BANK."),
    "fva": entry(
        "Compute FVA",
        "A flag. Y computes the funding value adjustment.",
        "Switches the funding value adjustment (FVA) on or off. FVA prices the cost of funding the part of the "
        "exposure that no collateral covers: we borrow to fund a positive exposure and we lend what a negative "
        "exposure releases. It needs a borrowing curve and a lending curve, and both come from the market data. "
        "The cost depends on the spread of these curves over the overnight rate, so a small spread gives a "
        "small FVA.",
        "No.",
        FLAG),
    "fvaBorrowingCurve": entry(
        "FVA borrowing curve",
        "The name of a yield curve.",
        "Names the yield curve at which we borrow, for the FVA. This spelling is the older one, and ORE reads "
        "it only when the parameter Borrowing curve is missing. The curve must be in the market data, and its "
        "spread over the overnight rate is what makes the funding cost. For example, BANK_EUR_BORROW. Use the "
        "newer parameter in a new document.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "fvaLendingCurve": entry(
        "FVA lending curve",
        "The name of a yield curve.",
        "Names the yield curve at which we lend, for the FVA. This spelling is the older one, and ORE reads it "
        "only when the parameter Lending curve is missing. The curve must be in the market data. A negative "
        "exposure releases funding that we lend at this curve. For example, BANK_EUR_LEND. Use the newer "
        "parameter in a new document.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "borrowingCurve": entry(
        "Borrowing curve",
        "The name of a yield curve.",
        "Names the yield curve at which we borrow, for the FVA. The curve must be in the market data. The FVA "
        "charges the difference between this curve and the overnight rate on the positive uncollateralised "
        "exposure, so a wider spread gives a larger charge. For example, BANK_EUR_BORROW. The FVA needs both "
        "this curve and the lending curve.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "lendingCurve": entry(
        "Lending curve",
        "The name of a yield curve.",
        "Names the yield curve at which we lend, for the FVA. The curve must be in the market data. The FVA "
        "credits the difference between this curve and the overnight rate on the negative uncollateralised "
        "exposure. For example, BANK_EUR_LEND. The FVA needs both this curve and the borrowing curve.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "colva": entry(
        "Compute COLVA",
        "A flag. Y computes the collateral value adjustment.",
        "Switches the collateral value adjustment (COLVA) on or off. COLVA prices the difference between the "
        "rate that the collateral earns under the credit support annex and the overnight rate, on the expected "
        "collateral balance. It matters for netting sets with large collateral and a collateral rate that "
        "differs from the overnight rate. The collateral model of the netting set definitions sets the "
        "collateral rate.",
        "No.",
        FLAG),
    "collateralFloor": entry(
        "Compute the collateral floor",
        "A flag. Y computes the collateral floor value.",
        "Switches the calculation of the collateral floor on or off. Some credit support annexes floor the rate "
        "that the collateral earns, for example at zero when the overnight rate is negative. The calculation "
        "values that floor as an option on the collateral rate, and ORE adds its value to the COLVA. Switch it "
        "on for netting sets whose agreement has a floor.",
        "No.",
        FLAG),
    "mva": entry(
        "Compute MVA",
        "A flag. Y computes the margin value adjustment.",
        "Switches the margin value adjustment (MVA) on or off. MVA prices the funding cost of the initial "
        "margin that we post over the life of the trades. It takes the projected initial margin from the "
        "dynamic initial margin model that the DIM model parameter selects, so DIM must be on. The result "
        "depends on the margin model, so compare MVA only between runs that use the same model.",
        "No.",
        FLAG),
    "kva": entry(
        "Compute KVA",
        "A flag. Y computes the capital value adjustment.",
        "Switches the capital value adjustment (KVA) on or off. KVA prices the cost of the regulatory capital "
        "that the counterparty credit risk needs over the life of the trades, as the capital hurdle times the "
        "capital discounted at the capital discount rate. The KVA parameters set the capital model. Switch it "
        "on when a price must carry the cost of capital as well as the cost of credit and funding.",
        "No.",
        FLAG),
    "kvaCapitalDiscountRate": entry(
        "KVA capital discount rate",
        "A rate, for example 0.10.",
        "Sets the rate at which ORE discounts the cost of capital in the KVA. A higher rate gives a smaller "
        "KVA, because the future cost of capital counts for less. The default is 0.10, which is ten percent. "
        "ORE uses it only when KVA is on. For example, 0.08 discounts the cost of capital at eight percent.",
        "0.10",
        REAL),
    "kvaAlpha": entry(
        "KVA alpha",
        "A number.",
        "Sets the multiplier alpha of the regulatory exposure at default in the KVA calculation. The regulatory "
        "exposure at default is alpha times the expected positive exposure, and the Basel rules fix alpha at "
        "1.4. Change it only to test a different rule. ORE uses it only when KVA is on.",
        "1.4",
        REAL),
    "kvaRegAdjustment": entry(
        "KVA regulatory adjustment",
        "A number.",
        "Sets the regulatory adjustment factor that turns the capital requirement into risk weighted assets in "
        "the KVA calculation. The default is 12.5, the inverse of the eight percent capital ratio. Change it "
        "only to follow a different capital ratio. ORE uses it only when KVA is on. For example, 10 follows a "
        "ratio of ten percent.",
        "12.5",
        REAL),
    "kvaCapitalHurdle": entry(
        "KVA capital hurdle",
        "A rate, for example 0.012.",
        "Sets the return on capital that the KVA charges, the capital hurdle rate. The default is 0.012, which "
        "is 1.2 percent. A higher hurdle gives a larger KVA, because each unit of capital must earn more. ORE "
        "uses it only when KVA is on. For example, 0.02 charges two percent.",
        "0.012",
        REAL),
    "kvaOurPdFloor": entry(
        "KVA floor on our default probability",
        "A probability.",
        "Sets the lowest default probability that the KVA uses for us. A very safe name has a default "
        "probability near zero, which would make the capital charge too low, so the floor lifts it. The default "
        "is 0.03. ORE uses it only when KVA is on. For example, 0.0003 sets a floor of three basis points.",
        "0.03",
        REAL),
    "kvaTheirPdFloor": entry(
        "KVA floor on their default probability",
        "A probability.",
        "Sets the lowest default probability that the KVA uses for the counterparty. A very safe counterparty "
        "has a default probability near zero, which would make the capital charge too low, so the floor lifts "
        "it. The default is 0.03. ORE uses it only when KVA is on. For example, 0.0003 sets a floor of three "
        "basis points.",
        "0.03",
        REAL),
    "kvaOurCvaRiskWeight": entry(
        "KVA CVA risk weight for us",
        "A number.",
        "Sets the CVA risk weight that the KVA applies to our own credit in the capital part of the "
        "calculation. A higher weight gives a larger capital charge and so a larger KVA. The default is 0.05. "
        "ORE uses it only when KVA is on. For example, 0.03 lowers the charge.",
        "0.05",
        REAL),
    "kvaTheirCvaRiskWeight": entry(
        "KVA CVA risk weight for them",
        "A number.",
        "Sets the CVA risk weight that the KVA applies to the credit of the counterparty in the capital part of "
        "the calculation. A higher weight gives a larger capital charge and so a larger KVA. The default is "
        "0.05. ORE uses it only when KVA is on. For example, 0.08 raises the charge for a counterparty of lower "
        "quality.",
        "0.05",
        REAL),
    "dynamicCredit": entry(
        "Dynamic credit",
        "A flag. Y uses the survival probabilities along each path.",
        "Decides whether ORE uses the survival probabilities of each simulation path when it computes the "
        "increments of CVA, DVA, FVA and MVA from the exposures. When it is N, ORE takes the survival "
        "probabilities from the curves of the valuation date. The simulation must store the survival "
        "probabilities.",
        "No. ORE uses the survival probabilities of the valuation date.",
        FLAG),
    "cvaSensi": entry(
        "CVA sensitivities",
        "A flag. Y computes the CVA sensitivities.",
        "Decides whether the analytic computes the sensitivity of the CVA to the credit spreads of the "
        "counterparties, one number for each tenor of the grid. Use it to hedge the credit risk of the CVA with "
        "credit default swaps. The CVA sensitivity grid sets the tenors and the shift size sets the bump. The "
        "calculation bumps each spread and runs the CVA again, so the run takes longer.",
        "No.",
        FLAG),
    "cvaSensiGrid": entry(
        "CVA sensitivity grid",
        "A comma-separated list of periods.",
        "Lists the tenors of the credit spread curve for which the analytic computes the CVA sensitivity. The "
        "analytic bumps the spread at each tenor and reports the change of the CVA. For example, "
        "6M,1Y,2Y,3Y,5Y,10Y gives six sensitivities for each counterparty. More tenors give a finer hedge and a "
        "longer run. ORE uses it only when CVA sensitivities are on.",
        "None.",
        "A comma-separated list of periods such as 1Y,5Y."),
    "cvaSensiShiftSize": entry(
        "CVA sensitivity shift size",
        "A number.",
        "Sets the size of the bump of the credit spread in the CVA sensitivity calculation. The default is "
        "0.0001, which is one basis point. A smaller bump follows the slope of the CVA more closely, and a "
        "larger bump is steadier when the result is noisy. ORE uses it only when CVA sensitivities are on.",
        "0.0001",
        REAL),
    "dim": entry(
        "Compute DIM",
        "A flag. Y computes the dynamic initial margin.",
        "Switches the dynamic initial margin (DIM) calculation on or off. DIM projects the initial margin of "
        "each netting set along each path, as a margin model such as the SIMM would compute it on that path. "
        "The parameter DIM model selects the method. The margin value adjustment needs the result. The "
        "calculation is costly, so switch it on only when the run needs the margin.",
        "No.",
        FLAG),
    "dimModel": entry(
        "DIM model",
        "Regression, Flat, DeltaVaR, DeltaGammaNormalVaR, DeltaGammaVaR, DynamicIM or SimmAnalytic.",
        "Selects the method for the dynamic initial margin. Regression regresses the change of the netting "
        "set value over the margin period on the value at the start. Flat projects the initial margin of the "
        "valuation date unchanged along each path, and needs the collateral balances file. DeltaVaR, "
        "DeltaGammaNormalVaR and DeltaGammaVaR use stored sensitivities and need the simulation to store "
        "them. SimmAnalytic uses stored sensitivities and the SIMM formula. DynamicIM needs the CG engine "
        "with its dynamic IM switch on. The run stops if the value is none of these." + DIM,
        "Regression",
        "One of Regression, Flat, DeltaVaR, DeltaGammaNormalVaR, DeltaGammaVaR, DynamicIM or SimmAnalytic."),
    "dimQuantile": entry(
        "DIM quantile",
        "A number between 0 and 1.",
        "Sets the quantile of the margin movement that the DIM covers. A higher quantile gives a larger margin. "
        "For example, 0.99 covers the 99th percentile, which is the usual level of a margin model. ORE uses it "
        "only when DIM is on. Change it to follow the margin model of the agreement, for example 0.995.",
        "0.99",
        REAL),
    "dimHorizonCalendarDays": entry(
        "DIM horizon in calendar days",
        "A whole number of days.",
        "Sets the length of the margin period of risk that the DIM covers, in calendar days. The margin has to "
        "cover the movement of the portfolio over this period. The default is 14 calendar days, which is about "
        "ten business days, the period of the standard SIMM.",
        "14",
        COUNT),
    "dimRegressionOrder": entry(
        "DIM regression order",
        "A whole number.",
        "Sets the order of the regression polynomial of the DIM model Regression. The regression relates the "
        "clean change of the netting set value over the period to the value at the start of the period. A "
        "higher order follows a non-linear relation better and needs more paths to stay stable. ORE uses it "
        "only when DIM is on. For example, 2 fits a quadratic curve.",
        "0",
        COUNT),
    "dimRegressors": entry(
        "DIM regressors",
        "A comma-separated list of names.",
        "Lists the variables that the DIM regression uses as regressors, for a regression in one or more "
        "dimensions. The names must match the entries of the aggregation scenario data in the simulation "
        "configuration file, that is its currencies and indices. When the list is empty, the regression uses the "
        "NPV as its only regressor. For example, EUR-EURIBOR-3M,USD-LIBOR-3M,USD." + DIM,
        "None. The regression uses the NPV.",
        "A comma-separated list of currency codes and index names."),
    "dimLocalRegressionEvaluations": entry(
        "DIM local regression evaluations",
        "A whole number.",
        "Sets the number of points at which ORE evaluates a local (Nadaraya-Watson) regression to validate the "
        "polynomial regression. The local regression needs many samples to give a meaningful result, and each "
        "evaluation costs time, so keep the number small. A value of zero turns the check off." + DIM,
        "0",
        COUNT),
    "dimLocalRegressionBandwidth": entry(
        "DIM local regression bandwidth",
        "A number of standard deviations.",
        "Sets the bandwidth of the local regression that validates the polynomial regression of the DIM, in "
        "standard deviations of the NPV. A wider band uses more paths for each local fit and gives a smoother "
        "check. The default is 0.25. ORE uses it only when DIM is on and the DIM model uses regression.",
        "0.25",
        REAL),
    "dimScaling": entry(
        "DIM scaling",
        "A number.",
        "Sets a factor that ORE applies to the DIM values of the Regression and DeltaVaR family models. The "
        "factor applies to all netting sets, and ORE then ignores the initial margin of the collateral "
        "balances. When the parameter is missing, ORE derives a factor for each netting set from the initial "
        "margin of the collateral balances file divided by the model DIM at the valuation date. That initial "
        "margin must be above zero, or the run stops. A value of 1.0 applies no scaling. It has no effect for "
        "SimmAnalytic and DynamicIM." + DIM,
        "None. ORE derives the factor from the collateral balances.",
        REAL),
    "dimEvolutionFile": entry(
        "DIM evolution file",
        "A file name.",
        "Names the file for the evolution of the zero-order DIM and the average of the n-th order DIM over "
        "time. Use it to see how the margin runs off as the trades mature. For example, dim_evolution.csv. The "
        "analytic writes the file only when it has a name. ORE uses it only when DIM is on.",
        "None. The analytic does not write the file.",
        FILE),
    "dimRegressionFiles": entry(
        "DIM regression snapshot files",
        "A comma-separated list of file names.",
        "Names the files for snapshots of the DIM regression, one file for each grid point that DIM output grid "
        "points lists. A snapshot shows the data and the fit of the regression at a date, so you can check the "
        "quality of the fit. For example, dim_regression_1.csv,dim_regression_2.csv. ORE uses it only when DIM "
        "is on.",
        "None. The analytic does not write the files.",
        "A comma-separated list of file names."),
    "dimOutputNettingSet": entry(
        "DIM snapshot netting set",
        "A netting set id.",
        "Names the netting set for which the analytic writes the DIM regression snapshot. The snapshot is "
        "large, so the analytic writes it for one netting set only. For example, CPTY_A writes the snapshot of "
        "the netting set CPTY_A. ORE uses it only when DIM is on and the regression files are named.",
        "None.",
        "A netting set id."),
    "dimOutputGridPoints": entry(
        "DIM snapshot grid points",
        "A comma-separated list of whole numbers.",
        "Lists the points of the time grid for which the analytic writes a DIM regression snapshot. Each number "
        "is the index of a grid date, counted from zero. For example, 1,10 writes the snapshots of the second "
        "and the eleventh date. One file is written for each point. ORE uses it only when DIM is on.",
        "None.",
        LIST_INT),
    "dimDistributionCoveredStdDevs": entry(
        "DIM distribution range",
        "A number of standard deviations, or inf.",
        "Sets the range of the DIM distribution report: ORE covers this many standard deviations around the "
        "mean of the data. A wider range shows more of the tails and spreads the grid over a larger interval. "
        "The default is 5.0. The value inf covers all of the data. ORE uses it only when DIM is on.",
        "5.0",
        "A number, or the word inf."),
    "dimDistributionGridSize": entry(
        "DIM distribution grid size",
        "A whole number.",
        "Sets the number of points of the grid of the DIM distribution report. A larger grid gives a finer "
        "picture of the distribution of the margin and a larger file. The default is 50. ORE uses it only when "
        "DIM is on. For example, 100 doubles the resolution.",
        "50",
        COUNT),
    "deterministicInitialMarginFile": entry(
        "Deterministic initial margin file",
        "A file name.",
        "Names a file with the initial margin of each netting set as a time series. The analytic uses these "
        "amounts for the initial margin instead of a model, so the margin does not change with the paths. Use "
        "it when a clearing house or a risk system supplies the margin. ORE looks for the file in the input "
        "path of the Setup block.",
        "None.",
        INPUT_FILE),
    "fullInitialCollateralisation": entry(
        "Full initial collateralisation",
        "A flag. Y sets the collateral at the start to the NPV.",
        "Decides what the collateral balance is at the valuation date. When it is Y, ORE sets the balance of "
        "each netting set to its NPV, so that the expected exposure, the expected negative exposure and the "
        "PFE are all zero at the start. When it is N, ORE sets the balance to zero.",
        "No. The collateral balance at the start is zero.",
        FLAG),
    "firstMporCollateralAdjustment": entry(
        "First MPoR collateral adjustment",
        "A flag. Y adjusts the collateral in the first margin period.",
        "Decides whether ORE corrects the collateral in the first margin period of risk for a mismatch "
        "between the variation margin balance and the NPV at the valuation date. Switch it on when you "
        "start from real balances that do not equal the NPV.",
        "No.",
        FLAG),
    "mporCashFlowMode": entry(
        "MPoR cash flow mode",
        "NonePay, BothPay, WePay, TheyPay or Unspecified.",
        "Sets the assumption about who pays the cash flows inside the margin period of risk. NonePay assumes "
        "no payment, BothPay assumes that both parties pay, WePay that only we pay, and TheyPay that only "
        "the counterparty pays. With Unspecified, ORE assumes NonePay if the simulation uses a sticky "
        "close-out date and BothPay otherwise.",
        "Unspecified",
        "One of NonePay, BothPay, WePay, TheyPay or Unspecified."),
    "flipViewXVA": entry(
        "Flip the view of the XVA",
        "A flag. Y computes the XVA from the counterparty's view.",
        "Decides whether ORE switches the perspective of the XVA calculation to that of the counterparty. "
        "ORE then reverses the NPVs and the netting sets. It looks for the borrowing and lending curves "
        "under the name of the counterparty plus a postfix, which the next two parameters set.",
        "No.",
        FLAG),
    "flipViewBorrowingCurvePostfix": entry(
        "Flip view borrowing curve postfix",
        "A text.",
        "Sets the text that ORE appends to the name of the counterparty to find the borrowing curve when the "
        "view is flipped. A flipped view prices the XVA from the point of view of the counterparty, so it needs "
        "the curves of the counterparty. For example, _BORROW looks for the curve CPTY_A_BORROW. ORE uses it "
        "only when Flip the view is on.",
        "_BORROW",
        "A text."),
    "flipViewLendingCurvePostfix": entry(
        "Flip view lending curve postfix",
        "A text.",
        "Sets the text that ORE appends to the name of the counterparty to find the lending curve when the view "
        "is flipped. A flipped view prices the XVA from the point of view of the counterparty, so it needs the "
        "curves of the counterparty. For example, _LEND looks for the curve CPTY_A_LEND. ORE uses it only when "
        "Flip the view is on.",
        "_LEND",
        "A text."),
    "rawCubeOutput": entry(
        "Write the raw cube report",
        "A flag. Y writes the report.",
        "Decides whether the analytic writes the trade NPV cube as a text report. The cube holds the NPV of "
        "each trade on each date and path, so the file is very large for a real portfolio. Use it to check a "
        "small test case. A value in Raw cube report file also switches the report on and sets its name. "
        "Without a name the file is rawcube.csv.",
        "No.",
        FLAG),
    "netCubeOutput": entry(
        "Write the net cube report",
        "A flag. Y writes the report.",
        "Decides whether the analytic writes the aggregated NPV cube as a text report. The net cube holds the "
        "netted and collateralised exposure of each netting set on each date and path. It is smaller than the "
        "raw cube. A value in Net cube report file also switches the report on and sets its name. Without a "
        "name the file is netcube.csv.",
        "No.",
        FLAG),
    "timeAveragedNettedExposureOutputFile": entry(
        "Time averaged exposure report file",
        "A file name.",
        "Names the file for the time averaged netted exposure of each netting set. The report averages the "
        "expected exposure over time, which is the number that some capital rules use. For example, "
        "timeaveraged.csv. Setting the parameter switches the report on.",
        "None. The analytic does not write the file.",
        FILE),
    "outputCrossAssetModelData": entry(
        "Write the cross asset model",
        "A flag. Y writes the model data.",
        "Decides whether the analytic writes the cross asset model data of the run to the file "
        "crossassetmodel_xva.xml in the output path. The file holds the calibrated parameters of the model. Use "
        "it to check the model that the run used, or to reuse a calibration in another run.",
        "No.",
        FLAG),
    "generateCorrelations": entry(
        "Generate correlations",
        "A flag. Y runs the correlation analytic.",
        "Decides whether the XVA analytic first runs the correlation analytic and uses its result as the "
        "correlations of the simulation model. Use it when the correlations should come from the data of the "
        "run and not from a file. The correlation analytic reads its parameters from the xva block when it runs "
        "this way. The default is No.",
        "No.",
        FLAG),
    "correlationInputFile": entry(
        "Correlation input file",
        "A file name.",
        "Names a file with correlation data for the risk factor pairs. The analytic reads the correlations "
        "from the file. The name correlationUri is an older name for the same parameter. ORE looks for the "
        "file in the input path of the Setup block.",
        "None.",
        INPUT_FILE),
    "creditMigration": entry(
        "Credit migration",
        "A flag. Y runs the credit migration analytic.",
        "Switches the credit migration calculation on or off. It computes the distribution of the portfolio "
        "value under rating migrations of the counterparties, so it shows the loss that a downgrade causes "
        "before any default. It needs a simulation that stores the NPVs for each credit state, and the credit "
        "migration configuration file.",
        "No.",
        FLAG),
    "creditMigrationConfig": entry(
        "Credit migration configuration file",
        "A file name.",
        "Names the XML file with the credit simulation parameters: the rating states and the migration "
        "matrices. The credit migration calculation reads the matrix that gives the chance of a move from one "
        "rating to another over a period. For example, creditmigration.xml. ORE uses it only when credit "
        "migration is on.",
        "None.",
        INPUT_FILE),
    "creditMigrationDistributionGrid": entry(
        "Credit migration distribution grid",
        "A comma-separated list of numbers.",
        "Sets the grid on which ORE reports the distribution of the portfolio value in the credit migration "
        "calculation. Write three numbers: the lowest value, the highest value and the number of points between "
        "them. For example, -2E7,1E7,300 reports the distribution from minus twenty million to ten million on "
        "300 points. ORE uses it only when credit migration is on.",
        "None.",
        LIST_REAL),
    "creditMigrationTimeSteps": entry(
        "Credit migration time steps",
        "A comma-separated list of whole numbers.",
        "Lists the time steps of the simulation grid at which ORE reports the credit migration distribution. "
        "Each number is the index of a grid date. For example, 12,24 reports the distribution at the twelfth "
        "and the twenty-fourth step. ORE uses it only when credit migration is on.",
        "None.",
        LIST_INT),
    "creditMigrationOutputFiles": entry(
        "Credit migration output files",
        "A file name stem.",
        "Sets the stem of the output files of the credit migration. ORE appends an underscore and the time step "
        "to the stem to name each file. For example, migration gives migration_12.csv for the twelfth step. ORE "
        "uses it only when credit migration is on.",
        "None.",
        "A file name stem without extension."),
    "scenarioCorrSimulation": entry(
        "Correlation scenario simulation file",
        "A file name.",
        "Names the XML file with the simulation market for the correlation calculation. The name "
        "scenarioSimulationUri is an older name for the same parameter." + CORRELATION,
        "None.",
        INPUT_FILE),
    "simulationConfigFile": entry(
        "Simulation configuration file",
        "A file name.",
        "Names the XML file with the simulation configuration for the calculation that ORE runs from the xva "
        "block. For example, simulation.xml." + CORRELATION,
        "None.",
        INPUT_FILE),
    "historicalScenarioFile": entry(
        "Historical scenario file",
        "A file name.",
        "Names the file with the historical market scenarios for the correlation calculation. The name "
        "scenarioFile is an older name for the same parameter." + CORRELATION,
        "None.",
        INPUT_FILE),
    "sensitivityConfigFile": entry(
        "Sensitivity configuration file",
        "A file name.",
        "Names the XML file with the sensitivity configuration, that is the risk factors and the shifts, for "
        "the correlation calculation." + CORRELATION,
        "None.",
        INPUT_FILE),
    "historicalPeriod": entry(
        "Historical period",
        "Two dates or a list of dates.",
        "Sets the period of the history that the correlation calculation uses. The names lookbackPeriod, "
        "benchmarkVarPeriod and benchmarkPeriod are other names for the same parameter." + CORRELATION,
        "None. The correlation calculation needs it.",
        "Dates as the correlation analytic reads them."),
    "mporDays": entry(
        "MPoR days",
        "A whole number of days.",
        "Sets the number of days of the horizon over which the correlation calculation measures the moves "
        "of the risk factors. The name horizonDays is another name for the same parameter." + CORRELATION,
        "10",
        COUNT),
    "mporCalendar": entry(
        "MPoR calendar",
        "A calendar name.",
        "Names the calendar that the correlation calculation uses to count the days of the horizon. The "
        "name horizonCalendar is another name for the same parameter." + CORRELATION,
        "The calendar of the base currency.",
        "A calendar name such as TARGET or UK."),
    "useXvaRunner": entry(
        "Use the XVA runner",
        "A flag. ORE no longer reads it.",
        "Selected an older way to run the XVA calculation. ORE stopped reading it in 1.8.9.1.",
        "None. " + IGNORED,
        FLAG),
    "hyperCube": entry(
        "Hyper cube",
        "A flag. ORE no longer reads it.",
        "Selected the use of the hyper cube in an older version of the XVA calculation. ORE stopped reading "
        "it in 1.8.9.1.",
        "None. " + IGNORED,
        FLAG),
    "collateralSpread": entry(
        "Collateral spread",
        "A number. ORE no longer reads it.",
        "Set the difference between the collateral rate and the overnight rate for the COLVA, as an absolute "
        "number: one basis point is 0.0001. ORE stopped reading it in 1.8.3.0. Today the collateral model "
        "of the netting set definitions sets the collateral rate.",
        "None. " + IGNORED,
        REAL),
    "fvaFundingCurve": entry(
        "FVA funding curve",
        "The name of a yield curve. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it. Use Borrowing curve and "
        "Lending curve to name the curves of the FVA.",
        "None. " + IGNORED,
        "The name of a yield curve in the market data."),
    "fvaInvestmentCurve": entry(
        "FVA investment curve",
        "The name of a yield curve. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it. Use Borrowing curve and "
        "Lending curve to name the curves of the FVA.",
        "None. " + IGNORED,
        "The name of a yield curve in the market data."),
    "colvaSpread": entry(
        "COLVA spread",
        "A number. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it. The collateral model of "
        "the netting set definitions sets the collateral rate.",
        "None. " + IGNORED,
        REAL),
    "eoniaFloor": entry(
        "EONIA floor",
        "A flag. ORE ignores it.",
        "ORE does not read this parameter in any release. Example files carry it. To floor the collateral "
        "rate, use Compute the collateral floor.",
        "None. " + IGNORED,
        FLAG),
})

ENTRIES = {"xva": XVA, "pfe": PFE}
