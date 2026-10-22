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
            "simulated index fixings and FX rates. Variation margin and dynamic initial margin need it. For "
            "example, scenariodata.csv.gz.",
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
            "Names the currency in which the analytic states all NPVs, exposures and value adjustments. ORE "
            "converts the amounts that are in another currency. For example, EUR states all values in euro.",
            "The base currency of the Setup block.",
            "A currency code of three letters, for example EUR."),
        "exposureProfiles": entry(
            "Write exposure profiles",
            "A flag. Y writes the profiles for each netting set.",
            "Decides whether the analytic writes an exposure report for each netting set: the expected "
            "positive and negative exposure and the potential future exposure, for each date." + both,
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
            "Sets the confidence level of the potential future exposure (PFE) in the exposure reports. For "
            "example, 0.95 reports the 95th percentile of the exposure distribution." + both,
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
            "to model a counterparty relationship in which each party can end a trade at its break dates." + both,
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
        "Names the netting set cube file that the XVA analytic reads when the run has no simulation. It is the "
        "file that an earlier simulation wrote. For example, nettingSetCube.csv.gz.",
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
        "Switches the credit value adjustment (CVA) on or off. CVA prices the loss from the default of the "
        "counterparty on the positive exposure.",
        "Yes.",
        FLAG),
    "dva": entry(
        "Compute DVA",
        "A flag. Y computes the debit value adjustment.",
        "Switches the debit value adjustment (DVA) on or off. DVA is the gain from our own default on the "
        "negative exposure. Set the DVA name too, so that ORE can find our own default curve.",
        "No.",
        FLAG),
    "dvaName": entry(
        "DVA name",
        "A credit name.",
        "Names the credit entity that ORE uses to look up our own default probability curve and recovery rate "
        "for the DVA. For example, BANK.",
        "None. A DVA calculation needs a name.",
        "A credit name as it appears in the market data, for example BANK."),
    "fva": entry(
        "Compute FVA",
        "A flag. Y computes the funding value adjustment.",
        "Switches the funding value adjustment (FVA) on or off. FVA prices the cost of funding the "
        "uncollateralised exposure. It needs a borrowing curve and a lending curve.",
        "No.",
        FLAG),
    "fvaBorrowingCurve": entry(
        "FVA borrowing curve",
        "The name of a yield curve.",
        "Names the yield curve at which we borrow, for the FVA. This spelling is the older one, and ORE reads it "
        "only when the parameter Borrowing curve is missing. For example, BANK_EUR_BORROW.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "fvaLendingCurve": entry(
        "FVA lending curve",
        "The name of a yield curve.",
        "Names the yield curve at which we lend, for the FVA. This spelling is the older one, and ORE reads it "
        "only when the parameter Lending curve is missing. For example, BANK_EUR_LEND.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "borrowingCurve": entry(
        "Borrowing curve",
        "The name of a yield curve.",
        "Names the yield curve at which we borrow, for the FVA. For example, BANK_EUR_BORROW.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "lendingCurve": entry(
        "Lending curve",
        "The name of a yield curve.",
        "Names the yield curve at which we lend, for the FVA. For example, BANK_EUR_LEND.",
        "None. An FVA calculation needs a curve.",
        "The name of a yield curve in the market data."),
    "colva": entry(
        "Compute COLVA",
        "A flag. Y computes the collateral value adjustment.",
        "Switches the collateral value adjustment (COLVA) on or off. COLVA prices the difference between the "
        "rate that the collateral earns and the overnight rate.",
        "No.",
        FLAG),
    "collateralFloor": entry(
        "Compute the collateral floor",
        "A flag. Y computes the collateral floor value.",
        "Switches the calculation of the collateral floor on or off. The calculation values a floor on "
        "the collateral rate.",
        "No.",
        FLAG),
    "mva": entry(
        "Compute MVA",
        "A flag. Y computes the margin value adjustment.",
        "Switches the margin value adjustment (MVA) on or off. MVA prices the funding cost of the initial "
        "margin. It takes the dynamic initial margin from the DIM model that the DIM model parameter selects.",
        "No.",
        FLAG),
    "kva": entry(
        "Compute KVA",
        "A flag. Y computes the capital value adjustment.",
        "Switches the capital value adjustment (KVA) on or off. KVA prices the cost of the regulatory capital "
        "that the counterparty credit risk needs. The KVA parameters set the capital model.",
        "No.",
        FLAG),
    "kvaCapitalDiscountRate": entry(
        "KVA capital discount rate",
        "A rate, for example 0.10.",
        "Sets the rate at which ORE discounts the cost of capital in the KVA." + KVA,
        "0.10",
        REAL),
    "kvaAlpha": entry(
        "KVA alpha",
        "A number.",
        "Sets the multiplier alpha of the regulatory exposure at default in the KVA calculation." + KVA,
        "1.4",
        REAL),
    "kvaRegAdjustment": entry(
        "KVA regulatory adjustment",
        "A number.",
        "Sets the regulatory adjustment factor that turns the capital requirement into risk weighted assets "
        "in the KVA calculation." + KVA,
        "12.5",
        REAL),
    "kvaCapitalHurdle": entry(
        "KVA capital hurdle",
        "A rate, for example 0.012.",
        "Sets the return on capital that the KVA charges, the capital hurdle rate." + KVA,
        "0.012",
        REAL),
    "kvaOurPdFloor": entry(
        "KVA floor on our default probability",
        "A probability.",
        "Sets the lowest default probability that the KVA uses for us." + KVA,
        "0.03",
        REAL),
    "kvaTheirPdFloor": entry(
        "KVA floor on their default probability",
        "A probability.",
        "Sets the lowest default probability that the KVA uses for the counterparty." + KVA,
        "0.03",
        REAL),
    "kvaOurCvaRiskWeight": entry(
        "KVA CVA risk weight for us",
        "A number.",
        "Sets the CVA risk weight that the KVA uses for us." + KVA,
        "0.05",
        REAL),
    "kvaTheirCvaRiskWeight": entry(
        "KVA CVA risk weight for them",
        "A number.",
        "Sets the CVA risk weight that the KVA uses for the counterparty." + KVA,
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
        "counterparties. The CVA sensitivity grid and the shift size set the points and the bump.",
        "No.",
        FLAG),
    "cvaSensiGrid": entry(
        "CVA sensitivity grid",
        "A comma-separated list of periods.",
        "Lists the tenors of the credit spread curve for which the analytic computes the CVA sensitivity. For "
        "example, 6M,1Y,2Y,3Y,5Y,10Y. ORE uses it only when CVA sensitivities are on.",
        "None.",
        "A comma-separated list of periods such as 1Y,5Y."),
    "cvaSensiShiftSize": entry(
        "CVA sensitivity shift size",
        "A number.",
        "Sets the size of the bump of the credit spread in the CVA sensitivity calculation. The default is "
        "one basis point. ORE uses it only when CVA sensitivities are on.",
        "0.0001",
        REAL),
    "dim": entry(
        "Compute DIM",
        "A flag. Y computes the dynamic initial margin.",
        "Switches the dynamic initial margin (DIM) calculation on or off. DIM projects the initial margin "
        "of each netting set along each path. The parameter DIM model selects the method. MVA needs it.",
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
        "Sets the quantile of the margin movement that the DIM covers. For example, 0.99 covers the 99th "
        "percentile." + DIM,
        "0.99",
        REAL),
    "dimHorizonCalendarDays": entry(
        "DIM horizon in calendar days",
        "A whole number of days.",
        "Sets the length of the margin period of risk that the DIM covers, in calendar days. For example, "
        "14 is two weeks." + DIM,
        "14",
        COUNT),
    "dimRegressionOrder": entry(
        "DIM regression order",
        "A whole number.",
        "Sets the order of the regression polynomial of the DIM model Regression. The regression relates the "
        "clean change of the netting set value over the period to the value at the start of the period." + DIM,
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
        "Sets the bandwidth of the local regression that validates the polynomial regression, in standard "
        "deviations of the NPV." + DIM,
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
        "time." + DIM,
        "None. The analytic does not write the file.",
        FILE),
    "dimRegressionFiles": entry(
        "DIM regression snapshot files",
        "A comma-separated list of file names.",
        "Names the files for snapshots of the DIM regression. The files go with the grid points that DIM "
        "output grid points lists, one file for each point." + DIM,
        "None. The analytic does not write the files.",
        "A comma-separated list of file names."),
    "dimOutputNettingSet": entry(
        "DIM snapshot netting set",
        "A netting set id.",
        "Names the netting set for which the analytic writes the DIM regression snapshot. For example, CPTY_A." + DIM,
        "None.",
        "A netting set id."),
    "dimOutputGridPoints": entry(
        "DIM snapshot grid points",
        "A comma-separated list of whole numbers.",
        "Lists the points of the time grid for which the analytic writes a DIM regression snapshot." + DIM,
        "None.",
        LIST_INT),
    "dimDistributionCoveredStdDevs": entry(
        "DIM distribution range",
        "A number of standard deviations, or inf.",
        "Sets the range of the DIM distribution report: ORE covers this many standard deviations around "
        "the mean of the data. The value inf covers all of the data." + DIM,
        "5.0",
        "A number, or the word inf."),
    "dimDistributionGridSize": entry(
        "DIM distribution grid size",
        "A whole number.",
        "Sets the number of points of the grid of the DIM distribution report." + DIM,
        "50",
        COUNT),
    "deterministicInitialMarginFile": entry(
        "Deterministic initial margin file",
        "A file name.",
        "Names a file with the initial margin of each netting set as a time series. The analytic uses these "
        "amounts for the initial margin instead of a model. ORE looks for the file in the input path of the "
        "Setup block.",
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
        "view is flipped. For example, _BORROW looks for the curve CPTY_A_BORROW. ORE uses it only when "
        "Flip the view is on.",
        "_BORROW",
        "A text."),
    "flipViewLendingCurvePostfix": entry(
        "Flip view lending curve postfix",
        "A text.",
        "Sets the text that ORE appends to the name of the counterparty to find the lending curve when the "
        "view is flipped. For example, _LEND looks for the curve CPTY_A_LEND. ORE uses it only when "
        "Flip the view is on.",
        "_LEND",
        "A text."),
    "rawCubeOutput": entry(
        "Write the raw cube report",
        "A flag. Y writes the report.",
        "Decides whether the analytic writes the trade NPV cube as a text report. A value in Raw cube report "
        "file also switches the report on and sets its name. Without a name the file is rawcube.csv.",
        "No.",
        FLAG),
    "netCubeOutput": entry(
        "Write the net cube report",
        "A flag. Y writes the report.",
        "Decides whether the analytic writes the aggregated NPV cube as a text report. A value in Net cube "
        "report file also switches the report on and sets its name. Without a name the file is netcube.csv.",
        "No.",
        FLAG),
    "timeAveragedNettedExposureOutputFile": entry(
        "Time averaged exposure report file",
        "A file name.",
        "Names the file for the time averaged netted exposure of each netting set. Setting it switches the "
        "report on.",
        "None. The analytic does not write the file.",
        FILE),
    "outputCrossAssetModelData": entry(
        "Write the cross asset model",
        "A flag. Y writes the model data.",
        "Decides whether the analytic writes the cross asset model data of the run to the file "
        "crossassetmodel_xva.xml in the output path. Use it to check the model that the run used.",
        "No.",
        FLAG),
    "generateCorrelations": entry(
        "Generate correlations",
        "A flag. Y runs the correlation analytic.",
        "Decides whether the XVA analytic first runs the correlation analytic and uses its result as the "
        "correlations of the simulation model. The correlation analytic reads its parameters from the xva "
        "block when it runs this way.",
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
        "value under rating migrations of the counterparties. It needs a simulation that stores the NPVs "
        "for each credit state.",
        "No.",
        FLAG),
    "creditMigrationConfig": entry(
        "Credit migration configuration file",
        "A file name.",
        "Names the XML file with the credit simulation parameters: the rating states and the migration "
        "matrices. ORE uses it only when credit migration is on.",
        "None.",
        INPUT_FILE),
    "creditMigrationDistributionGrid": entry(
        "Credit migration distribution grid",
        "A comma-separated list of numbers.",
        "Lists the points of the grid on which ORE reports the distribution of the portfolio value in the "
        "credit migration calculation. ORE uses it only when credit migration is on.",
        "None.",
        LIST_REAL),
    "creditMigrationTimeSteps": entry(
        "Credit migration time steps",
        "A comma-separated list of whole numbers.",
        "Lists the time steps of the simulation grid at which ORE reports the credit migration "
        "distribution. ORE uses it only when credit migration is on.",
        "None.",
        LIST_INT),
    "creditMigrationOutputFiles": entry(
        "Credit migration output files",
        "A file name stem.",
        "Sets the stem of the output files of the credit migration. ORE appends an underscore and the time "
        "step to the stem to name each file. ORE uses it only when credit migration is on.",
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
