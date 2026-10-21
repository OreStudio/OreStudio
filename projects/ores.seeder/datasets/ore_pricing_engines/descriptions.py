"""
The descriptions the ORE user guide chapter does not give: one for each model, engine and
sensitivity template, and the wording of the settings whose guide text is only a list of
values or is too short to read on its own.

Each model and engine entry is (description, detail, source). The description is one
line for a list. The detail is the paragraph that the screen shows next to the choice.
The source says where the text comes from:

  ore_guide    the text restates the ORE user guide or the ORE models guide
  ore_code     the text restates what the ORE code builds (the engine class it creates)
  authored     the text is written from the standard literature or from QuantLib, because
               the ORE documents do not describe the item. A person must review it.
"""

GUIDE = "ore_guide"
CODE = "ore_code"
AUTHORED = "authored"

MODELS = {
    "Accrual": (
        "Accrues a repo at its rate; no market model.",
        "The repo value follows from accruing the cash and the security on the repo terms. No stochastic model and no volatility input are involved.",
        CODE),
    "BGM": (
        "Brace-Gatarek-Musiela market model for forward rates.",
        "A LIBOR market model. ORE uses it in the range accrual pricer, which builds the model from the smile sections of the swaption volatility surface and prices each daily range observation.",
        GUIDE),
    "Black": (
        "Black (1976) lognormal model on a forward.",
        "Prices an option on a forward with a lognormal or shifted lognormal volatility. ORE reads the volatility from the market surface that the product names.",
        AUTHORED),
    "BlackBachelier": (
        "Black or Bachelier, chosen by the volatility input.",
        "The swaption volatility surface decides the model. A lognormal or shifted lognormal surface prices with Black. A normal surface prices with Bachelier.",
        GUIDE),
    "BlackOrBachelier": (
        "Black or Bachelier for coupon pricers, chosen by the volatility input.",
        "The caplet volatility surface decides the model. A lognormal or shifted lognormal surface prices the capped and floored coupons with Black. A normal surface prices them with Bachelier.",
        GUIDE),
    "BlackQuantoAdjusted": (
        "Black model with a quanto adjustment from the FX-equity correlation.",
        "Prices an equity coupon paid in another currency. The forward of the equity is adjusted with the FX volatility and the equity-FX correlation. A setting can switch the convexity adjustment off.",
        GUIDE),
    "BlackScholes": (
        "Black-Scholes model with a volatility surface and no dividend yield.",
        "The classic lognormal model with a constant rate and volatility for each strike and expiry. The market volatility surface supplies the volatility.",
        AUTHORED),
    "BlackScholesMerton": (
        "Black-Scholes model with a continuous dividend yield.",
        "The Black-Scholes model for an equity that pays a continuous dividend yield. The forward of the equity follows from the equity curve and the dividend curve.",
        AUTHORED),
    "BrigoMercurio": (
        "Lognormal model for CMS spread options, after Brigo and Mercurio.",
        "Prices the spread of two constant maturity swap rates with two correlated lognormal variables. The reference is Brigo and Mercurio, Interest Rate Models - Theory and Practice, section 13.16.2.",
        GUIDE),
    "CapFlooredNonStdYYModel": (
        "Year-on-year inflation coupon model for non-standard legs.",
        "Chooses a Black, unit displaced Black or Bachelier inflation coupon pricer from the volatility type of the market surface. The non-standard pricers handle coupons whose accrual does not follow the standard year-on-year timing.",
        GUIDE),
    "CapFlooredYYModel": (
        "Year-on-year inflation coupon model.",
        "Chooses a Black, unit displaced Black or Bachelier year-on-year inflation coupon pricer from the volatility type of the market surface.",
        GUIDE),
    "CpiCapModel": (
        "CPI cap and floor model, Black or Bachelier.",
        "Chooses the Black or the Bachelier CPI cap floor pricer from the volatility type of the market surface.",
        GUIDE),
    "CrossAssetModel": (
        "ORE cross asset model: Gaussian rates, FX, inflation, credit, equity and commodity.",
        "Joins single-asset parametrizations with a correlation matrix. It can be one LGM model for one currency, a cross currency LGM model, or a model for rates, FX, inflation, credit, equity and commodity together. ORE uses it to simulate exposure, and with the MC and AMC engines to price products by Monte Carlo.",
        GUIDE),
    "DefaultableEquityJumpDiffusion": (
        "Equity jump-to-default model for convertible bonds.",
        "The stock price follows a diffusion with a default intensity that depends on the stock price, and it jumps to zero at default. The model follows Andersen and Buffum, Calibration and Implementation of Convertible Bond Models (2002).",
        GUIDE),
    "DiscountedCashflows": (
        "Discounts the known cashflows; no stochastic model.",
        "Projects each cashflow from the curves of the market and discounts it. The result needs no volatility. Credit-risky products also use the default curve and the recovery rate.",
        CODE),
    "GarmanKohlhagen": (
        "Black-Scholes model for FX with a domestic and a foreign rate.",
        "The Garman-Kohlhagen extension of Black-Scholes for a currency pair. The foreign rate takes the place of the dividend yield.",
        AUTHORED),
    "GaussCopula": (
        "One-factor Gaussian copula for credit index tranches.",
        "Links the defaults of the names of an index with one common Gaussian factor. The tranche loss follows from the loss distribution of the pool.",
        AUTHORED),
    "Hagan": (
        "Hagan replication pricers for constant maturity swap coupons.",
        "Prices a CMS coupon by replicating the coupon with swaptions, after Hagan. The analytic pricer uses a closed-form approximation. The numerical pricer integrates numerically.",
        GUIDE),
    "IborCapModel": (
        "Cap and floor model, Black or Bachelier.",
        "Chooses the Black or the Bachelier cap floor engine from the volatility type of the market surface.",
        GUIDE),
    "LGM": (
        "Linear Gauss Markov model, a one-factor Gaussian rates model.",
        "A one-factor Gaussian short rate model in the form of Hagan. In ORE it is a cross asset model with one currency. Its state process always uses analytical moments, which makes simulation fast. ORE calibrates it to swaption prices.",
        GUIDE),
    "LinearTSR": (
        "Linear terminal swap rate model for CMS convexity adjustments.",
        "Models the annuity mapping as a linear function of the swap rate. The policy setting bounds the rates and the strikes of the replication.",
        GUIDE),
    "LognormalAdjustedIndexSpread": (
        "Lognormal model for the spread of an index CDS.",
        "Prices an option on an index CDS with a lognormal spread. The model adjusts the index spread for the front end protection.",
        AUTHORED),
    "OneFactorCopula": (
        "One-factor copula for cash flow CBO tranches.",
        "Links the defaults in the pool of a collateralised bond obligation with one common factor and prices the tranches by Monte Carlo.",
        AUTHORED),
    "ScriptedTrade": (
        "Scripted trade model; the script defines the payoff.",
        "The scripted trade framework simulates the underlyings and evaluates the script of the trade. The model and engine settings come from the pricing engine configuration of the product type ScriptedTrade.",
        GUIDE),
    "YYCapModel": (
        "Year-on-year inflation cap and floor model.",
        "Chooses a Black, unit displaced Black or Bachelier year-on-year cap floor engine from the volatility type of the market surface.",
        GUIDE),
}

ENGINES = {
    "AMC": (
        "American Monte Carlo engine for use inside an exposure simulation.",
        "Builds an engine that prices the product on the paths of the exposure simulation with a regression, so the product is valued once for all dates. See the ORE AMC documentation.",
        GUIDE),
    "AMCCG": (
        "American Monte Carlo engine on the computation graph.",
        "The AMC engine for the computation graph. It supports automatic differentiation for sensitivities. See the ORE AMC documentation.",
        GUIDE),
    "AccrualRepoEngine": (
        "Values a bond repo by accrual.",
        "Builds an AccrualBondRepoEngine. The engine accrues the repo cash at the repo rate.",
        CODE),
    "Analytic": (
        "Closed-form pricing engine of the product.",
        "Prices the product with a closed formula of the model. The product decides which formula.",
        AUTHORED),
    "AnalyticBarrierEngine": (
        "Closed-form price of a single barrier option.",
        "Prices a European barrier option with the formulas of Haug, Reiner and Rubinstein.",
        AUTHORED),
    "AnalyticCashSettledEuropeanEngine": (
        "Closed-form price of a cash-settled European option.",
        "Prices a European option that settles in cash, so the payment follows the price at expiry and not the delivery of the underlying.",
        AUTHORED),
    "AnalyticContinuousGeometricAPEngine": (
        "Closed-form price of a continuous geometric average price Asian option.",
        "The average is the geometric mean over a continuous window, which gives a closed formula.",
        AUTHORED),
    "AnalyticDigitalAmericanEngine": (
        "Closed-form price of a one-touch or no-touch digital option.",
        "Builds an AnalyticDigitalAmericanEngine for a one touch, or an AnalyticDigitalAmericanKOEngine for a no touch.",
        CODE),
    "AnalyticDiscreteGeometricAPEngine": (
        "Closed-form price of a discrete geometric average price Asian option.",
        "The average is the geometric mean over fixing dates, which gives a closed formula.",
        AUTHORED),
    "AnalyticDiscreteGeometricASEngine": (
        "Closed-form price of a discrete geometric average strike Asian option.",
        "The strike is the geometric mean over fixing dates, which gives a closed formula.",
        AUTHORED),
    "AnalyticDoubleBarrierBinaryEngine": (
        "Closed-form price of a double barrier binary option.",
        "Prices a digital option that knocks in or out at an upper and a lower barrier.",
        AUTHORED),
    "AnalyticDoubleBarrierEngine": (
        "Closed-form price of a double barrier option.",
        "Prices a vanilla option that knocks in or out at an upper and a lower barrier, with the Ikeda-Kunitomo series.",
        AUTHORED),
    "AnalyticEuropeanEngine": (
        "Closed-form Black-Scholes price of a European option.",
        "Prices a European option with the Black-Scholes formula for the model of the pair.",
        AUTHORED),
    "AnalyticEuropeanForwardEngine": (
        "Closed-form price of an option on a forward.",
        "Prices a European option on a forward or future with the Black formula. The underlying is the forward and not the spot.",
        AUTHORED),
    "AnalyticalApproximation": (
        "Analytical approximation for average price options.",
        "Builds a CommodityAveragePriceOptionAnalyticalEngine. The setting Volatility selects the model for the average.",
        CODE),
    "BaroneAdesiWhaleyApproximationEngine": (
        "Barone-Adesi and Whaley approximation for American options.",
        "A quick analytic approximation of the early exercise premium of an American option.",
        AUTHORED),
    "BlackAnalytic": (
        "Black or Bachelier CPI coupon pricer.",
        "Builds a BlackCPICouponPricer or a BachelierCPICouponPricer, depending on the volatility input.",
        CODE),
    "BlackAverageBMACouponPricer": (
        "Coupon pricer for capped and floored average BMA coupons.",
        "Prices the cap and floor of an average BMA indexed coupon with Black or Bachelier.",
        CODE),
    "BlackAverageONIndexedCouponPricer": (
        "Coupon pricer for capped and floored average overnight coupons.",
        "Prices the cap and floor of an average overnight indexed coupon with Black or Bachelier.",
        CODE),
    "BlackBachelierSwaptionEngine": (
        "Black or Bachelier price of a European swaption.",
        "Builds a BlackMultiLegOptionEngine. The volatility surface decides between Black and Bachelier.",
        CODE),
    "BlackBondOptionEngine": (
        "Black price of a bond option.",
        "Builds a BlackBondOptionEngine. The option is on the forward price of the bond.",
        CODE),
    "BlackCdsOptionEngine": (
        "Black price of an option on a credit default swap.",
        "Builds a BlackCdsOptionEngine. The option is on the forward spread of the default swap.",
        CODE),
    "BlackIborCouponPricer": (
        "Coupon pricer for capped and floored Ibor coupons.",
        "Prices the cap and floor of an Ibor coupon with Black or Bachelier.",
        CODE),
    "BlackIndexCdsOptionEngine": (
        "Black price of an option on an index credit default swap.",
        "Builds a BlackIndexCdsOptionEngine. The option is on the forward spread of the index.",
        CODE),
    "BlackInterpolatedIborCouponPricer": (
        "Coupon pricer for capped and floored interpolated Ibor coupons.",
        "Prices the cap and floor of a coupon that interpolates two Ibor tenors, with Black or Bachelier.",
        CODE),
    "BlackOvernightIndexedCouponPricer": (
        "Coupon pricer for capped and floored overnight coupons.",
        "Prices the cap and floor of an overnight indexed coupon with Black or Bachelier.",
        CODE),
    "BlackQuantoEquityCouponPricer": (
        "Equity coupon pricer with a quanto adjustment.",
        "Prices an equity coupon in another currency. The setting ApplyConvexityAdjustment switches the convexity adjustment.",
        GUIDE),
    "Bucketing": (
        "Hull-White bucketing of the loss distribution of a tranche.",
        "Builds an IndexCdsTrancheEngine. The loss distribution comes from the bucketing method of Hull and White.",
        CODE),
    "CallSpreadCouponPricer": (
        "Range accrual pricer by call spread replication.",
        "Builds a RangeAccrualPricerByCallSpread. The pricer reads the optionlet surface directly and prices each daily observation as a call spread on caplets.",
        GUIDE),
    "CallSpreadEngine": (
        "Digital option price by call spread replication.",
        "Prices a digital option as the spread of two vanilla options around the strike, so the volatility smile enters the price.",
        GUIDE),
    "CapFlooredNonStdYYCouponPricer": (
        "Coupon pricer for non-standard year-on-year inflation coupons.",
        "Builds a non-standard Black, unit displaced Black or Bachelier year-on-year coupon pricer, depending on the volatility input.",
        CODE),
    "CapFlooredYYCouponPricer": (
        "Coupon pricer for year-on-year inflation coupons.",
        "Builds a Black, unit displaced Black or Bachelier year-on-year coupon pricer, depending on the volatility input.",
        CODE),
    "CommodityCalendarSpreadOptionBachelierEngine": (
        "Bachelier price of a commodity calendar spread option.",
        "Prices an option on the difference of two forward prices of one commodity at two delivery dates.",
        AUTHORED),
    "CommoditySpreadOptionEngine": (
        "Analytic price of a commodity spread option.",
        "Builds a CommoditySpreadOptionAnalyticalEngine. It uses the Bachelier model or the Kirk approximation of Clark, Commodity Option Pricing, section 2.9. The correlation of the two commodities comes from the market.",
        GUIDE),
    "CommoditySwapEngine": (
        "Discounting price of a commodity swap.",
        "Builds a DiscountingSwapEngine for the legs of a commodity swap.",
        CODE),
    "CpiCapEngine": (
        "Black or Bachelier price of a CPI cap or floor.",
        "Builds a CPIBlackCapFloorEngine or a CPIBachelierCapFloorEngine, depending on the volatility type of the market surface.",
        CODE),
    "DiscountingBondFutureEngine": (
        "Discounting price of a bond future.",
        "Builds a DiscountingBondFutureEngine for the future on the cheapest to deliver bond.",
        CODE),
    "DiscountingBondTRSEngine": (
        "Discounting price of a bond total return swap.",
        "Builds a DiscountingBondTRSEngine. The bond leg and the funding leg are discounted.",
        CODE),
    "DiscountingCommodityForwardEngine": (
        "Discounting price of a commodity forward.",
        "Builds a DiscountingCommodityForwardEngine.",
        CODE),
    "DiscountingCreditLinkedSwapEngine": (
        "Discounting price of a credit linked swap.",
        "Builds a DiscountingCreditLinkedSwapEngine.",
        CODE),
    "DiscountingCrossCurrencySwapEngine": (
        "Discounting price of a cross currency swap.",
        "Builds the discounting currency swap engine. Each leg is discounted in its own currency and converted at the spot rate.",
        CODE),
    "DiscountingEquityForwardEngine": (
        "Discounting price of an equity forward.",
        "Builds a DiscountingEquityForwardEngine.",
        CODE),
    "DiscountingForwardBondEngine": (
        "Discounting price of a forward bond.",
        "Builds a DiscountingForwardBondEngine.",
        CODE),
    "DiscountingFxForwardEngine": (
        "Discounting price of an FX forward.",
        "Builds a DiscountingFxForwardEngine. Each currency leg is discounted and converted at the spot rate.",
        CODE),
    "DiscountingRepoEngine": (
        "Discounting price of a bond repo.",
        "Builds a DiscountingBondRepoEngine.",
        CODE),
    "DiscountingRiskyBondEngine": (
        "Discounting price of a bond with credit risk.",
        "Builds a DiscountingRiskyBondEngine. It discounts the coupons with the survival probability of the issuer and adds the recovery on default.",
        CODE),
    "DiscountingRiskyBondEngineMultiState": (
        "Risky bond engine for the ORE credit model.",
        "Builds a DiscountingRiskyBondEngineMultiState for use in the credit model. See the ORE credit model documentation.",
        GUIDE),
    "DiscountingSwapEngine": (
        "Discounting price of a swap.",
        "Builds a DiscountingSwapEngine. Each leg is projected on its curve and discounted.",
        CODE),
    "EquityCouponPricer": (
        "Pricer for an equity leg coupon.",
        "Takes no setting. It prices the coupons of an equity leg from the equity forward.",
        GUIDE),
    "EquityVarianceSwap": (
        "Replicating price of a variance or volatility swap.",
        "Builds a GeneralisedReplicatingVarianceSwapEngine or a VolatilityFromVarianceSwapEngine, depending on the moment type.",
        CODE),
    "FD": (
        "Finite difference engine.",
        "Solves the pricing equation of the model on a grid of state and time. The settings set the grid size, the scheme and the time steps.",
        GUIDE),
    "FdBlackScholesBarrierEngine": (
        "Finite difference price of a barrier option.",
        "Builds a FdBlackScholesBarrierEngine. It handles barriers that have no closed formula.",
        CODE),
    "FdBlackScholesVanillaEngine": (
        "Finite difference price of an American vanilla option.",
        "Builds a FdBlackScholesVanillaEngine. The grid handles early exercise.",
        CODE),
    "FloatingRateCouponPricer": (
        "Range accrual coupon pricer on a swaption smile.",
        "Builds a RangeAccrualPricerByBgm for each coupon of the range accrual leg.",
        GUIDE),
    "Grid": (
        "Convolution grid engine for the LGM model.",
        "Rolls the value back on a grid of the LGM state with numerical integration, after Hagan, Methodology for callable swaps and Bermudan exercise into swaptions. The settings set the width and the point count of the grid.",
        GUIDE),
    "IborCapEngine": (
        "Black or Bachelier price of a cap or floor.",
        "Builds a BlackCapFloorEngine or a BachelierCapFloorEngine, depending on the volatility type.",
        CODE),
    "Intrinsic": (
        "Intrinsic value of the product.",
        "Builds an IntrinsicAscotEngine. The value is the payoff at the current market with no time value.",
        CODE),
    "LinearTSRPricer": (
        "Linear terminal swap rate pricer for CMS coupons.",
        "Builds a LinearTSRPricer. The settings choose the replication policy and the rate bounds.",
        CODE),
    "MC": (
        "Monte Carlo engine.",
        "Prices by simulation of the model with the Longstaff-Schwartz regression for early exercise. The settings choose the sequence, the seed, the number of samples and the regression basis.",
        GUIDE),
    "MCCG": (
        "Monte Carlo engine on the computation graph.",
        "Builds a MacCgMultiLegOptionEngine. It supports automatic differentiation for first-order sensitivities.",
        GUIDE),
    "MCDiscreteArithmeticAPEngine": (
        "Monte Carlo price of an arithmetic average price Asian option.",
        "Builds a MCDiscreteArithmeticAPEngine with Sobol sequences.",
        CODE),
    "MCDiscreteArithmeticASEngine": (
        "Monte Carlo price of an arithmetic average strike Asian option.",
        "Builds a MCDiscreteArithmeticASEngine with Sobol sequences.",
        CODE),
    "MCDiscreteGeometricAPEngine": (
        "Monte Carlo price of a geometric average price Asian option.",
        "Builds a MCDiscreteGeometricAPEngine with Sobol sequences.",
        CODE),
    "MCScript": (
        "Monte Carlo engine that runs a script.",
        "Builds a CliquetOptionMcScriptEngine, which uses the scripted trade framework.",
        CODE),
    "MidPointCdsEngine": (
        "Mid-point price of a credit default swap.",
        "Builds a MidPointCdsEngine. The default time in each period is the mid point of the period.",
        CODE),
    "MidPointCdsEngineMultiState": (
        "Credit default swap engine for the ORE credit model.",
        "Builds a MidPointCdsEngineMultiState. Only the credit model uses it. See the ORE credit model documentation.",
        GUIDE),
    "MidPointIndexCdsEngine": (
        "Mid-point price of an index credit default swap.",
        "Builds a MidPointIndexCdsEngine.",
        CODE),
    "MonteCarlo": (
        "Monte Carlo engine.",
        "Prices by simulation of the underlying. The settings choose the number of samples, the seed and the variance reduction.",
        AUTHORED),
    "MonteCarloCBOEngine": (
        "Monte Carlo price of a collateralised bond obligation.",
        "Builds a MonteCarloCBOEngine. It simulates the defaults of the pool and the waterfall of the tranches.",
        CODE),
    "Numerical": (
        "Numerical integration pricer for CMS coupons.",
        "Builds a NumericHaganPricer. It integrates the replication integral numerically.",
        CODE),
    "NumericalIntegrationEngine": (
        "Numerical integration price of an index CDS option.",
        "Builds a NumericalIntegrationIndexCdsOptionEngine.",
        CODE),
    "ScriptedTrade": (
        "Scripted trade pricing engine.",
        "Builds a ScriptedInstrumentPricingEngine, or a variant. See the ORE scripted trade documentation.",
        GUIDE),
    "TurnbullWakemanAsianEngine": (
        "Turnbull-Wakeman approximation for arithmetic Asian options.",
        "Prices an arithmetic average option by matching the first two moments of the average to a lognormal variable.",
        AUTHORED),
    "YYCapEngine": (
        "Black or Bachelier price of a year-on-year inflation cap or floor.",
        "Builds a Black, unit displaced Black or Bachelier year-on-year cap floor engine, depending on the volatility type of the market surface.",
        CODE),
}

# A sensitivity template is a key. The pricing engine configuration puts it on a product,
# and the sensitivity configuration gives shift sizes and schemes to the products that carry it.
TEMPLATES = {
    "IR_Analytical": ("IR", "Analytical", "Interest rate products priced by a closed formula."),
    "IR_Semianalytical": ("IR", "Semianalytical", "Interest rate products priced by a formula that needs a numerical step, such as CMS replication."),
    "IR_FD": ("IR", "Finite difference", "Interest rate products priced on a finite difference grid, such as Bermudan swaptions."),
    "IR_MC": ("IR", "Monte Carlo", "Interest rate products priced by Monte Carlo."),
    "FX_Analytical": ("FX", "Analytical", "FX products priced by a closed formula."),
    "FX_Semianalytical": ("FX", "Semianalytical", "FX products priced by a formula with a numerical step."),
    "FX_FD": ("FX", "Finite difference", "FX products priced on a finite difference grid."),
    "FX_MC": ("FX", "Monte Carlo", "FX products priced by Monte Carlo."),
    "EQ_Analytical": ("EQ", "Analytical", "Equity products priced by a closed formula."),
    "EQ_Semianalytical": ("EQ", "Semianalytical", "Equity products priced by a formula with a numerical step."),
    "EQ_FD": ("EQ", "Finite difference", "Equity products priced on a finite difference grid, such as American options."),
    "EQ_MC": ("EQ", "Monte Carlo", "Equity products priced by Monte Carlo, such as Asian options."),
    "COMM_Analytical": ("COMM", "Analytical", "Commodity products priced by a closed formula."),
    "COMM_Semianalytical": ("COMM", "Semianalytical", "Commodity products priced by a formula with a numerical step."),
    "COMM_FD": ("COMM", "Finite difference", "Commodity products priced on a finite difference grid."),
    "COMM_MC": ("COMM", "Monte Carlo", "Commodity products priced by Monte Carlo."),
    "CR_Semianalytical": ("CR", "Semianalytical", "Credit products priced by a formula with a numerical step."),
    "CR_MC": ("CR", "Monte Carlo", "Credit products priced by Monte Carlo."),
    "SensiTrade": ("ALL", "Trade level", "Products whose sensitivities the trade computes on its own, not by shifting the market."),
}

# The guide text of these settings only lists values or is too short to read on its own.
SETTING_TEXT = {
    "Calibration": "How the model is fitted to the market: Bootstrap fits one instrument at a time, BestFit fits all at once, None keeps the starting values.",
    "CalibrationStrategy": "Which swaptions the fit uses: those that end with the deal and start at the money, those at the strike of the deal, or the delta gamma adjusted set.",
    "ReversionType": "The form of the mean reversion parameter: Hagan or Hull-White.",
    "VolatilityType": "The form of the volatility parameter: Hagan or Hull-White.",
    "Reversion": "The mean reversion speed of the model. A calibrated model starts here.",
    "TimeStepsPerYear": "The number of time steps for each year of the grid or of the simulation.",
    "p": "The parameter p of the model that links the stock price to the default intensity.",
    "eta": "The parameter eta of the model that links the stock price to the default intensity.",
    "Curve": "Which curve the product reads: the index curve, or the curves of the constituents.",
    "FepCurve": "Which curve is used for the front end protection: the index curve, or the curves of the constituents.",
    "SensitivityDecomposition": "How the index sensitivity splits across the constituents: by underlying, by notional, by loss or by delta.",
    "Policy": "How the replication limits its integration range: by rate bounds, by vega ratio, by price threshold or by standard deviations.",
    "VegaRatio": "The vega ratio that sets the bounds of the replication when the policy is VegaRatio.",
    "PriceThreshold": "The price threshold that sets the bounds of the replication when the policy is PriceThreshold.",
    "BsStdDev": "The number of standard deviations that set the bounds of the replication when the policy is BsStdDev.",
    "steps": "The number of steps of the numerical integration.",
    "useQuadrature": "Whether to integrate by quadrature in place of by simulation.",
    "Samples": "The number of Monte Carlo samples.",
    "samples": "The number of Monte Carlo samples.",
    "LossDistributionPeriods": "The periods at which the loss distribution of the pool is computed.",
    "XGrid": "The specification of the state grid of the finite difference solver.",
    "seed": "The seed of the random number generator.",
    "Seed": "The seed of the random number generator.",
    "sy": "The width of the grid on the value axis, in standard deviations, in the notation of Hagan.",
    "sx": "The width of the grid on the state axis, in standard deviations, in the notation of Hagan.",
    "ny": "The number of grid points on the value axis, in the notation of Hagan.",
    "nx": "The number of grid points on the state axis, in the notation of Hagan.",
    "SensitivityTemplate": "The sensitivity template of the product. The sensitivity configuration sets shift sizes and schemes for each template.",
    "Volatility": "The volatility of the model. A calibrated model starts here.",
    "min": "The lower bound of the standard deviation of the Gaussian copula.",
    "max": "The upper bound of the standard deviation of the Gaussian copula.",
    "beta": "The parameter of the correlation parametrization.",
}

# Spelling mistakes of the guide text.
TYPOS = {
    "traning": "training",
    "incoroporated": "incorporated",
    "determinisitc": "deterministic",
    "depneding": "depending",
    "HulWhite": "Hull-White",
    "Antithetc": "Antithetic",
    "Chbyshev": "Chebyshev",
    "Option,": "Optional,",
}

# Names the guide spells differently from the code.
SETTING_NAME_FIX = {"Bootstrp.CalibrationGrid": "Bootstrap.CalibrationGrid"}

# Items of a list in the guide that the parser reads as a setting but that are values.
NOT_SETTINGS = {"Simple", "LaggedFX"}
