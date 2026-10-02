#+title: Configuration types derivation

Where the seventeen seeded configuration types come from, measured at
=8c3d1e5dfa= (main after PR #2518).

* The schemas' global elements

Every ORE configuration document has a schema under =external/ore/xsd= whose
global element is the document's root. This lists them:

#+begin_src python
import glob, os
import xml.etree.ElementTree as ET
XS = "{http://www.w3.org/2001/XMLSchema}"
for path in sorted(glob.glob("external/ore/xsd/*.xsd")):
    root = ET.parse(path).getroot()
    names = [e.get("name") for e in root.findall(XS + "element") if e.get("name")]
    if names:
        print(f"{os.path.basename(path)[:-4]}: {', '.join(names)}")
#+end_src

#+begin_example
baselTrafficLightconfig: BaselTrafficLightConfig
calendaradjustment: CalendarAdjustments
collateralbalance: CollateralBalances
conventions: Conventions
counterparty: CounterpartyInformation
creditsimulation: CreditSimulation
currencyconfig: CurrencyConfig, Currency
curveconfig: CurveConfiguration
historicalreturnconfig: ReturnConfiguration
iborfallbackconfig: IborFallbackConfig
input: Portfolio, subTradeGroup, Trade, SubTrade
instruments: exerciseDatesGroup, ExerciseDates, ExerciseSchedule, legDataType, CashflowData, FixedLegData, FloatingLegData, RangeAccrualLegData, CPILegData, YYLegData, CMSLegData, CMBLegData, DigitalCMSLegData, DurationAdjustedCMSLegData, CMSSpreadLegData, DigitalCMSSpreadLegData, EquityLegData, ZeroCouponFixedLegData, EquityMarginLegData, CommodityFixedLegData, CommodityFloatingLegData, IntradayPowerFloatingLegData, creditCurveIdType, CreditCurveId, ReferenceInformation, underlyingTypes, Name, Underlying, Underlyings, nettingSetGroup, NettingSetId, NettingSetDetails, strikeGroup, Strike, StrikeData, levelGroup, Level, LevelData, FormulaBasedLegData
nettingsetdefinitions: CollateralBalances, NettingSetDefinitions
ore: ORE
ore_types: DerivedScheduleGroup, DerivedSchedule, Derived
pricingengines: PricingEngines
referencedata: ReferenceData
scriptlibrary: ScriptLibrary
sensitivity: SensitivityAnalysis
simmcalibration: SIMMCalibrationData
simulation: Simulation, CrossAssetModel
stress: StressTesting
todaysmarket: TodaysMarket
#+end_example

* From elements to kinds

Seventeen of these are configuration a report binds. The rest are excluded:

| Schema | Why it is not a configuration type |
|--------+------------------------------------|
| =ore= | The run document. A report owns its run setup directly through =report_definition_id=, not through a configuration. |
| =input=, =instruments= | Trades, portfolios and their fragments. |
| =ore_types= | Shared type definitions, not a document. |
| =collateralbalance= | Collateral balances are data, not configuration. |
| =scriptlibrary= | Scripted-trade definitions, not report configuration. |
| =currencyconfig= =Currency=, =simulation= =CrossAssetModel=, =nettingsetdefinitions= =CollateralBalances= | Elements that also appear inside the kind's root document. |

* The run-document parameter

Each kind's =run_parameter= is the =Setup= or analytic parameter a shipped run
document uses to name the file, counted with:

#+begin_src sh
grep -rhoE '<Parameter name="[A-Za-z]*(File|Config|Configuration|Adjustment)[A-Za-z]*"' \
    external/ore/examples --include="ore*.xml" \
    | sed -E 's/.*name="([^"]*)"/\1/' | sort | uniq -c | sort -rn
#+end_src

No shipped run document names a historical return or a Basel traffic light
file, and ORE's own documentation is not vendored, so both are null.

* The owning component

Taken from the story's decision on where a configuration's entities live: a
kind ORE names in its =Analytics= block belongs to =ores.analytics=; curves,
conventions, calendars, currencies and the other shared reference data to
=ores.refdata=; netting sets to =ores.trading=. The older
=ore_report_configuration_mapping.md= predates that decision and names
=reporting= for the analytics kinds.
