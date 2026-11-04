/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/document.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/roundtrip.hpp"
#include "ores.ore.core/xml/value_differ.hpp"
#include "ores.ore.core/xml/value_fingerprint.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include "test_suite.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <functional>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <vector>

/**
 * @file xml_portfolio_corpus_content_roundtrip_tests.cpp
 * @brief Every trade of the ORE example corpus comes back as it was read.
 *
 * Each portfolio is read into the domain and written back, in memory, and each
 * trade is compared with the trade it was read as, value by value. A value the
 * model loses, adds or changes fails here, unless the losses below name it.
 */

namespace {

namespace fs = std::filesystem;
using namespace ores::logging;
using namespace ores::ore::xml;

/**
 * @brief The differences the model is known to make, as trade type, change and
 * path.
 *
 * Asserted for equality: a new difference fails, and a difference that stops
 * happening fails until its entry is removed, so the list can only shrink. An
 * entry is a gap in what we model, and each one is a reason to fix the model
 * rather than a licence to keep it.
 */
const std::set<std::string>& known_losses() {
    static const std::set<std::string> losses = {
        "Bond lost Trade/Envelope/AdditionalFields",
        "BondRepo added Trade/BondRepoData/RepoData/LegData/FixedLegData/Rates/Rate[1]",
        "BondRepo added Trade/BondRepoData/RepoData/LegData/FixedLegData/Rates/Rate[2]",
        "BondRepo lost Trade/BondRepoData/RepoData/LegData/FixedLegData/Rates/Rate",
        "BondTRS added Trade/BondTRSData/FundingData/LegData/FixedLegData/Rates/Rate[1]",
        "BondTRS added Trade/BondTRSData/FundingData/LegData/FixedLegData/Rates/Rate[2]",
        "BondTRS lost Trade/BondTRSData/FundingData/LegData/FixedLegData/Rates/Rate",
        "CallableBond lost Trade/Envelope/AdditionalFields",
        "CapFloor lost Trade/CapFloorData/Floors",
        "CommodityAveragePriceOption changed Trade/CommodityAveragePriceOptionData/Strike",
        "CommodityAveragePriceOption lost Trade/CommodityAveragePriceOptionData/CommodityPayRelativeTo",
        "CommodityAveragePriceOption lost Trade/CommodityAveragePriceOptionData/CommodityQuantityFrequency",
        "CommodityAveragePriceOption lost Trade/CommodityAveragePriceOptionData/DeliveryRollDays",
        "CommodityAveragePriceOption lost Trade/CommodityAveragePriceOptionData/FutureMonthOffset",
        "CommodityAveragePriceOption lost Trade/CommodityAveragePriceOptionData/IncludePeriodEnd",
        "CommodityForward changed Trade/CommodityForwardData/Position",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Calls/LongShorts/LongShort",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Calls/Strikes/Strike",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/LegData/CommodityFloatingLegData/CommodityPayRelativeTo",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/LegData/CommodityFloatingLegData/CommodityQuantityFrequency",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/LegData/CommodityFloatingLegData/PricingDateRule",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Premiums/Premium/Amount",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Premiums/Premium/Currency",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Premiums/Premium/PayDate",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Puts/LongShorts/LongShort",
        "CommodityOptionStrip lost Trade/CommodityOptionStripData/Puts/Strikes/Strike",
        "CommoditySwaption lost Trade/CommoditySwaptionData/LegData[1]/CommodityFixedLegData/CommodityPayRelativeTo",
        "CommoditySwaption lost Trade/CommoditySwaptionData/LegData[2]/CommodityFloatingLegData/CommodityPayRelativeTo",
        "CommoditySwaption lost Trade/CommoditySwaptionData/LegData[2]/CommodityFloatingLegData/CommodityQuantityFrequency",
        "CommoditySwaption lost Trade/CommoditySwaptionData/LegData[2]/CommodityFloatingLegData/PricingDateRule",
        "CommodityVarianceSwap added Trade/CommodityVarianceSwapData/Name",
        "CommodityVarianceSwap changed Trade/CommodityVarianceSwapData/Calendar",
        "CommodityVarianceSwap lost Trade/CommodityVarianceSwapData/AddPastDividends",
        "CommodityVarianceSwap lost Trade/CommodityVarianceSwapData/MomentType",
        "CommodityVarianceSwap lost Trade/CommodityVarianceSwapData/Underlying/Name",
        "CommodityVarianceSwap lost Trade/CommodityVarianceSwapData/Underlying/Type",
        "ContractForDifference lost Trade/ContractForDifferenceData/UnderlyingData/SubTrade/EquityPositionData/Underlying/IdentifierType",
        "ContractForDifference lost Trade/ContractForDifferenceData/UnderlyingData/SubTrade/EquityPositionData/Underlying/Weight",
        "CreditDefaultSwap changed Trade/CreditDefaultSwapData/LegData/ScheduleData/Rules/Convention",
        "CreditDefaultSwap lost Trade/CreditDefaultSwapData/LegData/ScheduleData/Rules/Calendar",
        "CreditDefaultSwap lost Trade/CreditDefaultSwapData/LegData/ScheduleData/Rules/Rule",
        "CreditLinkedSwap changed Trade/CreditLinkedSwapData/ContingentPayments/LegData[1]/ScheduleData/Rules/Convention",
        "CreditLinkedSwap lost Trade/CreditLinkedSwapData/ContingentPayments/LegData[1]/ScheduleData/Rules/Calendar",
        "CreditLinkedSwap lost Trade/CreditLinkedSwapData/ContingentPayments/LegData[1]/ScheduleData/Rules/Rule",
        "CreditLinkedSwap lost Trade/CreditLinkedSwapData/DefaultPayments",
        "CrossCurrencySwap lost Trade/CrossCurrencySwapData/LegData[2]/Notionals/FXReset/FXIndex",
        "CrossCurrencySwap lost Trade/CrossCurrencySwapData/LegData[2]/Notionals/FXReset/ForeignAmount",
        "CrossCurrencySwap lost Trade/CrossCurrencySwapData/LegData[2]/Notionals/FXReset/ForeignCurrency",
        "CrossCurrencySwap lost Trade/CrossCurrencySwapData/Settlement",
        "EquityAccumulator lost Trade/EquityAccumulatorData/SettlementCalendar",
        "EquityAccumulator lost Trade/EquityAccumulatorData/SettlementConvention",
        "EquityAccumulator lost Trade/EquityAccumulatorData/SettlementLag",
        "EquityAccumulator lost Trade/EquityAccumulatorData/Underlying/IdentifierType",
        "EquityAsianOption lost Trade/EquityAsianOptionData/Settlement",
        "EquityAsianOption lost Trade/EquityAsianOptionData/StrikeData/Currency",
        "EquityAsianOption lost Trade/EquityAsianOptionData/Underlying/Currency",
        "EquityBarrierOption lost Trade/EquityBarrierOptionData/Calendar",
        "EquityBarrierOption lost Trade/EquityBarrierOptionData/EQIndex",
        "EquityBarrierOption lost Trade/EquityBarrierOptionData/StartDate",
        "EquityCliquetOption added Trade/EquityCliquetOptionData/Name",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/GlobalFloor",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/LocalCap",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/Premium",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/PremiumPaymentDate",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/SettlementDays",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/Underlying/IdentifierType",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/Underlying/Name",
        "EquityCliquetOption lost Trade/EquityCliquetOptionData/Underlying/Type",
        "EquityDigitalOption changed Trade/EquityDigitalOptionData/Quantity",
        "EquityDigitalOption lost Trade/EquityDigitalOptionData/PayoffCurrency",
        "EquityDoubleBarrierOption lost Trade/EquityDoubleBarrierOptionData/Calendar",
        "EquityDoubleBarrierOption lost Trade/EquityDoubleBarrierOptionData/EQIndex",
        "EquityDoubleBarrierOption lost Trade/EquityDoubleBarrierOptionData/StartDate",
        "EquityOption added Trade/EquityOptionData/Name",
        "EquityOption lost Trade/EquityOptionData/StrikeCurrency",
        "EquityOption lost Trade/EquityOptionData/Underlying/Currency",
        "EquityOption lost Trade/EquityOptionData/Underlying/Exchange",
        "EquityOption lost Trade/EquityOptionData/Underlying/IdentifierType",
        "EquityOption lost Trade/EquityOptionData/Underlying/Name",
        "EquityOption lost Trade/EquityOptionData/Underlying/Type",
        "EquityOutperformanceOption changed Trade/EquityOutperformanceOptionData/InitialPrice1",
        "EquityOutperformanceOption changed Trade/EquityOutperformanceOptionData/InitialPrice2",
        "EquityOutperformanceOption lost Trade/EquityOutperformanceOptionData/KnockInPrice",
        "EquityTaRF lost Trade/EquityTaRFData/RangeBounds/RangeBound[1]/Leverage",
        "EquityTaRF lost Trade/EquityTaRFData/RangeBounds/RangeBound[1]/RangeFrom",
        "EquityTaRF lost Trade/EquityTaRFData/RangeBounds/RangeBound[1]/RangeTo",
        "EquityTaRF lost Trade/EquityTaRFData/RangeBounds/RangeBound[2]/Leverage",
        "EquityTaRF lost Trade/EquityTaRFData/RangeBounds/RangeBound[2]/RangeFrom",
        "EquityTaRF lost Trade/EquityTaRFData/RangeBounds/RangeBound[2]/RangeTo",
        "EquityTaRF lost Trade/EquityTaRFData/RangeBounds/RangeBound[2]/StrikeAdjustment",
        "EquityTaRF lost Trade/EquityTaRFData/Underlying/IdentifierType",
        "EquityTouchOption lost Trade/EquityTouchOptionData/Calendar",
        "EquityTouchOption lost Trade/EquityTouchOptionData/EQIndex",
        "EquityTouchOption lost Trade/EquityTouchOptionData/StartDate",
        "EquityVarianceSwap added Trade/EquityVarianceSwapData/Name",
        "EquityVarianceSwap changed Trade/EquityVarianceSwapData/Calendar",
        "EquityVarianceSwap lost Trade/EquityVarianceSwapData/MomentType",
        "EquityVarianceSwap lost Trade/EquityVarianceSwapData/Underlying/IdentifierType",
        "EquityVarianceSwap lost Trade/EquityVarianceSwapData/Underlying/Name",
        "EquityVarianceSwap lost Trade/EquityVarianceSwapData/Underlying/Type",
        "EquityWorstOfBasketSwap changed Trade/EquityWorstOfBasketSwapData/FixedRate",
        "EquityWorstOfBasketSwap changed Trade/EquityWorstOfBasketSwapData/FloatingIndex",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/AccruingFixedCoupons",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/AccumulatingFixedCoupons",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/BermudanKnockIn",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/BaseSchedule",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/Calendar",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/Convention",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingFixingSchedule/DerivedSchedule/Shift",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/FloatingSpread",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/InitialFixedRate",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/KnockInLevel",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/Underlyings/Underlying[1]/IdentifierType",
        "EquityWorstOfBasketSwap lost Trade/EquityWorstOfBasketSwapData/Underlyings/Underlying[2]/IdentifierType",
        "ForwardBond lost Trade/Envelope/AdditionalFields",
        "FxAverageForward changed Trade/FxAverageForwardData/FixedPayer",
        "FxBarrierOption lost Trade/FxBarrierOptionData/FXIndex",
        "FxDigitalBarrierOption lost Trade/FxDigitalBarrierOptionData/PayoffCurrency",
        "FxDoubleTouchOption lost Trade/FxDoubleTouchOptionData/Calendar",
        "FxDoubleTouchOption lost Trade/FxDoubleTouchOptionData/FXIndex",
        "FxDoubleTouchOption lost Trade/FxDoubleTouchOptionData/StartDate",
        "FxForward lost Trade/FxForwardData/SettlementData/Currency",
        "FxForward lost Trade/FxForwardData/SettlementData/Date",
        "FxForward lost Trade/FxForwardData/SettlementData/FXIndex",
        "FxForward lost Trade/FxForwardData/SettlementData/Rules/PaymentCalendar",
        "FxForward lost Trade/FxForwardData/SettlementData/Rules/PaymentConvention",
        "FxForward lost Trade/FxForwardData/SettlementData/Rules/PaymentLag",
        "FxGenericBarrierOption added Trade/FxGenericBarrierOptionData/Name",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/KikoType",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Calendar",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Convention",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/EndDate",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Rule",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/StartDate",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Barriers/ScheduleData/Rules/Tenor",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Quantity",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/SettlementDate",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Strike",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Underlying/Name",
        "FxGenericBarrierOption lost Trade/FxGenericBarrierOptionData/Underlying/Type",
        "FxKIKOBarrierOption lost Trade/FxKIKOBarrierOptionData/Calendar",
        "FxKIKOBarrierOption lost Trade/FxKIKOBarrierOptionData/FXIndex",
        "FxKIKOBarrierOption lost Trade/FxKIKOBarrierOptionData/StartDate",
        "FxSwap changed Trade/FxSwapData/FarBoughtAmount",
        "FxSwap changed Trade/FxSwapData/FarDate",
        "FxSwap changed Trade/FxSwapData/FarSoldAmount",
        "FxTaRF lost Trade/FxTaRFData/RangeBounds/RangeBound[1]/Leverage",
        "FxTaRF lost Trade/FxTaRFData/RangeBounds/RangeBound[1]/RangeFrom",
        "FxTaRF lost Trade/FxTaRFData/RangeBounds/RangeBound[1]/RangeTo",
        "FxTaRF lost Trade/FxTaRFData/RangeBounds/RangeBound[2]/Leverage",
        "FxTaRF lost Trade/FxTaRFData/RangeBounds/RangeBound[2]/RangeFrom",
        "FxTaRF lost Trade/FxTaRFData/RangeBounds/RangeBound[2]/RangeTo",
        "FxTaRF lost Trade/FxTaRFData/TargetAmount",
        "FxVarianceSwap added Trade/FxVarianceSwapData/Name",
        "FxVarianceSwap changed Trade/FxVarianceSwapData/Calendar",
        "FxVarianceSwap lost Trade/FxVarianceSwapData/MomentType",
        "FxVarianceSwap lost Trade/FxVarianceSwapData/Underlying/Name",
        "FxVarianceSwap lost Trade/FxVarianceSwapData/Underlying/Type",
        "IndexCreditDefaultSwap changed Trade/IndexCreditDefaultSwapData/LegData/ScheduleData/Rules/Convention",
        "IndexCreditDefaultSwap lost Trade/IndexCreditDefaultSwapData/LegData/ScheduleData/Rules/Calendar",
        "IndexCreditDefaultSwap lost Trade/IndexCreditDefaultSwapData/LegData/ScheduleData/Rules/Rule",
        "IndexCreditDefaultSwapOption changed Trade/IndexCreditDefaultSwapOptionData/IndexCreditDefaultSwapData/LegData/ScheduleData/Rules/Convention",
        "IndexCreditDefaultSwapOption lost Trade/IndexCreditDefaultSwapOptionData/IndexCreditDefaultSwapData/LegData/ScheduleData/Rules/Calendar",
        "IndexCreditDefaultSwapOption lost Trade/IndexCreditDefaultSwapOptionData/IndexCreditDefaultSwapData/LegData/ScheduleData/Rules/Rule",
        "IndexCreditDefaultSwapOption lost Trade/IndexCreditDefaultSwapOptionData/IndexCreditDefaultSwapData/UpfrontDate",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/StrikeIncluded",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[1]/PriceType",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[1]/Weight",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[2]/PriceType",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[2]/Weight",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[3]/Weight",
        "PerformanceOption_01 lost Trade/PerformanceOption01Data/Underlyings/Underlying[4]/Weight",
        "Swap lost Trade/Envelope/AdditionalFields",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CMSSpreadLegData/FixingDays",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CMSSpreadLegData/Index1",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CMSSpreadLegData/Index2",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CMSSpreadLegData/IsInArrears",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CMSSpreadLegData/Spreads/Spread",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CallPayoffs/Payoff",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CallPosition",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/CallStrikes/Strike",
        "Swap lost Trade/SwapData/LegData[1]/DigitalCMSSpreadLegData/IsCallATMIncluded",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/EquityLegData/FixingDays",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/EquityLegData/InitialPrice",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/EquityLegData/Name",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/EquityLegData/NotionalReset",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/EquityLegData/ReturnType",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/InitialMarginFactor",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/Multiplier",
        "Swap lost Trade/SwapData/LegData[1]/EquityMarginLegData/Rates/Rate",
        "Swap lost Trade/SwapData/LegData[1]/FloatingLegData/Spreads",
        "Swap lost Trade/SwapData/LegData[1]/ScheduleData/Rules/FirstDate",
        "Swap lost Trade/SwapData/LegData[1]/ScheduleData/Rules/LastDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[1]",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[1]/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[2]",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[2]/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[3]",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[3]/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[4]",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[4]/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[5]",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[5]/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[6]",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[6]/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[7]",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/HistoricalFixings/Fixing[7]/@fixingDate",
        "Swap lost Trade/SwapData/LegData[2]/FloatingLegData/Spreads",
        "Swap lost Trade/SwapData/LegData[2]/FormulaBasedLegData/FixingDays",
        "Swap lost Trade/SwapData/LegData[2]/FormulaBasedLegData/Index",
        "Swap lost Trade/SwapData/LegData[2]/Notionals/FXReset/FXIndex",
        "Swap lost Trade/SwapData/LegData[2]/Notionals/FXReset/ForeignAmount",
        "Swap lost Trade/SwapData/LegData[2]/Notionals/FXReset/ForeignCurrency",
        "Swap lost Trade/SwapData/LegData[2]/ScheduleData/Rules/FirstDate",
        "Swap lost Trade/SwapData/LegData[2]/ScheduleData/Rules/LastDate",
        "Swap lost Trade/SwapData/Settlement",
        "SyntheticCDO changed Trade/CdoData/LegData/ScheduleData/Rules/Convention",
        "SyntheticCDO lost Trade/CdoData/LegData/ScheduleData/Rules/Calendar",
        "SyntheticCDO lost Trade/CdoData/LegData/ScheduleData/Rules/Rule",
        "TotalReturnSwap added Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying/Name",
        "TotalReturnSwap added Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying/Type",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/Derivative[1]/SubTrade/EquityPositionData/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/Derivative[2]/SubTrade/EquityPositionData/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/Derivative[3]/SubTrade/EquityPositionData/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/Derivative[4]/SubTrade/@id",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Identifier",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Quantity",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/Name",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/Type",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[1]/Weight",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/Name",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/Type",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/BondBasketData/Underlying[2]/Weight",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[1]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[1]/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[2]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityOptionPositionData/Underlying[2]/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade/EquityPositionData/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[1]/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[1]/Name",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[1]/Type",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[1]/Weight",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[2]/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[2]/Name",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[2]/Type",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[1]/EquityPositionData/Underlying[2]/Weight",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[1]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[1]/Underlying/IdentifierType",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[2]/OptionData/ExerciseDates/ExerciseDate",
        "TotalReturnSwap lost Trade/TotalReturnSwapData/UnderlyingData/SubTrade[2]/EquityOptionPositionData/Underlying[2]/Underlying/IdentifierType",
    };
    return losses;
}

/**
 * @brief The ORE trade types no family maps, which come back as their type and
 * envelope alone.
 *
 * Asserted for equality, as the losses are.
 */
const std::set<std::string>& known_unmapped_types() {
    static const std::set<std::string> types = {};
    return types;
}

/// The last element of a path, without the position of a repeated element.
std::string_view leaf_of(std::string_view path) {
    const auto slash = path.rfind('/');
    auto leaf = slash == std::string_view::npos ? path : path.substr(slash + 1);
    const auto bracket = leaf.find('[');
    return bracket == std::string_view::npos ? leaf : leaf.substr(0, bracket);
}

/**
 * @brief The value as ORE reads it, for the spellings ORE reads as one.
 *
 * Only ORE's own equivalences, taken from its parsers (OREData
 * ored/utilities/parsers.cpp): parseBool, parseBusinessDayConvention and
 * parseDate, which reads a date of eight characters as it reads one of ten. A
 * spelling ORE reads differently, such as F and MF, stays a difference.
 */
std::string ore_equivalent(std::string_view path, const std::string& value) {
    static const std::map<std::string, std::string, std::less<>> booleans = {{"Y", "true"},
                                                                             {"YES", "true"},
                                                                             {"TRUE", "true"},
                                                                             {"True", "true"},
                                                                             {"true", "true"},
                                                                             {"N", "false"},
                                                                             {"NO", "false"},
                                                                             {"FALSE", "false"},
                                                                             {"False", "false"},
                                                                             {"false", "false"}};
    static const std::map<std::string, std::string, std::less<>> conventions = {
        {"F", "Following"},
        {"Following", "Following"},
        {"FOLLOWING", "Following"},
        {"MF", "ModifiedFollowing"},
        {"ModifiedFollowing", "ModifiedFollowing"},
        {"Modified Following", "ModifiedFollowing"},
        {"MODIFIEDF", "ModifiedFollowing"},
        {"MODFOLLOWING", "ModifiedFollowing"},
        {"P", "Preceding"},
        {"Preceding", "Preceding"},
        {"PRECEDING", "Preceding"},
        {"MP", "ModifiedPreceding"},
        {"ModifiedPreceding", "ModifiedPreceding"},
        {"Modified Preceding", "ModifiedPreceding"},
        {"MODIFIEDP", "ModifiedPreceding"},
        {"U", "Unadjusted"},
        {"Unadjusted", "Unadjusted"},
        {"INDIFF", "Unadjusted"},
        {"NONE", "Unadjusted"},
        {"HalfMonthModifiedFollowing", "HalfMonthModifiedFollowing"},
        {"HMMF", "HalfMonthModifiedFollowing"},
        {"Half Month Modified Following", "HalfMonthModifiedFollowing"},
        {"HALFMONTHMF", "HalfMonthModifiedFollowing"},
        {"HalfMonthMF", "HalfMonthModifiedFollowing"},
        {"NEAREST", "Nearest"},
    };

    if (const auto found = booleans.find(value); found != booleans.end())
        return found->second;
    const auto leaf = leaf_of(path);
    if (leaf.ends_with("Convention")) {
        if (const auto found = conventions.find(value); found != conventions.end())
            return found->second;
    }
    if (leaf.find("Date") != std::string_view::npos && value.size() == 8 &&
        std::ranges::all_of(value, [](unsigned char c) { return std::isdigit(c) != 0; }))
        return value.substr(0, 4) + "-" + value.substr(4, 2) + "-" + value.substr(6, 2);
    return value;
}

/// A fingerprint with every value as ORE reads it, sorted again.
std::vector<value_entry> as_ore_reads(std::vector<value_entry> values) {
    for (auto& entry : values)
        entry.value = ore_equivalent(entry.path, entry.value);
    std::ranges::sort(values, [](const value_entry& lhs, const value_entry& rhs) {
        return lhs.path != rhs.path ? lhs.path < rhs.path : lhs.value < rhs.value;
    });
    return values;
}

/// The trade type a trade's fingerprint states.
std::string trade_type_of(const keyed_fingerprint& trade) {
    for (const auto& value : trade.values)
        if (value.path == "Trade/TradeType")
            return value.value;
    return {};
}

std::string_view name_of(value_change change) {
    switch (change) {
        case value_change::lost:
            return "lost";
        case value_change::added:
            return "added";
        case value_change::changed:
            break;
    }
    return "changed";
}

/// One line per difference, which the known losses name.
std::string loss_of(const std::string& trade_type, const value_difference& difference) {
    return trade_type + " " + std::string(name_of(difference.change)) + " " + difference.path;
}

/// Lists a set, one entry a line, for a failure message.
std::string listed(const std::set<std::string>& entries) {
    std::string out;
    for (const auto& entry : entries)
        out += "\n  " + entry;
    return out;
}

}

TEST_CASE("every trade in the ORE example corpus comes back as it was read",
          "[ore][xml][roundtrip][external]") {
    auto lg(make_logger(test_suite));
    using ores::platform::filesystem::file;

    const auto input =
        ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades");
    REQUIRE(fs::exists(input));

    std::set<std::string> losses;
    std::set<std::string> unmapped;
    // An example of each loss, so a failure shows the values and the file.
    std::map<std::string, std::string> example_of;
    std::size_t portfolios = 0;
    std::size_t trades = 0;

    for (const auto& entry : fs::recursive_directory_iterator(input)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (detect_document_kind(entry.path()) != document_kind::portfolio)
            continue;
        ++portfolios;

        const auto relative = fs::relative(entry.path(), input).generic_string();
        const auto text = file::read_content(entry.path());
        const auto written = roundtrip_portfolio(text, relative);
        for (const auto& type : written.passthrough_types)
            unmapped.insert(type);

        const auto before = document::read(text);
        const auto after = document::read(written.xml);
        REQUIRE(before);
        REQUIRE(after);

        std::map<std::string, std::vector<value_entry>> written_trades;
        for (auto& trade : value_fingerprints_of(*after, "Trade", "id"))
            written_trades[trade.key] = as_ore_reads(std::move(trade.values));

        for (const auto& trade : value_fingerprints_of(*before, "Trade", "id")) {
            ++trades;
            const auto type = trade_type_of(trade);
            const auto found = written_trades.find(trade.key);
            const auto& other =
                found == written_trades.end() ? std::vector<value_entry>{} : found->second;
            for (const auto& difference : diff_values(as_ore_reads(trade.values), other)) {
                const auto loss = loss_of(type, difference);
                losses.insert(loss);
                example_of.try_emplace(
                    loss, relative + " trade " + trade.key + ": " + describe(difference));
            }
        }
    }

    INFO("portfolios=" << portfolios << " trades=" << trades << " differences=" << losses.size()
                       << " known=" << known_losses().size());
    CHECK(portfolios > 0);

    if (losses != known_losses()) {
        std::set<std::string> new_losses;
        std::set<std::string> fixed;
        for (const auto& loss : losses)
            if (!known_losses().contains(loss))
                new_losses.insert(loss);
        for (const auto& loss : known_losses())
            if (!losses.contains(loss))
                fixed.insert(loss);

        std::string detail = "the trades the model writes back differ from what it read.";
        if (!new_losses.empty()) {
            detail += "\nNew differences, with an example of each:";
            for (const auto& loss : new_losses)
                detail += "\n  " + loss + "\n      " + example_of[loss];
        }
        if (!fixed.empty())
            detail += "\nNo longer happening, so remove from known_losses():" + listed(fixed);
        FAIL_CHECK(detail);
    }

    if (unmapped != known_unmapped_types()) {
        std::string detail =
            "the trade types no family maps changed. Mapped none of:" + listed(unmapped) +
            "\nKnown as unmapped:" + listed(known_unmapped_types());
        FAIL_CHECK(detail);
    }
}
