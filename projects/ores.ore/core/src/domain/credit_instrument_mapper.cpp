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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore.core/domain/credit_instrument_mapper.hpp"
#include "ores.ore.core/domain/ore_boundary_decimal.hpp"
#include "ores.ore.core/domain/ore_code_tables.hpp"
#include "ores.ore.core/domain/swap_instrument_mapper.hpp"
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
#include "ores.platform/time/datetime.hpp"
#include <algorithm>
#include <chrono>
#include <optional>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::credit_basket_constituent;
using ores::trading::domain::credit_instrument;
using ores::trading::domain::credit_instrument_data;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

// The trading domain holds a date as std::chrono::year_month_day; the ORE XML
// holds its ISO-8601 spelling. An absent ORE date maps to a default
// (invalid) calendar date, which renders back to an empty string.
std::chrono::year_month_day to_domain_date(const std::string& s) {
    if (s.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(s);
}

std::string to_ore_date(const std::chrono::year_month_day& d) {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

std::string to_ore_date(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

credit_instrument make_base(const std::string& trade_type_code) {
    credit_instrument r;
    r.identity.trade_type_code = trade_type_code;
    r.audit.modified_by = "ores";
    r.audit.performed_by = "ores";
    r.audit.change_reason_code = "system.external_data_import";
    r.audit.change_commentary = "Imported from ORE XML";
    return r;
}

std::optional<std::chrono::year_month_day> first_exercise_date(const optionData& opt) {
    if (!opt.exerciseDatesGroup)
        return std::nullopt;
    if (!opt.exerciseDatesGroup->ExerciseDates)
        return std::nullopt;
    if (opt.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(
        std::string(opt.exerciseDatesGroup->ExerciseDates->ExerciseDate.front()));
}

} // namespace

// ---------------------------------------------------------------------------
// Common: legData → credit_instrument economic fields
// ---------------------------------------------------------------------------

void credit_instrument_mapper::map_cds_leg(const legData& ld, credit_instrument& instr) {
    if (ld.Currency)
        instr.currency = std::string(*ld.Currency);
    if (ld.Notionals && !ld.Notionals->Notional.empty())
        instr.notional = exact_decimal(ld.Notionals->Notional.front());
    if (ld.DayCounter)
        instr.day_count_fraction_code = to_string(*ld.DayCounter);
    if (ld.legDataType && ld.legDataType->FixedLegData &&
        !ld.legDataType->FixedLegData->Rates.Rate.empty())
        instr.spread = exact_decimal(ld.legDataType->FixedLegData->Rates.Rate.front());
    if (!ld.ScheduleData || ld.ScheduleData->Rules.empty())
        return;
    const auto& rule = ld.ScheduleData->Rules.front();
    instr.start_date = to_domain_date(std::string(rule.StartDate));
    if (rule.EndDate)
        instr.maturity_date = to_domain_date(std::string(*rule.EndDate));
    instr.payment_frequency_code = tenor_to_payment_frequency(std::string(rule.Tenor));
}

// ---------------------------------------------------------------------------
// Common: credit_instrument → legData
// ---------------------------------------------------------------------------

legData credit_instrument_mapper::reverse_cds_leg(const credit_instrument& instr) {
    legData ld;
    ld.LegType = legType::Fixed;
    ld.Payer = true;
    if (!instr.currency.empty())
        ld.Currency = instr.currency;
    if (!instr.notional.is_zero()) {
        legData_Notionals_t n;
        legData_Notionals_t_Notional_t nv;
        static_cast<float&>(nv) = static_cast<float>(instr.notional.to_double());
        n.Notional.push_back(nv);
        ld.Notionals = std::move(n);
    }
    if (instr.spread != ores::utility::decimal::decimal{}) {
        _FixedLegData_t fld;
        _FixedLegData_t_Rates_t_Rate_t rate;
        static_cast<float&>(rate) = static_cast<float>(instr.spread.to_double());
        fld.Rates.Rate.push_back(rate);
        legDataType_group_t ldt;
        ldt.FixedLegData = std::move(fld);
        ld.legDataType = std::move(ldt);
    }
    if (instr.start_date.ok() || instr.maturity_date.ok()) {
        scheduleData_Rules_t rule;
        if (instr.start_date.ok())
            rule.StartDate = to_ore_date(instr.start_date);
        if (instr.maturity_date.ok()) {
            date ed;
            static_cast<std::string&>(ed) = to_ore_date(instr.maturity_date);
            rule.EndDate = xsd::optional<date>(ed);
        }
        if (!instr.payment_frequency_code.empty())
            static_cast<std::string&>(rule.Tenor) =
                payment_frequency_to_tenor(instr.payment_frequency_code);
        scheduleData sched;
        sched.Rules.push_back(std::move(rule));
        ld.ScheduleData = std::move(sched);
    }
    return ld;
}

// ---------------------------------------------------------------------------
// Forward: CreditDefaultSwap
// ---------------------------------------------------------------------------

trading::domain::credit_instrument credit_instrument_mapper::instrument_cds(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CreditDefaultSwap: " << std::string(t.id);
    trading::domain::credit_instrument result = make_base("CreditDefaultSwap");
    if (!t.CreditDefaultSwapData)
        return result;
    const auto& d = *t.CreditDefaultSwapData;
    if (d.IssuerId)
        result.reference_entity = std::string(*d.IssuerId);
    else if (d.creditCurveIdType.CreditCurveId)
        result.reference_entity = std::string(*d.creditCurveIdType.CreditCurveId);
    if (d.FixedRecoveryRate)
        result.recovery_rate = static_cast<double>(*d.FixedRecoveryRate);
    map_cds_leg(d.LegData, result);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: IndexCreditDefaultSwap
// ---------------------------------------------------------------------------

trading::domain::credit_instrument credit_instrument_mapper::instrument_index_cds(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping IndexCreditDefaultSwap: " << std::string(t.id);
    trading::domain::credit_instrument result = make_base("IndexCreditDefaultSwap");
    if (!t.IndexCreditDefaultSwapData)
        return result;
    const auto& d = *t.IndexCreditDefaultSwapData;
    result.reference_entity = std::string(d.CreditCurveId);
    result.index_name = std::string(d.CreditCurveId);
    map_cds_leg(d.LegData, result);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: IndexCreditDefaultSwapOption
// ---------------------------------------------------------------------------

trading::domain::credit_instrument
credit_instrument_mapper::instrument_index_cds_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping IndexCreditDefaultSwapOption: "
                               << std::string(t.id);
    trading::domain::credit_instrument result = make_base("IndexCreditDefaultSwapOption");
    if (!t.IndexCreditDefaultSwapOptionData)
        return result;
    const auto& d = *t.IndexCreditDefaultSwapOptionData;
    result.reference_entity = std::string(d.IndexCreditDefaultSwapData.CreditCurveId);
    result.index_name = std::string(d.IndexCreditDefaultSwapData.CreditCurveId);
    if (d.Strike)
        result.option_strike = exact_decimal(*d.Strike);
    result.option_expiry_date = first_exercise_date(d.OptionData);
    map_cds_leg(d.IndexCreditDefaultSwapData.LegData, result);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: CreditLinkedSwap
// ---------------------------------------------------------------------------

trading::domain::credit_instrument
credit_instrument_mapper::instrument_credit_linked_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CreditLinkedSwap: " << std::string(t.id);
    trading::domain::credit_instrument result = make_base("CreditLinkedSwap");
    if (!t.CreditLinkedSwapData)
        return result;
    const auto& d = *t.CreditLinkedSwapData;
    result.reference_entity = std::string(d.CreditCurveId);
    result.linked_asset_code = std::string(d.CreditCurveId);
    if (d.FixedRecoveryRate)
        result.recovery_rate = static_cast<double>(*d.FixedRecoveryRate);
    // Extract currency/notional from first contingent payment leg if available.
    if (d.ContingentPayments && !d.ContingentPayments->LegData.empty())
        map_cds_leg(d.ContingentPayments->LegData.front(), result);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: SyntheticCDO
// ---------------------------------------------------------------------------

trading::domain::credit_instrument credit_instrument_mapper::instrument_synthetic_cdo(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping SyntheticCDO: " << std::string(t.id);
    trading::domain::credit_instrument result = make_base("SyntheticCDO");
    if (!t.CdoData)
        return result;
    const auto& d = *t.CdoData;
    result.reference_entity = std::string(d.Qualifier);
    result.start_date = to_domain_date(std::string(d.ProtectionStart));
    result.tranche_attachment = static_cast<double>(d.AttachmentPoint);
    result.tranche_detachment = static_cast<double>(d.DetachmentPoint);
    if (d.FixedRecoveryRate)
        result.recovery_rate = static_cast<double>(*d.FixedRecoveryRate);
    map_cds_leg(d.LegData, result);
    return result;
}

// ---------------------------------------------------------------------------
// Forward: RiskParticipationAgreement
// ---------------------------------------------------------------------------

trading::domain::credit_instrument credit_instrument_mapper::instrument_rpa(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping RiskParticipationAgreement: "
                               << std::string(t.id);
    trading::domain::credit_instrument result = make_base("RiskParticipationAgreement");
    if (!t.RiskParticipationAgreementData)
        return result;
    const auto& d = *t.RiskParticipationAgreementData;
    result.reference_entity = std::string(d.CreditCurveId);
    if (d.IssuerId)
        result.reference_entity = std::string(*d.IssuerId);
    result.start_date = to_domain_date(std::string(d.ProtectionStart));
    result.maturity_date = to_domain_date(std::string(d.ProtectionEnd));
    if (d.FixedRecoveryRate)
        result.recovery_rate = static_cast<double>(*d.FixedRecoveryRate);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: CreditDefaultSwap
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::trade_cds(const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CreditDefaultSwap";
    trade t;
    t.TradeType = oreTradeType::CreditDefaultSwap;
    creditDefaultSwapData d;
    if (!instr.reference_entity.empty()) {
        _CreditCurveId_t cid;
        static_cast<std::string&>(cid) = instr.reference_entity;
        d.creditCurveIdType.CreditCurveId = std::move(cid);
    }
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    d.LegData = reverse_cds_leg(instr);
    t.CreditDefaultSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: IndexCreditDefaultSwap
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::trade_index_cds(const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping IndexCreditDefaultSwap";
    trade t;
    t.TradeType = oreTradeType::IndexCreditDefaultSwap;
    indexCreditDefaultSwapData d;
    static_cast<std::string&>(d.CreditCurveId) = instr.reference_entity;
    d.LegData = reverse_cds_leg(instr);
    t.IndexCreditDefaultSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: IndexCreditDefaultSwapOption
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::trade_index_cds_option(const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping IndexCreditDefaultSwapOption";
    trade t;
    t.TradeType = oreTradeType::IndexCreditDefaultSwapOption;
    indexCreditDefaultSwapOptionData d;
    static_cast<std::string&>(d.IndexCreditDefaultSwapData.CreditCurveId) = instr.reference_entity;
    d.IndexCreditDefaultSwapData.LegData = reverse_cds_leg(instr);
    if (instr.option_strike) {
        moneyFloat strike;
        set_exact(strike, *instr.option_strike);
        d.Strike = std::move(strike);
    }
    if (instr.option_expiry_date) {
        _ExerciseDates_t exd;
        date ed;
        static_cast<std::string&>(ed) = to_ore_date(instr.option_expiry_date);
        exd.ExerciseDate.push_back(ed);
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        d.OptionData.exerciseDatesGroup = std::move(eg);
    }
    t.IndexCreditDefaultSwapOptionData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: CreditLinkedSwap
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::trade_credit_linked_swap(const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CreditLinkedSwap";
    trade t;
    t.TradeType = oreTradeType::CreditLinkedSwap;
    creditLinkedSwapData d;
    static_cast<std::string&>(d.CreditCurveId) = instr.reference_entity;
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    if (!instr.currency.empty() || !instr.notional.is_zero()) {
        creditLinkedSwapData_ContingentPayments_t cp;
        cp.LegData.push_back(reverse_cds_leg(instr));
        d.ContingentPayments = std::move(cp);
    }
    t.CreditLinkedSwapData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: SyntheticCDO
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::trade_synthetic_cdo(const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping SyntheticCDO";
    trade t;
    t.TradeType = oreTradeType::SyntheticCDO;
    cdoData d;
    static_cast<std::string&>(d.Qualifier) = instr.reference_entity;
    if (instr.start_date.ok())
        static_cast<std::string&>(d.ProtectionStart) = to_ore_date(instr.start_date);
    if (instr.tranche_attachment)
        d.AttachmentPoint = static_cast<float>(*instr.tranche_attachment);
    if (instr.tranche_detachment)
        d.DetachmentPoint = static_cast<float>(*instr.tranche_detachment);
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    d.LegData = reverse_cds_leg(instr);
    t.CdoData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// Reverse: RiskParticipationAgreement
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::trade_rpa(const credit_instrument& instr) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping RiskParticipationAgreement";
    trade t;
    t.TradeType = oreTradeType::RiskParticipationAgreement;
    rpaData d;
    static_cast<std::string&>(d.CreditCurveId) = instr.reference_entity;
    if (instr.start_date.ok())
        static_cast<std::string&>(d.ProtectionStart) = to_ore_date(instr.start_date);
    if (instr.maturity_date.ok())
        static_cast<std::string&>(d.ProtectionEnd) = to_ore_date(instr.maturity_date);
    if (instr.recovery_rate != 0.0)
        d.FixedRecoveryRate = static_cast<float>(instr.recovery_rate);
    t.RiskParticipationAgreementData = std::move(d);
    return t;
}

// ---------------------------------------------------------------------------
// The carrier: the legs and the basket beside the instrument
// ---------------------------------------------------------------------------

void credit_instrument_mapper::append_leg(credit_instrument_data& result,
                                          const legData& ld,
                                          const std::string& group) {
    const int leg_number = static_cast<int>(result.legs.size()) + 1;
    auto leg = swap_instrument_mapper::map_leg(ld, leg_number);
    leg.leg_group = group;
    result.legs.push_back(std::move(leg));
    for (auto& amount : swap_instrument_mapper::map_leg_amounts(ld, leg_number))
        result.leg_amounts.push_back(std::move(amount));
    for (auto& rate : swap_instrument_mapper::map_leg_rates(ld, leg_number))
        result.leg_rates.push_back(std::move(rate));
}

void credit_instrument_mapper::append_basket(credit_instrument_data& result,
                                             const basketData& basket) {
    int sequence = 0;
    for (const auto& name : basket.Name) {
        credit_basket_constituent row;
        row.sequence_number = ++sequence;
        row.issuer_id = std::string(name.IssuerId);
        if (name.Qualifier)
            row.qualifier = std::string(*name.Qualifier);
        if (name.creditCurveIdType.CreditCurveId)
            row.credit_curve_id = std::string(*name.creditCurveIdType.CreditCurveId);
        if (name.Notional)
            row.notional = exact_decimal(*name.Notional);
        if (name.Weight)
            row.weight = static_cast<double>(*name.Weight);
        if (name.Currency)
            row.currency = to_string(*name.Currency);
        row.modified_by = "ores";
        row.performed_by = "ores";
        row.change_reason_code = "system.external_data_import";
        row.change_commentary = "Imported from ORE XML";
        result.constituents.push_back(std::move(row));
    }
}

std::vector<legData> credit_instrument_mapper::reverse_group(const credit_instrument_data& data,
                                                             const std::string& group) {
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<std::chrono::year_month_day> maturity_date;
    if (data.instrument.start_date.ok())
        start_date = data.instrument.start_date;
    if (data.instrument.maturity_date.ok())
        maturity_date = data.instrument.maturity_date;

    std::vector<ores::trading::domain::swap_leg> legs;
    for (const auto& leg : data.legs) {
        if (leg.leg_group && *leg.leg_group == group)
            legs.push_back(leg);
    }
    std::stable_sort(legs.begin(), legs.end(), [](const auto& a, const auto& b) {
        return a.identity.leg_number < b.identity.leg_number;
    });

    std::vector<legData> out;
    for (const auto& leg : legs) {
        std::vector<ores::trading::domain::swap_leg_amount> amounts;
        for (const auto& amount : data.leg_amounts) {
            if (amount.leg_number == leg.identity.leg_number)
                amounts.push_back(amount);
        }
        std::vector<ores::trading::domain::swap_leg_rate> rates;
        for (const auto& rate : data.leg_rates) {
            if (rate.leg_number == leg.identity.leg_number)
                rates.push_back(rate);
        }
        out.push_back(
            swap_instrument_mapper::reverse_leg(start_date, maturity_date, leg, amounts, rates));
    }
    return out;
}

basketData credit_instrument_mapper::reverse_basket(
    const std::vector<ores::trading::domain::credit_basket_constituent>& names) {
    auto ordered = names;
    std::stable_sort(ordered.begin(), ordered.end(), [](const auto& a, const auto& b) {
        return a.sequence_number < b.sequence_number;
    });

    basketData basket;
    for (const auto& row : ordered) {
        nameData n;
        static_cast<std::string&>(n.IssuerId) = row.issuer_id;
        if (row.qualifier) {
            nameData_Qualifier_t q;
            static_cast<std::string&>(q) = *row.qualifier;
            n.Qualifier = std::move(q);
        }
        if (row.credit_curve_id) {
            _CreditCurveId_t cid;
            static_cast<std::string&>(cid) = *row.credit_curve_id;
            n.creditCurveIdType.CreditCurveId = std::move(cid);
        }
        if (row.notional) {
            moneyFloat notional;
            set_exact(notional, *row.notional);
            n.Notional = std::move(notional);
        }
        if (row.weight)
            n.Weight = static_cast<float>(*row.weight);
        if (row.currency)
            n.Currency = parse_currency_code(*row.currency);
        basket.Name.push_back(std::move(n));
    }
    return basket;
}

// ---------------------------------------------------------------------------
// Forward: the carrier
// ---------------------------------------------------------------------------

credit_instrument_data credit_instrument_mapper::forward_cds(const trade& t) {
    credit_instrument_data result;
    result.instrument = instrument_cds(t);
    if (t.CreditDefaultSwapData)
        append_leg(result, t.CreditDefaultSwapData->LegData, "premium");
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_index_cds(const trade& t) {
    credit_instrument_data result;
    result.instrument = instrument_index_cds(t);
    if (t.IndexCreditDefaultSwapData) {
        const auto& d = *t.IndexCreditDefaultSwapData;
        append_leg(result, d.LegData, "premium");
        if (d.BasketData)
            append_basket(result, *d.BasketData);
    }
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_index_cds_option(const trade& t) {
    credit_instrument_data result;
    result.instrument = instrument_index_cds_option(t);
    if (t.IndexCreditDefaultSwapOptionData) {
        const auto& d = t.IndexCreditDefaultSwapOptionData->IndexCreditDefaultSwapData;
        append_leg(result, d.LegData, "premium");
        if (d.BasketData)
            append_basket(result, *d.BasketData);
    }
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_credit_linked_swap(const trade& t) {
    credit_instrument_data result;
    result.instrument = instrument_credit_linked_swap(t);
    if (!t.CreditLinkedSwapData)
        return result;
    const auto& d = *t.CreditLinkedSwapData;
    const auto append_group = [&](const auto& payments, const std::string& group) {
        if (!payments)
            return;
        for (const auto& ld : payments->LegData)
            append_leg(result, ld, group);
    };
    append_group(d.IndependentPayments, "independent");
    append_group(d.ContingentPayments, "contingent");
    append_group(d.DefaultPayments, "default");
    append_group(d.RecoveryPayments, "recovery");
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_synthetic_cdo(const trade& t) {
    credit_instrument_data result;
    result.instrument = instrument_synthetic_cdo(t);
    if (t.CdoData) {
        const auto& d = *t.CdoData;
        append_leg(result, d.LegData, "premium");
        if (d.BasketData)
            append_basket(result, *d.BasketData);
    }
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_rpa(const trade& t) {
    credit_instrument_data result;
    result.instrument = instrument_rpa(t);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: the carrier
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_cds(const credit_instrument_data& data) {
    auto t = trade_cds(data.instrument);
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        t.CreditDefaultSwapData->LegData = legs.front();
    return t;
}

trade credit_instrument_mapper::reverse_index_cds(const credit_instrument_data& data) {
    auto t = trade_index_cds(data.instrument);
    auto& d = *t.IndexCreditDefaultSwapData;
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.LegData = legs.front();
    if (!data.constituents.empty())
        d.BasketData = reverse_basket(data.constituents);
    return t;
}

trade credit_instrument_mapper::reverse_index_cds_option(const credit_instrument_data& data) {
    auto t = trade_index_cds_option(data.instrument);
    auto& d = t.IndexCreditDefaultSwapOptionData->IndexCreditDefaultSwapData;
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.LegData = legs.front();
    if (!data.constituents.empty())
        d.BasketData = reverse_basket(data.constituents);
    return t;
}

trade credit_instrument_mapper::reverse_credit_linked_swap(const credit_instrument_data& data) {
    auto t = trade_credit_linked_swap(data.instrument);
    auto& d = *t.CreditLinkedSwapData;
    const auto fill_group = [&](auto& slot, const std::string& group) {
        auto legs = reverse_group(data, group);
        if (legs.empty())
            return;
        std::remove_cvref_t<decltype(*slot)> payments;
        for (auto& leg : legs)
            payments.LegData.push_back(std::move(leg));
        slot = std::move(payments);
    };
    fill_group(d.IndependentPayments, "independent");
    fill_group(d.ContingentPayments, "contingent");
    fill_group(d.DefaultPayments, "default");
    fill_group(d.RecoveryPayments, "recovery");
    return t;
}

trade credit_instrument_mapper::reverse_synthetic_cdo(const credit_instrument_data& data) {
    auto t = trade_synthetic_cdo(data.instrument);
    auto& d = *t.CdoData;
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.LegData = legs.front();
    if (!data.constituents.empty())
        d.BasketData = reverse_basket(data.constituents);
    return t;
}

trade credit_instrument_mapper::reverse_rpa(const credit_instrument_data& data) {
    return trade_rpa(data.instrument);
}

}
