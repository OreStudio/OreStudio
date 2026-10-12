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
#include "ores.trading.api/domain/credit_fact_variant.hpp"
#include <algorithm>
#include <chrono>
#include <optional>

namespace ores::ore::domain {

using namespace ores::logging;
using ores::trading::domain::credit_basket_constituent;
using ores::trading::domain::credit_default_swap_instrument;
using ores::trading::domain::credit_instrument;
using ores::trading::domain::credit_instrument_data;
using ores::trading::domain::credit_linked_swap_instrument;
using ores::trading::domain::index_credit_default_swap_instrument;
using ores::trading::domain::index_credit_default_swap_option_instrument;
using ores::trading::domain::instrument_schedule;
using ores::trading::domain::instrument_schedule_date;
using ores::trading::domain::instrument_strike;
using ores::trading::domain::risk_participation_agreement_instrument;
using ores::trading::domain::synthetic_cdo_instrument;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

namespace {

// The trading domain holds a date as std::chrono::year_month_day; the ORE XML
// holds its ISO-8601 spelling.
std::chrono::year_month_day to_domain_date(const std::string& s) {
    if (s.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(s);
}

std::optional<std::chrono::year_month_day> to_optional_date(const std::string& s) {
    if (s.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(s);
}

std::string to_ore_date(const std::chrono::year_month_day& d) {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

std::string to_ore_date(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

template <typename Row>
void stamp_audit(Row& row) {
    row.modified_by = "ores";
    row.performed_by = "ores";
    row.change_reason_code = "system.external_data_import";
    row.change_commentary = "Imported from ORE XML";
}

credit_instrument make_header(const std::string& trade_type_code) {
    credit_instrument r;
    r.identity.trade_type_code = trade_type_code;
    r.audit.modified_by = "ores";
    r.audit.performed_by = "ores";
    r.audit.change_reason_code = "system.external_data_import";
    r.audit.change_commentary = "Imported from ORE XML";
    return r;
}

// The header's common dates are the first schedule rule of the leg: its start
// and its end.
void map_leg_dates(const legData& ld, credit_instrument& header) {
    if (!ld.ScheduleData || ld.ScheduleData->Rules.empty())
        return;
    const auto& rule = ld.ScheduleData->Rules.front();
    header.start_date = to_optional_date(std::string(rule.StartDate));
    if (rule.EndDate)
        header.maturity_date = to_optional_date(std::string(*rule.EndDate));
}

template <typename Fact>
Fact make_fact() {
    Fact f;
    stamp_audit(f);
    return f;
}

std::optional<double> to_double(const xsd::optional<float>& v) {
    if (!v)
        return std::nullopt;
    return static_cast<double>(*v);
}

template <typename Fact>
const Fact& fact_of(const credit_instrument_data& data) {
    static const Fact empty{};
    const auto* f = std::get_if<Fact>(&data.facts);
    return f ? *f : empty;
}

// The index option states its strike as one shared strike row and its exercise
// dates as shared schedule rows under the option owner.
void append_option(credit_instrument_data& result,
                   const indexCreditDefaultSwapOptionData& d) {
    if (d.Strike) {
        instrument_strike row;
        row.bare_value = exact_decimal(*d.Strike);
        stamp_audit(row);
        result.strikes.push_back(std::move(row));
    }
    const auto& group = d.OptionData.exerciseDatesGroup;
    if (!group || !group->ExerciseDates || group->ExerciseDates->ExerciseDate.empty())
        return;

    instrument_schedule schedule;
    schedule.owner_role = "option";
    schedule.owner_number = 1;
    schedule.schedule_role = "exercise_dates";
    schedule.sequence_number = 1;
    schedule.schedule_kind = "dates";
    stamp_audit(schedule);
    result.schedules.push_back(std::move(schedule));

    int date_number = 0;
    for (const auto& date : group->ExerciseDates->ExerciseDate) {
        instrument_schedule_date row;
        row.owner_role = "option";
        row.owner_number = 1;
        row.schedule_role = "exercise_dates";
        row.schedule_sequence_number = 1;
        row.sequence_number = ++date_number;
        row.schedule_date = to_domain_date(std::string(date));
        stamp_audit(row);
        result.schedule_dates.push_back(std::move(row));
    }
}

std::vector<std::chrono::year_month_day>
exercise_dates_of(const std::vector<instrument_schedule_date>& rows) {
    std::vector<const instrument_schedule_date*> picked;
    for (const auto& row : rows) {
        if (row.owner_role == "option" && row.schedule_role == "exercise_dates")
            picked.push_back(&row);
    }
    std::stable_sort(picked.begin(), picked.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });
    std::vector<std::chrono::year_month_day> out;
    for (const auto* row : picked)
        out.push_back(row->schedule_date);
    return out;
}

} // namespace

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
    start_date = data.instrument.start_date;
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
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CreditDefaultSwap: " << std::string(t.id);
    credit_instrument_data result;
    result.instrument = make_header("CreditDefaultSwap");
    auto fact = make_fact<credit_default_swap_instrument>();
    if (t.CreditDefaultSwapData) {
        const auto& d = *t.CreditDefaultSwapData;
        if (d.creditCurveIdType.CreditCurveId)
            fact.credit_curve_id = std::string(*d.creditCurveIdType.CreditCurveId);
        if (d.IssuerId)
            fact.issuer_id = std::string(*d.IssuerId);
        fact.recovery_rate = to_double(d.FixedRecoveryRate);
        result.instrument.reference_entity =
            fact.issuer_id ? *fact.issuer_id : fact.credit_curve_id.value_or("");
        map_leg_dates(d.LegData, result.instrument);
        append_leg(result, d.LegData, "premium");
    }
    result.facts = std::move(fact);
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_index_cds(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping IndexCreditDefaultSwap: " << std::string(t.id);
    credit_instrument_data result;
    result.instrument = make_header("IndexCreditDefaultSwap");
    auto fact = make_fact<index_credit_default_swap_instrument>();
    if (t.IndexCreditDefaultSwapData) {
        const auto& d = *t.IndexCreditDefaultSwapData;
        fact.credit_curve_id = std::string(d.CreditCurveId);
        result.instrument.reference_entity = fact.credit_curve_id;
        map_leg_dates(d.LegData, result.instrument);
        append_leg(result, d.LegData, "premium");
        if (d.BasketData)
            append_basket(result, *d.BasketData);
    }
    result.facts = std::move(fact);
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_index_cds_option(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping IndexCreditDefaultSwapOption: "
                               << std::string(t.id);
    credit_instrument_data result;
    result.instrument = make_header("IndexCreditDefaultSwapOption");
    auto fact = make_fact<index_credit_default_swap_option_instrument>();
    if (t.IndexCreditDefaultSwapOptionData) {
        const auto& od = *t.IndexCreditDefaultSwapOptionData;
        const auto& d = od.IndexCreditDefaultSwapData;
        fact.credit_curve_id = std::string(d.CreditCurveId);
        result.instrument.reference_entity = fact.credit_curve_id;
        map_leg_dates(d.LegData, result.instrument);
        append_leg(result, d.LegData, "premium");
        if (d.BasketData)
            append_basket(result, *d.BasketData);
        append_option(result, od);
    }
    result.facts = std::move(fact);
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_credit_linked_swap(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CreditLinkedSwap: " << std::string(t.id);
    credit_instrument_data result;
    result.instrument = make_header("CreditLinkedSwap");
    auto fact = make_fact<credit_linked_swap_instrument>();
    if (t.CreditLinkedSwapData) {
        const auto& d = *t.CreditLinkedSwapData;
        fact.credit_curve_id = std::string(d.CreditCurveId);
        fact.recovery_rate = to_double(d.FixedRecoveryRate);
        result.instrument.reference_entity = fact.credit_curve_id;
        // The header's dates are those of the first contingent payment leg.
        if (d.ContingentPayments && !d.ContingentPayments->LegData.empty())
            map_leg_dates(d.ContingentPayments->LegData.front(), result.instrument);
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
    }
    result.facts = std::move(fact);
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_synthetic_cdo(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping SyntheticCDO: " << std::string(t.id);
    credit_instrument_data result;
    result.instrument = make_header("SyntheticCDO");
    auto fact = make_fact<synthetic_cdo_instrument>();
    if (t.CdoData) {
        const auto& d = *t.CdoData;
        fact.qualifier = std::string(d.Qualifier);
        fact.protection_start_date = to_domain_date(std::string(d.ProtectionStart));
        fact.attachment_point = static_cast<double>(d.AttachmentPoint);
        fact.detachment_point = static_cast<double>(d.DetachmentPoint);
        fact.recovery_rate = to_double(d.FixedRecoveryRate);
        result.instrument.reference_entity = fact.qualifier;
        result.instrument.start_date = to_optional_date(std::string(d.ProtectionStart));
        map_leg_dates(d.LegData, result.instrument);
        append_leg(result, d.LegData, "premium");
        if (d.BasketData)
            append_basket(result, *d.BasketData);
    }
    result.facts = std::move(fact);
    return result;
}

credit_instrument_data credit_instrument_mapper::forward_rpa(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping RiskParticipationAgreement: "
                               << std::string(t.id);
    credit_instrument_data result;
    result.instrument = make_header("RiskParticipationAgreement");
    auto fact = make_fact<risk_participation_agreement_instrument>();
    if (t.RiskParticipationAgreementData) {
        const auto& d = *t.RiskParticipationAgreementData;
        fact.credit_curve_id = std::string(d.CreditCurveId);
        if (d.IssuerId)
            fact.issuer_id = std::string(*d.IssuerId);
        fact.recovery_rate = to_double(d.FixedRecoveryRate);
        result.instrument.reference_entity =
            fact.issuer_id ? *fact.issuer_id : fact.credit_curve_id;
        result.instrument.start_date = to_optional_date(std::string(d.ProtectionStart));
        result.instrument.maturity_date = to_optional_date(std::string(d.ProtectionEnd));
    }
    result.facts = std::move(fact);
    return result;
}

// ---------------------------------------------------------------------------
// Reverse: the carrier
// ---------------------------------------------------------------------------

trade credit_instrument_mapper::reverse_cds(const credit_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CreditDefaultSwap";
    const auto& fact = fact_of<credit_default_swap_instrument>(data);
    trade t;
    t.TradeType = oreTradeType::CreditDefaultSwap;
    creditDefaultSwapData d;
    if (fact.credit_curve_id) {
        _CreditCurveId_t cid;
        static_cast<std::string&>(cid) = *fact.credit_curve_id;
        d.creditCurveIdType.CreditCurveId = std::move(cid);
    }
    if (fact.issuer_id) {
        creditDefaultSwapData_IssuerId_t iid;
        static_cast<std::string&>(iid) = *fact.issuer_id;
        d.IssuerId = std::move(iid);
    }
    if (fact.recovery_rate)
        d.FixedRecoveryRate = static_cast<float>(*fact.recovery_rate);
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.LegData = legs.front();
    t.CreditDefaultSwapData = std::move(d);
    return t;
}

trade credit_instrument_mapper::reverse_index_cds(const credit_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping IndexCreditDefaultSwap";
    const auto& fact = fact_of<index_credit_default_swap_instrument>(data);
    trade t;
    t.TradeType = oreTradeType::IndexCreditDefaultSwap;
    indexCreditDefaultSwapData d;
    static_cast<std::string&>(d.CreditCurveId) = fact.credit_curve_id;
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.LegData = legs.front();
    if (!data.constituents.empty())
        d.BasketData = reverse_basket(data.constituents);
    t.IndexCreditDefaultSwapData = std::move(d);
    return t;
}

trade credit_instrument_mapper::reverse_index_cds_option(const credit_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping IndexCreditDefaultSwapOption";
    const auto& fact = fact_of<index_credit_default_swap_option_instrument>(data);
    trade t;
    t.TradeType = oreTradeType::IndexCreditDefaultSwapOption;
    indexCreditDefaultSwapOptionData d;
    static_cast<std::string&>(d.IndexCreditDefaultSwapData.CreditCurveId) = fact.credit_curve_id;
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.IndexCreditDefaultSwapData.LegData = legs.front();
    if (!data.constituents.empty())
        d.IndexCreditDefaultSwapData.BasketData = reverse_basket(data.constituents);
    if (!data.strikes.empty() && data.strikes.front().bare_value) {
        moneyFloat strike;
        set_exact(strike, *data.strikes.front().bare_value);
        d.Strike = std::move(strike);
    }
    const auto dates = exercise_dates_of(data.schedule_dates);
    if (!dates.empty()) {
        _ExerciseDates_t exd;
        for (const auto& day : dates) {
            date ed;
            static_cast<std::string&>(ed) = to_ore_date(day);
            exd.ExerciseDate.push_back(ed);
        }
        exerciseDatesGroup_group_t eg;
        eg.ExerciseDates = std::move(exd);
        d.OptionData.exerciseDatesGroup = std::move(eg);
    }
    t.IndexCreditDefaultSwapOptionData = std::move(d);
    return t;
}

trade credit_instrument_mapper::reverse_credit_linked_swap(const credit_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping CreditLinkedSwap";
    const auto& fact = fact_of<credit_linked_swap_instrument>(data);
    trade t;
    t.TradeType = oreTradeType::CreditLinkedSwap;
    creditLinkedSwapData d;
    static_cast<std::string&>(d.CreditCurveId) = fact.credit_curve_id;
    if (fact.recovery_rate)
        d.FixedRecoveryRate = static_cast<float>(*fact.recovery_rate);
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
    t.CreditLinkedSwapData = std::move(d);
    return t;
}

trade credit_instrument_mapper::reverse_synthetic_cdo(const credit_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping SyntheticCDO";
    const auto& fact = fact_of<synthetic_cdo_instrument>(data);
    trade t;
    t.TradeType = oreTradeType::SyntheticCDO;
    cdoData d;
    static_cast<std::string&>(d.Qualifier) = fact.qualifier;
    if (fact.protection_start_date.ok())
        static_cast<std::string&>(d.ProtectionStart) = to_ore_date(fact.protection_start_date);
    d.AttachmentPoint = static_cast<float>(fact.attachment_point);
    d.DetachmentPoint = static_cast<float>(fact.detachment_point);
    if (fact.recovery_rate)
        d.FixedRecoveryRate = static_cast<float>(*fact.recovery_rate);
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.LegData = legs.front();
    if (!data.constituents.empty())
        d.BasketData = reverse_basket(data.constituents);
    t.CdoData = std::move(d);
    return t;
}

trade credit_instrument_mapper::reverse_rpa(const credit_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping RiskParticipationAgreement";
    const auto& fact = fact_of<risk_participation_agreement_instrument>(data);
    trade t;
    t.TradeType = oreTradeType::RiskParticipationAgreement;
    rpaData d;
    static_cast<std::string&>(d.CreditCurveId) = fact.credit_curve_id;
    if (fact.issuer_id) {
        rpaData_IssuerId_t iid;
        static_cast<std::string&>(iid) = *fact.issuer_id;
        d.IssuerId = std::move(iid);
    }
    if (data.instrument.start_date)
        static_cast<std::string&>(d.ProtectionStart) = to_ore_date(data.instrument.start_date);
    if (data.instrument.maturity_date)
        static_cast<std::string&>(d.ProtectionEnd) = to_ore_date(data.instrument.maturity_date);
    if (fact.recovery_rate)
        d.FixedRecoveryRate = static_cast<float>(*fact.recovery_rate);
    t.RiskParticipationAgreementData = std::move(d);
    return t;
}

}
