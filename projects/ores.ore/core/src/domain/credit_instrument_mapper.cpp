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

bool flag_is_true(bool_ v) {
    switch (v) {
    case bool_::Y:
    case bool_::YES:
    case bool_::TRUE_:
    case bool_::True:
    case bool_::true_:
    case bool_::_1:
        return true;
    default:
        return false;
    }
}

std::optional<bool> to_flag(const xsd::optional<bool_>& v) {
    if (!v)
        return std::nullopt;
    return flag_is_true(*v);
}

void set_flag(xsd::optional<bool_>& field, const std::optional<bool>& v) {
    if (v)
        field = *v ? bool_::Y : bool_::N;
}

template <typename Text>
std::optional<std::string> to_text(const xsd::optional<Text>& v) {
    if (!v)
        return std::nullopt;
    return std::string(*v);
}

template <typename Text>
void set_text(xsd::optional<Text>& field, const std::optional<std::string>& v) {
    if (!v)
        return;
    Text value;
    static_cast<std::string&>(value) = *v;
    field = std::move(value);
}

std::optional<std::chrono::year_month_day> to_date(const xsd::optional<date>& v) {
    if (!v)
        return std::nullopt;
    return to_optional_date(std::string(*v));
}

void set_date(xsd::optional<date>& field,
              const std::optional<std::chrono::year_month_day>& v) {
    if (!v)
        return;
    date value;
    static_cast<std::string&>(value) = to_ore_date(*v);
    field = std::move(value);
}

std::optional<ores::utility::decimal::decimal> to_money(const xsd::optional<moneyFloat>& v) {
    if (!v)
        return std::nullopt;
    return exact_decimal(*v);
}

void set_money(xsd::optional<moneyFloat>& field,
               const std::optional<ores::utility::decimal::decimal>& v) {
    if (!v)
        return;
    moneyFloat value;
    set_exact(value, *v);
    field = std::move(value);
}

// The CDO states its upfront fee as text that is empty or a number.
std::optional<ores::utility::decimal::decimal> to_money(const xsd::optional<emptyFloat>& v) {
    if (!v || std::string(*v).empty())
        return std::nullopt;
    auto parsed = ores::utility::decimal::decimal::from_string(std::string(*v));
    if (!parsed)
        return std::nullopt;
    return *parsed;
}

void set_money(xsd::optional<emptyFloat>& field,
               const std::optional<ores::utility::decimal::decimal>& v) {
    if (v)
        field = emptyFloat(v->to_string());
}

std::optional<int> to_days(const xsd::optional<uint64_t>& v) {
    if (!v)
        return std::nullopt;
    return static_cast<int>(*v);
}

void set_days(xsd::optional<uint64_t>& field, const std::optional<int>& v) {
    if (v)
        field = static_cast<uint64_t>(*v);
}

std::optional<bool> to_plain_flag(const xsd::optional<bool>& v) {
    if (!v)
        return std::nullopt;
    return *v;
}

void append_option_rows(credit_instrument_data& result, swap_instrument_mapper::option_rows rows) {
    const auto move_into = [](auto& to, auto& from) {
        to.insert(to.end(), std::make_move_iterator(from.begin()), std::make_move_iterator(from.end()));
    };
    move_into(result.options, rows.options);
    move_into(result.option_premiums, rows.premiums);
    move_into(result.option_exercise_fees, rows.exercise_fees);
    move_into(result.option_payment_dates, rows.payment_dates);
    move_into(result.option_exercise_prices, rows.exercise_prices);
    move_into(result.schedules, rows.schedules);
    move_into(result.schedule_dates, rows.schedule_dates);
}

swap_instrument_mapper::option_rows option_rows_of(const credit_instrument_data& data) {
    swap_instrument_mapper::option_rows rows;
    rows.options = data.options;
    rows.premiums = data.option_premiums;
    rows.exercise_fees = data.option_exercise_fees;
    rows.payment_dates = data.option_payment_dates;
    rows.exercise_prices = data.option_exercise_prices;
    rows.schedules = data.schedules;
    rows.schedule_dates = data.schedule_dates;
    return rows;
}

template <typename Fact>
const Fact& fact_of(const credit_instrument_data& data) {
    static const Fact empty{};
    const auto* f = std::get_if<Fact>(&data.facts);
    return f ? *f : empty;
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
        if (auto schedule = swap_instrument_mapper::rebuild_schedule(
                data.schedules, data.schedule_dates, "swap", leg.identity.leg_number, "schedule"))
            out.back().ScheduleData = std::move(*schedule);
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

namespace {

// The swap-like products state the same accrual and payment elements.
template <typename Fact, typename Data>
void read_accrual(Fact& fact, const Data& d) {
    fact.settles_accrual = to_flag(d.SettlesAccrual);
    fact.rebates_accrual = to_flag(d.RebatesAccrual);
    fact.pays_at_default_time = to_flag(d.PaysAtDefaultTime);
    fact.protection_payment_time = to_text(d.ProtectionPaymentTime);
}

template <typename Fact, typename Data>
void write_accrual(const Fact& fact, Data& d) {
    set_flag(d.SettlesAccrual, fact.settles_accrual);
    set_flag(d.RebatesAccrual, fact.rebates_accrual);
    set_flag(d.PaysAtDefaultTime, fact.pays_at_default_time);
    set_text(d.ProtectionPaymentTime, fact.protection_payment_time);
}

index_credit_default_swap_instrument read_index_swap(const indexCreditDefaultSwapData& d) {
    auto fact = make_fact<index_credit_default_swap_instrument>();
    fact.credit_curve_id = std::string(d.CreditCurveId);
    fact.issuer_id = to_text(d.IssuerId);
    read_accrual(fact, d);
    fact.protection_start_date = to_date(d.ProtectionStart);
    fact.upfront_date = to_date(d.UpfrontDate);
    fact.upfront_fee = to_money(d.UpfrontFee);
    fact.trade_date = to_date(d.TradeDate);
    fact.cash_settlement_days = to_days(d.CashSettlementDays);
    return fact;
}

void write_index_swap(const index_credit_default_swap_instrument& fact,
                      indexCreditDefaultSwapData& d) {
    static_cast<std::string&>(d.CreditCurveId) = fact.credit_curve_id;
    set_text(d.IssuerId, fact.issuer_id);
    write_accrual(fact, d);
    set_date(d.ProtectionStart, fact.protection_start_date);
    set_date(d.UpfrontDate, fact.upfront_date);
    set_money(d.UpfrontFee, fact.upfront_fee);
    set_date(d.TradeDate, fact.trade_date);
    set_days(d.CashSettlementDays, fact.cash_settlement_days);
}

} // namespace

credit_instrument_data credit_instrument_mapper::forward_cds(const trade& t) {
    BOOST_LOG_SEV(lg(), debug) << "Forward-mapping CreditDefaultSwap: " << std::string(t.id);
    credit_instrument_data result;
    result.instrument = make_header("CreditDefaultSwap");
    auto fact = make_fact<credit_default_swap_instrument>();
    if (t.CreditDefaultSwapData) {
        const auto& d = *t.CreditDefaultSwapData;
        if (d.creditCurveIdType.CreditCurveId)
            fact.credit_curve_id = std::string(*d.creditCurveIdType.CreditCurveId);
        fact.issuer_id = to_text(d.IssuerId);
        fact.reference_obligation = to_text(d.ReferenceObligation);
        read_accrual(fact, d);
        fact.protection_start_date = to_date(d.ProtectionStart);
        fact.upfront_date = to_date(d.UpfrontDate);
        fact.upfront_fee = to_money(d.UpfrontFee);
        fact.recovery_rate = to_double(d.FixedRecoveryRate);
        fact.trade_date = to_date(d.TradeDate);
        fact.cash_settlement_days = to_days(d.CashSettlementDays);
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
        fact = read_index_swap(d);
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
        fact.knock_out = to_flag(od.KnockOut);
        fact.index_term = to_text(od.IndexTerm);
        if (od.StrikeType)
            fact.strike_type = to_string(*od.StrikeType);
        fact.trade_date = to_date(od.TradeDate);
        fact.front_end_protection_start_date = to_date(od.FrontEndProtectionStartDate);

        result.wrapped_index_swap = read_index_swap(d);
        result.instrument.reference_entity = result.wrapped_index_swap->credit_curve_id;
        map_leg_dates(d.LegData, result.instrument);
        append_leg(result, d.LegData, "premium");
        if (d.BasketData)
            append_basket(result, *d.BasketData);

        // The strike is the option's one shared strike row.
        if (od.Strike) {
            instrument_strike row;
            row.bare_value = exact_decimal(*od.Strike);
            stamp_audit(row);
            result.strikes.push_back(std::move(row));
        }
        append_option_rows(result, swap_instrument_mapper::map_option_block(od.OptionData));
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
        fact.settles_accrual = to_flag(d.SettlesAccrual);
        fact.recovery_rate = to_double(d.FixedRecoveryRate);
        fact.default_payment_time = to_text(d.DefaultPaymentTime);
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
        fact.upfront_date = to_date(d.UpfrontDate);
        fact.upfront_fee = to_money(d.UpfrontFee);
        read_accrual(fact, d);
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
        fact.participation_rate = static_cast<double>(d.ParticipationRate);
        fact.credit_curve_id = std::string(d.CreditCurveId);
        if (d.IssuerId)
            fact.issuer_id = std::string(*d.IssuerId);
        fact.settles_accrual = to_plain_flag(d.SettlesAccrual);
        fact.recovery_rate = to_double(d.FixedRecoveryRate);
        fact.naked_option = to_plain_flag(d.Underlying.NakedOption);
        result.instrument.reference_entity =
            fact.issuer_id ? *fact.issuer_id : fact.credit_curve_id;
        result.instrument.start_date = to_optional_date(std::string(d.ProtectionStart));
        result.instrument.maturity_date = to_optional_date(std::string(d.ProtectionEnd));

        // A leg keeps its own schedule when it states one, because the
        // header's dates are the protection period and not the leg's.
        const auto append_with_schedule = [&](const legData& ld, const std::string& group) {
            const int leg_number = static_cast<int>(result.legs.size()) + 1;
            append_leg(result, ld, group);
            if (!ld.ScheduleData)
                return;
            auto rows = swap_instrument_mapper::map_schedule(
                *ld.ScheduleData, "swap", leg_number, "schedule");
            result.schedules.insert(result.schedules.end(),
                                    std::make_move_iterator(rows.schedules.begin()),
                                    std::make_move_iterator(rows.schedules.end()));
            result.schedule_dates.insert(result.schedule_dates.end(),
                                         std::make_move_iterator(rows.dates.begin()),
                                         std::make_move_iterator(rows.dates.end()));
        };
        for (const auto& ld : d.ProtectionFee.LegData)
            append_with_schedule(ld, "protection_fee");
        for (const auto& ld : d.Underlying.LegData)
            append_with_schedule(ld, "underlying");

        // The agreement has no option of its own, so the option rows are the
        // underlying's.
        if (d.Underlying.OptionData)
            append_option_rows(result,
                               swap_instrument_mapper::map_option_block(*d.Underlying.OptionData));

        if (d.Underlying.TreasuryLockData)
            BOOST_LOG_SEV(lg(), warn) << "Treasury lock of the underlying is not stored: "
                                      << std::string(t.id);
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
    set_text(d.IssuerId, fact.issuer_id);
    set_text(d.ReferenceObligation, fact.reference_obligation);
    write_accrual(fact, d);
    set_date(d.ProtectionStart, fact.protection_start_date);
    set_date(d.UpfrontDate, fact.upfront_date);
    set_money(d.UpfrontFee, fact.upfront_fee);
    if (fact.recovery_rate)
        d.FixedRecoveryRate = static_cast<float>(*fact.recovery_rate);
    set_date(d.TradeDate, fact.trade_date);
    set_days(d.CashSettlementDays, fact.cash_settlement_days);
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.LegData = legs.front();
    t.CreditDefaultSwapData = std::move(d);
    return t;
}

trade credit_instrument_mapper::reverse_index_cds(const credit_instrument_data& data) {
    BOOST_LOG_SEV(lg(), debug) << "Reverse-mapping IndexCreditDefaultSwap";
    trade t;
    t.TradeType = oreTradeType::IndexCreditDefaultSwap;
    indexCreditDefaultSwapData d;
    write_index_swap(fact_of<index_credit_default_swap_instrument>(data), d);
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
    set_flag(d.KnockOut, fact.knock_out);
    set_text(d.IndexTerm, fact.index_term);
    if (fact.strike_type)
        d.StrikeType = *fact.strike_type == "Spread" ? cdsOptionstrikeType::Spread
                                                     : cdsOptionstrikeType::Price;
    set_date(d.TradeDate, fact.trade_date);
    set_date(d.FrontEndProtectionStartDate, fact.front_end_protection_start_date);

    if (data.wrapped_index_swap) {
        write_index_swap(*data.wrapped_index_swap, d.IndexCreditDefaultSwapData);
    } else {
        // The importer writes the wrapped swap's row beside the option's, so
        // a missing row means a partial write. The export has no credit curve.
        BOOST_LOG_SEV(lg(), warn) << "Index option has no wrapped index swap row; "
                                     "the exported CreditCurveId is empty";
    }
    const auto legs = reverse_group(data, "premium");
    if (!legs.empty())
        d.IndexCreditDefaultSwapData.LegData = legs.front();
    if (!data.constituents.empty())
        d.IndexCreditDefaultSwapData.BasketData = reverse_basket(data.constituents);
    if (!data.strikes.empty() && data.strikes.front().bare_value)
        set_money(d.Strike, data.strikes.front().bare_value);

    d.OptionData = swap_instrument_mapper::rebuild_option_block(option_rows_of(data));
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
    set_flag(d.SettlesAccrual, fact.settles_accrual);
    if (fact.recovery_rate)
        d.FixedRecoveryRate = static_cast<float>(*fact.recovery_rate);
    set_text(d.DefaultPaymentTime, fact.default_payment_time);
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
    set_date(d.UpfrontDate, fact.upfront_date);
    set_money(d.UpfrontFee, fact.upfront_fee);
    write_accrual(fact, d);
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
    d.ParticipationRate = static_cast<float>(fact.participation_rate);
    static_cast<std::string&>(d.CreditCurveId) = fact.credit_curve_id;
    if (fact.issuer_id) {
        rpaData_IssuerId_t iid;
        static_cast<std::string&>(iid) = *fact.issuer_id;
        d.IssuerId = std::move(iid);
    }
    if (fact.settles_accrual)
        d.SettlesAccrual = *fact.settles_accrual;
    if (data.instrument.start_date)
        static_cast<std::string&>(d.ProtectionStart) = to_ore_date(data.instrument.start_date);
    if (data.instrument.maturity_date)
        static_cast<std::string&>(d.ProtectionEnd) = to_ore_date(data.instrument.maturity_date);
    if (fact.recovery_rate)
        d.FixedRecoveryRate = static_cast<float>(*fact.recovery_rate);

    for (auto& leg : reverse_group(data, "protection_fee"))
        d.ProtectionFee.LegData.push_back(std::move(leg));
    for (auto& leg : reverse_group(data, "underlying"))
        d.Underlying.LegData.push_back(std::move(leg));
    if (fact.naked_option)
        d.Underlying.NakedOption = *fact.naked_option;
    if (!data.options.empty())
        d.Underlying.OptionData = swap_instrument_mapper::rebuild_option_block(option_rows_of(data));

    t.RiskParticipationAgreementData = std::move(d);
    return t;
}

}
