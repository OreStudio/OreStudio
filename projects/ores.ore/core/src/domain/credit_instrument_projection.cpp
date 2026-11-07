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
#include "ores.ore.core/domain/option_block_projection.hpp"
#include "ores.ore.core/domain/payment_frequency_conversion.hpp"
#include "ores.ore.core/domain/portfolio_instrument_projection.hpp"
#include "ores.ore.core/xml/binding.hpp"
#include "ores.platform/time/datetime.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::ore::domain {

namespace {

/*
 * An engine document names its credit curve or issuer itself; nothing in it is
 * an LEI. So the reference is stated as what it is: an internal identifier, and
 * not a claim about a legal entity the projection cannot make.
 */
void set_internal_reference_entity(ores::trading::domain::credit_instrument& instrument,
                                   const std::string& identifier) {
    instrument.reference_entity_id_type = "Internal";
    instrument.reference_entity_id = identifier;
}

using ores::utility::decimal::decimal;

using trading::domain::credit_basket_constituent;
using trading::domain::credit_default_swap_instrument;
using trading::domain::credit_instrument;
using trading::domain::credit_instrument_data;
using trading::domain::credit_linked_swap_instrument;
using trading::domain::index_credit_default_swap_instrument;
using trading::domain::index_credit_default_swap_option_instrument;
using trading::domain::instrument_option;
using trading::domain::instrument_option_exercise_fee;
using trading::domain::instrument_option_exercise_price;
using trading::domain::instrument_option_payment_date;
using trading::domain::instrument_option_premium;
using trading::domain::instrument_schedule;
using trading::domain::instrument_schedule_date;
using trading::domain::instrument_strike;
using trading::domain::risk_participation_agreement_instrument;
using trading::domain::swap_leg;
using trading::domain::swap_leg_amount;
using trading::domain::swap_leg_rate;
using trading::domain::synthetic_cdo_instrument;

using xml::record_reader;
using xml::record_writer;

// ---------------------------------------------------------------------------
// The vocabulary both directions share
// ---------------------------------------------------------------------------

/// The actor and reason every imported row states.
constexpr std::string_view credit_instrument_projection_actor = "ores";
constexpr std::string_view credit_instrument_projection_change_reason =
    "system.external_data_import";
constexpr std::string_view credit_instrument_projection_change_commentary = "Imported from ORE XML";

/**
 * @brief The trade types the family states, which are the ones the old
 * dispatcher took.
 */
constexpr bool is_credit_trade_type(std::string_view type) {
    return type == "CreditDefaultSwap" || type == "IndexCreditDefaultSwap" ||
           type == "IndexCreditDefaultSwapOption" || type == "CreditLinkedSwap" ||
           type == "SyntheticCDO" || type == "RiskParticipationAgreement";
}

void stamp(swap_leg& row) {
    row.audit.modified_by = std::string(credit_instrument_projection_actor);
    row.audit.performed_by = std::string(credit_instrument_projection_actor);
    row.audit.change_reason_code = std::string(credit_instrument_projection_change_reason);
    row.audit.change_commentary = std::string(credit_instrument_projection_change_commentary);
}

template <typename Row>
void credit_instrument_projection_stamp_row(Row& row) {
    row.modified_by = std::string(credit_instrument_projection_actor);
    row.performed_by = std::string(credit_instrument_projection_actor);
    row.change_reason_code = std::string(credit_instrument_projection_change_reason);
    row.change_commentary = std::string(credit_instrument_projection_change_commentary);
}

auto make_header(std::string_view trade_type_code) -> credit_instrument {
    credit_instrument r;
    r.identity.trade_type_code = std::string(trade_type_code);
    r.audit.modified_by = std::string(credit_instrument_projection_actor);
    r.audit.performed_by = std::string(credit_instrument_projection_actor);
    r.audit.change_reason_code = std::string(credit_instrument_projection_change_reason);
    r.audit.change_commentary = std::string(credit_instrument_projection_change_commentary);
    return r;
}

// ---------------------------------------------------------------------------
// Reading: the values, dates and spellings a row states
// ---------------------------------------------------------------------------

auto credit_instrument_projection_to_domain_date(const std::string& text)
    -> std::chrono::year_month_day {
    if (text.empty())
        return {};
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto credit_instrument_projection_to_optional_domain_date(const std::string& text)
    -> std::optional<std::chrono::year_month_day> {
    if (text.empty())
        return std::nullopt;
    return ores::platform::time::datetime::from_iso8601_date(text);
}

auto credit_instrument_projection_to_ore_date(const std::chrono::year_month_day& d) -> std::string {
    return d.ok() ? ores::platform::time::datetime::to_iso8601_date(d) : std::string{};
}

auto credit_instrument_projection_to_ore_date(const std::optional<std::chrono::year_month_day>& d)
    -> std::string {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

auto credit_instrument_projection_text_of(const record_reader& r,
                                          std::string_view name) -> std::string {
    if (const auto value = r.text(name))
        return *value;
    return {};
}

auto credit_instrument_projection_spelling_of(const record_reader& r,
                                              std::string_view name) -> std::string {
    if (const auto value = r.spelling(name))
        return *value;
    return {};
}

// A boolean member the schema types as text keeps the spelling the document
// wrote, so the reading accepts every spelling the binder accepts for a flag.
auto credit_instrument_projection_is_flag_on(std::string_view text) -> bool {
    return text == "Y" || text == "YES" || text == "TRUE" || text == "True" || text == "true" ||
           text == "1";
}

auto credit_instrument_projection_flag_text(bool value) -> std::string_view {
    return value ? "true" : "false";
}

/// The exact decimal an xs:simpleContent numeric leaf carries.
auto credit_instrument_projection_decimal_text(const record_reader& r) -> decimal {
    return r.own_decimal().value_or(decimal{});
}

auto credit_instrument_projection_optional_decimal_of(const record_reader& r, std::string_view name)
    -> std::optional<decimal> {
    const auto value = decimal_as_double(r, name);
    if (!value)
        return std::nullopt;
    return decimal_from_double(*value);
}

/// A number the document spelled as text, such as a CDO's upfront fee.
auto credit_instrument_projection_decimal_from_text(const std::string& text)
    -> std::optional<decimal> {
    if (text.empty())
        return std::nullopt;
    if (auto parsed = decimal::from_string(text))
        return std::move(*parsed);
    return std::nullopt;
}

auto optional_int_of(const record_reader& r, std::string_view name) -> std::optional<int> {
    if (const auto value = r.integer(name))
        return static_cast<int>(*value);
    return std::nullopt;
}

/// The first schedule rule's tenor, which is the leg's payment frequency.
auto first_tenor(const std::optional<record_reader>& schedule) -> std::string {
    if (!schedule)
        return {};
    const auto rules = schedule->children("Rules");
    if (rules.empty())
        return {};
    return credit_instrument_projection_text_of(rules.front(), "Tenor");
}

/// The header's dates are the first schedule rule of the leg: its start and end.
void map_leg_dates(const record_reader& leg, credit_instrument& header) {
    const auto schedule = leg.child("ScheduleData");
    if (!schedule)
        return;
    const auto rules = schedule->children("Rules");
    if (rules.empty())
        return;
    const auto& rule = rules.front();
    header.start_date = credit_instrument_projection_to_optional_domain_date(
        credit_instrument_projection_text_of(rule, "StartDate"));
    if (rule.has("EndDate"))
        header.maturity_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(rule, "EndDate"));
}

// The accrual flags and the protection payment time are the same three
// elements on every swap-like product.
template <typename Fact>
void read_accrual(Fact& fact, const record_reader& d) {
    fact.settles_accrual = d.flag("SettlesAccrual");
    fact.rebates_accrual = d.flag("RebatesAccrual");
    fact.pays_at_default_time = d.flag("PaysAtDefaultTime");
    fact.protection_payment_time = d.text("ProtectionPaymentTime");
}

// ---------------------------------------------------------------------------
// Reading: the shared children
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// Reading: the legs, their economics and the basket
// ---------------------------------------------------------------------------

swap_leg credit_instrument_projection_read_leg(const record_reader& leg,
                                               int leg_number,
                                               std::string_view group) {
    swap_leg row;
    row.identity.leg_number = leg_number;
    row.payer =
        credit_instrument_projection_is_flag_on(credit_instrument_projection_text_of(leg, "Payer"));
    row.leg_type_code = credit_instrument_projection_spelling_of(leg, "LegType");
    row.currency = credit_instrument_projection_text_of(leg, "Currency");
    row.day_count_fraction_code = credit_instrument_projection_spelling_of(leg, "DayCounter");
    row.business_day_convention_code =
        credit_instrument_projection_spelling_of(leg, "PaymentConvention");
    row.payment_frequency_code = tenor_to_payment_frequency(first_tenor(leg.child("ScheduleData")));
    if (const auto floating = leg.child("FloatingLegData"))
        row.floating_index_code = credit_instrument_projection_text_of(*floating, "Index");
    row.leg_group = std::string(group);
    stamp(row);
    return row;
}

std::vector<swap_leg_amount> read_leg_amounts(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_amount> amounts;
    const auto append = [&](const decimal& value, const std::string& start_date) {
        swap_leg_amount row;
        row.leg_number = leg_number;
        row.sequence_number = static_cast<int>(amounts.size()) + 1;
        row.amount = value;
        if (!start_date.empty())
            row.start_date = credit_instrument_projection_to_domain_date(start_date);
        credit_instrument_projection_stamp_row(row);
        amounts.push_back(std::move(row));
    };

    // The document states its notional either as a plain list or as an
    // amortisation schedule, and a cashflow leg states its amounts as dated
    // payments. Each arm lands in the same column.
    if (const auto notionals = leg.child("Notionals")) {
        for (const auto& notional : notionals->children("Notional"))
            append(credit_instrument_projection_decimal_text(notional),
                   credit_instrument_projection_text_of(notional, "startDate"));
    } else if (const auto cashflow_data = leg.child("CashflowData")) {
        if (const auto cashflow = cashflow_data->child("Cashflow"))
            for (const auto& amount : cashflow->children("Amount"))
                append(credit_instrument_projection_decimal_text(amount),
                       credit_instrument_projection_text_of(amount, "date"));
    } else if (const auto amortizations = leg.child("Amortizations")) {
        for (const auto& amortization : amortizations->children("AmortizationData")) {
            const auto value =
                amortization.has("Value") ?
                    credit_instrument_projection_optional_decimal_of(amortization, "Value")
                        .value_or(decimal{}) :
                    decimal{};
            append(value, credit_instrument_projection_text_of(amortization, "StartDate"));
        }
    }
    return amounts;
}

std::vector<swap_leg_rate> read_leg_rates(const record_reader& leg, int leg_number) {
    std::vector<swap_leg_rate> rates;
    const auto append = [&](std::string_view role, const decimal& value) {
        swap_leg_rate row;
        row.leg_number = leg_number;
        row.rate_role = std::string(role);
        row.sequence_number = static_cast<int>(rates.size()) + 1;
        row.value = value;
        credit_instrument_projection_stamp_row(row);
        rates.push_back(std::move(row));
    };

    if (const auto fixed = leg.child("FixedLegData")) {
        if (const auto list = fixed->child("Rates"))
            for (const auto& rate : list->children("Rate"))
                append("fixed", credit_instrument_projection_decimal_text(rate));
    }
    if (const auto floating = leg.child("FloatingLegData")) {
        if (const auto list = floating->child("Spreads"))
            for (const auto& spread : list->children("Spread"))
                append("spread", credit_instrument_projection_decimal_text(spread));
    }
    return rates;
}

void append_leg(credit_instrument_data& result, const record_reader& leg, std::string_view group) {
    const int leg_number = static_cast<int>(result.legs.size()) + 1;
    result.legs.push_back(credit_instrument_projection_read_leg(leg, leg_number, group));
    for (auto& amount : read_leg_amounts(leg, leg_number))
        result.leg_amounts.push_back(std::move(amount));
    for (auto& rate : read_leg_rates(leg, leg_number))
        result.leg_rates.push_back(std::move(rate));
    // A leg keeps its own schedule, because the header's dates are the
    // protection period and not the leg's.
    if (const auto schedule = leg.child("ScheduleData"))
        option_block::read_schedule(result, *schedule, "swap", leg_number, "schedule");
}

/// The legs of one of the four payment groups a credit linked swap states.
void append_payment_group(credit_instrument_data& result,
                          const record_reader& payments,
                          std::string_view group) {
    for (const auto& leg : payments.children("LegData"))
        append_leg(result, leg, group);
}

void append_basket(credit_instrument_data& result, const record_reader& basket) {
    int sequence = 0;
    for (const auto& name : basket.children("Name")) {
        credit_basket_constituent row;
        row.sequence_number = ++sequence;
        row.issuer_id = credit_instrument_projection_text_of(name, "IssuerId");
        if (name.has("Qualifier"))
            row.qualifier = credit_instrument_projection_text_of(name, "Qualifier");
        if (name.has("CreditCurveId"))
            row.credit_curve_id = credit_instrument_projection_text_of(name, "CreditCurveId");
        row.notional = name.decimal("Notional");
        row.weight = decimal_as_double(name, "Weight");
        if (name.has("Currency"))
            row.currency = credit_instrument_projection_spelling_of(name, "Currency");
        credit_instrument_projection_stamp_row(row);
        result.constituents.push_back(std::move(row));
    }
}

// ---------------------------------------------------------------------------
// Reading: one function per product
// ---------------------------------------------------------------------------

/// The fact fields an index swap states, which its option also wraps.
auto read_index_swap(const record_reader& d) -> index_credit_default_swap_instrument {
    index_credit_default_swap_instrument fact;
    fact.credit_curve_id = credit_instrument_projection_text_of(d, "CreditCurveId");
    fact.issuer_id = d.text("IssuerId");
    read_accrual(fact, d);
    fact.protection_start_date = credit_instrument_projection_to_optional_domain_date(
        credit_instrument_projection_text_of(d, "ProtectionStart"));
    fact.upfront_date = credit_instrument_projection_to_optional_domain_date(
        credit_instrument_projection_text_of(d, "UpfrontDate"));
    fact.upfront_fee = d.decimal("UpfrontFee");
    fact.trade_date = credit_instrument_projection_to_optional_domain_date(
        credit_instrument_projection_text_of(d, "TradeDate"));
    fact.cash_settlement_days = optional_int_of(d, "CashSettlementDays");
    return fact;
}

auto read_cds(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("CreditDefaultSwap");

    credit_default_swap_instrument fact;
    if (const auto d = trade.child("CreditDefaultSwapData")) {
        if (const auto curve = d->text("CreditCurveId"))
            fact.credit_curve_id = *curve;
        fact.issuer_id = d->text("IssuerId");
        fact.reference_obligation = d->text("ReferenceObligation");
        read_accrual(fact, *d);
        fact.protection_start_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*d, "ProtectionStart"));
        fact.upfront_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*d, "UpfrontDate"));
        fact.upfront_fee = d->decimal("UpfrontFee");
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        fact.trade_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*d, "TradeDate"));
        fact.cash_settlement_days = optional_int_of(*d, "CashSettlementDays");
        set_internal_reference_entity(result.instrument,
                                      fact.issuer_id ? *fact.issuer_id :
                                                       fact.credit_curve_id.value_or(""));
        if (const auto leg = d->child("LegData")) {
            map_leg_dates(*leg, result.instrument);
            append_leg(result, *leg, "premium");
        }
    }
    credit_instrument_projection_stamp_row(fact);
    result.credit_default_swap_instruments.push_back(std::move(fact));
    return result;
}

auto read_index_cds(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("IndexCreditDefaultSwap");

    index_credit_default_swap_instrument fact;
    if (const auto d = trade.child("IndexCreditDefaultSwapData")) {
        fact = read_index_swap(*d);
        set_internal_reference_entity(result.instrument, fact.credit_curve_id);
        if (const auto leg = d->child("LegData")) {
            map_leg_dates(*leg, result.instrument);
            append_leg(result, *leg, "premium");
        }
        if (const auto basket = d->child("BasketData"))
            append_basket(result, *basket);
    }
    credit_instrument_projection_stamp_row(fact);
    result.index_credit_default_swap_instruments.push_back(std::move(fact));
    return result;
}

auto read_index_cds_option(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("IndexCreditDefaultSwapOption");

    index_credit_default_swap_option_instrument fact;
    if (const auto od = trade.child("IndexCreditDefaultSwapOptionData")) {
        fact.knock_out = od->flag("KnockOut");
        fact.index_term = od->text("IndexTerm");
        if (od->has("StrikeType"))
            fact.strike_type = credit_instrument_projection_spelling_of(*od, "StrikeType");
        fact.trade_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*od, "TradeDate"));
        fact.front_end_protection_start_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*od, "FrontEndProtectionStartDate"));

        if (const auto d = od->child("IndexCreditDefaultSwapData")) {
            result.wrapped_index_swap = read_index_swap(*d);
            credit_instrument_projection_stamp_row(*result.wrapped_index_swap);
            set_internal_reference_entity(result.instrument,
                                          result.wrapped_index_swap->credit_curve_id);
            if (const auto leg = d->child("LegData")) {
                map_leg_dates(*leg, result.instrument);
                append_leg(result, *leg, "premium");
            }
            if (const auto basket = d->child("BasketData"))
                append_basket(result, *basket);
        }

        // The strike is the option's one shared strike row, and its role says
        // so because the same table holds a cap/floor's arms.
        if (const auto strike = od->decimal("Strike")) {
            instrument_strike row;
            row.strike_role = "single";
            row.sequence_number = 1;
            row.bare_value = *strike;
            credit_instrument_projection_stamp_row(row);
            result.strikes.push_back(std::move(row));
        }

        if (const auto option = od->child("OptionData"))
            option_block::read_option_block(result, *option);
    }
    credit_instrument_projection_stamp_row(fact);
    result.index_credit_default_swap_option_instruments.push_back(std::move(fact));
    return result;
}

auto read_credit_linked_swap(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("CreditLinkedSwap");

    credit_linked_swap_instrument fact;
    if (const auto d = trade.child("CreditLinkedSwapData")) {
        fact.credit_curve_id = credit_instrument_projection_text_of(*d, "CreditCurveId");
        fact.settles_accrual = d->flag("SettlesAccrual");
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        fact.default_payment_time = d->text("DefaultPaymentTime");
        set_internal_reference_entity(result.instrument, fact.credit_curve_id);

        // The header's dates are those of the first contingent payment leg.
        if (const auto contingent = d->child("ContingentPayments")) {
            const auto legs = contingent->children("LegData");
            if (!legs.empty())
                map_leg_dates(legs.front(), result.instrument);
        }
        if (const auto payments = d->child("IndependentPayments"))
            append_payment_group(result, *payments, "independent");
        if (const auto payments = d->child("ContingentPayments"))
            append_payment_group(result, *payments, "contingent");
        if (const auto payments = d->child("DefaultPayments"))
            append_payment_group(result, *payments, "default");
        if (const auto payments = d->child("RecoveryPayments"))
            append_payment_group(result, *payments, "recovery");
    }
    credit_instrument_projection_stamp_row(fact);
    result.credit_linked_swap_instruments.push_back(std::move(fact));
    return result;
}

auto read_synthetic_cdo(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("SyntheticCDO");

    synthetic_cdo_instrument fact;
    if (const auto d = trade.child("CdoData")) {
        fact.qualifier = credit_instrument_projection_text_of(*d, "Qualifier");
        fact.protection_start_date = credit_instrument_projection_to_domain_date(
            credit_instrument_projection_text_of(*d, "ProtectionStart"));
        fact.upfront_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*d, "UpfrontDate"));
        fact.upfront_fee = credit_instrument_projection_decimal_from_text(
            credit_instrument_projection_text_of(*d, "UpfrontFee"));
        read_accrual(fact, *d);
        fact.attachment_point = decimal_as_double(*d, "AttachmentPoint").value_or(0.0);
        fact.detachment_point = decimal_as_double(*d, "DetachmentPoint").value_or(0.0);
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        set_internal_reference_entity(result.instrument, fact.qualifier);
        result.instrument.start_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*d, "ProtectionStart"));
        if (const auto leg = d->child("LegData")) {
            map_leg_dates(*leg, result.instrument);
            append_leg(result, *leg, "premium");
        }
        if (const auto basket = d->child("BasketData"))
            append_basket(result, *basket);
    }
    credit_instrument_projection_stamp_row(fact);
    result.synthetic_cdo_instruments.push_back(std::move(fact));
    return result;
}

auto read_rpa(const record_reader& trade) -> credit_instrument_data {
    credit_instrument_data result;
    result.instrument = make_header("RiskParticipationAgreement");

    risk_participation_agreement_instrument fact;
    if (const auto d = trade.child("RiskParticipationAgreementData")) {
        fact.participation_rate = decimal_as_double(*d, "ParticipationRate").value_or(0.0);
        fact.credit_curve_id = credit_instrument_projection_text_of(*d, "CreditCurveId");
        if (const auto issuer = d->text("IssuerId"))
            fact.issuer_id = *issuer;
        if (d->has("SettlesAccrual"))
            fact.settles_accrual = credit_instrument_projection_is_flag_on(
                credit_instrument_projection_text_of(*d, "SettlesAccrual"));
        fact.recovery_rate = decimal_as_double(*d, "FixedRecoveryRate");
        set_internal_reference_entity(result.instrument,
                                      fact.issuer_id ? *fact.issuer_id : fact.credit_curve_id);
        result.instrument.start_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*d, "ProtectionStart"));
        result.instrument.maturity_date = credit_instrument_projection_to_optional_domain_date(
            credit_instrument_projection_text_of(*d, "ProtectionEnd"));

        if (const auto protection_fee = d->child("ProtectionFee"))
            for (const auto& leg : protection_fee->children("LegData"))
                append_leg(result, leg, "protection_fee");

        if (const auto underlying = d->child("Underlying")) {
            for (const auto& leg : underlying->children("LegData"))
                append_leg(result, leg, "underlying");
            if (underlying->has("NakedOption"))
                fact.naked_option = credit_instrument_projection_is_flag_on(
                    credit_instrument_projection_text_of(*underlying, "NakedOption"));

            // The agreement has no option of its own, so the option rows are
            // the underlying's.
            if (const auto option = underlying->child("OptionData"))
                option_block::read_option_block(result, *option);
        }
    }
    credit_instrument_projection_stamp_row(fact);
    result.risk_participation_agreement_instruments.push_back(std::move(fact));
    return result;
}

// ---------------------------------------------------------------------------
// Writing: the spellings a table refuses
// ---------------------------------------------------------------------------

auto credit_instrument_projection_accepts_spelling(const record_writer& w,
                                                   std::string_view name,
                                                   std::string_view value) -> bool {
    const auto* f = xml::find_field(w.type(), name, false);
    if (f == nullptr || f->tokens == nullptr)
        return false;
    for (std::int16_t i = 0; f->tokens[i] != nullptr; ++i)
        if (value == f->tokens[i])
            return true;
    return false;
}

// The old export replaced a stored code the schema does not state with a
// default rather than failing the document, so the default is stated here too.
void credit_instrument_projection_set_spelling_or(record_writer& w,
                                                  std::string_view name,
                                                  const std::string& value,
                                                  std::string_view fallback,
                                                  std::string_view what) {
    w.set_spelling(name,
                   credit_instrument_projection_accepts_spelling(w, name, value) ? value : fallback,
                   what);
}

void set_optional_date(record_writer& w,
                       std::string_view name,
                       const std::optional<std::chrono::year_month_day>& value) {
    if (value)
        w.set_text(name, credit_instrument_projection_to_ore_date(*value));
}

void set_optional_days(record_writer& w, std::string_view name, const std::optional<int>& value) {
    if (value)
        w.set_integer(name, *value);
}

void set_optional_decimal_from_double(record_writer& w,
                                      std::string_view name,
                                      const std::optional<double>& value) {
    if (value)
        set_decimal_from_double(w, name, *value);
}

// ---------------------------------------------------------------------------
// Writing: the shared children
// ---------------------------------------------------------------------------

void write_schedule_rules(record_writer& schedule,
                          const std::optional<std::chrono::year_month_day>& start_date,
                          const std::optional<std::chrono::year_month_day>& end_date,
                          const std::string& tenor) {
    auto rule = schedule.append("Rules");
    rule.set_text("StartDate", credit_instrument_projection_to_ore_date(start_date));
    if (end_date)
        rule.set_text("EndDate", credit_instrument_projection_to_ore_date(*end_date));
    rule.set_text("Tenor", tenor);
    rule.set_spelling("Convention", "MF", "business day convention");
}

// ---------------------------------------------------------------------------
// Writing: the legs, their economics and the basket
// ---------------------------------------------------------------------------

std::vector<swap_leg_amount> amounts_for_leg(const credit_instrument_data& v, int leg_number) {
    std::vector<swap_leg_amount> out;
    for (const auto& row : v.leg_amounts)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

std::vector<swap_leg_rate> rates_for_leg(const credit_instrument_data& v, int leg_number) {
    std::vector<swap_leg_rate> out;
    for (const auto& row : v.leg_rates)
        if (row.leg_number == leg_number)
            out.push_back(row);
    return out;
}

/// The legs one group of a trade states, in the order the document stated them.
std::vector<const swap_leg*> legs_in_group(const credit_instrument_data& v,
                                           std::string_view group) {
    std::vector<const swap_leg*> legs;
    for (const auto& leg : v.legs)
        if (leg.leg_group && *leg.leg_group == group)
            legs.push_back(&leg);
    std::stable_sort(legs.begin(), legs.end(), [](const auto* a, const auto* b) {
        return a->identity.leg_number < b->identity.leg_number;
    });
    return legs;
}

void credit_instrument_projection_write_leg(record_writer& leg,
                                            const credit_instrument_data& v,
                                            const swap_leg& row,
                                            const std::vector<swap_leg_amount>& amounts,
                                            const std::vector<swap_leg_rate>& rates) {
    leg.set_spelling("LegType", row.leg_type_code, "leg type");
    leg.set_text("Payer", credit_instrument_projection_flag_text(row.payer.value_or(false)));
    leg.set_text("Currency", row.currency);
    if (!row.day_count_fraction_code.empty())
        credit_instrument_projection_set_spelling_or(
            leg, "DayCounter", row.day_count_fraction_code, "A360", "day counter");
    if (!row.business_day_convention_code.empty())
        credit_instrument_projection_set_spelling_or(leg,
                                                     "PaymentConvention",
                                                     row.business_day_convention_code,
                                                     "MF",
                                                     "business day convention");

    // A cashflow leg has dated payments and no notional or schedule.
    if (row.leg_type_code == "Cashflow") {
        auto cashflow = leg.child("CashflowData").child("Cashflow");
        for (const auto& amount : amounts) {
            auto writer = cashflow.append("Amount");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("date",
                                credit_instrument_projection_to_ore_date(*amount.start_date));
        }
        return;
    }

    if (!amounts.empty()) {
        auto notionals = leg.child("Notionals");
        for (const auto& amount : amounts) {
            auto writer = notionals.append("Notional");
            writer.set_own_text(amount.amount.to_string());
            if (amount.start_date)
                writer.set_text("startDate",
                                credit_instrument_projection_to_ore_date(*amount.start_date));
        }
    }

    // The stored schedule is the document's own; a leg that states none gets
    // the period the header spans, which is what the old export wrote.
    auto schedule = leg.child("ScheduleData");
    if (!option_block::write_stored_schedule(
            schedule, v, "swap", row.identity.leg_number, "schedule"))
        write_schedule_rules(schedule,
                             v.instrument.start_date,
                             v.instrument.maturity_date,
                             payment_frequency_to_tenor(row.payment_frequency_code));

    const auto has_role = [&](std::string_view role) {
        return std::any_of(rates.begin(), rates.end(), [&](const swap_leg_rate& rate) {
            return rate.rate_role == role;
        });
    };
    if (row.leg_type_code == "Fixed" && has_role("fixed")) {
        auto list = leg.child("FixedLegData").child("Rates");
        for (const auto& rate : rates)
            if (rate.rate_role == "fixed")
                list.append("Rate").set_own_text(rate.value.to_string());
    } else if (row.leg_type_code == "Floating") {
        auto floating = leg.child("FloatingLegData");
        floating.set_text("Index", row.floating_index_code);
        const auto spreads = std::count_if(rates.begin(), rates.end(), [](const auto& rate) {
            return rate.rate_role == "spread";
        });
        if (spreads != 0) {
            auto list = floating.child("Spreads");
            for (const auto& rate : rates)
                if (rate.rate_role == "spread")
                    list.append("Spread").set_own_text(rate.value.to_string());
        }
    }
}

/// The whole of one leg, at the element the caller names.
void append_leg_writer(record_writer& parent,
                       std::string_view name,
                       const credit_instrument_data& v,
                       const swap_leg& row) {
    auto leg = parent.append(name);
    credit_instrument_projection_write_leg(leg,
                                           v,
                                           row,
                                           amounts_for_leg(v, row.identity.leg_number),
                                           rates_for_leg(v, row.identity.leg_number));
}

/// The first leg of a group, at the single element the product states it in.
void write_first_leg(record_writer& parent,
                     std::string_view name,
                     const credit_instrument_data& v,
                     std::string_view group) {
    const auto legs = legs_in_group(v, group);
    if (legs.empty())
        return;
    auto leg = parent.child(name);
    credit_instrument_projection_write_leg(leg,
                                           v,
                                           *legs.front(),
                                           amounts_for_leg(v, legs.front()->identity.leg_number),
                                           rates_for_leg(v, legs.front()->identity.leg_number));
}

/// Every leg of one payment group, at the repeated element it sits in.
void write_payment_group(record_writer& parent,
                         std::string_view name,
                         const credit_instrument_data& v,
                         std::string_view group) {
    const auto legs = legs_in_group(v, group);
    if (legs.empty())
        return;
    auto payments = parent.child(name);
    for (const auto* row : legs)
        append_leg_writer(payments, "LegData", v, *row);
}

void write_basket(record_writer basket, const credit_instrument_data& v) {
    std::vector<const credit_basket_constituent*> ordered;
    for (const auto& row : v.constituents)
        ordered.push_back(&row);
    std::stable_sort(ordered.begin(), ordered.end(), [](const auto* a, const auto* b) {
        return a->sequence_number < b->sequence_number;
    });

    for (const auto* row : ordered) {
        auto name = basket.append("Name");
        name.set_text("IssuerId", row->issuer_id);
        if (row->qualifier)
            name.set_text("Qualifier", *row->qualifier);
        if (row->credit_curve_id)
            name.set_text("CreditCurveId", *row->credit_curve_id);
        if (row->notional)
            name.set_decimal("Notional", *row->notional);
        if (row->weight)
            set_decimal_from_double(name, "Weight", *row->weight);
        if (row->currency)
            credit_instrument_projection_set_spelling_or(
                name, "Currency", *row->currency, "USD", "currency");
    }
}

// ---------------------------------------------------------------------------
// Writing: one function per product
// ---------------------------------------------------------------------------

template <typename Fact>
auto fact_of(const credit_instrument_data& data) -> const Fact& {
    static const Fact empty{};
    const auto& rows = fact_rows<Fact>(data);
    return rows.empty() ? empty : rows.front();
}

void write_accrual(record_writer& d,
                   const std::optional<bool>& settles_accrual,
                   const std::optional<bool>& rebates_accrual,
                   const std::optional<bool>& pays_at_default_time,
                   const std::optional<std::string>& protection_payment_time) {
    d.set_optional_flag("SettlesAccrual", settles_accrual);
    d.set_optional_flag("RebatesAccrual", rebates_accrual);
    d.set_optional_flag("PaysAtDefaultTime", pays_at_default_time);
    d.set_optional_text("ProtectionPaymentTime", protection_payment_time);
}

void write_cds_data(record_writer& d,
                    const credit_default_swap_instrument& fact,
                    const credit_instrument_data& v) {
    if (fact.credit_curve_id)
        d.set_text("CreditCurveId", *fact.credit_curve_id);
    d.set_optional_text("IssuerId", fact.issuer_id);
    d.set_optional_text("ReferenceObligation", fact.reference_obligation);
    write_accrual(d,
                  fact.settles_accrual,
                  fact.rebates_accrual,
                  fact.pays_at_default_time,
                  fact.protection_payment_time);
    set_optional_date(d, "ProtectionStart", fact.protection_start_date);
    set_optional_date(d, "UpfrontDate", fact.upfront_date);
    d.set_optional_decimal("UpfrontFee", fact.upfront_fee);
    set_optional_decimal_from_double(d, "FixedRecoveryRate", fact.recovery_rate);
    set_optional_date(d, "TradeDate", fact.trade_date);
    set_optional_days(d, "CashSettlementDays", fact.cash_settlement_days);
    write_first_leg(d, "LegData", v, "premium");
}

void write_index_swap_data(record_writer& d,
                           const index_credit_default_swap_instrument& fact,
                           const credit_instrument_data& v) {
    d.set_text("CreditCurveId", fact.credit_curve_id);
    d.set_optional_text("IssuerId", fact.issuer_id);
    write_accrual(d,
                  fact.settles_accrual,
                  fact.rebates_accrual,
                  fact.pays_at_default_time,
                  fact.protection_payment_time);
    set_optional_date(d, "ProtectionStart", fact.protection_start_date);
    set_optional_date(d, "UpfrontDate", fact.upfront_date);
    d.set_optional_decimal("UpfrontFee", fact.upfront_fee);
    set_optional_date(d, "TradeDate", fact.trade_date);
    set_optional_days(d, "CashSettlementDays", fact.cash_settlement_days);
    write_first_leg(d, "LegData", v, "premium");
    if (!v.constituents.empty())
        write_basket(d.child("BasketData"), v);
}

void write_cds(record_writer& trade, const credit_instrument_data& v) {
    auto data = trade.child("CreditDefaultSwapData");
    write_cds_data(data, fact_of<credit_default_swap_instrument>(v), v);
}

void write_index_cds(record_writer& trade, const credit_instrument_data& v) {
    auto data = trade.child("IndexCreditDefaultSwapData");
    write_index_swap_data(data, fact_of<index_credit_default_swap_instrument>(v), v);
}

void write_index_cds_option(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<index_credit_default_swap_option_instrument>(v);
    auto data = trade.child("IndexCreditDefaultSwapOptionData");
    data.set_optional_flag("KnockOut", fact.knock_out);
    data.set_optional_text("IndexTerm", fact.index_term);
    if (fact.strike_type)
        data.set_spelling(
            "StrikeType", *fact.strike_type == "Spread" ? "Spread" : "Price", "strike type");
    set_optional_date(data, "TradeDate", fact.trade_date);
    set_optional_date(data, "FrontEndProtectionStartDate", fact.front_end_protection_start_date);

    // The importer writes the wrapped swap's row beside the option's, so a
    // missing row means a partial write. The old export left the curve empty
    // rather than dropping the required element.
    auto swap_data = data.child("IndexCreditDefaultSwapData");
    const auto& wrapped = v.wrapped_index_swap;
    write_index_swap_data(
        swap_data, wrapped ? *wrapped : index_credit_default_swap_instrument{}, v);

    // An index option states one strike with no arm, so the row it reads is the
    // one whose role says so rather than whichever row came first.
    const auto strike = std::find_if(v.strikes.begin(), v.strikes.end(), [](const auto& row) {
        return row.strike_role == "single";
    });
    if (strike != v.strikes.end() && strike->bare_value)
        set_decimal_from_double(data, "Strike", strike->bare_value->to_double());

    option_block::write_option_data(data.child("OptionData"), v);
}

void write_credit_linked_swap(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<credit_linked_swap_instrument>(v);
    auto data = trade.child("CreditLinkedSwapData");
    data.set_text("CreditCurveId", fact.credit_curve_id);
    data.set_optional_flag("SettlesAccrual", fact.settles_accrual);
    set_optional_decimal_from_double(data, "FixedRecoveryRate", fact.recovery_rate);
    data.set_optional_text("DefaultPaymentTime", fact.default_payment_time);
    write_payment_group(data, "IndependentPayments", v, "independent");
    write_payment_group(data, "ContingentPayments", v, "contingent");
    write_payment_group(data, "DefaultPayments", v, "default");
    write_payment_group(data, "RecoveryPayments", v, "recovery");
}

void write_synthetic_cdo(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<synthetic_cdo_instrument>(v);
    auto data = trade.child("CdoData");
    data.set_text("Qualifier", fact.qualifier);
    if (fact.protection_start_date.ok())
        data.set_text("ProtectionStart",
                      credit_instrument_projection_to_ore_date(fact.protection_start_date));
    set_optional_date(data, "UpfrontDate", fact.upfront_date);
    if (fact.upfront_fee)
        data.set_text("UpfrontFee", fact.upfront_fee->to_string());
    write_accrual(data,
                  fact.settles_accrual,
                  fact.rebates_accrual,
                  fact.pays_at_default_time,
                  fact.protection_payment_time);
    set_decimal_from_double(data, "AttachmentPoint", fact.attachment_point);
    set_decimal_from_double(data, "DetachmentPoint", fact.detachment_point);
    set_optional_decimal_from_double(data, "FixedRecoveryRate", fact.recovery_rate);
    write_first_leg(data, "LegData", v, "premium");
    if (!v.constituents.empty())
        write_basket(data.child("BasketData"), v);
}

void write_rpa(record_writer& trade, const credit_instrument_data& v) {
    const auto& fact = fact_of<risk_participation_agreement_instrument>(v);
    auto data = trade.child("RiskParticipationAgreementData");
    set_decimal_from_double(data, "ParticipationRate", fact.participation_rate);
    data.set_text("CreditCurveId", fact.credit_curve_id);
    if (fact.issuer_id)
        data.set_text("IssuerId", *fact.issuer_id);
    if (fact.settles_accrual)
        data.set_text("SettlesAccrual",
                      credit_instrument_projection_flag_text(*fact.settles_accrual));
    if (v.instrument.start_date)
        data.set_text("ProtectionStart",
                      credit_instrument_projection_to_ore_date(*v.instrument.start_date));
    if (v.instrument.maturity_date)
        data.set_text("ProtectionEnd",
                      credit_instrument_projection_to_ore_date(*v.instrument.maturity_date));
    set_optional_decimal_from_double(data, "FixedRecoveryRate", fact.recovery_rate);

    auto protection_fee = data.child("ProtectionFee");
    for (const auto* row : legs_in_group(v, "protection_fee"))
        append_leg_writer(protection_fee, "LegData", v, *row);

    auto underlying = data.child("Underlying");
    for (const auto* row : legs_in_group(v, "underlying"))
        append_leg_writer(underlying, "LegData", v, *row);
    if (fact.naked_option)
        underlying.set_text("NakedOption",
                            credit_instrument_projection_flag_text(*fact.naked_option));
    if (!v.options.empty())
        option_block::write_option_data(underlying.child("OptionData"), v);
}

}

auto read_credit_instrument(xml::record_reader trade, std::string_view trade_type)
    -> std::optional<trading::domain::trade_instrument> {
    if (!is_credit_trade_type(trade_type))
        return std::nullopt;

    if (trade_type == "CreditDefaultSwap")
        return trading::domain::trade_instrument{read_cds(trade)};
    if (trade_type == "IndexCreditDefaultSwap")
        return trading::domain::trade_instrument{read_index_cds(trade)};
    if (trade_type == "IndexCreditDefaultSwapOption")
        return trading::domain::trade_instrument{read_index_cds_option(trade)};
    if (trade_type == "CreditLinkedSwap")
        return trading::domain::trade_instrument{read_credit_linked_swap(trade)};
    if (trade_type == "SyntheticCDO")
        return trading::domain::trade_instrument{read_synthetic_cdo(trade)};
    return trading::domain::trade_instrument{read_rpa(trade)};
}

auto write_credit_instrument(xml::record_writer& trade,
                             std::string_view trade_type,
                             const trading::domain::trade_instrument& instrument,
                             const portfolio_reference_curves&) -> bool {
    const auto* v = std::get_if<credit_instrument_data>(&instrument);
    if (v == nullptr || !is_credit_trade_type(trade_type))
        return false;

    if (trade_type == "CreditDefaultSwap")
        write_cds(trade, *v);
    else if (trade_type == "IndexCreditDefaultSwap")
        write_index_cds(trade, *v);
    else if (trade_type == "IndexCreditDefaultSwapOption")
        write_index_cds_option(trade, *v);
    else if (trade_type == "CreditLinkedSwap")
        write_credit_linked_swap(trade, *v);
    else if (trade_type == "SyntheticCDO")
        write_synthetic_cdo(trade, *v);
    else
        write_rpa(trade, *v);
    return true;
}

}
