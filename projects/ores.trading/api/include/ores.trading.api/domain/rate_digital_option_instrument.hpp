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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TRADING_API_DOMAIN_RATE_DIGITAL_OPTION_INSTRUMENT_HPP
#define ORES_TRADING_API_DOMAIN_RATE_DIGITAL_OPTION_INSTRUMENT_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief Rate digital option facts.
 *
 * Represents a RateDigitalOption: a fixed payout if an interest rate fixing
 * ends above (call) or below (put) the strike. The OptionData block is an
 * instrument option row.
 *
 * This row is the family's fact table, not its identity. The header,
 * ores.trading.rate_instruments, holds the trade type code, the party, the
 * instrument's start and maturity dates and its description; this table holds
 * only the product's own fields, and joins the header by trade_id.
 */
struct rate_digital_option_instrument final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The trade this instrument belongs to, and the instrument's own key.
     *
     * The trade id identifies both the trade and its instrument, so the instrument carries no
     * identity of its own: this column is the key, and the trade it names gives the instrument its
     * scope.
     */
    boost::uuids::uuid trade_id;

    /**
     * @brief The activity that wrote this version.
     */
    boost::uuids::uuid trade_activity_id;

    /**
     * @brief The index that fixes, as Index states it, stored as its address.
     */
    std::string index_name;

    /**
     * @brief The rate the fixing is compared with, as Strike states it; it may be negative, as a
     * rate may.
     */
    ores::utility::decimal::decimal strike;

    /**
     * @brief The amount paid if the option ends in the money, as PayoffAmount states it.
     */
    ores::utility::decimal::decimal payoff_amount;

    /**
     * @brief The currency of the payout, as PayoffCurrency states it.
     */
    std::string payoff_currency;

    /**
     * @brief The date the index fixes, as FixingDate states it.
     */
    std::chrono::year_month_day fixing_date;

    /**
     * @brief The date the payout is made, as PaymentDate states it; absent, ORE pays on the fixing
     * date.
     */
    std::optional<std::chrono::year_month_day> payment_date;

    /**
     * @brief Username of the person who last modified this rate digital option instrument.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const rate_digital_option_instrument&,
                           const rate_digital_option_instrument&) = default;
};

/**
 * @brief The canonical text of the members of rate_digital_option_instrument the customer agreed
 * to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const rate_digital_option_instrument& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "index_name", v.index_name);
    ores::utility::digest::append_member(out, "strike", v.strike);
    ores::utility::digest::append_member(out, "payoff_amount", v.payoff_amount);
    ores::utility::digest::append_member(out, "payoff_currency", v.payoff_currency);
    ores::utility::digest::append_member(out, "fixing_date", v.fixing_date);
    ores::utility::digest::append_member(out, "payment_date", v.payment_date);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for rate_digital_option_instrument, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const rate_digital_option_instrument&) {
    return "ores.trading.rate_digital_option_instrument";
}

}

#endif
