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
#ifndef ORES_TRADING_API_DOMAIN_BOND_ISSUE_CALL_DATE_HPP
#define ORES_TRADING_API_DOMAIN_BOND_ISSUE_CALL_DATE_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/digest/canonical_form.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One call date of a callable bond issue, keyed to the issue.
 *
 * One row per call date of a callable bond issue, family-owned by the
 * issue. The row carries the date and the call's terms on it: the style,
 * price, price type and include-accrual flag of callableBondCallData
 * (instruments.xsd lines 2430-2494). ORE states each term as a list
 * beside the schedule and resolves it to one value per call date
 * (buildCallabilityData in callablebond.cpp); the row holds the
 * resolved value, so a call that reprices keeps its price.
 */
struct bond_issue_call_date final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief UUID of the issue this call date belongs to.
     *
     * The child rows are family-owned and ride the issue's scope.
     */
    boost::uuids::uuid issue_id;

    /**
     * @brief Ordinal of this call date within the issue's schedule.
     *
     * The ER names this member sequence; the org spells it sequence_number to keep the SQL keyword
     * out of the identifier. The emitted SQL shows whether the spelling matters.
     */
    int sequence_number{};

    /**
     * @brief The call date (ISO 8601 date string).
     */
    std::chrono::year_month_day call_date;

    /**
     * @brief The exercise style of the call on this date: Bermudan or American.
     *
     * ORE states the style as a list beside the schedule, each entry either positional or in force
     * from its startDate, and resolves it to one value per call date. The row holds that resolved
     * value, so the call on each date is stated once and the writer gives it back per date.
     */
    std::string style;

    /**
     * @brief The price the bond is called at on this date, as a fraction of par.
     */
    ores::utility::decimal::decimal price;

    /**
     * @brief Whether the call price is clean or dirty on this date.
     *
     * Soft FK to ores_trading_price_types_tbl: the values are the closed ORE bondPriceType set
     * (Clean, Dirty).
     */
    std::string price_type;

    /**
     * @brief Whether the call on this date pays the accrued coupon.
     *
     * ORE reads the flag as its loose bool, so the column holds the spelling the document states,
     * as bond_issue.credit_risk does.
     */
    std::string include_accrual;

    /**
     * @brief Username of the person who last modified this bond issue call date.
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
    friend bool operator==(const bond_issue_call_date&, const bond_issue_call_date&) = default;
};

/**
 * @brief The canonical text of the members of bond_issue_call_date the customer agreed to.
 *
 * Renders, in member order, the columns the model declares economic and each
 * group whose own model declares an economic field. The economic digest hashes
 * this text, so a member the customer did not agree to never moves it.
 */
[[nodiscard]] inline std::string canonical_form(const bond_issue_call_date& v) {
    std::string out = "{";
    ores::utility::digest::append_member(out, "call_date", v.call_date);
    ores::utility::digest::append_member(out, "style", v.style);
    ores::utility::digest::append_member(out, "price", v.price);
    ores::utility::digest::append_member(out, "price_type", v.price_type);
    ores::utility::digest::append_member(out, "include_accrual", v.include_accrual);
    out.push_back('}');
    return out;
}

/**
 * @brief Dispatch-key identifier for bond_issue_call_date, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_issue_call_date&) {
    return "ores.trading.bond_issue_call_date";
}

}

#endif
