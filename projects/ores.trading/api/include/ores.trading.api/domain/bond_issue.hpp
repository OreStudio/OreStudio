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
#ifndef ORES_TRADING_API_DOMAIN_BOND_ISSUE_HPP
#define ORES_TRADING_API_DOMAIN_BOND_ISSUE_HPP

#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The bond issue: the terms of one ISIN (security_id), the stable row every instrument of
 * the family references.
 *
 * One row per bond issue (ISIN), the stable row every instrument of the
 * family references. The columns map from bondReferenceDatum
 * (referencedata.xsd lines 97-113): security_id, issuer, the three curve
 * identifiers, the six fields beside them, issue date, settlement days and
 * calendar. Only IssuerId is required by that schema; every other
 * element is optional, so every other column is nullable and an absent
 * element is stored as NULL rather than as an empty string or a zero.
 *
 * The bond's coupon terms are not here. ORE states them on legData
 * alone -- bondReferenceDatum carries no coupon rate, coupon frequency,
 * day counter or currency -- and the security's legs are
 * bond_issue_leg and its children, so the leg holds them once and this
 * row is not a second copy.
 *
 * face_value is the issue's per-unit value and repeats the first leg's
 * notional. It stays because a row set that holds an issue and no legs
 * still has to export a leg, and it is what that leg's notional is built
 * from.
 */
struct bond_issue final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Workspace this record belongs to.
     *
     * Defaults to the Live workspace sentinel.
     */
    boost::uuids::uuid workspace_id = utility::uuid::live_workspace_id();

    /**
     * @brief UUID uniquely identifying this bond issue.
     *
     * Surrogate key of the issue row. Instrument rows and the issue's own child rows reference it;
     * the row is stable while instruments open against it are amended.
     */
    boost::uuids::uuid issue_id;

    /**
     * @brief ISIN or other security identifier of the issue.
     *
     * Unique among the current rows of a tenant (the partial unique index below). The deduplication
     * key of the migration: one issue row per ISIN, shared by every trade of it.
     */
    std::string security_id;

    /**
     * @brief Issuer of the bond.
     */
    std::string issuer;

    /**
     * @brief Face value per unit of the bond.
     */
    std::optional<ores::utility::decimal::decimal> face_value;

    /**
     * @brief Issue date of the bond (ISO 8601 date string).
     */
    std::optional<std::chrono::year_month_day> issue_date;

    /**
     * @brief Settlement days of the bond, a market convention of the issue.
     */
    int settlement_days = 0;

    /**
     * @brief Calendar the issue's dates are adjusted against, when the document states one at the
     * bond level rather than on a leg.
     */
    std::optional<std::string> calendar;

    /**
     * @brief Credit curve the document names for the issue.
     */
    std::optional<std::string> credit_curve_id;

    /**
     * @brief Reference curve the document names for the issue.
     */
    std::optional<std::string> reference_curve_id;

    /**
     * @brief Income curve the document names for the issue.
     */
    std::optional<std::string> income_curve_id;

    /**
     * @brief Credit group the document names for the issue.
     */
    std::optional<std::string> credit_group;

    /**
     * @brief Volatility curve the document names for the issue.
     */
    std::optional<std::string> volatility_curve_id;

    /**
     * @brief Price quote method the document states for the issue.
     */
    std::optional<std::string> price_quote_method;

    /**
     * @brief Price quote base value the document states for the issue.
     */
    std::optional<std::string> price_quote_base_value;

    /**
     * @brief Bond sub-type the document states for the issue.
     */
    std::optional<std::string> sub_type;

    /**
     * @brief The price type the document states.
     *
     * Soft FK to ores_trading_price_types_tbl: the values are the closed ORE bondPriceType set
     * (Clean, Dirty). PR 4 tightens the soft reference into a real foreign key.
     */
    std::optional<std::string> price_type;

    /**
     * @brief Username of the person who last modified this bond issue.
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
    friend bool operator==(const bond_issue&, const bond_issue&) = default;
};

/**
 * @brief Dispatch-key identifier for bond_issue, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bond_issue&) {
    return "ores.trading.bond_issue";
}

}

#endif
