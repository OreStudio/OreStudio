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
#ifndef ORES_MARKETDATA_API_DOMAIN_FX_FORWARD_PUBLISH_HPP
#define ORES_MARKETDATA_API_DOMAIN_FX_FORWARD_PUBLISH_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The subject and provenance of one FX forward set publish.
 *
 * The detail row of one marketdata.fx_forward_publish approval request: the FX
 * forward set the request authorises, where each of its numbers came from, and the
 * record of the act once the set is published.
 *
 * A forward set is derived data, so it belongs to a party, as a funding curve does.
 * The subject is the party, the pair and the as-of instant. The set is computed
 * when the request is raised, so the person who decides reads the numbers they
 * approve, and the apply publishes exactly those numbers. The tenors are the
 * fx_forward_publish_point rows of the publish, and the spot observations it read
 * are its fx_forward_publish_spot rows: one for a USD pair, two for a cross.
 *
 * The provenance is on this row: the publish of each funding curve the set
 * discounts with, the version of the pair's convention, the calendars its dates
 * roll on, and the method that combined them. Each names its own record, so a
 * reader follows a number back to the market and to the curves. A curve
 * publish is named and not copied, so the withdrawal of a curve shows on every set
 * built from it without a write here.
 *
 * published_at and published_by are the publication record, empty while the
 * request waits and written in one act when the approved set is published.
 * request_id is the natural key, so a request authorises one publish.
 */
struct fx_forward_publish final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The approval request this record is the subject of.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The party the set belongs to. Two parties that publish the same pair at the same
     * instant publish two sets, each from its own curves.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The currency pair, in the form of its convention, such as EUR/USD.
     */
    std::string pair_code;

    /**
     * @brief The instant the set reads the market at: the latest spot at or before it, and the
     * funding curves published at or before it.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The spot date the forwards are measured from, rolled on the pair's calendars.
     */
    std::chrono::year_month_day spot_date;

    /**
     * @brief The spot rate of the pair the set used. For a cross it is derived from the two USD
     * legs its spot rows name.
     */
    double spot_rate{};

    /**
     * @brief The curve_publish of the base currency's funding curve the set discounts with.
     */
    boost::uuids::uuid base_curve_publish_id;

    /**
     * @brief The curve_publish of the quote currency's funding curve the set discounts with.
     */
    boost::uuids::uuid quote_curve_publish_id;

    /**
     * @brief The version of the pair's currency_pair_convention the set read its spot lag, roll and
     * pip factor from.
     */
    int convention_version = 0;

    /**
     * @brief The calendars the dates roll on, comma separated, in the order the convention names
     * them.
     */
    std::string calendar_codes;

    /**
     * @brief How the set combined its inputs. covered_interest_parity is F = S x DF_base /
     * DF_quote, each discount factor measured from the spot date.
     */
    std::string method_code;

    /**
     * @brief The username of the account that applied the request, or empty while it waits.
     */
    std::string published_by = "";

    /**
     * @brief When the set was published, or null while the request waits.
     */
    std::optional<std::chrono::system_clock::time_point> published_at;

    /**
     * @brief Username of the person who last modified this FX forward publish.
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
    friend bool operator==(const fx_forward_publish&, const fx_forward_publish&) = default;
};

/**
 * @brief Dispatch-key identifier for fx_forward_publish, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const fx_forward_publish&) {
    return "ores.marketdata.fx_forward_publish";
}

}

#endif
