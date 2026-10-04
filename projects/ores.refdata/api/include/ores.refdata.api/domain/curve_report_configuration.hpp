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
#ifndef ORES_REFDATA_API_DOMAIN_CURVE_REPORT_CONFIGURATION_HPP
#define ORES_REFDATA_API_DOMAIN_CURVE_REPORT_CONFIGURATION_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/nil_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The grids one ORE volatility entry is reported on.
 *
 * The Report element of a volatility or correlation entry: which grids the
 * built surface is reported on, and the points of each. Ten section types carry
 * the same element, so it is one table keyed to the entry. A row exists exactly
 * when the entry writes the element. The grids are comma separated lists in
 * ORE's own text, held as written.
 */
struct curve_report_configuration final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate key for the row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the document this row belongs to. Set from the session that writes
     * the document, and enforced by row level security, so a party sees only its own configuration.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The curve entry this row belongs to.
     */
    boost::uuids::uuid curve_definition_id;

    /**
     * @brief Whether the surface is reported on a delta grid, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> report_on_delta_grid;

    /**
     * @brief Whether the surface is reported on a moneyness grid, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> report_on_moneyness_grid;

    /**
     * @brief Whether the surface is reported on a strike grid, as the ORE boolean the document
     * wrote.
     */
    std::optional<std::string> report_on_strike_grid;

    /**
     * @brief Whether the surface is reported on a strike spread grid, as the ORE boolean the
     * document wrote.
     */
    std::optional<std::string> report_on_strike_spread_grid;

    /**
     * @brief The deltas of the delta grid.
     */
    std::optional<std::string> deltas;

    /**
     * @brief The moneyness levels of the moneyness grid.
     */
    std::optional<std::string> moneyness;

    /**
     * @brief The strikes of the strike grid.
     */
    std::optional<std::string> strikes;

    /**
     * @brief The strike spreads of the strike spread grid.
     */
    std::optional<std::string> strike_spreads;

    /**
     * @brief The expiries reported on.
     */
    std::optional<std::string> expiries;

    /**
     * @brief The pillar dates reported on.
     */
    std::optional<std::string> pillar_dates;

    /**
     * @brief The underlying tenors reported on.
     */
    std::optional<std::string> underlying_tenors;

    /**
     * @brief The expiry a continuation contract is reported at.
     */
    std::optional<std::string> continuation_expiry;

    /**
     * @brief Username of the person who last modified this curve report configuration.
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
    friend bool operator==(const curve_report_configuration&,
                           const curve_report_configuration&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_report_configuration, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_report_configuration&) {
    return "ores.refdata.curve_report_configuration";
}

}

#endif
