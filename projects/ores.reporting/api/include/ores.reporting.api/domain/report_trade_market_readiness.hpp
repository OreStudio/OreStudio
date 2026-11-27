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
#ifndef ORES_REPORTING_API_DOMAIN_REPORT_TRADE_MARKET_READINESS_HPP
#define ORES_REPORTING_API_DOMAIN_REPORT_TRADE_MARKET_READINESS_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string_view>

namespace ores::reporting::domain {

/**
 * @brief Whether a run's market answered every address one trade names.
 *
 * One row per trade the run's scope holds, written before the engine sees any
 * work.
 *
 * A trade is complete when every oresmd address its instruments name is answered
 * at the run's as-of, and incomplete otherwise. An incomplete trade is left out
 * of the run rather than sent to the engine, so this row is the run's account of
 * which trades it lost.
 *
 * The addresses that make an incomplete trade incomplete are the run's own rows
 * in report_trade_market_gap, one per address the market did not answer. They
 * are written from the same decision that wrote this row, so the count here and
 * the rows there are two readings of one fact.
 */
struct report_trade_market_readiness final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this readiness row.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party that owns the run.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The run this row states the readiness of.
     */
    boost::uuids::uuid report_instance_id;

    /**
     * @brief The engine's own identifier for the trade. It is text because the engine names trades
     * with strings, not with uuids.
     */
    std::string trade_id;

    /**
     * @brief Whether every address the trade names was answered, which is what decides whether the
     * run submits it.
     */
    bool complete = false;

    /**
     * @brief How many of the addresses it names went unanswered. It is stored so that a reader of
     * an incomplete trade sees how short the market was without reading the address detail, and a
     * complete trade states zero.
     */
    int gap_count = 0;

    /**
     * @brief Username of the person who last modified this trade readiness.
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
    friend bool operator==(const report_trade_market_readiness&,
                           const report_trade_market_readiness&) = default;
};

/**
 * @brief Dispatch-key identifier for report_trade_market_readiness, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const report_trade_market_readiness&) {
    return "ores.reporting.report_trade_market_readiness";
}

}

#endif
