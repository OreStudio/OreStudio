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
#ifndef ORES_SYNTHETIC_API_DOMAIN_MARKET_DATA_PROVIDER_HPP
#define ORES_SYNTHETIC_API_DOMAIN_MARKET_DATA_PROVIDER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::synthetic::domain {

/**
 * @brief Simulated market data provider a synthetic collection stands for: a vendor, an exchange, a
 * broker, or a reference set.
 *
 * A catalogue of the providers whose market data the synthetic feeds simulate.
 * A system-scope collection names the provider it stands for, so the installation
 * can run a simulated Bloomberg, Refinitiv or CME once and every tenant reads it
 * as that provider's data. Every collection names its provider, so every feed has
 * one. A reference set such as the ORE examples is a provider of its own.
 *
 * Every provider here is simulated, and the synthetic screens show it without a
 * mark, because everything on them is synthetic. Every source name a provider's feed
 * publishes on starts synthetic.<provider code>., so a simulated feed is never read
 * as the real one. A real provider integration, when one comes, belongs in market data
 * and not in this catalogue.
 *
 * Scoped to ores.synthetic (not ores.refdata) because only synthetic
 * generation uses it today, following the yield_curve_process_type
 * precedent. Managed by the system tenant.
 */
struct market_data_provider final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique provider code: lower case letters, digits, underscores and hyphens, starting
     * with a letter. It is the provider segment of a source name.
     *
     * Examples: 'ore', 'bloomberg', 'refinitiv', 'cme'.
     */
    std::string code;

    /**
     * @brief The provider's name as a person knows it, without the synthetic mark, which a screen
     * adds.
     */
    std::string name;

    /**
     * @brief What sort of provider it is: data_vendor, exchange, broker or reference.
     */
    std::string kind;

    /**
     * @brief What the provider's feeds simulate, in a sentence or two: the market the provider
     * stands for and what a person gets by running its feeds.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief Username of the person who last modified this market data provider.
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
    friend bool operator==(const market_data_provider&, const market_data_provider&) = default;
};

/**
 * @brief Dispatch-key identifier for market_data_provider, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const market_data_provider&) {
    return "ores.synthetic.market_data_provider";
}

}

#endif
