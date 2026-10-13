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
#ifndef ORES_DQ_API_DOMAIN_SYNTHETIC_MARKET_DATA_PROVIDER_HPP
#define ORES_DQ_API_DOMAIN_SYNTHETIC_MARKET_DATA_PROVIDER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <string>
#include <string_view>

namespace ores::dq::domain {

/**
 * @brief Market Data Provider artefacts - the simulated providers staged for the system_core bundle
 *
 * Market Data Provider artefacts: one row per simulated market data provider
 * (Bloomberg, Refinitiv, the exchanges, a broker, and the ORE examples),
 * staged for the system_core bundle and published into
 * ores_synthetic_market_data_providers_tbl in the system tenant. Every
 * provider is simulated; see the synthetic catalogue's own model for the
 * synthetic mark.
 */
struct synthetic_market_data_provider final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The provider code, the provider segment of a source name.
     */
    std::string code;

    /**
     * @brief The provider as a person knows it, without the synthetic mark.
     */
    std::string name;

    /**
     * @brief data_vendor, exchange, broker or reference.
     */
    std::string kind;

    /**
     * @brief What the provider publishes, in a sentence.
     */
    std::string description;

    /**
     * @brief Order for display.
     */
    int display_order = 0;

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
    friend bool operator==(const synthetic_market_data_provider&,
                           const synthetic_market_data_provider&) = default;
};

/**
 * @brief Dispatch-key identifier for synthetic_market_data_provider, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const synthetic_market_data_provider&) {
    return "ores.dq.synthetic_market_data_provider";
}

}

#endif
