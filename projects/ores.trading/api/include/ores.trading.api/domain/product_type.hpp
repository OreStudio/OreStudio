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
#ifndef ORES_TRADING_API_DOMAIN_PRODUCT_TYPE_HPP
#define ORES_TRADING_API_DOMAIN_PRODUCT_TYPE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One kind of instrument a desk can book.
 *
 * The catalogue of the instrument types a tenant can book, keyed by the code
 * ORE uses. One row per kind: a BondOption, a CrossCurrencySwap, a
 * ScriptedTrade.
 *
 * The row is the golden source for what a product type is. It carries the name a
 * desk uses, the description and the source that description came from, the
 * product family that names its contract shape, the construction method that says
 * how the payoff is expressed, and the instrument table that holds a trade of the
 * kind. The markets it references are a junction, because a kind may reference
 * more than one.
 *
 * The catalogue is declared once, in ores.trading.trade_type_catalogue, and
 * seeded into the system tenant. Every tenant reads those rows; a tenant does not
 * hold a product type of its own.
 */
struct product_type final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The ORE code, which is the identity.
     *
     * Examples: 'Swap', 'FxForward', 'CapFloor', 'Swaption'.
     */
    std::string code;

    /**
     * @brief The name a desk uses for the product.
     */
    std::string name;

    /**
     * @brief What the product is, in prose: its cashflows and when a desk uses it.
     */
    std::string description;

    /**
     * @brief The body that states the description, such as the ORE User Guide.
     */
    std::string source;

    /**
     * @brief Where in that body the description is stated, as a link.
     */
    std::string source_ref;

    /**
     * @brief The contract shape the product type takes. Null where the shape is not stated.
     */
    std::string product_family_code;

    /**
     * @brief How the payoff is expressed: native, scripted or container.
     */
    std::string construction;

    /**
     * @brief True when a trade of this type carries an option block.
     */
    bool has_options = false;

    /**
     * @brief True when a trade of this type carries an extension block.
     */
    bool has_extension = false;

    /**
     * @brief The instrument table that holds a trade of this type. Null when nothing holds it yet,
     * so no trade of the type can be imported.
     */
    std::string instrument_table;

    /**
     * @brief The order the catalogue lists the type in.
     */
    int display_order = 0;

    /**
     * @brief The knowledge note that describes the product, by document id.
     */
    std::string knowledge_note;

    /**
     * @brief Username of the person who last modified this product type.
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
    friend bool operator==(const product_type&, const product_type&) = default;
};

/**
 * @brief Dispatch-key identifier for product_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const product_type&) {
    return "ores.trading.product_type";
}

}

#endif
