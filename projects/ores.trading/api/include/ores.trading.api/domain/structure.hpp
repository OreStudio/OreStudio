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
#ifndef ORES_TRADING_API_DOMAIN_STRUCTURE_HPP
#define ORES_TRADING_API_DOMAIN_STRUCTURE_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief The immutable identity of a deal assembled from several legs.
 *
 * The immutable identity of a deal assembled from several trades. It holds
 * what never changes for the deal's life: the party, the counterparty, the
 * kind of binding, the template it was shaped by, and its parent when it is
 * itself a leg of a larger deal.
 *
 * It is the anchor of the composition ladder. The legs are not owned
 * children: a trade points at the structure that holds it, so the same trade
 * can be unlinked from one deal and linked to another without being
 * rewritten. The economics live on the legs, and the structure's internal
 * version is read from them rather than stored beside them.
 *
 * A structure nests one level at most, because a deal inside a deal inside a
 * deal has no confirmation to hang on. The insert trigger enforces it.
 */
struct structure final {
    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The structure id: the firm's own identifier of the deal.
     */
    boost::uuids::uuid id;

    /**
     * @brief The firm's legal entity that is party to the deal. Every leg shares it, and the pin on
     * the leg is what keeps the sharing true.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The counterparty the deal is struck with. Every leg shares it.
     */
    boost::uuids::uuid counterparty_id;

    /**
     * @brief The rung of the composition ladder the deal sits on, which is what decides whether it
     * confirms as one document or trade by trade.
     */
    std::string kind;

    /**
     * @brief The template that shaped the deal, when one did. A package has none, because it binds
     * its legs to nothing and no template describes it.
     */
    std::string template_code;

    /**
     * @brief The structure this one is a leg of, when the deal nests. Null for a deal the customer
     * sees on its own.
     */
    boost::uuids::uuid parent_structure_id;

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
    friend bool operator==(const structure&, const structure&) = default;
};

/**
 * @brief Dispatch-key identifier for structure, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const structure&) {
    return "ores.trading.structure";
}

}

#endif
