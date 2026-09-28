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
#ifndef ORES_TRADING_API_DOMAIN_COMPOSITE_LEG_HPP
#define ORES_TRADING_API_DOMAIN_COMPOSITE_LEG_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.trading.api/domain/composite_leg_identity.hpp"
#include <boost/uuid/uuid.hpp>
#include <string>
#include <string_view>

namespace ores::trading::domain {

/**
 * @brief One constituent trade of a composite instrument basket.
 *
 * Child table of composite_instruments. Each row is one constituent trade
 * of a composite basket and names the parent through trade_id, with
 * leg_sequence giving the leg's 1-based ordinal inside the basket.
 *
 * The leg carries no economics of its own: the ORE schema states a basket as
 * a list of whole trades, so the constituent is a child row that points at
 * the parent and records which constituent it is. The table is bi-temporal
 * and audited, so the model takes the ordinary audited shape and needs no
 * shape flag.
 *
 * The row keeps its own id surrogate: composite_legs is one of the five
 * id-keyed tables the component's investigation names, and its
 * trade_id is a foreign key to the instrument rather than the row's
 * own key.
 */
struct composite_leg final {
    composite_leg_identity identity;

    /**
     * @brief UUID string or type name identifying the constituent trade.
     */
    std::string constituent_trade_id;

    ores::dq::domain::audit_record audit;
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
    friend bool operator==(const composite_leg&, const composite_leg&) = default;
};

/**
 * @brief Dispatch-key identifier for composite_leg, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const composite_leg&) {
    return "ores.trading.composite_leg";
}

}

#endif
