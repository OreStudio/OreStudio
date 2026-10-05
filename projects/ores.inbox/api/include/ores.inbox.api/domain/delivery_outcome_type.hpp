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
#ifndef ORES_INBOX_API_DOMAIN_DELIVERY_OUTCOME_TYPE_HPP
#define ORES_INBOX_API_DOMAIN_DELIVERY_OUTCOME_TYPE_HPP

#include <string>
#include <string_view>

namespace ores::inbox::domain {

/**
 * @brief The closed set of outcomes of a notification delivery.
 *
 * The outcomes of one attempt to reach a person through one channel: 'pending',
 * 'delivered' and 'failed'.
 *
 * The set is closed, and the C++ enum domain::delivery_outcome holds the same
 * values. A code with no enum value cannot be read back, so the table is
 * immutable: it is seeded once and never changes at run time. Because it is
 * immutable, a delivery references it with a database foreign key rather than a
 * trigger check. The table has no tenant: the set is the same for every tenant.
 */
struct delivery_outcome_type final {
    /**
     * @brief Unique delivery outcome code.
     *
     * Examples: 'pending', 'delivered'.
     */
    std::string code;

    /**
     * @brief What the code means.
     */
    std::string description;

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
    friend bool operator==(const delivery_outcome_type&, const delivery_outcome_type&) = default;
};

/**
 * @brief Dispatch-key identifier for delivery_outcome_type, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const delivery_outcome_type&) {
    return "ores.inbox.delivery_outcome_type";
}

}

#endif
