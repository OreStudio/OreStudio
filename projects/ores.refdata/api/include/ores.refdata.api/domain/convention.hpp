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
#ifndef ORES_REFDATA_API_DOMAIN_CONVENTION_HPP
#define ORES_REFDATA_API_DOMAIN_CONVENTION_HPP

#include "ores.dq.api/domain/audit_record.hpp"
#include "ores.refdata.api/domain/convention_identity.hpp"
#include "ores.refdata.api/domain/convention_payload.hpp"
#include <chrono>
#include <span>
#include <string_view>
#include <type_traits>

namespace ores::refdata::domain {

/**
 * @brief The central row every convention kind shares.
 *
 * The central row of the atomised convention. Every convention kind -- a swap, a
 * deposit, a tenor basis swap -- is one row here plus the parts its family names.
 * The row carries the key, the party that owns the row, the kind the row is, and
 * the name it carries in the source document.
 *
 * The name is the identifier the source document gives the convention, the <Id>
 * element of that document. It is kept so a round trip through the ORE XML is
 * lossless, and it is part of the convention's identity: the party, the kind and
 * the name together are what make a convention the one a caller means. Two kinds
 * may hold the same name, because the kind is part of that identity.
 *
 * A convention's kind-specific fields live in that kind's parts, reached from
 * this row by its key. The central row carries no kind-specific column.
 */
struct convention final {
    convention_identity identity;
    convention_payload payload;
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
    friend bool operator==(const convention&, const convention&) = default;
};

/**
 * @brief Names the members of convention that are economic.
 *
 * The economic digest reads this at compile time and decides nothing by a
 * field's name. A type without this overload cannot be digested.
 */
[[nodiscard]] constexpr std::span<const std::string_view>
economic_fields_of(std::type_identity<convention>) {
    return {};
}

/**
 * @brief Dispatch-key identifier for convention, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const convention&) {
    return "ores.refdata.convention";
}

}

#endif
