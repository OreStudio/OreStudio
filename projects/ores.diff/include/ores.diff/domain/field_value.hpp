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
#ifndef ORES_DIFF_DOMAIN_FIELD_VALUE_HPP
#define ORES_DIFF_DOMAIN_FIELD_VALUE_HPP

#include <string>
#include <string_view>

namespace ores::diff::domain {

/**
 * @brief One field of a version, rendered as display text.
 *
 * One field of an entity version, rendered as the text a person reads. It is
 * never stored: it is a member of a history version and reaches a client inside a
 * history response.
 *
 * Generating this model overwrites the hand-written
 * projects/ores.diff/include/ores.diff/domain/field_value.hpp, which is what it
 * is for. The hand-written header and this model state the same two members, and
 * the point of moving the declaration here is that the C++ and the TypeScript
 * twins are then generated from one source rather than written twice.
 */
struct field_value final {
    /**
     * @brief Display name of the field, e.g. "ISO Code".
     */
    std::string name;

    /**
     * @brief The field's value rendered as a display string.
     */
    std::string value;

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
    friend bool operator==(const field_value&, const field_value&) = default;
};

/**
 * @brief Dispatch-key identifier for field_value, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const field_value&) {
    return "ores.diff.field_value";
}

}

#endif
