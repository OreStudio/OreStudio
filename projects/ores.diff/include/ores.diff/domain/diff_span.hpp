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
#ifndef ORES_DIFF_DOMAIN_DIFF_SPAN_HPP
#define ORES_DIFF_DOMAIN_DIFF_SPAN_HPP

#include <string_view>

namespace ores::diff::domain {

/**
 * @brief The byte range within a value that changed.
 *
 * A byte range within a rendered value. It is never stored: it is a member of a
 * diff entry and reaches a client inside a history response.
 *
 * Generating this model overwrites the hand-written
 * projects/ores.diff/include/ores.diff/domain/diff_span.hpp, so it is still a
 * probe rather than the real declaration of the type: the three sibling payloads
 * (field_value, diff_entry, diff_result) have no model yet, and the
 * hand-written TypeScript in wire-protocol/src/diff/protocol.ts is still what the
 * protocol imports. The shape itself is right, and that is what it proves.
 */
struct diff_span final {
    /**
     * @brief Where the changed run begins.
     */
    std::size_t offset;

    /**
     * @brief How many bytes it covers.
     */
    std::size_t length;

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
    friend bool operator==(const diff_span&, const diff_span&) = default;
};

/**
 * @brief Dispatch-key identifier for diff_span, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const diff_span&) {
    return "ores.diff.diff_span";
}

}

#endif
