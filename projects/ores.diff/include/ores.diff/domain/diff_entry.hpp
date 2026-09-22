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
#ifndef ORES_DIFF_DOMAIN_DIFF_ENTRY_HPP
#define ORES_DIFF_DOMAIN_DIFF_ENTRY_HPP

#include "ores.diff/domain/diff_span.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::diff::domain {

/**
 * @brief One changed field, with the spans that changed inside it.
 *
 * One field that changed between two versions, with the byte ranges that changed
 * inside each value. It is never stored: it is a member of a diff result and
 * reaches a client inside a history response.
 *
 * The two span lists are members of this record rather than types of their own
 * because a span means nothing apart from the value it points into: an offset
 * into one string is not an offset into another.
 */
struct diff_entry final {
    /**
     * @brief Display name of the field, e.g. "ISO Code".
     */
    std::string field_name;

    /**
     * @brief The previous version's rendered value; empty when the field was added.
     */
    std::string old_value;

    /**
     * @brief The current version's rendered value; empty when the field was removed.
     */
    std::string new_value;

    /**
     * @brief Byte ranges into old_value that changed relative to new_value. Empty only when the
     * field was added, because old_value is already empty. A value that changed entirely still gets
     * one span covering the whole string, not an empty list.
     */
    std::vector<ores::diff::domain::diff_span> old_spans;

    /**
     * @brief Byte ranges into new_value that changed relative to old_value. Empty only when the
     * field was removed, because new_value is already empty. A value that changed entirely still
     * gets one span covering the whole string, not an empty list.
     */
    std::vector<ores::diff::domain::diff_span> new_spans;

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
    friend bool operator==(const diff_entry&, const diff_entry&) = default;
};

/**
 * @brief Dispatch-key identifier for diff_entry, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const diff_entry&) {
    return "ores.diff.diff_entry";
}

}

#endif
