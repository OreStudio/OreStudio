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
#ifndef ORES_DIFF_DOMAIN_DIFF_RESULT_HPP
#define ORES_DIFF_DOMAIN_DIFF_RESULT_HPP

#include "ores.diff/domain/diff_entry.hpp"
#include <string_view>
#include <vector>

namespace ores::diff::domain {

/**
 * @brief Every field that changed between two versions.
 *
 * Every field that changed between two versions, in the order the engine found
 * them. It is never stored: it is a member of a history version and reaches a
 * client inside a history response.
 */
struct diff_result final {
    /**
     * @brief One entry per changed, added or removed field, in the order the engine found them.
     * Empty when the two versions are identical, which is how a caller tells "nothing changed" from
     * "nothing compared".
     */
    std::vector<ores::diff::domain::diff_entry> entries;

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
    friend bool operator==(const diff_result&, const diff_result&) = default;
};

/**
 * @brief Dispatch-key identifier for diff_result, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const diff_result&) {
    return "ores.diff.diff_result";
}

}

#endif
