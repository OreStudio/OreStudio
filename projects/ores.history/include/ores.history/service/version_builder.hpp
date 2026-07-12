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
#ifndef ORES_HISTORY_SERVICE_VERSION_BUILDER_HPP
#define ORES_HISTORY_SERVICE_VERSION_BUILDER_HPP

#include "ores.diff/engine/compare.hpp"
#include "ores.history/messaging/history_protocol.hpp"
#include <chrono>
#include <concepts>
#include <vector>

namespace ores::history::service {

/**
 * @brief The shape every domain version type must have to feed
 * build_entity_history_versions: a modified_by actor and a
 * recorded_at timestamp, alongside whatever domain fields its
 * render function reads.
 */
template <typename T>
concept history_version_source = requires(const T& v) {
    { v.modified_by } -> std::convertible_to<std::string>;
    { v.recorded_at } -> std::convertible_to<std::chrono::system_clock::time_point>;
};

/**
 * @brief Builds the entity_history_version list a history_provider
 * returns, from a repository's newest-first version list and that
 * entity's field-mapper render function.
 *
 * Shared, mechanical glue every per-entity history_provider needs:
 * render each version's fields, diff each against the next-older
 * version (empty diff for the oldest), and number versions
 * descending from @p versions.size() down to 1, matching the
 * newest-first repository ordering every *_repository::read_all(id)
 * uses.
 *
 * @param versions Domain version rows for one entity, newest first
 * (repository read_all(id) ordering: version DESC).
 * @param render   The entity's codegen'd render_{entity}_fields().
 */
template <history_version_source T, typename RenderFn>
[[nodiscard]] std::vector<messaging::entity_history_version>
build_entity_history_versions(const std::vector<T>& versions, RenderFn render) {
    const auto version_count = versions.size();

    std::vector<std::vector<ores::diff::domain::field_value>> fields;
    fields.reserve(version_count);
    for (const auto& v : versions)
        fields.push_back(render(v));

    std::vector<messaging::entity_history_version> result;
    result.reserve(version_count);
    for (std::size_t i = 0; i < version_count; ++i) {
        messaging::entity_history_version ehv;
        ehv.version = static_cast<int>(version_count - i);
        ehv.modified_by = versions[i].modified_by;
        ehv.recorded_at = versions[i].recorded_at;
        if (i + 1 < version_count)
            ehv.changes = ores::diff::engine::compute(fields[i + 1], fields[i]);
        ehv.fields = std::move(fields[i]);
        result.push_back(std::move(ehv));
    }
    return result;
}

}

#endif
