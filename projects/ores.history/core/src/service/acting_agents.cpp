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
#include "ores.history.core/service/acting_agents.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.logging/logger.hpp"
#include <chrono>
#include <exception>
#include <string>
#include <unordered_map>

namespace ores::history::service {

namespace {

auto& acting_agents_lg() {
    using namespace ores::logging;
    static auto instance = make_logger("ores.history.service.acting_agents");
    return instance;
}

}

void mark_acting_agents(const ores::database::context& ctx,
                        const versioned_table& source,
                        const std::string& key,
                        std::vector<messaging::entity_history_version>& versions) {
    if (versions.empty() || key.empty())
        return;
    const auto sql = "select (extract(epoch from valid_from) * 1000000)::bigint::text, "
                     "acting_agent from \"" +
                     source.table + "\" where \"" + source.key_column + "\" = $1" +
                     (source.key_is_uuid ? "::uuid" : "") + " and acting_agent <> ''";
    try {
        const auto rows = ores::database::query_builder::execute_parameterized_multi_column_query(
            ctx, sql, {key}, acting_agents_lg(), "Reading the agents that wrote a record");
        std::unordered_map<long long, std::string> by_instant;
        for (const auto& row : rows)
            if (row.size() == 2 && row[0] && row[1])
                by_instant.emplace(std::stoll(*row[0]), *row[1]);
        if (by_instant.empty())
            return;
        for (auto& v : versions) {
            const auto at = std::chrono::duration_cast<std::chrono::microseconds>(
                                v.recorded_at.time_since_epoch())
                                .count();
            if (const auto it = by_instant.find(at); it != by_instant.end())
                v.acting_agent = it->second;
        }
    } catch (const std::exception& e) {
        using namespace ores::logging;
        ORES_LOG_SEV(acting_agents_lg(), warn)
            << "A record's agents could not be read from " << source.table << ": " << e.what();
    }
}

}
