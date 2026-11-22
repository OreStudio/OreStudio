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
#include "ores.trading.core/repository/script_library_version_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.database/query_builder/execute.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.trading.core/repository/script_calibration_entity.hpp"
#include "ores.trading.core/repository/script_calibration_mapper.hpp"
#include "ores.trading.core/repository/script_calibration_strike_entity.hpp"
#include "ores.trading.core/repository/script_calibration_strike_mapper.hpp"
#include "ores.trading.core/repository/script_coarsening_entity.hpp"
#include "ores.trading.core/repository/script_coarsening_mapper.hpp"
#include "ores.trading.core/repository/script_entity.hpp"
#include "ores.trading.core/repository/script_mapper.hpp"
#include "ores.trading.core/repository/script_result_entity.hpp"
#include "ores.trading.core/repository/script_result_mapper.hpp"
#include "ores.trading.core/repository/script_variant_entity.hpp"
#include "ores.trading.core/repository/script_variant_mapper.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <iterator>
#include <set>
#include <string>
#include <tuple>

namespace ores::trading::repository {

namespace query_builder = ores::database::query_builder;
using namespace ores::database::query_builder::literals;

namespace {

/// The rows beneath one version of a script, in one of the script's child tables.
template <typename Entity, typename Domain, typename Mapper>
std::vector<Domain> rows_of_version(const ores::database::context& ctx,
                                    const messaging::script_pin& pin,
                                    ores::logging::logger_t& lg,
                                    const char* what) {
    const auto tenant = boost::uuids::to_string(pin.tenant_id);
    const auto query =
        query_builder::read<std::vector<Entity>> |
        query_builder::where("tenant_id"_c == tenant && "script_name"_c == pin.name &&
                             "script_version"_c == pin.version);
    return query_builder::execute_read_query<Entity, Domain>(
        ctx, query, [](const auto& entities) { return Mapper::map(entities); }, lg, what);
}

template <typename T>
void append(std::vector<T>& to, std::vector<T> from) {
    to.insert(to.end(), std::make_move_iterator(from.begin()), std::make_move_iterator(from.end()));
}

}

void script_library_version_repository::read(const context& ctx,
                                             const std::vector<messaging::script_pin>& pins,
                                             messaging::export_script_library_response& out) {
    std::set<std::tuple<boost::uuids::uuid, std::string, int>> seen;
    for (const auto& pin : pins) {
        if (!seen.emplace(pin.tenant_id, pin.name, pin.version).second)
            continue;

        const auto tenant = boost::uuids::to_string(pin.tenant_id);
        const auto query = query_builder::read<std::vector<script_entity>> |
                           query_builder::where("tenant_id"_c == tenant && "name"_c == pin.name &&
                                                "version"_c == pin.version) |
                           query_builder::limit(1);
        auto scripts = query_builder::execute_read_query<script_entity, domain::script>(
            ctx,
            query,
            [](const auto& entities) { return script_mapper::map(entities); },
            lg(),
            "Reading a script at a stated version.");
        if (scripts.empty()) {
            out.missing.push_back(pin);
            continue;
        }
        append(out.scripts, std::move(scripts));

        append(
            out.variants,
            rows_of_version<script_variant_entity, domain::script_variant, script_variant_mapper>(
                ctx, pin, lg(), "Reading the variants of a script version."));
        append(out.results,
               rows_of_version<script_result_entity, domain::script_result, script_result_mapper>(
                   ctx, pin, lg(), "Reading the results of a script version."));
        append(out.calibrations,
               rows_of_version<script_calibration_entity,
                               domain::script_calibration,
                               script_calibration_mapper>(
                   ctx, pin, lg(), "Reading the calibrations of a script version."));
        append(out.calibration_strikes,
               rows_of_version<script_calibration_strike_entity,
                               domain::script_calibration_strike,
                               script_calibration_strike_mapper>(
                   ctx, pin, lg(), "Reading the calibration strikes of a script version."));
        append(out.coarsenings,
               rows_of_version<script_coarsening_entity,
                               domain::script_coarsening,
                               script_coarsening_mapper>(
                   ctx, pin, lg(), "Reading the coarsenings of a script version."));
    }
}

}
