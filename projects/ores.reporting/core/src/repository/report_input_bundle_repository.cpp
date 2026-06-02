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
#include "ores.reporting.core/repository/report_input_bundle_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.reporting.core/repository/report_input_bundle_entity.hpp"

namespace ores::reporting::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

void report_input_bundle_repository::create(
    context ctx, const report_input_bundle_entity& bundle) {

    BOOST_LOG_SEV(lg(), debug) << "Creating report_input_bundle: "
                               << bundle.id.value();

    const auto r = sqlgen::session(ctx.connection_pool())
        .and_then(insert(bundle));
    ensure_success(r, lg());

    BOOST_LOG_SEV(lg(), debug) << "report_input_bundle created.";
}

std::optional<report_input_bundle_entity>
report_input_bundle_repository::find_by_instance_id(
    context ctx, const std::string& instance_id) {

    BOOST_LOG_SEV(lg(), debug)
        << "Finding report_input_bundle for instance: " << instance_id;

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<report_input_bundle_entity>> |
        where("tenant_id"_c == tid &&
              "report_instance_id"_c == instance_id);

    auto results = execute_read_query<
        report_input_bundle_entity, report_input_bundle_entity>(
        ctx, query,
        [](const auto& entities) { return entities; },
        lg(), "Finding report_input_bundle by instance_id");

    if (results.empty()) return std::nullopt;
    return results.front();
}

}
