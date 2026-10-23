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
#include "ores.testing/publish_helper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.logging/make_logger.hpp"
#include <string>
#include <vector>

namespace ores::testing {
namespace {

inline static std::string_view logger_name = "ores.testing.publish_helper";

static auto& lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

bool publish_dataset(const ores::database::context& ctx,
                     const std::string& tenant,
                     const std::string& dataset_code,
                     const std::string& publish_function) {
    // The dataset row and the function's result rows are cross-joined, so the
    // count is zero exactly when the tree carries no such dataset. The function
    // itself runs as a side effect whether or not it returns rows.
    const auto counts = ores::database::repository::execute_parameterized_string_query(
        ctx,
        "SELECT count(*)::text FROM ores_dq_datasets_tbl d, " + publish_function +
            "(d.id, $1::uuid) p WHERE d.code = $2"
            " AND d.valid_to = ores_utility_infinity_timestamp_fn()",
        {tenant, dataset_code},
        lg(),
        "Publishing " + dataset_code);

    return !counts.empty() && counts.front() != "0";
}

bool publish_with_dependencies(const ores::database::context& ctx,
                               const std::string& tenant,
                               const std::string& dataset_code) {
    const auto dependencies = ores::database::repository::execute_parameterized_string_query(
        ctx,
        "SELECT dependency_code FROM ores_dq_dataset_dependencies_tbl"
        " WHERE dataset_code = $1 AND tenant_id = ores_utility_system_tenant_id_fn()"
        " AND valid_to = ores_utility_infinity_timestamp_fn() ORDER BY dependency_code",
        {dataset_code},
        lg(),
        "Reading the dependencies of " + dataset_code);
    for (const auto& dependency : dependencies)
        if (!publish_with_dependencies(ctx, tenant, dependency))
            return false;

    // The artefact type names the subject, for example
    // reporting.v1.ops.publish_ore_parameters_from_dq, and the function is the
    // subject's component and operation: ores_reporting_publish_ore_parameters_from_dq_fn.
    const auto functions = ores::database::repository::execute_parameterized_string_query(
        ctx,
        "SELECT 'ores_' || split_part(t.target_subject, '.', 1) || '_' ||"
        " split_part(t.target_subject, '.', 4) || '_fn'"
        " FROM ores_dq_datasets_tbl d JOIN ores_dq_artefact_types_tbl t"
        " ON t.code = d.artefact_type AND t.tenant_id = d.tenant_id"
        " AND t.valid_to = ores_utility_infinity_timestamp_fn()"
        " WHERE d.code = $1 AND d.valid_to = ores_utility_infinity_timestamp_fn()"
        " AND t.target_subject IS NOT NULL",
        {dataset_code},
        lg(),
        "Reading the publish function of " + dataset_code);
    return !functions.empty() && publish_dataset(ctx, tenant, dataset_code, functions.front());
}

bool publish_refdata_dataset(const ores::database::context& ctx,
                             const std::string& tenant,
                             const std::string& dataset_code,
                             const std::string& entity) {
    return publish_dataset(
        ctx, tenant, dataset_code, "ores_refdata_publish_" + entity + "_from_dq_fn");
}

}
