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
#include "ores.reporting.core/service/report_definition_template_service.hpp"

#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::reporting::service {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

report_definition_template_service::report_definition_template_service(
    context ctx) : ctx_(std::move(ctx)) {}

namespace {

// Columns: 0=name, 1=description, 2=report_type, 3=schedule_expression,
//          4=concurrency_policy, 5=display_order
domain::report_definition_template row_to_template(
    const std::vector<std::optional<std::string>>& row) {
    domain::report_definition_template t;
    if (row.size() > 0 && row[0]) t.name = *row[0];
    if (row.size() > 1 && row[1]) t.description = *row[1];
    if (row.size() > 2 && row[2]) t.report_type = *row[2];
    if (row.size() > 3 && row[3]) t.schedule_expression = *row[3];
    if (row.size() > 4 && row[4]) t.concurrency_policy = *row[4];
    if (row.size() > 5 && row[5]) t.display_order = std::stoi(*row[5]);
    return t;
}

} // namespace

std::vector<domain::report_definition_template>
report_definition_template_service::list_templates(
    const std::string& bundle_code) {
    BOOST_LOG_SEV(lg(), debug)
        << "Listing report definition templates for bundle: " << bundle_code;

    const auto rows = execute_parameterized_multi_column_query(
        ctx_,
        "SELECT a.name, a.description, a.report_type,"
        "       a.schedule_expression, a.concurrency_policy, a.display_order"
        " FROM ores_dq_report_definitions_artefact_tbl a"
        " JOIN ores_dq_datasets_tbl d ON d.id = a.dataset_id"
        "   AND d.valid_to = ores_utility_infinity_timestamp_fn()"
        "   AND d.tenant_id = ores_iam_system_tenant_id_fn()"
        " JOIN ores_dq_dataset_bundle_members_tbl m ON m.dataset_code = d.code"
        "   AND m.tenant_id = ores_iam_system_tenant_id_fn()"
        " WHERE m.bundle_code = $1"
        " ORDER BY a.display_order, a.name",
        {bundle_code},
        lg(), "Listing report definition templates for bundle: " + bundle_code);

    std::vector<domain::report_definition_template> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        try {
            result.push_back(row_to_template(row));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn)
                << "Skipping malformed template row: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), debug)
        << "Found " << result.size() << " templates for bundle: " << bundle_code;
    return result;
}

}
