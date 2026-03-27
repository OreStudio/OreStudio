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
#include "ores.dq.core/service/fsm_service.hpp"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"

namespace ores::dq::service {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

fsm_service::fsm_service(context ctx) : ctx_(std::move(ctx)) {}

namespace {

// Columns: 0=id, 1=machine_id, 2=name, 3=is_initial, 4=is_terminal,
//          5=tenant_id, 6=version, 7=modified_by
domain::fsm_state row_to_state(
    const std::vector<std::optional<std::string>>& row) {
    domain::fsm_state s;
    if (row.size() > 0 && row[0]) s.id = boost::lexical_cast<boost::uuids::uuid>(*row[0]);
    if (row.size() > 1 && row[1]) s.machine_id = boost::lexical_cast<boost::uuids::uuid>(*row[1]);
    if (row.size() > 2 && row[2]) s.name = *row[2];
    if (row.size() > 3 && row[3]) s.is_initial = (*row[3] == "1" || *row[3] == "t" || *row[3] == "true");
    if (row.size() > 4 && row[4]) s.is_terminal = (*row[4] == "1" || *row[4] == "t" || *row[4] == "true");
    if (row.size() > 5 && row[5]) s.tenant_id = ores::utility::uuid::tenant_id::from_string(*row[5]).value_or(
        ores::utility::uuid::tenant_id::system());
    if (row.size() > 6 && row[6]) s.version = std::stoi(*row[6]);
    if (row.size() > 7 && row[7]) s.modified_by = *row[7];
    return s;
}

} // namespace

std::vector<domain::fsm_state>
fsm_service::list_states_for_machine(const std::string& machine_name) {
    BOOST_LOG_SEV(lg(), debug) << "Listing FSM states for machine: " << machine_name;
    const auto sys_ctx =
        ores::database::service::tenant_context::with_system_tenant(ctx_);
    const auto rows = execute_parameterized_multi_column_query(
        sys_ctx,
        "SELECT s.id::text, s.machine_id::text, s.name,"
        "       s.is_initial, s.is_terminal,"
        "       s.tenant_id, s.version, s.modified_by"
        " FROM ores_dq_fsm_states_tbl s"
        " JOIN ores_dq_fsm_machines_tbl m ON m.id = s.machine_id"
        "   AND m.tenant_id = s.tenant_id"
        " WHERE m.name = $1"
        "   AND s.valid_to  = ores_utility_infinity_timestamp_fn()"
        "   AND m.valid_to  = ores_utility_infinity_timestamp_fn()"
        "   AND s.tenant_id = ores_iam_system_tenant_id_fn()"
        " ORDER BY s.name",
        {machine_name},
        lg(), "Listing FSM states for machine: " + machine_name);
    std::vector<domain::fsm_state> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        try {
            result.push_back(row_to_state(row));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to parse FSM state row: " << e.what();
        }
    }
    BOOST_LOG_SEV(lg(), debug) << "Found " << result.size()
        << " states for machine: " << machine_name;
    return result;
}

std::vector<domain::fsm_state> fsm_service::list_all_states() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all FSM states";
    const auto sys_ctx =
        ores::database::service::tenant_context::with_system_tenant(ctx_);
    const auto rows = execute_parameterized_multi_column_query(
        sys_ctx,
        "SELECT s.id::text, s.machine_id::text, s.name,"
        "       s.is_initial, s.is_terminal,"
        "       s.tenant_id, s.version, s.modified_by"
        " FROM ores_dq_fsm_states_tbl s"
        " WHERE s.valid_to  = ores_utility_infinity_timestamp_fn()"
        "   AND s.tenant_id = ores_iam_system_tenant_id_fn()"
        " ORDER BY s.name",
        {},
        lg(), "Listing all FSM states");
    std::vector<domain::fsm_state> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        try {
            result.push_back(row_to_state(row));
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Failed to parse FSM state row: " << e.what();
        }
    }
    BOOST_LOG_SEV(lg(), debug) << "Found " << result.size() << " FSM states";
    return result;
}

}
