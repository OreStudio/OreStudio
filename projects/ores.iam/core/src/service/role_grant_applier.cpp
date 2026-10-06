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
#include "ores.iam.core/service/role_grant_applier.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>

namespace ores::iam::service {

using namespace ores::logging;

role_grant_applier::role_grant_applier(ores::database::context ctx,
                                       ores::eventing::service::event_bus* event_bus)
    : ctx_(std::move(ctx))
    , event_bus_(event_bus) {}

int role_grant_applier::apply() {
    std::lock_guard lock(mutex_);

    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "select tenant_id::text, request_id::text, account_id::text, role_id::text,"
        " approved_by, held::text from ores_iam_unapplied_role_grants_fn()",
        {},
        lg(),
        "Reading approved role requests not yet applied");

    boost::uuids::string_generator parse;
    int granted = 0;
    for (const auto& row : rows) {
        const auto request_id = row[1].value_or("");
        const auto account_id = row[2].value_or("");
        const auto role_id = row[3].value_or("");
        try {
            const auto tenant = utility::uuid::tenant_id::from_string(row[0].value_or(""));
            if (!tenant)
                throw std::runtime_error("bad tenant " + row[0].value_or(""));
            const auto approver = row[4].value_or("");
            const auto ctx = ctx_.with_tenant(*tenant, approver);

            // A role the account already holds needs no grant; it is marked
            // applied all the same, so taking it away later is final.
            if (row[5].value_or("") != "true") {
                authorization_service auth(ctx, event_bus_);
                auth.assign_role(parse(account_id),
                                 parse(role_id),
                                 approver,
                                 "Approved request " + request_id,
                                 "access.role_change");
                ++granted;
                BOOST_LOG_SEV(lg(), info) << "Granted role " << role_id << " to account "
                                          << account_id << " for request " << request_id;
            }

            ores::database::repository::execute_parameterized_command(
                ctx,
                "insert into ores_iam_role_grant_request_roles_tbl (tenant_id, request_id,"
                " role_id, version, applied_at, modified_by, performed_by, change_reason_code,"
                " change_commentary)"
                " select tenant_id, request_id, role_id, version, clock_timestamp(), $3, $3,"
                " 'system.update', 'Applied'"
                " from ores_iam_role_grant_request_roles_tbl"
                " where request_id = $1::uuid and role_id = $2::uuid and applied_at is null"
                " and valid_to = ores_utility_infinity_timestamp_fn()",
                {request_id, role_id, approver},
                lg(),
                "Marking a request role applied");
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Applying role " << role_id << " for request " << request_id
                << " failed; the next run tries again: " << e.what();
        }
    }
    return granted;
}

}
