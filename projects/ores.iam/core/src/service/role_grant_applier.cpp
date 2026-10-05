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
        "select tenant_id::text, request_id::text, account_id::text, role_id::text, approved_by"
        " from ores_iam_unapplied_role_grants_fn()",
        {},
        lg(),
        "Reading approved role requests not yet granted");

    boost::uuids::string_generator parse;
    int granted = 0;
    for (const auto& row : rows) {
        const auto request_id = row[1].value_or("");
        try {
            const auto tenant = utility::uuid::tenant_id::from_string(row[0].value_or(""));
            if (!tenant)
                throw std::runtime_error("bad tenant " + row[0].value_or(""));
            const auto approver = row[4].value_or("");
            authorization_service auth(ctx_.with_tenant(*tenant, approver), event_bus_);
            auth.assign_role(parse(row[2].value_or("")),
                             parse(row[3].value_or("")),
                             approver,
                             "Approved request " + request_id,
                             "access.role_change");
            ++granted;
            BOOST_LOG_SEV(lg(), info) << "Granted role " << row[3].value_or("") << " to account "
                                      << row[2].value_or("") << " for request " << request_id;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error) << "Granting a role for request " << request_id
                                       << " failed; the next run tries again: " << e.what();
        }
    }
    return granted;
}

}
