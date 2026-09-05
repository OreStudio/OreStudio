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
#include "ores.compute.service/app/workunit_dispatcher.hpp"

#include "ores.compute.api/domain/result.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.api/net/compute_storage.hpp"
#include "ores.compute.core/repository/app_version_platform_repository.hpp"
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/domain/change_reason_codes.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::compute::service::app {

using namespace ores::logging;
using ores::service::messaging::stamp;

workunit_dispatcher::workunit_dispatcher(ores::nats::service::client& nats,
                                         ores::database::context ctx)
    : nats_(nats)
    , ctx_(std::move(ctx)) {}

void workunit_dispatcher::dispatch(const ores::compute::eventing::workunit_changed_event& evt) {
    if (evt.workunit_ids.empty())
        return;

    try {
        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(ctx_, evt.tenant_id);
        BOOST_LOG_SEV(lg(), debug) << "Dispatching " << evt.workunit_ids.size()
                                   << " workunit(s) for tenant " << evt.tenant_id;
        for (const auto& workunit_id : evt.workunit_ids) {
            try {
                dispatch_one(tenant_ctx, workunit_id);
            } catch (const std::exception& e) {
                BOOST_LOG_SEV(lg(), error)
                    << "Dispatch failed for workunit " << workunit_id << ": " << e.what();
            }
        }
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << "Cannot dispatch workunits for tenant " << evt.tenant_id << ": " << e.what();
    }
}

void workunit_dispatcher::dispatch_one(const ores::database::context& tenant_ctx,
                                       const std::string& workunit_id) {
    ores::compute::service::workunit_service wu_svc(tenant_ctx);
    const auto wu = wu_svc.get_workunit(workunit_id);
    if (!wu) {
        BOOST_LOG_SEV(lg(), warn) << "Workunit not found for dispatch: " << workunit_id;
        return;
    }

    // Workunit events fire on every row version (insert, update, close). A
    // workunit is dispatched exactly once, when its first version is created.
    repository::result_repository result_repo;
    if (result_repo.get_total_result_count_by_workunit_id(tenant_ctx, workunit_id) > 0) {
        BOOST_LOG_SEV(lg(), debug)
            << "Workunit " << workunit_id << " already dispatched; skipping";
        return;
    }

    if (wu->app_version_id.is_nil()) {
        BOOST_LOG_SEV(lg(), warn) << "Workunit " << workunit_id
                                  << " has no app_version_id; cannot dispatch";
        return;
    }

    repository::app_version_platform_repository avp_repo(tenant_ctx);
    const auto avps = avp_repo.read_latest_by_app_version(wu->app_version_id);
    if (avps.empty()) {
        BOOST_LOG_SEV(lg(), warn)
            << "No platform packages for app_version " << boost::uuids::to_string(wu->app_version_id)
            << "; cannot dispatch workunit " << workunit_id;
        return;
    }

    const auto app_version_id = boost::uuids::to_string(wu->app_version_id);
    const auto tenant_uuid = tenant_ctx.tenant_id().to_string();
    ores::compute::service::result_service result_svc(tenant_ctx);

    const auto redundancy = wu->target_redundancy;
    for (int i = 0; i < redundancy; ++i) {
        const auto& avp = avps[i % avps.size()];
        const auto result_id = boost::uuids::random_generator()();
        ores::compute::domain::result r;
        r.id = result_id;
        r.workunit_id = wu->id;
        r.server_state = 2; // Unsent
        r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
        r.change_commentary = "Created on workunit dispatch";
        stamp(r, tenant_ctx);
        result_svc.save_result(r);

        const auto result_id_str = boost::uuids::to_string(result_id);
        const auto event = ores::compute::messaging::work_assignment_event{
            .result_id = result_id_str,
            .workunit_id = workunit_id,
            .app_version_id = app_version_id,
            .package_uri = avp.package_uri,
            .package_sha256 = avp.sha256,
            .input_uri = wu->input_uri,
            .config_uri = wu->config_uri,
            .output_uri = ores::compute::net::compute_storage::output_path(result_id_str)};
        nats_.js_publish("compute.v1.work.assignments." + tenant_uuid + "." + avp.platform_code,
                         ores::nats::default_wire_codec().encode(event));
        BOOST_LOG_SEV(lg(), info) << "Dispatched result " << result_id_str << " for workunit "
                                  << workunit_id << " to platform " << avp.platform_code;
    }
}

}
