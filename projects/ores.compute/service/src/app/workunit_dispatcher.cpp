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
#include "ores.compute.core/repository/app_version_platform_repository.hpp"
#include "ores.compute.core/repository/result_repository.hpp"
#include "ores.compute.core/repository/work_ledger_repository.hpp"
#include "ores.compute.core/service/result_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.dq.api/domain/change_reason_codes.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <optional>
#include <vector>

namespace ores::compute::service::app {

using namespace ores::logging;
using ores::service::messaging::stamp;

namespace {

/// The most jobs of one tenant that wait in the queue. The cap is soft: the count and the
/// inserts are not one step, so two dispatches at once can pass it by one workunit each. At the
/// grid's pace of about 130 jobs a minute, this is a wait of about half an hour for the last job
/// of a tenant alone in the queue.
constexpr std::int64_t tenant_queue_limit = 3600;

}

workunit_dispatcher::workunit_dispatcher(ores::nats::service::client& nats,
                                         ores::database::context ctx)
    : nats_(nats)
    , ctx_(std::move(ctx)) {}

void workunit_dispatcher::dispatch(const ores::eventing::domain::entity_change_event& evt) {
    if (evt.entity_ids.empty())
        return;

    try {
        const auto tenant_ctx =
            ores::database::service::tenant_context::with_tenant(ctx_, evt.tenant_id);
        BOOST_LOG_SEV(lg(), debug) << "Dispatching " << evt.entity_ids.size()
                                   << " workunit(s) for tenant " << evt.tenant_id;
        for (const auto& workunit_id : evt.entity_ids) {
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
    const auto wu = wu_svc.get_workunit(boost::lexical_cast<boost::uuids::uuid>(workunit_id));
    if (!wu) {
        BOOST_LOG_SEV(lg(), warn) << "Workunit not found for dispatch: " << workunit_id;
        return;
    }

    // Workunit events fire on every row version (insert, update, close).
    // Dispatch converges on the redundancy target: a workunit short of its
    // target tops up the missing assignments on the next event, so a
    // partially failed publish is retried instead of stranding the workunit.
    repository::result_repository result_repo;
    const auto existing = static_cast<int>(
        result_repo.get_total_result_count_by_workunit_id(tenant_ctx, workunit_id));
    const auto missing = wu->target_redundancy - existing;
    if (missing <= 0) {
        BOOST_LOG_SEV(lg(), debug)
            << "Workunit " << workunit_id << " at redundancy target; skipping";
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
        BOOST_LOG_SEV(lg(), warn) << "No platform packages for app_version "
                                  << boost::uuids::to_string(wu->app_version_id)
                                  << "; cannot dispatch workunit " << workunit_id;
        return;
    }

    const auto tenant_uuid = tenant_ctx.tenant_id().to_string();

    // A tenant cannot fill the queue. A dispatch that would pass the limit is refused, and the
    // log says how many jobs wait. The workunit keeps no results until it is dispatched again.
    const auto waiting = ores::compute::repository::work_ledger_repository().waiting(
        ctx_, boost::lexical_cast<boost::uuids::uuid>(tenant_uuid));
    if (waiting + missing > tenant_queue_limit) {
        BOOST_LOG_SEV(lg(), warn)
            << "Refusing to dispatch workunit " << workunit_id << " for tenant " << tenant_uuid
            << ": " << waiting << " job(s) already wait and the limit is "
            << tenant_queue_limit;
        return;
    }

    ores::compute::service::result_service result_svc(tenant_ctx);

    for (int i = 0; i < missing; ++i) {
        const auto result_id = boost::uuids::random_generator()();
        ores::compute::domain::result r;
        r.id = result_id;
        r.workunit_id = wu->id;
        r.party_id = wu->party_id;
        // Unsent.
        r.server_state = 2;
        r.change_reason_code = ores::dq::domain::change_reasons::system_new_record;
        r.change_commentary = "Created on workunit dispatch";
        stamp(r, tenant_ctx);
        result_svc.save_result(r);

        // The queue of the grid is the work ledger, which the installation owns. The
        // service writes it as the system tenant, and the entry holds only ids, so a
        // node that claims it enters the tenant to reach the result.
        std::vector<std::string> platforms;
        platforms.reserve(avps.size());
        for (const auto& candidate : avps)
            platforms.push_back(candidate.platform_code);
        ores::compute::repository::work_ledger_repository().enqueue(
            ctx_,
            {.result_id = result_id,
             .work_tenant_id = boost::lexical_cast<boost::uuids::uuid>(tenant_uuid),
             .workunit_id = wu->id,
             .platforms = std::move(platforms),
             .priority = wu->priority});

        BOOST_LOG_SEV(lg(), info) << "Queued result " << boost::uuids::to_string(result_id)
                                  << " for workunit " << workunit_id;
    }

    // A plain notice with no job in it: idle nodes ask for work at once instead of
    // waiting for their next poll. A lost notice costs a few seconds.
    const ores::compute::messaging::work_available_event bell{.queued = missing};
    nats_.publish(std::string(ores::compute::messaging::work_available_event::nats_subject),
                  ores::nats::default_wire_codec().encode(bell));
}

}
