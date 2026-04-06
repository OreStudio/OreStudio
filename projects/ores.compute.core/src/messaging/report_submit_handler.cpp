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
#include "ores.compute.core/messaging/report_submit_handler.hpp"

#include <format>
#include <rfl/json.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.database/service/tenant_context.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/domain/workunit.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.core/service/batch_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.reporting.api/messaging/report_execution_protocol.hpp"

namespace ores::compute::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::reporting::messaging;

namespace {

// Subject for work assignment events: append tenant_id so wrappers can
// subscribe to their own tenant's work stream.
std::string assignment_subject(const std::string& tenant_id) {
    return "compute.v1.work.assignments." + tenant_id;
}

} // namespace

report_submit_handler::report_submit_handler(
    ores::nats::service::client& nats, ores::database::context ctx)
    : nats_(nats), ctx_(std::move(ctx)) {}

void report_submit_handler::submit(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf) return;

    const std::string_view sv(
        reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<submit_compute_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode submit_compute_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "submit_compute starting | instance="
                              << req.report_instance_id
                              << " tarballs=" << req.tarball_uris.size();

    try {
        if (req.tarball_uris.empty()) {
            wf->fail("submit_compute: no tarball URIs in request");
            return;
        }

        auto tenant_ctx = ores::database::service::tenant_context::with_tenant(
            ctx_, req.tenant_id);

        const auto batch_uuid = boost::uuids::random_generator()();
        const auto batch_id = boost::uuids::to_string(batch_uuid);

        // ── Create batch ──────────────────────────────────────────────
        domain::batch batch;
        batch.id = batch_uuid;
        batch.external_ref = req.report_instance_id;
        batch.status = "open";
        stamp(batch, tenant_ctx);

        service::batch_service batch_svc(tenant_ctx);
        batch_svc.save(batch);

        BOOST_LOG_SEV(lg(), info) << "Created compute batch: " << batch_id;

        // ── Create workunits and publish assignments ───────────────────
        service::workunit_service wu_svc(tenant_ctx);
        std::vector<std::string> workunit_ids;

        for (const auto& tarball_uri : req.tarball_uris) {
            const auto wu_uuid = boost::uuids::random_generator()();
            const auto wu_id = boost::uuids::to_string(wu_uuid);

            domain::workunit wu;
            wu.id = wu_uuid;
            wu.batch_id = batch_uuid;
            wu.app_version_id = {};   // Placeholder: no ORE app version yet
            wu.input_uri = tarball_uri;
            wu.priority = 1;
            wu.target_redundancy = 1;
            stamp(wu, tenant_ctx);

            wu_svc.save(wu);
            workunit_ids.push_back(wu_id);

            // Publish work assignment event (fire-and-forget).
            work_assignment_event evt;
            evt.workunit_id   = wu_id;
            evt.app_version_id = boost::uuids::to_string(wu.app_version_id);
            evt.input_uri     = tarball_uri;
            // result_id, package_uri, config_uri, output_uri left empty
            // until the compute app version is wired up.

            const auto json = rfl::json::write(evt);
            const auto data = std::as_bytes(std::span{json.data(), json.size()});
            nats_.publish(assignment_subject(req.tenant_id), data);

            BOOST_LOG_SEV(lg(), debug)
                << "Dispatched work assignment for workunit " << wu_id;
        }

        submit_compute_result result;
        result.success = true;
        result.batch_id = batch_id;
        result.message = std::format(
            "Created batch {} with {} workunit(s)",
            batch_id, workunit_ids.size());

        BOOST_LOG_SEV(lg(), info) << "submit_compute complete | instance="
                                  << req.report_instance_id
                                  << " batch=" << batch_id
                                  << " workunits=" << workunit_ids.size();

        // Phase 3.9: complete immediately for end-to-end validation.
        // Phase 3.10 will change this to async (assimilator fires
        // step_completed when the batch terminates).
        wf->complete(rfl::json::write(result));

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "submit_compute failed: " << e.what();
        wf->fail(e.what());
    }
}

}
