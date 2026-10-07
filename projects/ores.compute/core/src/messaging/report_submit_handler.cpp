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
#include "ores.compute.api/domain/batch.hpp"
#include "ores.compute.api/domain/workunit.hpp"
#include "ores.compute.core/repository/app_version_repository.hpp"
#include "ores.compute.core/repository/workflow_batch_link_repository.hpp"
#include "ores.compute.core/service/batch_service.hpp"
#include "ores.compute.core/service/workunit_service.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/service/tenant_context.hpp"
#include "ores.iam.client/client/run_token_minter.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.reporting.api/messaging/report_operations_protocol.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/messaging/workflow_helpers.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <chrono>
#include <format>
#include <rfl/json.hpp>

namespace ores::compute::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::reporting::messaging;

report_submit_handler::report_submit_handler(ores::nats::service::client& nats,
                                             ores::database::context ctx,
                                             ores::nats::service::nats_client service_nats)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , service_nats_(std::move(service_nats))
    , run_tokens_(ores::iam::client::make_run_token_minter(service_nats_)) {}

void report_submit_handler::submit(ores::nats::message msg) {
    auto wf = workflow_step_context::from_message(nats_, msg);
    if (!wf)
        return;

    const std::string_view sv(reinterpret_cast<const char*>(msg.data.data()), msg.data.size());
    auto parsed = rfl::json::read<submit_compute_request>(sv);
    if (!parsed) {
        wf->fail("Failed to decode submit_compute_request");
        return;
    }
    const auto& req = *parsed;

    BOOST_LOG_SEV(lg(), info) << "submit_compute starting | instance=" << req.report_instance_id
                              << " tarballs=" << req.tarball_uris.size();

    try {
        if (req.tarball_uris.empty()) {
            wf->fail("submit_compute: no tarball URIs in request");
            return;
        }

        ores::service::service::cache::run_token_step_scope step_tokens(run_tokens_,
                                                                       req.report_instance_id);
        const ores::service::service::cache::run_token_key key{
            .grant_id = req.run_grant_id, .run_id = req.report_instance_id};
        if (run_tokens_.token_for(key, req.tenant_id).empty()) {
            wf->fail("submit_compute: the run has no run token; its grant is missing or IAM "
                     "refused the exchange");
            return;
        }

        auto tenant_ctx = ores::service::messaging::for_requested_party(
            ores::database::service::tenant_context::with_tenant(ctx_, req.tenant_id),
            req.party_id);

        const auto batch_uuid = boost::uuids::random_generator()();
        const auto batch_id = boost::uuids::to_string(batch_uuid);

        // ── Create batch ──────────────────────────────────────────────
        domain::batch batch;
        batch.id = batch_uuid;
        batch.external_ref = req.report_instance_id;
        batch.status = "open";
        stamp(batch, tenant_ctx);

        service::batch_service batch_svc(tenant_ctx);
        batch_svc.save_batch(batch);

        BOOST_LOG_SEV(lg(), info) << "Created compute batch: " << batch_id;

        // ── Resolve the application version ───────────────────────────
        // A workunit must name one and the table refuses a nil value. The
        // report configuration states no version to run yet, so the highest
        // engine version stands in; a field on the configuration would make
        // the choice explicit rather than implied. App versions are a global
        // registry: the platform publishes the canonical ones under the system
        // tenant, and the repository reads the caller's own rows together with
        // those, which its row-level policy exposes to every tenant.
        repository::app_version_repository app_versions;
        const auto candidates = app_versions.read_latest(tenant_ctx);
        if (candidates.empty()) {
            wf->fail("submit_compute: no application version is available to run");
            return;
        }
        const auto app_version_uuid =
            std::ranges::max_element(candidates, {}, &domain::app_version::engine_version)->id;

        // ── Create workunits and publish assignments ───────────────────
        service::workunit_service wu_svc(tenant_ctx);
        std::vector<std::string> workunit_ids;

        for (const auto& tarball_uri : req.tarball_uris) {
            const auto wu_uuid = boost::uuids::random_generator()();
            const auto wu_id = boost::uuids::to_string(wu_uuid);

            domain::workunit wu;
            wu.id = wu_uuid;
            wu.batch_id = batch_uuid;
            wu.app_version_id = app_version_uuid;
            wu.input_uri = tarball_uri;
            wu.priority = 1;
            wu.target_redundancy = 1;
            stamp(wu, tenant_ctx);

            wu_svc.save_workunit(wu);
            workunit_ids.push_back(wu_id);
        }

        // Dispatch is the workunit dispatcher's job, on the workunit-changed
        // event the save above publishes: it resolves the per-platform package
        // and publishes to the triplet-qualified subject a wrapper subscribes
        // to. Publishing an assignment here as well would name no platform and
        // carry none of the fields a wrapper needs.

        // Record the async bridge row: batch_workflow_bridge will publish
        // step_completed_event once the batch reaches "closed" status.
        domain::workflow_batch_link link;
        link.batch_id = batch_uuid;
        link.tenant_id = utility::uuid::tenant_id::from_string(req.tenant_id).value();
        link.workflow_step_id = wf->step_id;
        link.workflow_instance_id = wf->instance_id;
        link.created_at = std::chrono::system_clock::now();

        repository::workflow_batch_link_repository link_repo;
        link_repo.write(tenant_ctx, link);

        BOOST_LOG_SEV(lg(), info) << "submit_compute deferred | instance=" << req.report_instance_id
                                  << " batch=" << batch_id << " workunits=" << workunit_ids.size()
                                  << " step=" << wf->step_id;

    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "submit_compute failed: " << e.what();
        wf->fail(e.what());
    }
}

}
