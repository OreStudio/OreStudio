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
#include "ores.compute.core/messaging/registrar.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/app_version_protocol.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.api/messaging/host_protocol.hpp"
#include "ores.compute.api/messaging/platform_protocol.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.compute.core/messaging/app_registrar.hpp"
#include "ores.compute.core/messaging/app_version_platform_registrar.hpp"
#include "ores.compute.core/messaging/app_version_registrar.hpp"
#include "ores.compute.core/messaging/batch_registrar.hpp"
#include "ores.compute.core/messaging/host_registrar.hpp"
#include "ores.compute.core/messaging/platform_handler.hpp"
#include "ores.compute.core/messaging/report_submit_handler.hpp"
#include "ores.compute.core/messaging/result_registrar.hpp"
#include "ores.compute.core/messaging/result_submit_handler.hpp"
#include "ores.compute.core/messaging/telemetry_handler.hpp"
#include "ores.compute.core/messaging/work_handler.hpp"
#include "ores.compute.core/messaging/workunit_registrar.hpp"
#include "ores.reporting.api/messaging/report_execution_protocol.hpp"
#include <memory>
#include <optional>

namespace ores::compute::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // Generated per-entity registrars (hosts, apps, app versions, the
    // app-version/platform junction, batches, workunits, results). Each wires
    // the standard CRUD surface -- list/save/delete/history -- plus the
    // per-FK list-by reads, all to the generated handler. subscription is
    // move-only, so fold each returned vector in with move iterators.
    const auto fold = [&subs](std::vector<ores::nats::service::subscription> s) {
        subs.insert(subs.end(),
                    std::make_move_iterator(s.begin()),
                    std::make_move_iterator(s.end()));
    };
    fold(register_host_handlers(nats, ctx, verifier));
    fold(register_app_handlers(nats, ctx, verifier));
    fold(register_app_version_handlers(nats, ctx, verifier));
    fold(register_app_version_platform_handlers(nats, ctx, verifier));
    fold(register_batch_handlers(nats, ctx, verifier));
    fold(register_workunit_handlers(nats, ctx, verifier));
    fold(register_result_handlers(nats, ctx, verifier));

    // ----------------------------------------------------------------
    // Work (pull/heartbeat/reap)
    // ----------------------------------------------------------------
    auto wh = std::make_shared<work_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        pull_work_request::nats_subject, "ores.compute.service", [wh](ores::nats::message msg) {
            wh->pull(std::move(msg));
        }));
    // Fire-and-forget subjects: use plain subscribe (not queue_subscribe)
    subs.push_back(nats.subscribe(heartbeat_message::nats_subject, [wh](ores::nats::message msg) {
        wh->heartbeat(std::move(msg));
    }));
    subs.push_back(nats.subscribe(reap_work_message::nats_subject,
                                  [wh](ores::nats::message msg) { wh->reap(std::move(msg)); }));

    // Result submit: trusted wrapper nodes (no JWT, same transport trust as
    // the heartbeat). Kept out of the generated result handler, which is
    // user-session gated and regenerated on every bind.
    auto result_submit = std::make_shared<result_submit_handler>(nats, ctx);
    subs.push_back(nats.queue_subscribe(
        submit_result_request::nats_subject,
        "ores.compute.service",
        [result_submit](ores::nats::message msg) { result_submit->submit(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Telemetry
    // ----------------------------------------------------------------
    auto th = std::make_shared<telemetry_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        get_grid_stats_request::nats_subject,
        "ores.compute.service",
        [th](ores::nats::message msg) { th->get_grid_stats(std::move(msg)); }));
    // Node sample publishes are fire-and-forget from wrapper nodes.
    subs.push_back(nats.subscribe(node_sample_message::nats_subject, [th](ores::nats::message msg) {
        th->ingest_node_sample(std::move(msg));
    }));

    // ----------------------------------------------------------------
    // Platforms (system data - list only)
    // ----------------------------------------------------------------
    auto ph = std::make_shared<platform_handler>(nats, ctx, verifier);
    subs.push_back(
        nats.queue_subscribe(list_platforms_request::nats_subject,
                             "ores.compute.service",
                             [ph](ores::nats::message msg) { ph->list(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Report execution: submit to compute grid (workflow step handler).
    // ----------------------------------------------------------------
    auto rsh = std::make_shared<report_submit_handler>(nats, ctx);
    subs.push_back(nats.queue_subscribe(
        std::string(ores::reporting::messaging::submit_compute_request::nats_subject),
        "ores.compute.service",
        [rsh](ores::nats::message msg) { rsh->submit(std::move(msg)); }));

    return subs;
}

} // namespace ores::compute::messaging
