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
#include "ores.compute/messaging/registrar.hpp"

#include <memory>
#include <optional>
#include "ores.compute/messaging/host_protocol.hpp"
#include "ores.compute/messaging/app_protocol.hpp"
#include "ores.compute/messaging/app_version_protocol.hpp"
#include "ores.compute/messaging/batch_protocol.hpp"
#include "ores.compute/messaging/workunit_protocol.hpp"
#include "ores.compute/messaging/result_protocol.hpp"
#include "ores.compute/messaging/work_protocol.hpp"
#include "ores.compute/messaging/host_handler.hpp"
#include "ores.compute/messaging/app_handler.hpp"
#include "ores.compute/messaging/app_version_handler.hpp"
#include "ores.compute/messaging/batch_handler.hpp"
#include "ores.compute/messaging/workunit_handler.hpp"
#include "ores.compute/messaging/result_handler.hpp"
#include "ores.compute/messaging/work_handler.hpp"

namespace ores::compute::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // ----------------------------------------------------------------
    // Hosts
    // ----------------------------------------------------------------
    auto hh = std::make_shared<host_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_hosts_request::nats_subject, "ores.compute.service",
        [hh](ores::nats::message msg) { hh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_host_request::nats_subject, "ores.compute.service",
        [hh](ores::nats::message msg) { hh->save(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        delete_host_request::nats_subject, "ores.compute.service",
        [hh](ores::nats::message msg) { hh->remove(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Apps
    // ----------------------------------------------------------------
    auto ah = std::make_shared<app_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_apps_request::nats_subject, "ores.compute.service",
        [ah](ores::nats::message msg) { ah->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_app_request::nats_subject, "ores.compute.service",
        [ah](ores::nats::message msg) { ah->save(std::move(msg)); }));

    // ----------------------------------------------------------------
    // App versions
    // ----------------------------------------------------------------
    auto avh = std::make_shared<app_version_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_app_versions_request::nats_subject, "ores.compute.service",
        [avh](ores::nats::message msg) { avh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_app_version_request::nats_subject, "ores.compute.service",
        [avh](ores::nats::message msg) { avh->save(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Batches
    // ----------------------------------------------------------------
    auto bh = std::make_shared<batch_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_batches_request::nats_subject, "ores.compute.service",
        [bh](ores::nats::message msg) { bh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_batch_request::nats_subject, "ores.compute.service",
        [bh](ores::nats::message msg) { bh->save(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Workunits
    // ----------------------------------------------------------------
    auto wuh = std::make_shared<workunit_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_workunits_request::nats_subject, "ores.compute.service",
        [wuh](ores::nats::message msg) { wuh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        save_workunit_request::nats_subject, "ores.compute.service",
        [wuh](ores::nats::message msg) { wuh->save(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Results
    // ----------------------------------------------------------------
    auto rh = std::make_shared<result_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        list_results_request::nats_subject, "ores.compute.service",
        [rh](ores::nats::message msg) { rh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        submit_result_request::nats_subject, "ores.compute.service",
        [rh](ores::nats::message msg) { rh->submit(std::move(msg)); }));

    // ----------------------------------------------------------------
    // Work (pull/heartbeat/reap)
    // ----------------------------------------------------------------
    auto wh = std::make_shared<work_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        pull_work_request::nats_subject, "ores.compute.service",
        [wh](ores::nats::message msg) { wh->pull(std::move(msg)); }));
    // Fire-and-forget subjects: use plain subscribe (not queue_subscribe)
    subs.push_back(nats.subscribe(
        heartbeat_message::nats_subject,
        [wh](ores::nats::message msg) { wh->heartbeat(std::move(msg)); }));
    subs.push_back(nats.subscribe(
        reap_work_message::nats_subject,
        [wh](ores::nats::message msg) { wh->reap(std::move(msg)); }));

    return subs;
}

} // namespace ores::compute::messaging
