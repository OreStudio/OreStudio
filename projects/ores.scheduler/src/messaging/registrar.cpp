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
#include "ores.scheduler/messaging/registrar.hpp"

#include <memory>
#include <optional>
#include "ores.scheduler/messaging/scheduler_protocol.hpp"
#include "ores.scheduler/messaging/job_definition_handler.hpp"

namespace ores::scheduler::messaging {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
    ores::database::context ctx,
    std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // ----------------------------------------------------------------
    // Job definitions
    // ----------------------------------------------------------------
    auto jdh = std::make_shared<job_definition_handler>(nats, ctx, verifier);
    subs.push_back(nats.queue_subscribe(
        get_job_definitions_request::nats_subject, "ores.scheduler.service",
        [jdh](ores::nats::message msg) { jdh->list(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        schedule_job_request::nats_subject, "ores.scheduler.service",
        [jdh](ores::nats::message msg) { jdh->schedule(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        unschedule_job_request::nats_subject, "ores.scheduler.service",
        [jdh](ores::nats::message msg) { jdh->unschedule(std::move(msg)); }));
    subs.push_back(nats.queue_subscribe(
        get_job_history_request::nats_subject, "ores.scheduler.service",
        [jdh](ores::nats::message msg) { jdh->history(std::move(msg)); }));

    return subs;
}

} // namespace ores::scheduler::messaging
