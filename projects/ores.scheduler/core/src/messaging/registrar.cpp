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
#include "ores.scheduler.core/messaging/registrar.hpp"
#include "ores.history.core/messaging/registrar.hpp"
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.scheduler.api/messaging/scheduling_operations_protocol.hpp"
#include "ores.scheduler.core/messaging/job_definition_history_provider_registrar.hpp"
#include "ores.scheduler.core/messaging/job_definition_registrar.hpp"
#include "ores.scheduler.core/messaging/job_instance_handler.hpp"
#include "ores.scheduler.core/messaging/scheduler_status_handler.hpp"
#include <memory>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace ores::scheduler::messaging {

namespace {

constexpr std::string_view queue_group = "ores.scheduler.service";

// The registry must outlive the history.v1.get subscription, and
// register_handlers is only ever called once per service process.
ores::history::service::dispatch_registry& history_registry() {
    static ores::history::service::dispatch_registry instance;
    return instance;
}

}

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;

    // The job-definition entity stack is generated: the CRUD, version and
    // history verbs come from the entity registrar.
    for (auto& sub : register_job_definition_handlers(nats, ctx, verifier))
        subs.push_back(std::move(sub));

    // The two operational views are computed, so their handlers are
    // hand-written beside the operation model that declares their subjects.
    {
        auto jih = std::make_shared<job_instance_handler>(nats, ctx, verifier);
        subs.push_back(nats.queue_subscribe(
            get_job_instances_request::nats_subject, queue_group, [jih](ores::nats::message msg) {
                jih->list(std::move(msg));
            }));

        auto ssh = std::make_shared<scheduler_status_handler>(nats, ctx, verifier);
        subs.push_back(
            nats.queue_subscribe(get_scheduler_status_request::nats_subject,
                                 queue_group,
                                 [ssh](ores::nats::message msg) { ssh->status(std::move(msg)); }));
    }

    // Job definition history comes from the generic history provider.
    {
        auto& hist_registry = history_registry();
        register_job_definition_history_provider(hist_registry);
        subs.push_back(ores::history::messaging::register_history_handlers(
            nats, hist_registry, "scheduler", queue_group, ctx, verifier));
    }

    return subs;
}

} // namespace ores::scheduler::messaging
