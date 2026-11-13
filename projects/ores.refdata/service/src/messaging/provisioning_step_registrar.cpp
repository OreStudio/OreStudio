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
#include "ores.refdata.service/messaging/provisioning_step_registrar.hpp"
#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.refdata.service/messaging/provisioning_step_handler.hpp"
#include "ores.service/messaging/step_command_stream.hpp"
#include <memory>

namespace ores::refdata::messaging {

std::vector<ores::nats::service::subscription>
provisioning_step_registrar::register_handlers(ores::nats::service::client& nats,
                                               ores::database::context ctx) {
    // The step commands come from the engine, which has proved them, so the
    // handler takes no verifier.
    ores::service::messaging::step_routes routes;
    auto steps = std::make_shared<provisioning_step_handler>(nats, std::move(ctx));
    routes.on(ores::iam::workflow::refdata_provision_tenant_step_subject,
              [steps](ores::nats::delivery delivery) { steps->step(std::move(delivery)); });

    std::vector<ores::nats::service::subscription> subs;
    subs.push_back(
        ores::service::messaging::serve_step_commands(nats, "ores.refdata.service", routes));
    return subs;
}

}
