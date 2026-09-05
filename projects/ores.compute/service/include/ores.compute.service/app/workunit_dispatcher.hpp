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
#ifndef ORES_COMPUTE_SERVICE_APP_WORKUNIT_DISPATCHER_HPP
#define ORES_COMPUTE_SERVICE_APP_WORKUNIT_DISPATCHER_HPP

#include "ores.compute.api/eventing/workunit_changed_event.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/client.hpp"
#include <string>

namespace ores::compute::service::app {

/**
 * @brief Dispatches new workunits to the compute grid.
 *
 * The shell dispatch-batch flow saves workunits directly through the entity
 * CRUD path; the backend side of that contract lives here: create one result
 * row per redundancy target and publish each as a JetStream work assignment.
 *
 * This class must live in the hand-written service app layer, not in the
 * generated workunit handler. The handler is regenerated from the entity
 * model on every bind, and hand-placed behavior is silently dropped on
 * regeneration — PR #2012 lost the original dispatcher exactly that way.
 *
 * Dispatch runs on the entity event pipeline after the workunit commit, so
 * it is asynchronous relative to the shell request: failures surface in the
 * service log, not in the shell reply.
 */
class workunit_dispatcher {
private:
    inline static std::string_view logger_name = "ores.compute.service.app.workunit_dispatcher";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    workunit_dispatcher(ores::nats::service::client& nats, ores::database::context ctx);

    /**
     * @brief Dispatches every workunit named in the event.
     *
     * Idempotent per workunit: a workunit that already has result rows is
     * skipped, so update events for an already-dispatched workunit are safe.
     */
    void dispatch(const ores::compute::eventing::workunit_changed_event& evt);

private:
    void dispatch_one(const ores::database::context& tenant_ctx, const std::string& workunit_id);

    ores::nats::service::client& nats_;
    ores::database::context ctx_;
};

}

#endif
