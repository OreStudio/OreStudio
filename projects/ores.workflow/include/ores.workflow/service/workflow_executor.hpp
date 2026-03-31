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
#ifndef ORES_WORKFLOW_SERVICE_WORKFLOW_EXECUTOR_HPP
#define ORES_WORKFLOW_SERVICE_WORKFLOW_EXECUTOR_HPP

#include <string>
#include "ores.database/domain/context.hpp"
#include "ores.nats/service/nats_client.hpp"

namespace ores::workflow::service {

/**
 * @brief Abstract base for saga workflow executors.
 *
 * An executor encapsulates one named workflow type.  The handler calls
 * execute(); on failure it calls compensate() to roll back completed steps.
 *
 * Each concrete executor:
 *  - Tracks per-step progress internally.
 *  - Writes workflow_step records to the database via the injected context.
 *  - Calls downstream services via the injected nats_client.
 *
 * The nats_client passed in should already have the end-user JWT set as
 * the delegation token (via nats_client::with_delegation) so that downstream
 * services receive the original caller's context.
 */
class workflow_executor {
public:
    virtual ~workflow_executor() = default;

    /**
     * @brief Execute all saga steps in order.
     *
     * @param ctx  Per-request database context for writing step records.
     * @param nats Authenticated NATS client (with delegation) for calling
     *             downstream services.
     * @return true on full success; false if any step fails (executor has
     *         already stored the error via error()).
     */
    virtual bool execute(ores::database::context ctx,
        ores::nats::service::nats_client& nats) = 0;

    /**
     * @brief Compensate completed steps in reverse order.
     *
     * Called by the handler when execute() returns false.  Must be idempotent.
     *
     * @param ctx  Per-request database context.
     * @param nats Authenticated NATS client (with delegation).
     */
    virtual void compensate(ores::database::context ctx,
        ores::nats::service::nats_client& nats) = 0;

    /**
     * @brief Human-readable reason for a failed execution.
     *
     * Valid only after execute() returns false.
     */
    [[nodiscard]] virtual const std::string& failure_reason() const = 0;
};

}

#endif
