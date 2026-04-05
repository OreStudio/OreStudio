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
 * Used by ores.ore.service::ore_import_workflow until Phase 2.2 migrates it
 * to the event-driven engine.  New workflows must use workflow_definition /
 * workflow_registry instead of inheriting from this class.
 *
 * @deprecated Will be removed in Phase 2.2 once ore_import is migrated.
 */
class workflow_executor {
public:
    virtual ~workflow_executor() = default;

    virtual bool execute(ores::database::context ctx,
        ores::nats::service::nats_client& nats) = 0;

    virtual void compensate(ores::database::context ctx,
        ores::nats::service::nats_client& nats) = 0;

    [[nodiscard]] virtual const std::string& failure_reason() const = 0;
};

}

#endif
