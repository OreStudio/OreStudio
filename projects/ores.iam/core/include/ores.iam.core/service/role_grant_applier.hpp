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
#ifndef ORES_IAM_CORE_SERVICE_ROLE_GRANT_APPLIER_HPP
#define ORES_IAM_CORE_SERVICE_ROLE_GRANT_APPLIER_HPP

#include "ores.database/domain/context.hpp"
#include "ores.eventing.api/service/event_bus.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.logging/make_logger.hpp"
#include <mutex>

namespace ores::iam::service {

/**
 * @brief Grants the roles approved iam.role_grant requests asked for.
 *
 * A reconciliation, not a reaction to one event: each run asks the database
 * which approved request roles an account does not yet hold, across every
 * tenant, and grants them in that tenant as the person who approved. Granting
 * takes a role out of the answer, so a second run grants nothing twice, and an
 * approval made while IAM was down is granted on the next run. The service
 * runs it at start and whenever an approval request changes.
 */
class ORES_IAM_CORE_EXPORT role_grant_applier {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance = ores::logging::make_logger("ores.iam.service.role_grant_applier");
        return instance;
    }

public:
    role_grant_applier(ores::database::context ctx, ores::eventing::service::event_bus* event_bus);

    /**
     * @brief Grants every approved role not yet held; answers how many.
     *
     * A grant that fails is logged and left for the next run.
     */
    int apply();

private:
    ores::database::context ctx_;
    ores::eventing::service::event_bus* event_bus_;
    std::mutex mutex_;
};

}

#endif
