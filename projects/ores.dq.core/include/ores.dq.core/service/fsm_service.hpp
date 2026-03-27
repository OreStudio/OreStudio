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
#ifndef ORES_DQ_CORE_SERVICE_FSM_SERVICE_HPP
#define ORES_DQ_CORE_SERVICE_FSM_SERVICE_HPP

#include <string>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.dq.api/domain/fsm_state.hpp"

namespace ores::dq::service {

/**
 * @brief Read-only service for FSM states.
 *
 * FSM states and machines are system-level reference data seeded at
 * provisioning time. This service always queries the system tenant.
 */
class fsm_service {
private:
    inline static std::string_view logger_name = "ores.dq.service.fsm_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    explicit fsm_service(context ctx);

    /**
     * @brief Lists all current FSM states for the named machine.
     *
     * @param machine_name The machine name (e.g. "report_definition_lifecycle").
     * @return States ordered by name. Empty if the machine is not found.
     */
    std::vector<domain::fsm_state>
    list_states_for_machine(const std::string& machine_name);

    /**
     * @brief Lists all current FSM states across all machines.
     */
    std::vector<domain::fsm_state> list_all_states();

private:
    context ctx_;
};

}

#endif
