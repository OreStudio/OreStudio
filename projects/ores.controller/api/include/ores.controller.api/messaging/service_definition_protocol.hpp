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
#ifndef ORES_CONTROLLER_API_MESSAGING_SERVICE_DEFINITION_PROTOCOL_HPP
#define ORES_CONTROLLER_API_MESSAGING_SERVICE_DEFINITION_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.controller.api/domain/service_definition.hpp"

namespace ores::controller::api::messaging {

// =============================================================================
// List service definitions
// =============================================================================

struct list_service_definitions_request {
    using response_type = struct list_service_definitions_response;
    static constexpr std::string_view nats_subject =
        "controller.v1.service_definitions.list";
};

struct list_service_definitions_response {
    bool success = false;
    std::string message;
    std::vector<domain::service_definition> service_definitions;
};

// =============================================================================
// Save service definition (create or update)
// =============================================================================

struct save_service_definition_request {
    using response_type = struct save_service_definition_response;
    static constexpr std::string_view nats_subject =
        "controller.v1.service_definitions.save";
    domain::service_definition service_definition;
    std::string change_reason_code;
    std::string change_commentary;
};

struct save_service_definition_response {
    bool success = false;
    std::string message;
};

}

#endif
