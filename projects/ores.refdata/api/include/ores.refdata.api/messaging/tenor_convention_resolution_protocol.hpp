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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_CONVENTION_RESOLUTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor_convention_resolution.hpp"
#include <string>
#include <string_view>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief Hand-authored: junction codegen doesn't generate a service/protocol/handler layer -- see
 * the story on retiring the legacy codegen profile system and adding junction C++ support.
 * Read-only (list only): resolution rows are reference data managed via Foundation-layer SQL
 * provisioning, not user edits through this protocol.
 */
struct get_tenor_convention_resolutions_request {
    using response_type = struct get_tenor_convention_resolutions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor-convention-resolutions.list";
};

struct get_tenor_convention_resolutions_response {
    std::vector<ores::refdata::domain::tenor_convention_resolution> resolutions;
    bool success = false;
    std::string message;
};

}

#endif
