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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_SYNTHETIC_API_MESSAGING_FAULT_OPERATION_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_FAULT_OPERATION_PROTOCOL_HPP

#include <cstdint>
#include <string>

namespace ores::synthetic::messaging {

/**
 * @brief Replace the faults the simulator injects.
 */
struct set_faults_request {
    using response_type = struct set_faults_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ops.set_faults";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    bool connection_lost = false;
    std::uint32_t tick_delay_ms = 0;
    bool repeat_ticks = false;
    bool out_of_range = false;
    bool not_entitled = false;
};

/**
 * @brief Whether the plan was set, and why not when it was not.
 */
struct set_faults_response {
    bool success = false;
    std::string message;
};

}

#endif
