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
#ifndef ORES_IAM_MESSAGING_SESSION_SAMPLES_PROTOCOL_HPP
#define ORES_IAM_MESSAGING_SESSION_SAMPLES_PROTOCOL_HPP
#include <string>

#include <cstdint>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief Time-series sample for session telemetry.
 */
struct session_sample_dto {
    std::uint64_t sample_time_ms = 0;
    std::uint64_t bytes_sent = 0;
    std::uint64_t bytes_received = 0;
};

struct get_session_samples_request {
    using response_type = struct get_session_samples_response;
    static constexpr std::string_view nats_subject = "ores.iam.v1.sessions.samples";
    std::string session_id;
};

struct get_session_samples_response {
    std::vector<session_sample_dto> samples;
};

}

#endif
