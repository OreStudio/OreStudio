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
#ifndef ORES_TELEMETRY_MESSAGING_NATS_SAMPLES_PROTOCOL_HPP
#define ORES_TELEMETRY_MESSAGING_NATS_SAMPLES_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>
#include "ores.telemetry/domain/nats_server_sample.hpp"
#include "ores.telemetry/domain/nats_stream_sample.hpp"
#include "ores.telemetry/domain/nats_samples_query.hpp"

namespace ores::telemetry::messaging {

struct get_nats_server_samples_request {
    using response_type = struct get_nats_server_samples_response;
    static constexpr std::string_view nats_subject =
        "ores.telemetry.v1.nats.server-samples.list";
    domain::nats_server_samples_query query;
};

struct get_nats_server_samples_response {
    bool success{false};
    std::string message;
    std::vector<domain::nats_server_sample> samples;
};

struct get_nats_stream_samples_request {
    using response_type = struct get_nats_stream_samples_response;
    static constexpr std::string_view nats_subject =
        "ores.telemetry.v1.nats.stream-samples.list";
    domain::nats_stream_samples_query query;
};

struct get_nats_stream_samples_response {
    bool success{false};
    std::string message;
    std::vector<domain::nats_stream_sample> samples;
};

}

#endif
