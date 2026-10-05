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
#ifndef ORES_ORE_SERVICE_MESSAGING_NATS_CALL_HPP
#define ORES_ORE_SERVICE_MESSAGING_NATS_CALL_HPP

#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <format>
#include <optional>
#include <string>

namespace ores::ore::service::messaging {

/**
 * @brief Makes an authenticated NATS request and deserialises the response.
 *
 * Returns nullopt and populates out_error on any error.
 */
template <typename Req>
std::optional<typename Req::response_type>
nats_call(ores::nats::service::nats_client& nats, const Req& request, std::string& out_error) {
    using Resp = typename Req::response_type;
    try {
        const auto& codec = ores::nats::default_wire_codec();
        const auto msg = nats.authenticated_request(Req::nats_subject, codec.encode(request));

        const auto err_it = msg.headers.find("X-Error");
        if (err_it != msg.headers.end()) {
            out_error = std::format("Service error on {}: {}", Req::nats_subject, err_it->second);
            return std::nullopt;
        }
        auto result = codec.decode<Resp>(msg.data);
        if (!result) {
            out_error = std::format(
                "Failed to parse response from {}: {}", Req::nats_subject, result.error().what());
            return std::nullopt;
        }
        // A canonical response states its outcome in a result; an older
        // response states it in success and message.
        if constexpr (requires { result->result.outcome; }) {
            if (result->result.outcome != ores::utility::domain::outcome::ok)
                out_error = result->result.message;
        } else if constexpr (requires {
                                 result->success;
                                 result->message;
                             }) {
            if (!result->success)
                out_error = result->message;
        }
        return *result;
    } catch (const std::exception& e) {
        out_error = std::format("Exception calling {}: {}", Req::nats_subject, e.what());
        return std::nullopt;
    }
}

}

#endif
