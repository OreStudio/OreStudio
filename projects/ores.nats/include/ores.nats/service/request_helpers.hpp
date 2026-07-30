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
#ifndef ORES_NATS_SERVICE_REQUEST_HELPERS_HPP
#define ORES_NATS_SERVICE_REQUEST_HELPERS_HPP

#include "ores.nats/domain/wire_codec.hpp"
#include "ores.nats/service/nats_client.hpp"
#include <chrono>
#include <string>
#include <string_view>

namespace ores::nats::service {

/**
 * @brief Encode @p req, send it as an unauthenticated request on
 * @p subject, and decode the reply as @p Response -- all via the
 * process-wide default_wire_codec().
 *
 * The shared primitive behind both ores::qt::ClientManager's and
 * ores.shell's request helpers: previously each had its own
 * hard-coded rfl::json::write/read pair (ClientManager's inline, and
 * copy-pasted independently into ~13 ores.shell command files).
 * Exceptions from the transport (nats_connect_error,
 * session_expired_error, etc.) propagate uncaught -- callers with
 * different error-reporting conventions (std::expected for
 * ClientManager, std::optional plus an ostream message for
 * ores.shell) wrap this in their own try/catch.
 */
template <typename Response, typename Request>
rfl::Result<Response>
request_and_decode(nats_client& session, std::string_view subject, const Request& req) {
    const auto& codec = default_wire_codec();
    const auto bytes = codec.encode(req);
    const std::string body(reinterpret_cast<const char*>(bytes.data()), bytes.size());
    const auto reply = session.request(subject, body);
    return codec.decode<Response>(reply.data);
}

/**
 * @brief Like request_and_decode(), but sends an authenticated
 * request (Bearer token from @p session) with an optional @p timeout.
 */
template <typename Response, typename Request>
rfl::Result<Response>
authenticated_request_and_decode(nats_client& session,
                                 std::string_view subject,
                                 const Request& req,
                                 std::chrono::milliseconds timeout = std::chrono::seconds(30)) {
    const auto& codec = default_wire_codec();
    const auto reply = session.authenticated_request(subject, codec.encode(req), timeout);
    return codec.decode<Response>(reply.data);
}

/**
 * @brief Like request_and_decode()/authenticated_request_and_decode(),
 * but derives the subject and response type from @p req itself
 * (@c Request::nats_subject / @c Request::response_type) rather than
 * taking them explicitly, and picks authenticated vs. unauthenticated
 * via @p authenticated. Matches the shape ores.shell's
 * provision_commands.cpp wizard call sites already used.
 */
template <typename Request>
rfl::Result<typename Request::response_type>
request_and_decode(nats_client& session,
                   const Request& req,
                   std::chrono::milliseconds timeout = std::chrono::seconds(30),
                   bool authenticated = false) {
    using Response = typename Request::response_type;
    const auto& codec = default_wire_codec();
    const auto bytes = codec.encode(req);
    const auto subject = std::string(req.nats_subject);
    const auto reply =
        authenticated ?
            session.authenticated_request(subject, bytes, timeout) :
            session.request(subject,
                            std::string(reinterpret_cast<const char*>(bytes.data()), bytes.size()));
    return codec.decode<Response>(reply.data);
}

}

#endif
