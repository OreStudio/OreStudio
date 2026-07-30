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
#ifndef ORES_SHELL_APP_REQUEST_HELPERS_HPP
#define ORES_SHELL_APP_REQUEST_HELPERS_HPP

#include "ores.nats/service/request_helpers.hpp"
#include "ores.shell/app/command_feedback.hpp"
#include <chrono>
#include <optional>
#include <ostream>
#include <string_view>

namespace ores::shell::app::commands {

/**
 * @brief Send an unauthenticated NATS request and decode its response,
 * reporting failure through the shell's ostream/command_feedback
 * convention.
 *
 * Thin wrapper over ores::nats::service::request_and_decode() -- the
 * primitive shared with ores::qt::ClientManager -- adapting its
 * rfl::Result return into the std::optional-plus-fail(out)-message
 * convention every shell command already expects. Replaces the
 * copy-pasted per-command-file @c do_request template that used to
 * hard-code @c rfl::json::write/read directly in ~13 files.
 */
template <typename Response, typename Request>
std::optional<Response> do_request(std::ostream& out,
                                   ores::nats::service::nats_client& session,
                                   std::string_view subject,
                                   const Request& req) {
    try {
        auto result = ores::nats::service::request_and_decode<Response>(session, subject, req);
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what() << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

/**
 * @brief Send an authenticated NATS request and decode its response.
 *
 * See do_request() -- same shape, plus a Bearer token from @p session and
 * an optional @p timeout (default 30s, matching the pre-existing
 * per-command-file copies' default).
 */
template <typename Response, typename Request>
std::optional<Response>
do_auth_request(std::ostream& out,
                ores::nats::service::nats_client& session,
                std::string_view subject,
                const Request& req,
                std::chrono::milliseconds timeout = std::chrono::seconds(30)) {
    try {
        auto result = ores::nats::service::authenticated_request_and_decode<Response>(
            session, subject, req, timeout);
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what() << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

/**
 * @brief Like do_request(), but derives the subject and response type from
 * @p req itself (@c Request::nats_subject / @c Request::response_type),
 * and can optionally send authenticated. Matches the shape
 * =provision_commands.cpp='s wizard call sites already used, kept as a
 * distinct overload rather than folded into do_request()/do_auth_request()
 * above since those two take an explicit subject and response type instead.
 */
template <typename Request>
std::optional<typename Request::response_type>
do_request(std::ostream& out,
           ores::nats::service::nats_client& session,
           const Request& req,
           std::chrono::milliseconds timeout = std::chrono::seconds(30),
           bool authenticated = false) {
    try {
        auto result = ores::nats::service::request_and_decode(session, req, timeout, authenticated);
        if (!result) {
            fail(out) << "Failed to parse response: " << result.error().what() << std::endl;
            return std::nullopt;
        }
        return *result;
    } catch (const std::exception& e) {
        fail(out) << "Request failed: " << e.what() << std::endl;
        return std::nullopt;
    }
}

}

#endif
