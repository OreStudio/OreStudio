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
#include "ores.shell/service/nats_session.hpp"

#include <span>
#include <stdexcept>
#include <rfl/json.hpp>
#include "ores.nats/service/client.hpp"
#include "ores.iam/messaging/login_protocol.hpp"
#include "ores.shell/service/session_expired_error.hpp"

namespace ores::shell::service {

using namespace ores::logging;

void nats_session::connect(nats::config::nats_options opts) {
    const std::string url = opts.url;
    const std::string prefix = opts.subject_prefix.empty() ? "(none)" : opts.subject_prefix;
    client_ = std::make_shared<ores::nats::service::client>(std::move(opts));
    try {
        client_->connect();
    } catch (...) {
        client_.reset();
        throw;
    }
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS at " << url
                              << " (namespace: '" << prefix << "')";
}

void nats_session::disconnect() {
    if (client_) {
        client_->disconnect();
        client_.reset();
    }
    auth_.reset();
    BOOST_LOG_SEV(lg(), info) << "Disconnected from NATS";
}

bool nats_session::is_connected() const noexcept {
    return client_ && client_->is_connected();
}

void nats_session::set_auth(login_info info) {
    auth_ = std::move(info);
}

void nats_session::clear_auth() {
    auth_.reset();
}

bool nats_session::is_logged_in() const noexcept {
    return auth_.has_value();
}

const nats_session::login_info& nats_session::auth() const {
    if (!auth_) {
        throw std::runtime_error("Not logged in");
    }
    return *auth_;
}

ores::nats::message nats_session::request(std::string_view subject,
                                          std::string_view json_body) {
    if (!client_) {
        throw std::runtime_error("Not connected to NATS");
    }
    const auto* begin = reinterpret_cast<const std::byte*>(json_body.data());
    auto data = std::span<const std::byte>(begin, json_body.size());
    return client_->request_sync(subject, data);
}

ores::nats::message nats_session::authenticated_request(std::string_view subject,
                                                        std::string_view json_body,
                                                        std::chrono::milliseconds timeout) {
    if (!client_) {
        throw std::runtime_error("Not connected to NATS");
    }
    if (!auth_) {
        throw std::runtime_error("Not authenticated");
    }
    const auto* begin = reinterpret_cast<const std::byte*>(json_body.data());
    auto data = std::span<const std::byte>(begin, json_body.size());
    std::unordered_map<std::string, std::string> headers;
    headers["Authorization"] = "Bearer " + auth_->jwt;
    auto reply = client_->request_sync(subject, data, headers, timeout);

    const auto x_error_it = reply.headers.find("X-Error");
    if (x_error_it != reply.headers.end()) {
        if (x_error_it->second == "token_expired") {
            BOOST_LOG_SEV(lg(), info) << "Token expired, attempting refresh";
            refresh();
            headers["Authorization"] = "Bearer " + auth_->jwt;
            reply = client_->request_sync(subject, data, std::move(headers), timeout);
            const auto retry_x_error_it = reply.headers.find("X-Error");
            if (retry_x_error_it != reply.headers.end()) {
                if (retry_x_error_it->second == "max_session_exceeded") {
                    throw session_expired_error(
                        "Session has expired after the maximum allowed duration. "
                        "Please log in again.");
                } else if (retry_x_error_it->second == "token_expired") {
                    throw session_expired_error(
                        "Token is still expired after refresh. Please log in again.");
                }
            }
        } else if (x_error_it->second == "max_session_exceeded") {
            throw session_expired_error(
                "Session has expired after the maximum allowed duration. "
                "Please log in again.");
        }
    }
    return reply;
}

void nats_session::refresh() {
    if (!client_ || !auth_) {
        throw std::runtime_error("Cannot refresh: not connected or not authenticated");
    }
    std::unordered_map<std::string, std::string> headers;
    headers["Authorization"] = "Bearer " + auth_->jwt;
    auto reply = client_->request_sync(
        iam::messaging::refresh_request::nats_subject, std::span<const std::byte>{},
        std::move(headers));

    const auto x_error_it = reply.headers.find("X-Error");
    if (x_error_it != reply.headers.end() &&
        x_error_it->second == "max_session_exceeded") {
        throw session_expired_error(
            "Session has expired after the maximum allowed duration. "
            "Please log in again.");
    }

    const std::string_view sv(
        reinterpret_cast<const char*>(reply.data.data()), reply.data.size());
    auto result = rfl::json::read<iam::messaging::refresh_response>(sv);
    if (!result || !result->success || result->token.empty()) {
        const auto msg = result ? result->message : "refresh parse error";
        throw std::runtime_error("Token refresh failed: " + msg);
    }
    auth_->jwt = result->token;
    BOOST_LOG_SEV(lg(), info) << "JWT refreshed successfully";
}

std::shared_ptr<ores::nats::service::client> nats_session::get_client() const {
    return client_;
}

}
