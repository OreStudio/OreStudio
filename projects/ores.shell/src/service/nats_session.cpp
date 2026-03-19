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
#include "ores.nats/service/client.hpp"

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
    return client_->request_sync(subject, data, std::move(headers), timeout);
}

std::shared_ptr<ores::nats::service::client> nats_session::get_client() const {
    return client_;
}

}
