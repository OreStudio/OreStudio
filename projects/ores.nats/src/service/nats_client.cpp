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
#include "ores.nats/service/nats_client.hpp"

#include <stdexcept>
#include <unordered_map>
#include "ores.nats/service/client.hpp"

namespace ores::nats::service {

using namespace ores::logging;

nats_client::nats_client(client& nats, token_provider provider)
    : external_client_(&nats)
    , token_provider_(std::move(provider)) {}

void nats_client::connect(config::nats_options opts) {
    const std::string url = opts.url;
    const std::string prefix =
        opts.subject_prefix.empty() ? "(none)" : opts.subject_prefix;
    owned_client_ = std::make_shared<client>(std::move(opts));
    try {
        owned_client_->connect();
    } catch (...) {
        owned_client_.reset();
        throw;
    }
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS at " << url
                              << " (namespace: '" << prefix << "')";
}

void nats_client::disconnect() {
    if (owned_client_) {
        owned_client_->disconnect();
        owned_client_.reset();
    }
    auth_.reset();
    BOOST_LOG_SEV(lg(), info) << "Disconnected from NATS";
}

bool nats_client::is_connected() const noexcept {
    if (owned_client_)
        return owned_client_->is_connected();
    return external_client_ != nullptr;
}

void nats_client::set_auth(login_info info) {
    auth_ = std::move(info);
}

void nats_client::clear_auth() {
    auth_.reset();
}

bool nats_client::is_logged_in() const noexcept {
    return auth_.has_value() || static_cast<bool>(token_provider_);
}

const nats_client::login_info& nats_client::auth() const {
    if (!auth_)
        throw std::runtime_error("Not logged in");
    return *auth_;
}

client& nats_client::active_client() const {
    if (owned_client_)
        return *owned_client_;
    if (external_client_)
        return *external_client_;
    throw std::runtime_error("Not connected to NATS");
}

message nats_client::request(std::string_view subject, std::string_view json_body) {
    return active_client().request_sync(subject, as_bytes(json_body));
}

message nats_client::do_authenticated_request(std::string_view subject,
    std::span<const std::byte> body, std::chrono::milliseconds timeout) {

    // Service path: token_provider owns acquisition and proactive refresh.
    if (token_provider_) {
        const auto make_headers = [&](bool force = false) {
            std::unordered_map<std::string, std::string> hdrs{
                {std::string(headers::authorization),
                 std::string(headers::bearer_prefix) + token_provider_(force)}};
            if (!delegation_token_.empty())
                hdrs[std::string(headers::delegated_authorization)] =
                    std::string(headers::bearer_prefix) + delegation_token_;
            if (!correlation_id_.empty())
                hdrs[std::string(headers::nats_correlation_id)] = correlation_id_;
            if (!session_id_.empty())
                hdrs[std::string(headers::nats_session_id)] = session_id_;
            return hdrs;
        };
        // Reactive re-auth: the server rejected the token as expired. Pass
        // force=true so the provider re-authenticates unconditionally,
        // covering extreme clock-skew cases where the client's own expiry
        // timer has not yet fired.
        auto reply = active_client().request_sync(
            subject, body, make_headers(), timeout);
        const auto x_err = reply.headers.find(std::string(headers::x_error));
        if (x_err != reply.headers.end() && x_err->second == "token_expired") {
            BOOST_LOG_SEV(lg(), info)
                << "Service token expired on " << subject << "; re-authenticating";
            reply = active_client().request_sync(
                subject, body, make_headers(true), timeout);
        }
        return reply;
    }

    // Interactive path: reactive — throw on server-signalled expiry.
    if (!auth_)
        throw std::runtime_error("Not authenticated");

    std::unordered_map<std::string, std::string> hdrs{
        {std::string(headers::authorization),
         std::string(headers::bearer_prefix) + auth_->jwt}};
    if (!delegation_token_.empty())
        hdrs[std::string(headers::delegated_authorization)] =
            std::string(headers::bearer_prefix) + delegation_token_;
    if (!correlation_id_.empty())
        hdrs[std::string(headers::nats_correlation_id)] = correlation_id_;
    if (!session_id_.empty())
        hdrs[std::string(headers::nats_session_id)] = session_id_;

    const auto reply = active_client().request_sync(subject, body, hdrs, timeout);

    const auto x_error_it = reply.headers.find(std::string(headers::x_error));
    if (x_error_it == reply.headers.end())
        return reply;

    if (x_error_it->second == "token_expired" ||
        x_error_it->second == "max_session_exceeded") {
        throw session_expired_error("Session has expired. Please log in again.");
    }
    return reply;
}

nats_client nats_client::with_delegation(std::string token) const {
    nats_client copy = *this;
    copy.delegation_token_ = std::move(token);
    return copy;
}

nats_client nats_client::with_correlation_id(std::string cid) const {
    nats_client copy = *this;
    copy.correlation_id_ = std::move(cid);
    return copy;
}

nats_client nats_client::with_session_id(std::string sid) const {
    nats_client copy = *this;
    copy.session_id_ = std::move(sid);
    return copy;
}

std::string extract_bearer(const ores::nats::message& msg) {
    const auto it = msg.headers.find(std::string(headers::authorization));
    if (it == msg.headers.end())
        return {};
    const auto& val = it->second;
    if (!val.starts_with(headers::bearer_prefix))
        return {};
    return val.substr(headers::bearer_prefix.size());
}

message nats_client::authenticated_request(std::string_view subject,
    std::string_view json_body, std::chrono::milliseconds timeout) {
    return do_authenticated_request(subject, as_bytes(json_body), timeout);
}

message nats_client::authenticated_request(std::string_view subject,
    std::span<const std::byte> body, std::chrono::milliseconds timeout) {
    return do_authenticated_request(subject, body, timeout);
}

std::shared_ptr<client> nats_client::get_client() const {
    return owned_client_;
}

} // namespace ores::nats::service
