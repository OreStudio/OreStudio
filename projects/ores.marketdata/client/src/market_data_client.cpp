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
#include "ores.marketdata.client/market_data_client.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <rfl/json.hpp>

namespace ores::marketdata::client {

namespace {

/**
 * @brief Issue a typed authenticated request and decode its response.
 *
 * Distinguishes three failure modes so callers get an actionable error rather
 * than an opaque "decode error":
 *   - a server error_reply (empty body + X-Error header) — surfaced as a
 *     proper authorisation/validation error;
 *   - a malformed/undecodable response body;
 *   - a transport exception.
 */
template <typename Request>
std::expected<typename Request::response_type, std::string>
send(ores::nats::service::nats_client& nats, const Request& request) {
    using Response = typename Request::response_type;
    try {
        const auto json = rfl::json::write(request);
        const auto reply = nats.authenticated_request(Request::nats_subject, json);

        // A handler that rejects the request (auth, permission, validation)
        // replies with an empty body and an X-Error header. Translate that into
        // an explicit error instead of attempting to decode an empty payload.
        const auto x_err = reply.headers.find(std::string(ores::nats::headers::x_error));
        if (x_err != reply.headers.end()) {
            const auto& code = x_err->second;
            std::string detail = code;
            if (code == "unauthorized")
                detail = "unauthorized: endpoint requires authentication";
            else if (code == "forbidden")
                detail = "forbidden: caller lacks the required permission";
            else if (code == "token_expired")
                detail = "token_expired: authentication token has expired";
            else if (code == "bad_request")
                detail = "bad_request: the request was rejected as invalid";
            return std::unexpected(std::string(Request::nats_subject) + " failed: " + detail);
        }

        const std::string_view sv(reinterpret_cast<const char*>(reply.data.data()),
                                  reply.data.size());
        auto result = rfl::json::read<Response>(sv);
        if (!result)
            return std::unexpected(std::string("Failed to decode response from ") +
                                   std::string(Request::nats_subject) + ": " +
                                   result.error().what());
        return std::move(*result);
    } catch (const std::exception& e) {
        return std::unexpected(std::string(e.what()));
    }
}

} // namespace

market_data_client::market_data_client(ores::nats::service::nats_client& nats)
    : nats_(nats) {}

std::expected<std::vector<domain::market_series>, std::string>
market_data_client::list_series(const std::string& /*series_type*/) {
    messaging::get_market_series_request req;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    return std::move(resp->market_series);
}

std::expected<int, std::string>
market_data_client::save_series(const std::vector<domain::market_series>& series) {
    int count = 0;
    for (const auto& s : series) {
        auto req = messaging::save_market_series_request::from(s);
        auto resp = send(nats_, req);
        if (!resp)
            return std::unexpected(resp.error());
        if (!resp->success)
            return std::unexpected(resp->message.empty() ? "save_series failed" : resp->message);
        ++count;
    }
    return count;
}

std::expected<std::optional<domain::market_series>, std::string> market_data_client::find_series(
    const std::string& series_type, const std::string& metric, const std::string& qualifier) {
    messaging::get_market_series_request req;
    req.limit = 10000;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    for (auto& s : resp->market_series) {
        if (s.series_type == series_type && s.metric == metric && s.qualifier == qualifier)
            return std::optional<domain::market_series>(std::move(s));
    }
    return std::optional<domain::market_series>();
}

std::expected<std::vector<domain::market_observation>, std::string>
market_data_client::list_observations(const std::string& series_id) {
    messaging::get_market_observations_request req;
    req.series_id = series_id;
    req.limit = 10000;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    return std::move(resp->market_observations);
}

std::expected<int, std::string>
market_data_client::save_observations(const std::vector<domain::market_observation>& observations) {
    int count = 0;
    for (const auto& obs : observations) {
        auto req = messaging::save_market_observation_request::from(obs);
        auto resp = send(nats_, req);
        if (!resp)
            return std::unexpected(resp.error());
        if (!resp->success)
            return std::unexpected(resp->message.empty() ? "save_observations failed" :
                                                           resp->message);
        ++count;
    }
    return count;
}

}
