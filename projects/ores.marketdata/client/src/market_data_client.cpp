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
#include "ores.marketdata.api/messaging/feed_binding_protocol.hpp"
#include "ores.marketdata.api/messaging/market_observation_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::marketdata::client {

namespace {

/**
 * @brief The page size that stands in for "give me everything".
 *
 * Every canonical read is paginated and the callers of this facade have always
 * read a single, generously sized page. Stating the number once keeps the
 * behaviour they rely on visible rather than repeated at each call site.
 */
constexpr std::uint32_t all_rows = 10000;

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
        const auto& codec = ores::nats::default_wire_codec();
        const auto bytes = codec.encode(request);
        const auto reply = nats.authenticated_request(Request::nats_subject, bytes);

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

        auto result = codec.decode<Response>(reply.data);
        if (!result)
            return std::unexpected(std::string("Failed to decode response from ") +
                                   std::string(Request::nats_subject) + ": " +
                                   result.error().what());
        return std::move(*result);
    } catch (const std::exception& e) {
        return std::unexpected(std::string(e.what()));
    }
}

/**
 * @brief The failure a response body states, as one line for the caller.
 *
 * A canonical response carries its outcome in the body, so a caller that only
 * checks the transport would read data the service refused to return.
 */
std::string describe(const ores::utility::domain::result& result) {
    if (result.message.empty())
        return result.code.empty() ? std::string("request failed") : result.code;
    return result.code.empty() ? result.message : result.code + ": " + result.message;
}

/**
 * @brief The series id a caller states as text, or why it is not a UUID.
 *
 * The methods keep taking the id as text as their callers do, and the canonical
 * request carries a UUID, so the conversion is stated once here.
 */
std::expected<boost::uuids::uuid, std::string> parse_series_id(const std::string& series_id) {
    try {
        boost::uuids::string_generator generate;
        return generate(series_id);
    } catch (const std::exception& e) {
        return std::unexpected("Invalid series id '" + series_id + "': " + e.what());
    }
}

} // namespace

market_data_client::market_data_client(ores::nats::service::nats_client& nats)
    : nats_(nats) {}

std::expected<std::vector<domain::market_series>, std::string>
market_data_client::list_series(const std::string& /*series_type*/) {
    messaging::list_market_series_request req;
    req.limit = all_rows;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    if (resp->result.outcome != ores::utility::domain::outcome::ok)
        return std::unexpected(describe(resp->result));
    return std::move(resp->market_series);
}

std::expected<int, std::string>
market_data_client::save_series(const std::vector<domain::market_series>& series) {
    int count = 0;
    for (const auto& s : series) {
        messaging::put_market_series_request req;
        req.change.write.id = s.id;
        req.change.write.party_id = s.party_id;
        req.change.write.oresmd_uri = s.oresmd_uri;
        req.change.write.series_subclass = s.series_subclass;
        req.change.write.derivation_kind = s.derivation_kind;
        req.change.write.derivation_config_id = s.derivation_config_id;
        req.change.write.derivation_config_version = s.derivation_config_version;
        // A save states no expectation about the row it writes, so the change
        // lands whether the row is new or already present.
        req.change.precondition.kind = ores::utility::domain::precondition_kind::any;
        req.intent.reason_code = s.change_reason_code;
        req.intent.commentary = s.change_commentary;
        auto resp = send(nats_, req);
        if (!resp)
            return std::unexpected(resp.error());
        if (resp->result.outcome != ores::utility::domain::outcome::ok)
            return std::unexpected(describe(resp->result));
        ++count;
    }
    return count;
}

std::expected<std::optional<domain::market_series>, std::string>
market_data_client::find_series_by_uri(const std::string& oresmd_uri, const std::string& party_id) {
    messaging::list_market_series_request req;
    req.limit = all_rows;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    if (resp->result.outcome != ores::utility::domain::outcome::ok)
        return std::unexpected(describe(resp->result));
    for (auto& s : resp->market_series) {
        if (s.oresmd_uri == oresmd_uri &&
            (party_id.empty() || boost::uuids::to_string(s.party_id) == party_id))
            return std::optional<domain::market_series>(std::move(s));
    }
    return std::optional<domain::market_series>();
}

std::expected<std::vector<domain::market_observation>, std::string>
market_data_client::list_observations(const std::string& series_id) {
    auto id = parse_series_id(series_id);
    if (!id)
        return std::unexpected(id.error());
    messaging::list_by_series_id_market_observations_request req;
    req.series_id = *id;
    req.limit = all_rows;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    if (resp->result.outcome != ores::utility::domain::outcome::ok)
        return std::unexpected(describe(resp->result));
    return std::move(resp->market_observations);
}

std::expected<std::vector<domain::market_observation>, std::string>
market_data_client::list_observations_page(const std::string& series_id,
                                           std::uint32_t offset,
                                           std::uint32_t limit) {
    auto id = parse_series_id(series_id);
    if (!id)
        return std::unexpected(id.error());
    messaging::list_by_series_id_market_observations_request req;
    req.series_id = *id;
    req.offset = offset;
    req.limit = limit;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    if (resp->result.outcome != ores::utility::domain::outcome::ok)
        return std::unexpected(describe(resp->result));
    return std::move(resp->market_observations);
}

std::expected<int, std::string>
market_data_client::save_observations(const std::vector<domain::market_observation>& observations) {
    int count = 0;
    for (const auto& obs : observations) {
        messaging::put_market_observation_request req;
        req.change.write.id = obs.id;
        req.change.write.party_id = obs.party_id;
        req.change.write.series_id = obs.series_id;
        req.change.write.observation_datetime = obs.observation_datetime;
        req.change.write.oresmd_uri = obs.oresmd_uri;
        req.change.write.value = obs.value;
        req.change.write.source = obs.source;
        // A save states no expectation about the row it writes, so the change
        // lands whether the row is new or already present.
        req.change.precondition.kind = ores::utility::domain::precondition_kind::any;
        auto resp = send(nats_, req);
        if (!resp)
            return std::unexpected(resp.error());
        if (resp->result.outcome != ores::utility::domain::outcome::ok)
            return std::unexpected(describe(resp->result));
        ++count;
    }
    return count;
}

std::expected<std::vector<domain::feed_binding>, std::string>
market_data_client::list_feed_bindings() {
    messaging::list_feed_bindings_request req;
    req.limit = all_rows;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    if (resp->result.outcome != ores::utility::domain::outcome::ok)
        return std::unexpected(describe(resp->result));
    return std::move(resp->feed_bindings);
}

std::expected<bool, std::string>
market_data_client::save_feed_binding(const domain::feed_binding& binding) {
    messaging::put_feed_binding_request req;
    req.change.write.id = binding.id;
    req.change.write.party_id = binding.party_id;
    req.change.write.oresmd_uri = binding.oresmd_uri;
    req.change.write.source_name = binding.source_name;
    req.change.write.asset_class = binding.asset_class;
    req.change.write.enabled = binding.enabled;
    // A save states no expectation about the row it writes, so the change
    // lands whether the row is new or already present.
    req.change.precondition.kind = ores::utility::domain::precondition_kind::any;
    req.intent.reason_code = binding.change_reason_code;
    req.intent.commentary = binding.change_commentary;
    auto resp = send(nats_, req);
    if (!resp)
        return std::unexpected(resp.error());
    if (resp->result.outcome != ores::utility::domain::outcome::ok)
        return std::unexpected(describe(resp->result));
    return true;
}

}
