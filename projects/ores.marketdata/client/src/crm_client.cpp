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
#include "ores.marketdata.client/crm_client.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <rfl/json.hpp>
#include <unordered_map>

namespace ores::marketdata::client {

crm_client::crm_client(ores::nats::service::client& nats,
                       std::function<std::string(bool)> token_provider)
    : nats_(nats)
    , token_provider_(std::move(token_provider)) {}

crm_client::rates_result
crm_client::rates(const std::string& party_id, const std::string& crm_name, bool inverted) {
    rates_result result;
    try {
        messaging::get_crm_rates_request req;
        req.party_id = party_id;
        req.crm_name = crm_name;
        req.inverted = inverted;
        const auto req_json = rfl::json::write(req);

        std::unordered_map<std::string, std::string> headers;
        if (token_provider_)
            headers[std::string(ores::nats::headers::authorization)] =
                std::string(ores::nats::headers::bearer_prefix) + token_provider_(false);

        const auto reply = nats_.request_sync(messaging::get_crm_rates_request::nats_subject,
                                              ores::nats::as_bytes(req_json),
                                              std::move(headers));
        auto resp = rfl::json::read<messaging::get_crm_rates_response>(
            ores::nats::as_string_view(reply.data));
        if (!resp || !resp->success) {
            result.error = resp ? resp->message : "parse error (malformed or error reply)";
            return result;
        }
        result.success = true;
        result.rates = std::move(resp->rates);
        return result;
    } catch (const std::exception& e) {
        result.error = e.what();
        return result;
    }
}

}
