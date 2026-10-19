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
#include "ores.iam.client/client/run_grant_creator.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.iam.client/client/run_token_minter.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/wire_codec.hpp"
#include <exception>

namespace ores::iam::client {

std::expected<std::string, std::string>
create_run_grant(ores::nats::service::nats_client& person_nats,
                 std::string_view resource,
                 std::string_view role,
                 std::string_view audience) {
    messaging::create_run_grant_request request;
    request.resource = std::string(resource);
    request.role = std::string(role);
    request.audience = std::string(audience);

    const auto& codec = ores::nats::default_wire_codec();
    try {
        const auto reply = person_nats.authenticated_request(
            messaging::create_run_grant_request::nats_subject, codec.encode(request));
        if (const auto error = reply.headers.find(std::string(ores::nats::headers::x_error));
            error != reply.headers.end())
            return std::unexpected("IAM refused the run grant for " + std::string(resource) + ": " +
                                   error->second);
        const auto granted = codec.decode<messaging::create_run_grant_response>(reply.data);
        if (!granted)
            return std::unexpected("IAM answered the run grant for " + std::string(resource) +
                                   " unreadably");
        if (!granted->success)
            return std::unexpected(granted->message);
        return granted->grant_id;
    } catch (const std::exception& e) {
        return std::unexpected(std::string("The IAM run grant call failed: ") + e.what());
    }
}

}
