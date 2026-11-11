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
#include "ores.http/routes/iam/tenant_parties.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/messaging/party_protocol.hpp"
#include <cstdint>

namespace ores::http::routes::iam {

namespace {

/// How many parties one page reads, as the BFF read them.
constexpr std::uint32_t tenant_party_page = 500;

}

std::vector<ores::refdata::domain::party>
tenant_parties::every(const ores::http::domain::http_request& req,
                      ores::nats::service::nats_client& session) {
    std::vector<ores::refdata::domain::party> parties;
    for (std::uint32_t offset = 0;; offset += tenant_party_page) {
        ores::refdata::messaging::list_parties_request msg;
        msg.offset = offset;
        msg.limit = tenant_party_page;
        const auto reply = ores::http::forwarding::forward(req, session, msg);
        ores::http::net::record_routes::require_done(reply.result);
        parties.insert(parties.end(), reply.parties.begin(), reply.parties.end());
        if (reply.parties.empty() || parties.size() >= reply.total)
            return parties;
    }
}

}
