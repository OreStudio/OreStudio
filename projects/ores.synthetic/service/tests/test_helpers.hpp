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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SYNTHETIC_SERVICE_TESTS_TEST_HELPERS_HPP_
#define ORES_SYNTHETIC_SERVICE_TESTS_TEST_HELPERS_HPP_

#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.synthetic.api/feeds/fx_spot_feed.hpp"
#include "provider_seed.hpp"
#include <catch2/catch_test_macros.hpp>

inline bool eventually(const std::function<bool()>& done) {
    for (int i = 0; i < 400; ++i) {
        if (done())
            return true;
        std::this_thread::sleep_for(std::chrono::milliseconds(10));
    }
    return done();
}

// Every seeded row is party-scoped and the JWT carries the same party, so
// the handler's RLS reads see what the test wrote. The party must exist in
// the parties table: set_party() and the JWT context both validate it.
inline boost::uuids::uuid resolve_test_party(ores::testing::scoped_database_helper& h) {
    namespace refdata_repo = ores::refdata::repository;
    const auto parties = refdata_repo::party_repository().read_latest(h.context());
    for (const auto& p : parties)
        if (p.tenant_id == h.tenant_id() && p.party_category == "System")
            return p.id;
    FAIL("No System party for tenant " << h.tenant_id().to_string());
    return {};
}

// Sends one folder request on the given subject with the Bearer header
// and decodes the response.
template <typename Req, typename Resp>
inline Resp send_request(ores::nats::service::client& nats,
                         const std::string& subject,
                         const std::string& token,
                         const Req& req) {
    auto reply = nats.request_sync(subject,
                                   ores::nats::default_wire_codec().encode(req),
                                   {{std::string(ores::nats::headers::authorization),
                                     std::string(ores::nats::headers::bearer_prefix) + token}});
    auto decoded = ores::nats::default_wire_codec().decode<Resp>(reply.data);
    REQUIRE(decoded);
    return *decoded;
}

inline const std::string test_audience("ores.synthetic.service");

inline const std::string test_issuer("ores.iam.test");

#endif
