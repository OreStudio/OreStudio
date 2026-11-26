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
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/messaging/registrar.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

// The handler suites prove that each read answers; they do not prove that the
// service subscribes it. A subject with a working handler and no subscription
// looks healthy from the handler's side and answers every caller with no
// responder. These cases hold the wiring itself: what
// registrar::register_handlers registers, which is what the service runs.

namespace {

using ores::marketdata::messaging::get_series_slice_request;

/// The subjects registrar::register_handlers subscribed, as NATS knows them:
/// the environment's prefix already applied.
std::vector<std::string> registered_subjects(ores::nats::service::client& nats,
                                             const ores::database::context& ctx) {
    ores::nats::service::nats_client auth_nats(nats, [](bool) { return std::string{}; });
    auto subs = ores::marketdata::messaging::registrar::register_handlers(
        nats, ctx, auth_nats, std::nullopt, "http://localhost:1");

    std::vector<std::string> subjects;
    subjects.reserve(subs.size());
    for (const auto& sub : subs)
        subjects.push_back(sub.subject());
    return subjects;
}

}

TEST_CASE("the_service_registers_the_series_slice_subject", "[messaging][registration]") {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();
    REQUIRE(nats.is_connected());

    const auto subjects = registered_subjects(nats, db.context());
    const auto slice = nats.make_subject(get_series_slice_request::nats_subject);

    CHECK(std::ranges::find(subjects, slice) != subjects.end());
}
