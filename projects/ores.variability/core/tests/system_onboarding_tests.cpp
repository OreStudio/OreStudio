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
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.variability.core/messaging/operations_handler.hpp"
#include "ores.variability.core/service/system_settings_service.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <functional>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string tags("[variability][onboarding]");

// Every tenant-wide setting is scoped to the tenant's system party, and the
// handler's token must carry that party for the settings table's party
// isolation to admit the write.
boost::uuids::uuid system_party(ores::testing::scoped_database_helper& h) {
    for (const auto& p : ores::refdata::repository::party_repository().read_latest(h.context()))
        if (p.tenant_id == h.tenant_id() && p.party_category == "System")
            return p.id;
    throw std::runtime_error("the test tenant has no system party");
}

// The test's own subject, on a queue the running service does not hold, so the
// request is answered here rather than by the fleet's variability service.
std::string subject() {
    return "ores.test.variability.complete_system_onboarding";
}

struct fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;
    boost::uuids::uuid party;

    fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        party = system_party(db);
        auto ops = std::make_shared<ores::variability::messaging::operations_handler>(
            nats, db.context(), std::optional(keys.verifier()));
        subs.push_back(nats.queue_subscribe(subject(), "ores.test", [ops](ores::nats::message m) {
            ops->complete_system_onboarding(std::move(m));
        }));
    }
};

}

using ores::testing::decode_reply;
using ores::testing::request;
using ores::utility::domain::outcome;
using ores::variability::messaging::complete_system_onboarding_request;
using ores::variability::messaging::complete_system_onboarding_response;
using ores::variability::service::system_settings_service;

TEST_CASE("the onboarding system flag the service writes is the one it reads back", tags) {
    ores::testing::scoped_database_helper db;
    system_settings_service svc(db.context(), db.tenant_id().to_string());

    svc.refresh();
    svc.set_onboarding_system_complete(false, "test", "system.test", "baseline");
    svc.refresh();
    REQUIRE_FALSE(svc.is_onboarding_system_complete());

    svc.set_onboarding_system_complete(true, "test", "system.test", "the wizard finished");
    svc.refresh();
    REQUIRE(svc.is_onboarding_system_complete());
}

TEST_CASE("complete_system_onboarding answers ok and lands the flag", tags) {
    fixture f;
    const auto token = f.keys.token(f.db.tenant_id().to_string(), f.party, {});

    const auto reply = request(f.nats, subject(), token, complete_system_onboarding_request{});
    const auto decoded = decode_reply<complete_system_onboarding_response>(reply);
    REQUIRE(decoded);
    CHECK(decoded->result.outcome == outcome::ok);

    system_settings_service svc(f.db.context(), f.db.tenant_id().to_string());
    svc.refresh();
    CHECK(svc.is_onboarding_system_complete());
}

TEST_CASE("complete_system_onboarding reports a failed write as operation_failed", tags) {
    fixture f;
    // A party that is not the row's own fails the settings table's party
    // isolation when the write lands, which is the handler's failure path.
    const boost::uuids::uuid outsider = boost::uuids::random_generator()();
    const auto token = f.keys.token(f.db.tenant_id().to_string(), outsider, {});

    const auto reply = request(f.nats, subject(), token, complete_system_onboarding_request{});
    const auto decoded = decode_reply<complete_system_onboarding_response>(reply);
    REQUIRE(decoded);
    CHECK(decoded->result.outcome == outcome::failed);
    CHECK(decoded->result.code == "operation_failed");
    CHECK_FALSE(decoded->result.message.empty());
}
