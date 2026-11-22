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
/**
 * The level gate on the bundle publish verb, driven through the handler.
 *
 * A platform or world bundle writes into the system tenant once, and a tenant
 * reads those rows where they lie, so only the system tenant may ask for one.
 * Three facts pin that rule, and the first two are what make the third
 * meaningful: a tenant session must be able to read the bundle row at all, it
 * must be able to tell a bundle that does not exist from one it may not
 * publish, and it must be refused in words that say why.
 */
#include "ores.dq.api/messaging/publish_bundle_protocol.hpp"
#include "ores.dq.core/messaging/publish_handler.hpp"
#include "ores.dq.core/service/publish_service.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string publish_bundle_subject("ores.test.dq.publish_bundle_level.");
const std::string publish_bundle_method("publish_bundle");

/**
 * @brief The publish handler on a subject of its own, over the test tenant.
 */
struct publish_bundle_level_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;

    publish_bundle_level_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs.push_back(
            nats.queue_subscribe(publish_bundle_subject + publish_bundle_method,
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::dq::messaging::publish_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .publish_bundle(std::move(m));
                                 }));
    }

    /**
     * @brief Asks for @p bundle_code as @p tenant, or as the test tenant when none is given.
     *
     * The caller holds the one permission the verb asks for, so a refusal comes
     * from the level and never from the permission check.
     */
    ores::dq::messaging::publish_bundle_response
    publish(const std::string& bundle_code,
            std::optional<ores::utility::uuid::tenant_id> tenant = std::nullopt) {
        const auto caller = tenant.value_or(db.tenant_id());
        const auto token = keys.token_without_party(caller.to_string(), {"dq::datasets:write"});

        ores::dq::messaging::publish_bundle_request req;
        req.bundle_code = bundle_code;
        req.published_by = "publish-bundle-level-test";

        const auto reply = ores::testing::request(
            nats, publish_bundle_subject + publish_bundle_method, token, req);
        const auto decoded =
            ores::testing::decode_reply<ores::dq::messaging::publish_bundle_response>(reply);
        REQUIRE(decoded);
        return *decoded;
    }
};

}

TEST_CASE("a tenant reads the level of a bundle the platform holds", "[messaging][publish]") {
    publish_bundle_level_fixture f;
    ores::dq::service::publish_service svc(f.db.context());
    const auto level = svc.bundle_level("world");
    REQUIRE(level);
    CHECK(*level == "world");
}

TEST_CASE("a tenant is refused a world bundle, in words", "[messaging][publish]") {
    publish_bundle_level_fixture f;
    const auto resp = f.publish("world");
    CHECK_FALSE(resp.success);
    CHECK(resp.error_message.find("publishes into the system tenant") != std::string::npos);
    CHECK(resp.error_message.find("world") != std::string::npos);
}

TEST_CASE("a tenant is told when the bundle does not exist at all", "[messaging][publish]") {
    publish_bundle_level_fixture f;
    const auto resp = f.publish("no_such_bundle_at_all");
    CHECK_FALSE(resp.success);
    CHECK(resp.error_message == "No such bundle: no_such_bundle_at_all");
}

TEST_CASE("the level gate does not refuse a tenant bundle", "[messaging][publish]") {
    publish_bundle_level_fixture f;
    const auto resp = f.publish("tenant_defaults");
    CHECK(resp.error_message.find("publishes into the system tenant") == std::string::npos);
    CHECK(resp.error_message.find("No such bundle") == std::string::npos);
}
