/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * The bundle roster verb, driven through the handler.
 *
 * The roster is the answer the coverage screen cannot build from the entity
 * reads: the level each bundle publishes at, how many datasets it carries, and
 * for the bundle a caller names, what each member has staged. Four facts pin
 * it: every bundle is named with its level, naming a bundle returns its members
 * in the bundle's own order, the members carry the counts the SQL function
 * returns, and the verb asks for the read and not the write.
 */
#include "ores.dq.api/messaging/bundle_roster_protocol.hpp"
#include "ores.dq.core/messaging/roster_handler.hpp"
#include "ores.dq.core/service/publish_service.hpp"
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string roster_subject("ores.test.dq.bundle_roster.");
const std::string roster_method("bundle_roster");

/**
 * @brief The roster handler on a subject of its own, over the test tenant.
 */
struct bundle_roster_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;

    bundle_roster_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs.push_back(nats.queue_subscribe(
            roster_subject + roster_method, "ores.test", [this](ores::nats::message m) {
                ores::dq::messaging::roster_handler(
                    nats, db.context(), std::optional(keys.verifier()))
                    .bundle_roster(std::move(m));
            }));
    }

    /**
     * @brief Asks for the roster, or for one bundle of it.
     *
     * The caller holds the read the verb asks for, so a refusal names the
     * permission gate and never the data.
     */
    ores::dq::messaging::bundle_roster_response roster(const std::string& bundle_code = "",
                                                       std::vector<std::string> permissions = {
                                                           "dq::datasets:read"}) {
        const auto token = keys.token_without_party(db.tenant_id().to_string(), permissions);
        ores::dq::messaging::bundle_roster_request req;
        req.bundle_code = bundle_code;

        const auto reply = ores::testing::request(nats, roster_subject + roster_method, token, req);
        const auto decoded =
            ores::testing::decode_reply<ores::dq::messaging::bundle_roster_response>(reply);
        REQUIRE(decoded);
        return *decoded;
    }
};

const ores::dq::messaging::bundle_roster_bundle*
find_bundle(const ores::dq::messaging::bundle_roster_response& resp, const std::string& code) {
    const auto it = std::find_if(
        resp.bundles.begin(), resp.bundles.end(), [&](const auto& b) { return b.code == code; });
    return it == resp.bundles.end() ? nullptr : &*it;
}

} // namespace

TEST_CASE("the roster names every bundle with the level it publishes at", "[messaging][roster]") {
    bundle_roster_fixture f;
    const auto resp = f.roster();

    REQUIRE(resp.success);
    CHECK(resp.error_message.empty());
    REQUIRE_FALSE(resp.bundles.empty());

    const auto* world = find_bundle(resp, "world");
    REQUIRE(world != nullptr);
    CHECK(world->level == "world");
    CHECK(world->dataset_count > 0);

    const auto* core = find_bundle(resp, "system_core");
    REQUIRE(core != nullptr);
    CHECK(core->level == "platform");

    // A roster that names no bundle states no members: the counts cost a scan
    // per member and are taken only for the bundle a caller names.
    CHECK(world->members.empty());
}

TEST_CASE("naming a bundle returns its members with what each has staged", "[messaging][roster]") {
    bundle_roster_fixture f;
    const auto resp = f.roster("world");

    REQUIRE(resp.success);
    const auto* world = find_bundle(resp, "world");
    REQUIRE(world != nullptr);
    REQUIRE_FALSE(world->members.empty());

    // The members arrive in the bundle's own order and none is a placeholder.
    const auto ordered = std::is_sorted(
        world->members.begin(), world->members.end(), [](const auto& left, const auto& right) {
            return left.display_order < right.display_order;
        });
    CHECK(ordered);
    CHECK(world->members.front().display_order == 1);
    for (const auto& member : world->members) {
        CHECK_FALSE(member.dataset_code.empty());
        CHECK_FALSE(member.artefact_type.empty());
    }

    // The counts are the function's, not a number this handler invented.
    ores::dq::service::publish_service svc(f.db.context());
    const auto direct = svc.list_bundle_members("world");
    REQUIRE(direct.size() == world->members.size());
    for (std::size_t i = 0; i < direct.size(); ++i) {
        CHECK(world->members[i].dataset_code == direct[i].dataset_code);
        CHECK(world->members[i].staged_count == direct[i].staged_count);
        CHECK(world->members[i].is_publishable == direct[i].is_publishable);
    }

    // The world bundle stages real rows, so at least one member counts above
    // zero; a run that returned all zeroes would be the failure this exists for.
    const auto staged = std::any_of(world->members.begin(),
                                    world->members.end(),
                                    [](const auto& member) { return member.staged_count > 0; });
    CHECK(staged);
}

TEST_CASE("a bundle that carries nothing answers no members", "[messaging][roster]") {
    bundle_roster_fixture f;
    const auto resp = f.roster("no_such_bundle_at_all");

    REQUIRE(resp.success);
    CHECK(find_bundle(resp, "no_such_bundle_at_all") == nullptr);
}

TEST_CASE("the roster asks for the read and not the write", "[messaging][roster]") {
    bundle_roster_fixture f;

    // A caller holding the write and not the read is refused, and the refusal
    // is an error reply rather than a roster that says it failed: the verb
    // never reaches the data.
    const auto token =
        f.keys.token_without_party(f.db.tenant_id().to_string(), {"dq::datasets:write"});
    ores::dq::messaging::bundle_roster_request req;
    const auto reply = ores::testing::request(f.nats, roster_subject + roster_method, token, req);

    REQUIRE(reply.headers.contains(std::string(ores::nats::headers::x_error)));
    CHECK(reply.headers.at(std::string(ores::nats::headers::x_error)) == "forbidden");
}
