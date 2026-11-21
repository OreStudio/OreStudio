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
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_nats_handler_system_owned_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.nats/domain/headers.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.orgmode.api/messaging/knowledge_page_protocol.hpp"
#include "ores.orgmode.core/messaging/knowledge_page_handler.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace {

const std::string knowledge_page_subjects("ores.test.orgmode.knowledge_page_system_owned.");

struct knowledge_page_system_owned_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;

    knowledge_page_system_owned_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs.push_back(
            nats.queue_subscribe(knowledge_page_subjects + "put_knowledge_page",
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::orgmode::messaging::knowledge_page_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .put_knowledge_page(std::move(m));
                                 }));
        subs.push_back(
            nats.queue_subscribe(knowledge_page_subjects + "put_many_knowledge_pages",
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::orgmode::messaging::knowledge_page_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .put_many_knowledge_pages(std::move(m));
                                 }));
        subs.push_back(
            nats.queue_subscribe(knowledge_page_subjects + "delete_knowledge_page",
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::orgmode::messaging::knowledge_page_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .delete_knowledge_page(std::move(m));
                                 }));
        subs.push_back(
            nats.queue_subscribe(knowledge_page_subjects + "delete_many_knowledge_pages",
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::orgmode::messaging::knowledge_page_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .delete_many_knowledge_pages(std::move(m));
                                 }));
    }

    /**
     * @brief The reply to a body no wire format decodes, from a session of @p tenant
     * that holds every write permission the entity names.
     */
    ores::nats::message send(const std::string& method,
                             const ores::utility::uuid::tenant_id& tenant) {
        const auto token = keys.token_without_party(
            tenant.to_string(),
            {"orgmode::knowledge_pages:write", "orgmode::knowledge_pages:delete"});
        std::unordered_map<std::string, std::string> headers;
        headers[std::string(ores::nats::headers::authorization)] =
            std::string(ores::nats::headers::bearer_prefix) + token;
        const std::array<std::byte, 1> undecodable{std::byte{0xc1}};
        return nats.request_sync(knowledge_page_subjects + method, undecodable, headers);
    }
};

}

TEST_CASE("knowledge_page_handler_denies_a_write_from_another_tenant",
          "[messaging][system_owned]") {
    knowledge_page_system_owned_fixture f;
    {
        const auto reply =
            ores::testing::decode_reply<ores::orgmode::messaging::put_knowledge_page_response>(
                f.send("put_knowledge_page", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
    {
        const auto reply = ores::testing::decode_reply<
            ores::orgmode::messaging::put_many_knowledge_pages_response>(
            f.send("put_many_knowledge_pages", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
    {
        const auto reply =
            ores::testing::decode_reply<ores::orgmode::messaging::delete_knowledge_page_response>(
                f.send("delete_knowledge_page", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
    {
        const auto reply = ores::testing::decode_reply<
            ores::orgmode::messaging::delete_many_knowledge_pages_response>(
            f.send("delete_many_knowledge_pages", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
}

TEST_CASE("knowledge_page_handler_lets_the_system_tenant_write", "[messaging][system_owned]") {
    knowledge_page_system_owned_fixture f;
    CHECK(ores::testing::error_of(f.send(
              "put_knowledge_page", ores::utility::uuid::tenant_id::system())) == "bad_request");
    CHECK(ores::testing::error_of(
              f.send("put_many_knowledge_pages", ores::utility::uuid::tenant_id::system())) ==
          "bad_request");
    CHECK(ores::testing::error_of(f.send(
              "delete_knowledge_page", ores::utility::uuid::tenant_id::system())) == "bad_request");
    CHECK(ores::testing::error_of(
              f.send("delete_many_knowledge_pages", ores::utility::uuid::tenant_id::system())) ==
          "bad_request");
}
