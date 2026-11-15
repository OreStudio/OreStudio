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
#include "ores.reporting.api/messaging/report_type_protocol.hpp"
#include "ores.reporting.core/messaging/report_type_handler.hpp"
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

const std::string report_type_subjects("ores.test.reporting.report_type_system_owned.");

struct report_type_system_owned_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;

    report_type_system_owned_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs.push_back(nats.queue_subscribe(
            report_type_subjects + "put_report_type", "ores.test", [this](ores::nats::message m) {
                ores::reporting::messaging::report_type_handler(
                    nats, db.context(), std::optional(keys.verifier()))
                    .put_report_type(std::move(m));
            }));
        subs.push_back(
            nats.queue_subscribe(report_type_subjects + "put_many_report_types",
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::reporting::messaging::report_type_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .put_many_report_types(std::move(m));
                                 }));
        subs.push_back(
            nats.queue_subscribe(report_type_subjects + "delete_report_type",
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::reporting::messaging::report_type_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .delete_report_type(std::move(m));
                                 }));
        subs.push_back(
            nats.queue_subscribe(report_type_subjects + "delete_many_report_types",
                                 "ores.test",
                                 [this](ores::nats::message m) {
                                     ores::reporting::messaging::report_type_handler(
                                         nats, db.context(), std::optional(keys.verifier()))
                                         .delete_many_report_types(std::move(m));
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
            {"reporting::report_types:write", "reporting::report_types:delete"});
        std::unordered_map<std::string, std::string> headers;
        headers[std::string(ores::nats::headers::authorization)] =
            std::string(ores::nats::headers::bearer_prefix) + token;
        const std::array<std::byte, 1> undecodable{std::byte{0xc1}};
        return nats.request_sync(report_type_subjects + method, undecodable, headers);
    }
};

}

TEST_CASE("report_type_handler_denies_a_write_from_another_tenant", "[messaging][system_owned]") {
    report_type_system_owned_fixture f;
    {
        const auto reply =
            ores::testing::decode_reply<ores::reporting::messaging::put_report_type_response>(
                f.send("put_report_type", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
    {
        const auto reply =
            ores::testing::decode_reply<ores::reporting::messaging::put_many_report_types_response>(
                f.send("put_many_report_types", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
    {
        const auto reply =
            ores::testing::decode_reply<ores::reporting::messaging::delete_report_type_response>(
                f.send("delete_report_type", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
    {
        const auto reply = ores::testing::decode_reply<
            ores::reporting::messaging::delete_many_report_types_response>(
            f.send("delete_many_report_types", f.db.tenant_id()));
        REQUIRE(reply);
        CHECK(reply->result.outcome == ores::utility::domain::outcome::denied);
        CHECK(reply->result.code == "system_tenant_required");
    }
}

TEST_CASE("report_type_handler_lets_the_system_tenant_write", "[messaging][system_owned]") {
    report_type_system_owned_fixture f;
    CHECK(ores::testing::error_of(f.send(
              "put_report_type", ores::utility::uuid::tenant_id::system())) == "bad_request");
    CHECK(ores::testing::error_of(f.send(
              "put_many_report_types", ores::utility::uuid::tenant_id::system())) == "bad_request");
    CHECK(ores::testing::error_of(f.send(
              "delete_report_type", ores::utility::uuid::tenant_id::system())) == "bad_request");
    CHECK(ores::testing::error_of(
              f.send("delete_many_report_types", ores::utility::uuid::tenant_id::system())) ==
          "bad_request");
}
