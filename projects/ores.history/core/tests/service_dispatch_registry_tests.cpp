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
#include "ores.history.core/service/dispatch_registry.hpp"
#include "ores.testing/database_helper.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[service][dispatch_registry]");

}

using ores::database::context;
using ores::history::messaging::entity_history_version;
using ores::history::messaging::get_entity_history_request;
using ores::history::service::dispatch_registry;
using ores::testing::database_helper;

TEST_CASE("dispatch_registry_starts_empty", tags) {
    dispatch_registry registry;

    CHECK(registry.provider_count() == 0);
    CHECK_FALSE(registry.has_provider("ores.refdata.currency"));
}

TEST_CASE("register_history_provider_makes_has_provider_true", tags) {
    dispatch_registry registry;

    registry.register_history_provider(
        "ores.refdata.currency",
        [](const context&, const std::string&) { return std::vector<entity_history_version>{}; });

    CHECK(registry.has_provider("ores.refdata.currency"));
    CHECK(registry.provider_count() == 1);
}

TEST_CASE("register_history_provider_twice_for_same_entity_type_replaces_not_duplicates", tags) {
    database_helper h;
    dispatch_registry registry;
    int first_call_count = 0;
    int second_call_count = 0;

    registry.register_history_provider("ores.refdata.currency",
                                       [&](const context&, const std::string&) {
                                           ++first_call_count;
                                           return std::vector<entity_history_version>{};
                                       });
    registry.register_history_provider("ores.refdata.currency",
                                       [&](const context&, const std::string&) {
                                           ++second_call_count;
                                           return std::vector<entity_history_version>{};
                                       });

    CHECK(registry.provider_count() == 1);

    const auto response = registry.dispatch(
        {.entity_type = "ores.refdata.currency", .entity_id = "USD"}, h.context());

    CHECK(response.success);
    CHECK(first_call_count == 0);
    CHECK(second_call_count == 1);
}

TEST_CASE("dispatch_with_no_registered_provider_returns_a_failure_response", tags) {
    database_helper h;
    dispatch_registry registry;

    const auto response =
        registry.dispatch({.entity_type = "ores.refdata.party", .entity_id = "1"}, h.context());

    CHECK_FALSE(response.success);
    CHECK(response.versions.empty());
    CHECK(response.message.find("ores.refdata.party") != std::string::npos);
}

TEST_CASE("dispatch_calls_the_registered_provider_with_the_requested_entity_id", tags) {
    database_helper h;
    dispatch_registry registry;
    std::string received_id;

    registry.register_history_provider("ores.refdata.currency",
                                       [&](const context&, const std::string& id) {
                                           received_id = id;
                                           entity_history_version v;
                                           v.version = 1;
                                           v.modified_by = "alice";
                                           return std::vector<entity_history_version>{v};
                                       });

    const auto response = registry.dispatch(
        {.entity_type = "ores.refdata.currency", .entity_id = "USD"}, h.context());

    CHECK(response.success);
    CHECK(received_id == "USD");
    REQUIRE(response.versions.size() == 1);
    CHECK(response.versions[0].version == 1);
    CHECK(response.versions[0].modified_by == "alice");
}

TEST_CASE("dispatch_passes_context_through_to_the_provider_unexamined", tags) {
    database_helper h;
    dispatch_registry registry;
    std::optional<ores::utility::uuid::tenant_id> received_tenant_id;

    registry.register_history_provider("ores.refdata.currency",
                                       [&](const context& ctx, const std::string&) {
                                           received_tenant_id = ctx.tenant_id();
                                           return std::vector<entity_history_version>{};
                                       });

    const auto response = registry.dispatch(
        {.entity_type = "ores.refdata.currency", .entity_id = "USD"}, h.context());

    CHECK(response.success);
    REQUIRE(received_tenant_id.has_value());
    CHECK(*received_tenant_id == h.context().tenant_id());
}

TEST_CASE("dispatch_only_calls_the_provider_matching_the_requested_entity_type", tags) {
    database_helper h;
    dispatch_registry registry;
    bool currency_provider_called = false;
    bool party_provider_called = false;

    registry.register_history_provider("ores.refdata.currency",
                                       [&](const context&, const std::string&) {
                                           currency_provider_called = true;
                                           return std::vector<entity_history_version>{};
                                       });
    registry.register_history_provider("ores.refdata.party",
                                       [&](const context&, const std::string&) {
                                           party_provider_called = true;
                                           return std::vector<entity_history_version>{};
                                       });

    const auto response =
        registry.dispatch({.entity_type = "ores.refdata.party", .entity_id = "1"}, h.context());

    CHECK(response.success);
    CHECK_FALSE(currency_provider_called);
    CHECK(party_provider_called);
}

TEST_CASE("dispatch_returns_a_failure_response_when_the_provider_throws", tags) {
    database_helper h;
    dispatch_registry registry;

    registry.register_history_provider(
        "ores.refdata.currency",
        [](const context&, const std::string&) -> std::vector<entity_history_version> {
            throw std::runtime_error("database unavailable");
        });

    const auto response = registry.dispatch(
        {.entity_type = "ores.refdata.currency", .entity_id = "USD"}, h.context());

    CHECK_FALSE(response.success);
    CHECK(response.versions.empty());
    CHECK(response.message.find("database unavailable") != std::string::npos);
}
