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
#include "ores.database/service/tenant_context.hpp"
#include "ores.marketdata.api/generators/adapter_generator.hpp"
#include "ores.marketdata.api/generators/supplier_generator.hpp"
#include "ores.marketdata.api/messaging/adapter_protocol.hpp"
#include "ores.marketdata.api/messaging/feed_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_protocol.hpp"
#include "ores.marketdata.api/messaging/session_protocol.hpp"
#include "ores.marketdata.api/messaging/supplier_protocol.hpp"
#include "ores.marketdata.core/repository/adapter_repository.hpp"
#include "ores.marketdata.core/repository/supplier_repository.hpp"
#include "ores.marketdata.core/service/adapter_service.hpp"
#include "ores.marketdata.core/service/feed_service.hpp"
#include "ores.marketdata.core/service/provider_service.hpp"
#include "ores.marketdata.core/service/session_service.hpp"
#include "ores.marketdata.core/service/supplier_service.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>

// The catalogue read surface: a list and a single read for each entity the
// Connectivity screen reads.

namespace {

using namespace ores::marketdata;
namespace messaging = ores::marketdata::messaging;
namespace db = ores::database::service;

struct catalogue_read_fixture {
    ores::testing::scoped_database_helper db_helper;
    ores::utility::generation::generation_context gen;
    ores::database::context ctx;

    catalogue_read_fixture()
        : gen(ores::testing::make_generation_context(db_helper))
        , ctx(db::tenant_context::with_system_tenant(db_helper.context())) {}
};

std::string unique_code(const std::string& prefix) {
    return prefix + "-" + boost::uuids::to_string(boost::uuids::random_generator{}()).substr(0, 8);
}

}

TEST_CASE("a_provider_is_listed_and_read", "[service][catalogue]") {
    catalogue_read_fixture f;
    service::provider_service service(f.ctx);
    messaging::list_providers_request list;
    list.limit = 1000;
    const auto listed = service.list_providers(list);
    REQUIRE(listed.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE_FALSE(listed.providers.empty());

    messaging::get_provider_request get;
    get.key.code = listed.providers.front().code;
    const auto read = service.get_provider(get);
    CHECK(read.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(read.provider.has_value());
    CHECK(read.provider->code == get.key.code);
}

TEST_CASE("a_supplier_is_listed_and_read", "[service][catalogue]") {
    catalogue_read_fixture f;
    auto supplier = generators::generate_synthetic_supplier(f.gen);
    supplier.code = unique_code("read-supplier");
    supplier.tenant_id = f.ctx.tenant_id();
    repository::supplier_repository{}.write(f.ctx, supplier);

    service::supplier_service service(f.ctx);
    messaging::list_suppliers_request list;
    list.limit = 1000;
    const auto listed = service.list_suppliers(list);
    REQUIRE(listed.result.outcome == ores::utility::domain::outcome::ok);
    const auto found = std::ranges::find(listed.suppliers, supplier.code, &domain::supplier::code);
    CHECK(found != listed.suppliers.end());

    messaging::get_supplier_request get;
    get.key.code = supplier.code;
    const auto read = service.get_supplier(get);
    CHECK(read.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(read.supplier.has_value());
    CHECK(read.supplier->code == supplier.code);
}

TEST_CASE("an_adapter_is_listed_and_read", "[service][catalogue]") {
    catalogue_read_fixture f;
    auto adapter = generators::generate_synthetic_adapter(f.gen);
    adapter.code = unique_code("read-adapter");
    adapter.tenant_id = f.ctx.tenant_id();
    repository::adapter_repository{}.write(f.ctx, adapter);

    service::adapter_service service(f.ctx);
    messaging::list_adapters_request list;
    list.limit = 1000;
    const auto listed = service.list_adapters(list);
    REQUIRE(listed.result.outcome == ores::utility::domain::outcome::ok);
    const auto found = std::ranges::find(listed.adapters, adapter.code, &domain::adapter::code);
    CHECK(found != listed.adapters.end());

    messaging::get_adapter_request get;
    get.key.code = adapter.code;
    const auto read = service.get_adapter(get);
    CHECK(read.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(read.adapter.has_value());
    CHECK(read.adapter->code == adapter.code);
}

TEST_CASE("a_session_is_listed_and_read", "[service][catalogue]") {
    catalogue_read_fixture f;
    service::session_service service(f.ctx);
    messaging::list_sessions_request list;
    list.limit = 1000;
    const auto listed = service.list_sessions(list);
    REQUIRE(listed.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE_FALSE(listed.sessions.empty());

    messaging::get_session_request get;
    get.key.code = listed.sessions.front().code;
    const auto read = service.get_session(get);
    CHECK(read.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(read.session.has_value());
    CHECK(read.session->code == get.key.code);
}

TEST_CASE("a_feed_is_listed_and_read", "[service][catalogue]") {
    catalogue_read_fixture f;
    service::feed_service service(f.ctx);
    messaging::list_feeds_request list;
    list.limit = 1000;
    const auto listed = service.list_feeds(list);
    REQUIRE(listed.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE_FALSE(listed.feeds.empty());

    messaging::get_feed_request get;
    get.key.name = listed.feeds.front().name;
    const auto read = service.get_feed(get);
    CHECK(read.result.outcome == ores::utility::domain::outcome::ok);
    REQUIRE(read.feed.has_value());
    CHECK(read.feed->name == get.key.name);
}
