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
#include "ores.iam.api/domain/tenant.hpp"
#include "ores.iam.api/generators/tenant_generator.hpp"
#include "ores.iam.api/messaging/tenant_protocol.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.iam.core/service/tenant_service.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

/*
 * The tenant model declares code and name sortable and orders by name. The
 * fixtures are found among every tenant the store holds, so each case writes
 * names and codes that sort the same way under any collation.
 */

namespace {

const std::string tags("[repository][stated_order]");

using ores::database::context;
using ores::iam::repository::tenant_repository;
using ores::utility::domain::order;

std::string unique_marker() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    text.erase(std::remove(text.begin(), text.end(), '-'), text.end());
    return "ord" + text.substr(0, 10);
}

context system_context(ores::testing::database_helper& h) {
    return h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
}

std::string
write_tenant(ores::testing::database_helper& h, const std::string& code, const std::string& name) {
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto t = ores::iam::generators::generate_synthetic_tenant(gen_ctx);
    t.code = code;
    t.name = name;
    t.hostname = code + ".example.com";
    tenant_repository{}.write(system_context(h), t);
    return t.code;
}

/*
 * Every tenant in the stated order, page by page, keeping only the codes that
 * carry the marker.
 */
std::vector<std::string>
codes_in_order(ores::testing::database_helper& h, const std::string& marker, const order& o) {
    const auto ctx = system_context(h);
    tenant_repository repo;
    const auto total = repo.get_total_tenant_count(ctx);
    std::vector<std::string> r;
    for (std::uint32_t offset = 0; offset < total; offset += 1000)
        for (const auto& t : repo.read_latest(ctx, offset, 1000, o))
            if (t.code.starts_with(marker))
                r.push_back(t.code);
    return r;
}

/*
 * Two tenants share a name, so the key breaks the tie: their codes come back
 * in the order of their ids whatever the stated direction.
 */
std::vector<std::string> by_id(ores::testing::database_helper& h, std::vector<std::string> codes) {
    const auto ctx = system_context(h);
    tenant_repository repo;
    std::ranges::sort(codes, [&](const auto& a, const auto& b) {
        return repo.read_latest_by_code(ctx, a).front().id <
               repo.read_latest_by_code(ctx, b).front().id;
    });
    return codes;
}

}

using ores::testing::database_helper;

TEST_CASE("tenants read in name order when no order is stated", tags) {
    database_helper h;
    const auto m = unique_marker();
    const auto b = write_tenant(h, m + "_1", "Order Fixture " + m + " b");
    const auto a1 = write_tenant(h, m + "_2", "Order Fixture " + m + " a");
    const auto a2 = write_tenant(h, m + "_3", "Order Fixture " + m + " a");
    const auto tied = by_id(h, {a1, a2});

    CHECK(codes_in_order(h, m, {}) == std::vector<std::string>{tied[0], tied[1], b});
    CHECK(codes_in_order(h, m, {.field = "name"}) == std::vector<std::string>{tied[0], tied[1], b});
}

TEST_CASE("a descending order reverses the field and keeps ties in key order", tags) {
    database_helper h;
    const auto m = unique_marker();
    const auto b = write_tenant(h, m + "_1", "Order Fixture " + m + " b");
    const auto a1 = write_tenant(h, m + "_2", "Order Fixture " + m + " a");
    const auto a2 = write_tenant(h, m + "_3", "Order Fixture " + m + " a");
    const auto tied = by_id(h, {a1, a2});

    CHECK(codes_in_order(h, m, {.field = "name", .descending = true}) ==
          std::vector<std::string>{b, tied[0], tied[1]});
    CHECK(codes_in_order(h, m, {.descending = true}) ==
          std::vector<std::string>{b, tied[0], tied[1]});
}

TEST_CASE("tenants read in code order when code is stated", tags) {
    database_helper h;
    const auto m = unique_marker();
    const auto c2 = write_tenant(h, m + "_2", "Order Fixture " + m + " a");
    const auto c1 = write_tenant(h, m + "_1", "Order Fixture " + m + " b");

    CHECK(codes_in_order(h, m, {.field = "code"}) == std::vector<std::string>{c1, c2});
    CHECK(codes_in_order(h, m, {.field = "code", .descending = true}) ==
          std::vector<std::string>{c2, c1});
}

TEST_CASE("the store refuses an order by a field the model does not declare", tags) {
    database_helper h;
    CHECK(tenant_repository::is_sortable("name"));
    CHECK(tenant_repository::is_sortable("code"));
    CHECK_FALSE(tenant_repository::is_sortable("hostname"));
    CHECK_THROWS_AS(
        tenant_repository{}.read_latest(system_context(h), 0, 10, {.field = "hostname"}),
        std::invalid_argument);
}

TEST_CASE("the service refuses an undeclared field and honours a declared one", tags) {
    database_helper h;
    ores::iam::service::tenant_service service(system_context(h));

    ores::iam::messaging::list_tenants_request refused;
    refused.order.field = "hostname";
    const auto no = service.list_tenants(refused);
    CHECK(no.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(no.result.code == "order_not_supported");
    CHECK(no.result.message == "A list of tenants cannot be ordered by hostname.");
    CHECK(no.tenants.empty());

    ores::iam::messaging::list_tenants_request honoured;
    honoured.order = {.field = "code", .descending = true};
    honoured.limit = 2;
    const auto yes = service.list_tenants(honoured);
    CHECK(yes.result.outcome == ores::utility::domain::outcome::ok);
    const auto stored = tenant_repository{}.read_latest(system_context(h), 0, 2, honoured.order);
    REQUIRE(yes.tenants.size() == 2);
    CHECK(yes.tenants[0].code == stored[0].code);
    CHECK(yes.tenants[1].code == stored[1].code);
    const auto ascending =
        tenant_repository{}.read_latest(system_context(h), 0, 1, {.field = "code"});
    CHECK(yes.tenants[0].code != ascending.front().code);
}
