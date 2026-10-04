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
#include <string>
#include <vector>

/*
 * The tenant model declares code, name and hostname searchable, and its key
 * has a one-of member. Each case writes tenants marked with text no other
 * test writes, so a search for the marker answers only them.
 */

namespace {

const std::string tags("[repository][list_filter]");

using ores::database::context;
using ores::iam::messaging::tenants_filter;
using ores::iam::repository::tenant_repository;

std::string unique_marker() {
    auto text = boost::uuids::to_string(boost::uuids::random_generator()());
    text.erase(std::remove(text.begin(), text.end(), '-'), text.end());
    return "flt" + text.substr(0, 10);
}

context system_context(ores::testing::database_helper& h) {
    return h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
}

ores::iam::domain::tenant
write_tenant(ores::testing::database_helper& h, const std::string& code, const std::string& name) {
    auto gen_ctx = ores::testing::make_generation_context(h);
    auto t = ores::iam::generators::generate_synthetic_tenant(gen_ctx);
    t.code = code;
    t.name = name;
    t.hostname = code + ".example.com";
    tenant_repository{}.write(system_context(h), t);
    return t;
}

std::vector<std::string> codes(const std::vector<ores::iam::domain::tenant>& tenants) {
    std::vector<std::string> r;
    for (const auto& t : tenants)
        r.push_back(t.code);
    std::ranges::sort(r);
    return r;
}

std::vector<ores::iam::domain::tenant> read(ores::testing::database_helper& h,
                                            const tenants_filter& f) {
    return tenant_repository{}.read_latest(system_context(h), 0, 1000, {}, f);
}

}

using ores::testing::database_helper;

TEST_CASE("a search matches the code, the name and the hostname, ignoring case", tags) {
    database_helper h;
    const auto m = unique_marker();
    write_tenant(h, m + "_a", "Filter Fixture " + m);
    write_tenant(h, "other_" + m.substr(3), "Named " + m + " Ltd");

    CHECK(codes(read(h, {.search = m})) ==
          std::vector<std::string>{m + "_a", "other_" + m.substr(3)});
    CHECK(codes(read(h, {.search = "NAMED " + m})) ==
          std::vector<std::string>{"other_" + m.substr(3)});
    CHECK(codes(read(h, {.search = m + "_a.EXAMPLE"})) == std::vector<std::string>{m + "_a"});
}

TEST_CASE("a search folds letters outside ASCII as the database does", tags) {
    database_helper h;
    const auto m = unique_marker();
    write_tenant(h, m + "_a", "Société " + m);

    CHECK(codes(read(h, {.search = "SOCIÉTÉ " + m})) == std::vector<std::string>{m + "_a"});
}

TEST_CASE("a search matches its text literally", tags) {
    database_helper h;
    const auto m = unique_marker();
    write_tenant(h, m + "_a", "Fifty% " + m);
    write_tenant(h, m + "_b", "Fifty " + m);
    write_tenant(h, m + "_c", "O'Brien " + m);

    CHECK(codes(read(h, {.search = "fifty% " + m})) == std::vector<std::string>{m + "_a"});
    CHECK(codes(read(h, {.search = m + "__"})).empty());
    CHECK(codes(read(h, {.search = "o'brien " + m})) == std::vector<std::string>{m + "_c"});
}

TEST_CASE("an empty search sets no condition", tags) {
    database_helper h;
    const auto ctx = system_context(h);
    CHECK(tenant_repository{}.get_total_tenant_count(ctx, tenants_filter{.search = ""}) ==
          tenant_repository{}.get_total_tenant_count(ctx));
}

TEST_CASE("a one-of member reads the rows for the keys it lists", tags) {
    database_helper h;
    const auto m = unique_marker();
    const auto a = write_tenant(h, m + "_a", "One Of " + m);
    write_tenant(h, m + "_b", "One Of " + m);
    const auto c = write_tenant(h, m + "_c", "One Of " + m);

    CHECK(codes(read(h, {.id_one_of = std::vector{a.id, c.id}})) ==
          std::vector<std::string>{m + "_a", m + "_c"});
    CHECK(read(h, {.id_one_of = std::vector<boost::uuids::uuid>{}}).empty());
}

TEST_CASE("the members a filter sets must all hold", tags) {
    database_helper h;
    const auto m = unique_marker();
    const auto a = write_tenant(h, m + "_a", "Both " + m);
    const auto b = write_tenant(h, m + "_b", "Neither " + m);

    CHECK(codes(read(h, {.id_one_of = std::vector{a.id, b.id}, .search = "both " + m})) ==
          std::vector<std::string>{m + "_a"});
}

TEST_CASE("the total counts the rows the filter matches", tags) {
    database_helper h;
    const auto m = unique_marker();
    write_tenant(h, m + "_a", "Counted " + m);
    write_tenant(h, m + "_b", "Counted " + m);
    write_tenant(h, m + "_c", "Counted " + m);

    const tenants_filter f{.search = "counted " + m};
    CHECK(tenant_repository{}.get_total_tenant_count(system_context(h), f) == 3);
    CHECK(tenant_repository{}.read_latest(system_context(h), 0, 2, {}, f).size() == 2);
}

TEST_CASE("the service answers a filtered page and its total", tags) {
    database_helper h;
    const auto m = unique_marker();
    write_tenant(h, m + "_a", "Served " + m);
    write_tenant(h, m + "_b", "Served " + m);
    ores::iam::service::tenant_service service(system_context(h));

    ores::iam::messaging::list_tenants_request request;
    request.filter = tenants_filter{.search = "served " + m};
    request.limit = 1;
    const auto r = service.list_tenants(request);
    CHECK(r.result.outcome == ores::utility::domain::outcome::ok);
    CHECK(r.tenants.size() == 1);
    CHECK(r.total == 2);
}

TEST_CASE("the service refuses a one-of list longer than 1000 values", tags) {
    database_helper h;
    ores::iam::service::tenant_service service(system_context(h));

    ores::iam::messaging::list_tenants_request request;
    request.filter = tenants_filter{
        .id_one_of = std::vector<boost::uuids::uuid>(1001, boost::uuids::random_generator()())};
    const auto r = service.list_tenants(request);
    CHECK(r.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(r.result.code == "filter_too_large");
    CHECK(r.tenants.empty());
}

TEST_CASE("the service refuses a search longer than 256 characters", tags) {
    database_helper h;
    ores::iam::service::tenant_service service(system_context(h));

    ores::iam::messaging::list_tenants_request request;
    request.filter = tenants_filter{.search = std::string(257, 'a')};
    const auto r = service.list_tenants(request);
    CHECK(r.result.outcome == ores::utility::domain::outcome::invalid);
    CHECK(r.result.code == "filter_too_large");

    request.filter = tenants_filter{.search = std::string(256, 'a')};
    CHECK(service.list_tenants(request).result.outcome == ores::utility::domain::outcome::ok);
}
