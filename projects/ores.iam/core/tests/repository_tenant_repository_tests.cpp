/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.iam.api/domain/tenant_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.api/generators/tenant_generator.hpp"
#include "ores.iam.core/repository/tenant_lookups.hpp"
#include "ores.iam.core/repository/tenant_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp"       // IWYU pragma: keep.
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[repository]");

}

using namespace ores::logging;
using namespace ores::iam::generators;

using ores::testing::database_helper;
using ores::iam::repository::tenant_repository;
using ores::iam::repository::read_active_tenant_by_hostname;
using ores::iam::repository::read_active_tenant_by_id;

TEST_CASE("write_single_tenant", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    auto gen_ctx = ores::testing::make_generation_context(h);

    tenant_repository repo;
    auto t = generate_synthetic_tenant(gen_ctx);

    BOOST_LOG_SEV(lg, debug) << "Tenant: " << t;
    CHECK_NOTHROW(repo.write(sys_ctx, t));
}

TEST_CASE("write_multiple_tenants", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    auto gen_ctx = ores::testing::make_generation_context(h);

    tenant_repository repo;
    auto tenants = generate_synthetic_tenants(3, gen_ctx);

    BOOST_LOG_SEV(lg, debug) << "Tenants: " << tenants;
    CHECK_NOTHROW(repo.write(sys_ctx, tenants));
}

TEST_CASE("read_latest_tenants", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    auto gen_ctx = ores::testing::make_generation_context(h);

    tenant_repository repo;
    auto written = generate_synthetic_tenants(3, gen_ctx);

    BOOST_LOG_SEV(lg, debug) << "Written tenants: " << written;
    repo.write(sys_ctx, written);

    auto read_tenants = repo.read_latest(sys_ctx);
    BOOST_LOG_SEV(lg, debug) << "Read tenants: " << read_tenants;

    CHECK(!read_tenants.empty());
    CHECK(read_tenants.size() >= written.size());
}

TEST_CASE("read_latest_tenant_by_id", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    auto gen_ctx = ores::testing::make_generation_context(h);

    tenant_repository repo;
    auto t = generate_synthetic_tenant(gen_ctx);
    const auto target_id = t.id;

    BOOST_LOG_SEV(lg, debug) << "Tenant: " << t;
    repo.write(sys_ctx, t);

    BOOST_LOG_SEV(lg, debug) << "Target ID: " << target_id;

    auto read_tenants = repo.read_latest(sys_ctx, boost::uuids::to_string(target_id));
    BOOST_LOG_SEV(lg, debug) << "Read tenants: " << read_tenants;

    REQUIRE(read_tenants.size() == 1);
    CHECK(read_tenants[0].id == target_id);
    CHECK(read_tenants[0].code == t.code);
    CHECK(read_tenants[0].name == t.name);
}

TEST_CASE("read_active_tenant_lookups_resolve_system_owned_records", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");
    auto gen_ctx = ores::testing::make_generation_context(h);

    tenant_repository repo;
    auto t = generate_synthetic_tenant(gen_ctx);
    const auto target_id = t.id;
    const auto target_hostname = t.hostname;

    BOOST_LOG_SEV(lg, debug) << "Tenant: " << t;
    repo.write(sys_ctx, t);

    // Tenant rows are system-owned (tenant_id = system, stamped by the
    // insert trigger), so the generated repository -- whose reads filter on
    // the tenant carried by the context -- can never read one, not even
    // under the row's own tenant context. The tenants_read_policy admits a
    // tenant row to the system tenant (tenant_id = current) and to the
    // row's own context (id = current); a peer tenant's context stays
    // isolated. Login resolves tenants by hostname before any tenant
    // context exists and the registrar warm-up reads on behalf of all
    // tenants; both run under the system context, and the hand-authored
    // lookups below (no tenant_id filter) are the reads that resolve the
    // row there and under the row's own tenant context.
    auto own_ctx =
        h.context().with_tenant(ores::utility::uuid::tenant_id::from_uuid(target_id).value(), "");

    auto scoped = repo.read_latest(own_ctx, boost::uuids::to_string(target_id));
    BOOST_LOG_SEV(lg, debug) << "Repository read under own tenant: " << scoped;
    CHECK(scoped.empty());

    auto by_id = read_active_tenant_by_id(own_ctx, target_id);
    BOOST_LOG_SEV(lg, debug) << "Read tenants by id: " << by_id;
    REQUIRE(by_id.size() == 1);
    CHECK(by_id[0].id == target_id);

    auto by_hostname = read_active_tenant_by_hostname(own_ctx, target_hostname);
    BOOST_LOG_SEV(lg, debug) << "Read tenants by hostname: " << by_hostname;
    REQUIRE(by_hostname.size() == 1);
    CHECK(by_hostname[0].id == target_id);

    auto bootstrap_by_id = read_active_tenant_by_id(sys_ctx, target_id);
    BOOST_LOG_SEV(lg, debug) << "Bootstrap read tenants by id: " << bootstrap_by_id;
    REQUIRE(bootstrap_by_id.size() == 1);
    CHECK(bootstrap_by_id[0].id == target_id);

    auto bootstrap_by_hostname = read_active_tenant_by_hostname(sys_ctx, target_hostname);
    BOOST_LOG_SEV(lg, debug) << "Bootstrap read tenants by hostname: " << bootstrap_by_hostname;
    REQUIRE(bootstrap_by_hostname.size() == 1);
    CHECK(bootstrap_by_hostname[0].id == target_id);

    // A peer tenant's context sees nothing: tenant rows are system-owned
    // and the policy admits them only to the system tenant and to the
    // row's own tenant context.
    const auto other_id = boost::uuids::random_generator()();
    const auto foreign_ctx =
        h.context().with_tenant(ores::utility::uuid::tenant_id::from_uuid(other_id).value(), "");

    auto foreign_scoped = repo.read_latest(foreign_ctx, boost::uuids::to_string(target_id));
    BOOST_LOG_SEV(lg, debug) << "Peer repository read tenants: " << foreign_scoped;
    CHECK(foreign_scoped.empty());

    auto foreign_by_id = read_active_tenant_by_id(foreign_ctx, target_id);
    BOOST_LOG_SEV(lg, debug) << "Peer read tenants by id: " << foreign_by_id;
    CHECK(foreign_by_id.empty());

    auto foreign_by_hostname = read_active_tenant_by_hostname(foreign_ctx, target_hostname);
    BOOST_LOG_SEV(lg, debug) << "Peer read tenants by hostname: " << foreign_by_hostname;
    CHECK(foreign_by_hostname.empty());
}

TEST_CASE("read_nonexistent_tenant", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    auto sys_ctx = h.context().with_tenant(ores::utility::uuid::tenant_id::system(), "");

    tenant_repository repo;

    const auto nonexistent_id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, debug) << "Non-existent ID: " << nonexistent_id;

    auto read_tenants = repo.read_latest(sys_ctx, boost::uuids::to_string(nonexistent_id));
    BOOST_LOG_SEV(lg, debug) << "Read tenants: " << read_tenants;

    CHECK(read_tenants.size() == 0);
}
