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
#include "ores.iam/generators/tenant_generator.hpp"
#include "ores.iam/generators/tenant_status_generator.hpp"
#include "ores.iam/generators/tenant_type_generator.hpp"
#include "ores.iam/generators/role_generator.hpp"
#include "ores.iam/generators/permission_generator.hpp"
#include "ores.iam/generators/session_generator.hpp"
#include "ores.iam/generators/login_info_generator.hpp"
#include "ores.iam/generators/account_role_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.iam.tests");
const std::string tags("[generators]");

}

using namespace ores::iam::generators;
using namespace ores::logging;
using ores::utility::generation::generation_context;

// --- tenant ---

TEST_CASE("tenant_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_tenant(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated tenant: " << sut.name;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.hostname.empty());
    CHECK(sut.status == "active");
    CHECK(sut.type == "automation");
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.test");
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("tenant_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_tenants(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.code.empty());
    }
}

// --- tenant_status ---

TEST_CASE("tenant_status_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_tenant_status(ctx);

    CHECK(sut.version == 1);
    CHECK(!sut.status.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(sut.display_order >= 1);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.test");
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("tenant_status_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_tenant_statuses(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.status.empty());
    }
}

// --- tenant_type ---

TEST_CASE("tenant_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_tenant_type(ctx);

    CHECK(sut.version == 1);
    CHECK(!sut.type.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(sut.display_order >= 1);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.test");
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("tenant_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_tenant_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.type.empty());
    }
}

// --- role ---

TEST_CASE("role_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_role(ctx);

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.test");
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("role_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_roles(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.name.empty());
    }
}

// --- permission ---

TEST_CASE("permission_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_permission(ctx);

    CHECK(!sut.id.is_nil());
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
}

TEST_CASE("permission_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_permissions(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.code.empty());
    }
}

// --- session ---

TEST_CASE("session_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_session(ctx);

    CHECK(!sut.id.is_nil());
    CHECK(!sut.account_id.is_nil());
    CHECK(!sut.client_identifier.empty());
    CHECK(!sut.username.empty());
    CHECK(sut.client_version_major == 1);
    CHECK(sut.country_code == "GB");
    CHECK(sut.protocol == ores::iam::domain::session_protocol::binary);
}

TEST_CASE("session_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_sessions(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.account_id.is_nil());
    }
}

// --- login_info ---

TEST_CASE("login_info_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_login_info(ctx);

    CHECK(!sut.account_id.is_nil());
    CHECK(sut.failed_logins == 0);
    CHECK(sut.locked == false);
    CHECK(sut.online == false);
    CHECK(sut.password_reset_required == false);
}

TEST_CASE("login_info_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_login_infos(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.account_id.is_nil());
    }
}

// --- account_role ---

TEST_CASE("account_role_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_account_role(ctx);

    CHECK(!sut.account_id.is_nil());
    CHECK(!sut.role_id.is_nil());
    CHECK(!sut.assigned_by.empty());
    CHECK(sut.change_reason_code == "system.test");
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("account_role_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_account_roles(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.account_id.is_nil());
        CHECK(!item.role_id.is_nil());
    }
}
