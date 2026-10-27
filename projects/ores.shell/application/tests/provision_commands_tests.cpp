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
#include "ores.logging/make_logger.hpp"
#include "ores.shell/app/commands/provision_commands.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <sstream>
#include <string>
#include <string_view>

using ores::shell::app::commands::provision_commands;
using namespace ores::logging;

namespace {

/// The fields a provisioning command prints its ending from.
provision_commands::tenant_fields demo_fields() {
    return provision_commands::tenant_fields{.code = "demo",
                                             .name = "Demo Tenant",
                                             .hostname = "demo",
                                             .description = "A tenant for the tests.",
                                             .admin_username = "demo_admin",
                                             .admin_email = "admin@demo.com",
                                             .admin_password = "Secure-Password-123"};
}

}

TEST_CASE("provision_commands_waits_on_the_run_a_successful_reply_names", "[commands][provision]") {
    auto lg(make_logger(test_suite));

    const auto run = provision_commands::started_run(true, "bac74724-221e-4594-ab54-bbe3b9acc5c9");

    BOOST_LOG_SEV(lg, info) << "Run to wait on: " << run.value_or("none");
    REQUIRE(run.has_value());
    CHECK(*run == "bac74724-221e-4594-ab54-bbe3b9acc5c9");
}

TEST_CASE("provision_commands_waits_on_no_run_when_the_reply_names_none", "[commands][provision]") {
    auto lg(make_logger(test_suite));

    // A failed request, and a success that names no instance, both leave
    // nothing to wait on. Polling either would block on a run that does not
    // exist and report a timeout the caller cannot act on.
    const auto failed =
        provision_commands::started_run(false, "bac74724-221e-4594-ab54-bbe3b9acc5c9");
    const auto unnamed = provision_commands::started_run(true, "");

    BOOST_LOG_SEV(lg, info) << "Run from a failed reply: " << failed.value_or("none")
                            << ", from an unnamed reply: " << unnamed.value_or("none");
    CHECK(!failed.has_value());
    CHECK(!unnamed.has_value());
}

TEST_CASE("provision_commands_calls_a_followed_tenant_ready", "[commands][provision]") {
    auto lg(make_logger(test_suite));

    std::ostringstream out;
    provision_commands::report_tenant_outcome(out,
                                              demo_fields(),
                                              provision_commands::tenant_creation::additional,
                                              provision_commands::tenant_setup::finished);

    BOOST_LOG_SEV(lg, info) << "Output for a followed tenant: " << out.str();
    CHECK(out.str().find("✓ Tenant 'Demo Tenant' provisioned.") != std::string::npos);
    CHECK(out.str().find("Next: logout, then: login demo_admin@demo <password>") !=
          std::string::npos);
    CHECK(out.str().find("the tenant is ready to work in") != std::string::npos);
    CHECK(out.str().find("setup continues") == std::string::npos);
}

TEST_CASE("provision_commands_does_not_call_a_tenant_ready_while_its_setup_continues", "[commands][provision]") {
    auto lg(make_logger(test_suite));

    std::ostringstream out;
    provision_commands::report_tenant_outcome(
        out,
        demo_fields(),
        provision_commands::tenant_creation::additional,
        provision_commands::tenant_setup::continues_elsewhere);

    BOOST_LOG_SEV(lg, info) << "Output for a tenant whose setup continues: " << out.str();
    CHECK(out.str().find("✓ Tenant 'Demo Tenant' created; its setup continues.") !=
          std::string::npos);
    CHECK(out.str().find("workflow wait <its instance id>") != std::string::npos);
    CHECK(out.str().find("the tenant is ready to work in") == std::string::npos);
}

TEST_CASE("provision_commands_names_the_system_it_provisioned", "[commands][provision]") {
    auto lg(make_logger(test_suite));

    std::ostringstream followed;
    provision_commands::report_tenant_outcome(followed,
                                              demo_fields(),
                                              provision_commands::tenant_creation::system,
                                              provision_commands::tenant_setup::finished);

    std::ostringstream continuing;
    provision_commands::report_tenant_outcome(
        continuing,
        demo_fields(),
        provision_commands::tenant_creation::system,
        provision_commands::tenant_setup::continues_elsewhere);

    BOOST_LOG_SEV(lg, info) << "Output for a followed system: " << followed.str()
                            << "Output for a system whose setup continues: " << continuing.str();
    CHECK(followed.str().find("✓ System provisioned. Tenant 'Demo Tenant'.") != std::string::npos);
    CHECK(continuing.str().find("✓ System provisioned. Tenant 'Demo Tenant' created; its setup "
                                "continues.") != std::string::npos);
}
