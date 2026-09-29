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

#include "ores.iam.core/service/tenant_hostname_check.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[tenant]");

using ores::iam::service::check_tenant_hostname;

}

TEST_CASE("check_tenant_hostname_accepts_a_name_the_routing_can_resolve", tags) {
    CHECK(check_tenant_hostname("northwind").empty());
    CHECK(check_tenant_hostname("northwind.example.com").empty());
    CHECK(check_tenant_hostname("acme_corporation").empty());
}

TEST_CASE("check_tenant_hostname_refuses_a_hostname_that_carries_a_port", tags) {
    // A principal is username@hostname and the login looks the hostname up
    // exactly, so a port makes the tenant's own administrator unresolvable.
    const auto refusal = check_tenant_hostname("northwind.example.com:8080");

    CHECK_FALSE(refusal.empty());
    CHECK(refusal.find("port") != std::string::npos);
}

TEST_CASE("check_tenant_hostname_reports_a_missing_hostname", tags) {
    CHECK_FALSE(check_tenant_hostname("").empty());
}
