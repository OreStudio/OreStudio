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
#include "ores.marketdata.api/domain/route.hpp"
#include "ores.marketdata.service/app/live_tenants.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>
#include <vector>

namespace {

using ores::marketdata::domain::route;
using ores::utility::uuid::tenant_id;

route route_of(const tenant_id& tenant) {
    route r;
    r.id = boost::uuids::random_generator{}();
    r.tenant_id = tenant;
    return r;
}

}

TEST_CASE("only the rows of live tenants are kept", "[app][live_tenants]") {
    boost::uuids::random_generator gen;
    const auto live = tenant_id::from_uuid(gen()).value();
    const auto terminated = tenant_id::from_uuid(gen()).value();
    const auto system = tenant_id::system();

    std::vector<route> rows{route_of(live), route_of(terminated), route_of(system)};
    ores::marketdata::service::app::keep_live_tenants(rows, {live.to_uuid(), system.to_uuid()});

    REQUIRE(rows.size() == 2);
    CHECK(rows[0].tenant_id == live);
    CHECK(rows[1].tenant_id == system);
}

TEST_CASE("no live tenant keeps no row", "[app][live_tenants]") {
    boost::uuids::random_generator gen;
    std::vector<route> rows{route_of(tenant_id::from_uuid(gen()).value())};

    ores::marketdata::service::app::keep_live_tenants(rows, {});

    CHECK(rows.empty());
}
