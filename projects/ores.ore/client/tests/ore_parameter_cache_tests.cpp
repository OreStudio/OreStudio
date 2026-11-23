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
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.ore.client/service/cache/ore_parameter_cache.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>

using ores::ore::service::cache::ore_parameter_cache;

// The load round-trips through NATS against a running ore service, which the
// reporting service's run document tests and the fleet exercise; this proves
// the generated class compiles against ores.ore's types and is empty before a
// load, which is the state a consumer must refuse on.
TEST_CASE("an ore_parameter_cache with nothing loaded misses every lookup and holds no analytics",
          "[client][cache]") {
    ores::nats::service::client nats{ores::nats::config::nats_options{}};
    ore_parameter_cache cache(nats);

    CHECK_FALSE(cache.lookup("some-tenant", boost::uuids::random_generator()()).has_value());
    CHECK(cache.analytics_parameters("some-tenant").empty());
}
