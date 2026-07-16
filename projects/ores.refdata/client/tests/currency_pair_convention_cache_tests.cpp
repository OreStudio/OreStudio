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
#include "ores.refdata.client/service/cache/currency_pair_convention_cache.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::refdata::service::cache::currency_pair_convention_cache;

namespace {

const std::string tags("[client][cache]");

} // namespace

// currency_pair_convention_cache::load() round-trips through a real NATS
// request/reply -- like party_cache (the exemplar this facet generalises
// from, see ores.iam.core), that is exercised at the integration level
// against a running ores.refdata.service, not with a fake/mock NATS client
// here: there is no in-process NATS test double in this codebase, and the
// underlying partitioned_cache primitive's own concurrency/replace/lookup
// behaviour is already thoroughly covered by
// ores.eventing.core's partitioned_cache_tests.cpp. What's left to prove
// here is that the generated class itself is real, compiling C++ wired to
// the right types -- not a stale/broken template render -- and behaves
// correctly before any load() has ever run.
TEST_CASE("a currency_pair_convention_cache with nothing loaded misses every lookup", tags) {
    // Construction alone never connects (connect() is a separate, explicit
    // call the cache's load() never makes) -- safe to build a client here
    // without a live NATS server as long as this test never calls load().
    ores::nats::service::client nats{ores::nats::config::nats_options{}};
    currency_pair_convention_cache cache(nats);

    CHECK_FALSE(cache.lookup("some-tenant", "EUR/USD").has_value());
}
