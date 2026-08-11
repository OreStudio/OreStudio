/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your option)
 * any later version.
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
#include "../src/feed_controller.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[feed_controller]");

using ores::synthetic::domain::binding_mode;
using ores::synthetic::feed::synthetic_producer_subject;
using ores::synthetic::service::should_ensure_feed_binding;

}

TEST_CASE("synthetic_producer_subject: bound publishes on the standard tick subject", tags) {
    CHECK(synthetic_producer_subject("EUR_USD_GBM", binding_mode::bound) ==
          "synthetic.v1.tick.EUR_USD_GBM");
}

TEST_CASE("synthetic_producer_subject: sandboxed publishes on a distinct subject the "
          "marketdata ingest loop never subscribes to",
          tags) {
    const auto bound_subject = synthetic_producer_subject("EUR_USD_GBM", binding_mode::bound);
    const auto sandboxed_subject =
        synthetic_producer_subject("EUR_USD_GBM", binding_mode::sandboxed);

    CHECK(sandboxed_subject == "synthetic.v1.sandbox.tick.EUR_USD_GBM");
    CHECK(sandboxed_subject != bound_subject);
    // The ingest loop always subscribes to "synthetic.v1.tick." + source_name
    // (see feed_ingest_loop.cpp) -- the sandboxed subject must not collide
    // with that prefix under any source_name, or the exclusion isn't real.
    CHECK(sandboxed_subject.starts_with("synthetic.v1.sandbox.tick."));
    CHECK_FALSE(bound_subject.starts_with("synthetic.v1.sandbox.tick."));
}

TEST_CASE("synthetic_producer_subject: same source_name never collides across binding modes, "
          "for a variety of source names",
          tags) {
    for (const std::string& source : {"eur.usd", "eur-usd", "EUR_USD_2", "weird name!*>"}) {
        const auto bound = synthetic_producer_subject(source, binding_mode::bound);
        const auto sandboxed = synthetic_producer_subject(source, binding_mode::sandboxed);
        CHECK(bound != sandboxed);
    }
}

TEST_CASE("synthetic_producer_subject: unsafe characters are still replaced under sandboxed "
          "binding mode, matching bound's sanitisation",
          tags) {
    CHECK(synthetic_producer_subject("weird name!*>", binding_mode::sandboxed) ==
          "synthetic.v1.sandbox.tick.weird_name___");
}

TEST_CASE("should_ensure_feed_binding: a brand-new feed is gated on the requested binding mode",
          tags) {
    CHECK(should_ensure_feed_binding(false, binding_mode::bound, binding_mode::sandboxed));
    CHECK_FALSE(should_ensure_feed_binding(false, binding_mode::sandboxed, binding_mode::bound));
}

TEST_CASE("should_ensure_feed_binding: an already-running feed is gated on its stored binding "
          "mode, not the requested one -- an unaware caller restarting an already-sandboxed "
          "feed must not create a bound feed_binding for it",
          tags) {
    CHECK_FALSE(should_ensure_feed_binding(true, binding_mode::bound, binding_mode::sandboxed));
    CHECK(should_ensure_feed_binding(true, binding_mode::sandboxed, binding_mode::bound));
}

TEST_CASE("should_ensure_feed_binding: an already-running bound feed still gets its binding "
          "ensured on every restart, regardless of what this call requested",
          tags) {
    CHECK(should_ensure_feed_binding(true, binding_mode::bound, binding_mode::bound));
    CHECK(should_ensure_feed_binding(true, binding_mode::sandboxed, binding_mode::bound));
}
