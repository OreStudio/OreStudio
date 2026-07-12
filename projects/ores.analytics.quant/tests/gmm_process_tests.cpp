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
#include "ores.analytics.quant/service/processes/gmm_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>

using ores::analytics::quant::service::gmm_process;

TEST_CASE("gmm_process reports the initial price before any next() call", "[gmm_process]") {
    gmm_process p({0.0}, {0.01}, {1.0}, 100.0);
    CHECK(p.current() == 100.0);
}

TEST_CASE("gmm_process next() updates and returns the same value as current()", "[gmm_process]") {
    gmm_process p({0.0}, {0.01}, {1.0}, 100.0);
    const double n = p.next();
    CHECK(n == p.current());
}

TEST_CASE("gmm_process is deterministic for a fixed seed", "[gmm_process]") {
    gmm_process a({0.001, -0.002}, {0.01, 0.02}, {0.5, 0.5}, 100.0, 7);
    gmm_process b({0.001, -0.002}, {0.01, 0.02}, {0.5, 0.5}, 100.0, 7);

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("gmm_process with different seeds diverges", "[gmm_process]") {
    gmm_process a({0.001}, {0.05}, {1.0}, 100.0, 1);
    gmm_process b({0.001}, {0.05}, {1.0}, 100.0, 2);

    bool any_different = false;
    for (int i = 0; i < 20; ++i) {
        if (a.next() != b.next())
            any_different = true;
    }
    CHECK(any_different);
}

TEST_CASE("gmm_process price stays strictly positive (geometric engine)", "[gmm_process]") {
    gmm_process p({-1.0, 1.0}, {0.5, 0.5}, {0.5, 0.5}, 100.0, 99);
    for (int i = 0; i < 500; ++i)
        REQUIRE(p.next() > 0.0);
}

TEST_CASE("gmm_process with a zero-variance component draws exactly the mean", "[gmm_process]") {
    // sd == 0 hits the degenerate branch (avoids libstdc++ aborting on sigma <= 0).
    gmm_process p({0.1}, {0.0}, {1.0}, 100.0);
    const double expected = 100.0 * std::exp(0.1);
    CHECK(p.next() == Catch::Approx(expected));
}

TEST_CASE("gmm_process rejects mismatched parameter vector sizes", "[gmm_process]") {
    CHECK_THROWS_AS(gmm_process({0.0, 0.0}, {0.01}, {1.0}, 100.0), std::invalid_argument);
    CHECK_THROWS_AS(gmm_process({0.0}, {0.01, 0.01}, {1.0}, 100.0), std::invalid_argument);
    CHECK_THROWS_AS(gmm_process({0.0}, {0.01}, {1.0, 0.0}, 100.0), std::invalid_argument);
}

TEST_CASE("gmm_process rejects an empty component set", "[gmm_process]") {
    CHECK_THROWS_AS(gmm_process({}, {}, {}, 100.0), std::invalid_argument);
}

TEST_CASE("gmm_process rejects a non-positive initial price", "[gmm_process]") {
    CHECK_THROWS_AS(gmm_process({0.0}, {0.01}, {1.0}, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(gmm_process({0.0}, {0.01}, {1.0}, -1.0), std::invalid_argument);
}
