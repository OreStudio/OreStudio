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
#include "ores.analytics.quant/service/process_factory.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>

using ores::analytics::quant::service::process_factory;

TEST_CASE("process_factory builds a geometric engine by default", "[process_factory]") {
    auto p = process_factory::make_process("geometric", {0.0}, {0.01}, {1.0}, 100.0, 7);
    REQUIRE(p != nullptr);
    CHECK(p->current() == 100.0);
    CHECK(p->next() > 0.0);
}

TEST_CASE("process_factory falls back to geometric for an unrecognised process_type",
    "[process_factory]") {
    auto p = process_factory::make_process("bogus", {0.0}, {0.01}, {1.0}, 100.0, 7);
    REQUIRE(p != nullptr);
    CHECK(p->current() == 100.0);
}

TEST_CASE("process_factory builds an arithmetic engine", "[process_factory]") {
    // sd == 0 makes next() deterministic: exactly current + mean.
    auto p = process_factory::make_process("arithmetic", {5.0}, {0.0}, {1.0}, 100.0, 7);
    REQUIRE(p != nullptr);
    CHECK(p->next() == 105.0);
}

TEST_CASE("process_factory builds an ou engine from repurposed mixing channels",
    "[process_factory]") {
    // weights.front() = kappa, stdevs.front() = sigma, initial_price doubles as theta.
    auto p = process_factory::make_process("ou", {}, {0.0}, {0.5}, 100.0, 7);
    REQUIRE(p != nullptr);
    // sigma == 0 makes next() deterministic: reverts toward its own starting theta.
    CHECK(p->next() == 100.0);
}

TEST_CASE("process_factory is deterministic for a fixed seed", "[process_factory]") {
    auto a = process_factory::make_process("geometric", {0.001}, {0.02}, {1.0}, 100.0, 42);
    auto b = process_factory::make_process("geometric", {0.001}, {0.02}, {1.0}, 100.0, 42);
    for (int i = 0; i < 10; ++i)
        CHECK(a->next() == b->next());
}

TEST_CASE("process_factory rejects invalid parameters before constructing a process",
    "[process_factory]") {
    CHECK_THROWS_AS(
        process_factory::make_process("geometric", {}, {}, {}, 100.0), std::invalid_argument);
    CHECK_THROWS_AS(
        process_factory::make_process("geometric", {0.0}, {0.01}, {1.0}, -5.0),
        std::invalid_argument);
    CHECK_THROWS_AS(
        process_factory::make_process("ou", {}, {-1.0}, {0.3}, 100.0), std::invalid_argument);
}

TEST_CASE("process_factory builds a vasicek yield curve process", "[process_factory]") {
    auto p = process_factory::make_yield_curve_process("vasicek", 0.3, {0.03}, 0.01, 0.03, 7);
    REQUIRE(p != nullptr);
    CHECK(p->current() == 0.03);
    CHECK(p->discount_factor(0) == 1.0);
    CHECK(p->discount_factor(1) > 0.0);
}

TEST_CASE("process_factory builds a hull_white yield curve process with a piecewise theta_path",
    "[process_factory]") {
    auto p = process_factory::make_yield_curve_process(
        "hull_white", 0.3, {0.03, 0.04, 0.05}, 0.01, 0.03, 7);
    REQUIRE(p != nullptr);
    CHECK(p->current() == 0.03);
}

TEST_CASE("process_factory builds a cir yield curve process", "[process_factory]") {
    auto p = process_factory::make_yield_curve_process("cir", 0.5, {0.04}, 0.1, 0.05, 7);
    REQUIRE(p != nullptr);
    CHECK(p->current() == 0.05);
    for (int i = 0; i < 100; ++i)
        CHECK(p->next() >= 0.0);
}

TEST_CASE("process_factory yield curve engines are deterministic for a fixed seed",
    "[process_factory]") {
    auto a = process_factory::make_yield_curve_process("cir", 0.5, {0.04}, 0.1, 0.05, 42);
    auto b = process_factory::make_yield_curve_process("cir", 0.5, {0.04}, 0.1, 0.05, 42);
    for (int i = 0; i < 10; ++i)
        CHECK(a->next() == b->next());
}

TEST_CASE("process_factory rejects an unrecognised yield curve process_type (no silent "
    "fallback)",
    "[process_factory]") {
    CHECK_THROWS_AS(
        process_factory::make_yield_curve_process("bogus", 0.3, {0.03}, 0.01, 0.03),
        std::invalid_argument);
}

TEST_CASE("process_factory rejects invalid yield curve parameters before constructing a "
    "process",
    "[process_factory]") {
    CHECK_THROWS_AS(
        process_factory::make_yield_curve_process("vasicek", 0.3, {}, 0.01, 0.03),
        std::invalid_argument);
    CHECK_THROWS_AS(
        process_factory::make_yield_curve_process("cir", 0.0, {0.03}, 0.01, 0.03),
        std::invalid_argument);
}
