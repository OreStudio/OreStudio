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
#include "ores.qt/BadgeCache.hpp"

#include <vector>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.qt.tests");
const std::string tags("[badge_cache]");

ores::dq::domain::badge_definition make_def(
    const std::string& code,
    const std::string& background,
    const std::string& text) {
    ores::dq::domain::badge_definition d;
    d.code = code;
    d.name = code;
    d.background_colour = background;
    d.text_colour = text;
    return d;
}

ores::dq::messaging::badge_mapping make_mapping(
    const std::string& domain,
    const std::string& entity,
    const std::string& badge) {
    ores::dq::messaging::badge_mapping m;
    m.code_domain_code = domain;
    m.entity_code = entity;
    m.badge_code = badge;
    return m;
}

}

using ores::qt::BadgeCache;
using namespace ores::logging;

TEST_CASE("resolve_returns_nullptr_when_not_loaded", tags) {
    auto lg(make_logger(test_suite));

    BadgeCache cache(nullptr);
    BOOST_LOG_SEV(lg, info) << "Testing resolve on unloaded cache";

    CHECK(!cache.isLoaded());
    CHECK(cache.resolve("login_status", "Online") == nullptr);
    CHECK(cache.resolve("any_domain", "any_value") == nullptr);
}

TEST_CASE("resolve_returns_definition_for_known_mapping", tags) {
    auto lg(make_logger(test_suite));

    BadgeCache cache(nullptr);
    cache.populate_for_testing(
        {make_def("login_online", "#22c55e", "#ffffff")},
        {make_mapping("login_status", "Online", "login_online")});

    BOOST_LOG_SEV(lg, info) << "Testing resolve for known mapping";

    REQUIRE(cache.isLoaded());
    const auto* result = cache.resolve("login_status", "Online");
    REQUIRE(result != nullptr);
    CHECK(result->code == "login_online");
    CHECK(result->background_colour == "#22c55e");
    CHECK(result->text_colour == "#ffffff");
}

TEST_CASE("resolve_returns_nullptr_for_missing_entity", tags) {
    auto lg(make_logger(test_suite));

    BadgeCache cache(nullptr);
    cache.populate_for_testing(
        {make_def("login_online", "#22c55e", "#ffffff")},
        {make_mapping("login_status", "Online", "login_online")});

    BOOST_LOG_SEV(lg, info) << "Testing resolve for missing entity";

    CHECK(cache.resolve("login_status", "Offline") == nullptr);
    CHECK(cache.resolve("login_status", "") == nullptr);
    CHECK(cache.resolve("nonexistent_domain", "Online") == nullptr);
}

TEST_CASE("resolve_domains_do_not_cross_contaminate", tags) {
    auto lg(make_logger(test_suite));

    BadgeCache cache(nullptr);
    cache.populate_for_testing(
        {make_def("active", "#22c55e", "#ffffff"),
         make_def("locked", "#ef4444", "#ffffff")},
        {make_mapping("party_status", "ACTIVE", "active"),
         make_mapping("account_locked", "Locked", "locked")});

    BOOST_LOG_SEV(lg, info) << "Testing cross-domain isolation";

    const auto* ra = cache.resolve("party_status", "ACTIVE");
    REQUIRE(ra != nullptr);
    CHECK(ra->code == "active");

    const auto* rb = cache.resolve("account_locked", "Locked");
    REQUIRE(rb != nullptr);
    CHECK(rb->code == "locked");

    // Cross-contamination: same entity_code in a different domain must not match
    CHECK(cache.resolve("party_status", "Locked") == nullptr);
    CHECK(cache.resolve("account_locked", "ACTIVE") == nullptr);
}

TEST_CASE("resolve_returns_nullptr_for_unmapped_badge_code", tags) {
    auto lg(make_logger(test_suite));

    // Mapping references a badge_code absent from definitions_ — buildIndex
    // must silently skip it and resolve must return nullptr.
    BadgeCache cache(nullptr);
    cache.populate_for_testing(
        {},
        {make_mapping("login_status", "Online", "nonexistent_badge")});

    BOOST_LOG_SEV(lg, info) << "Testing resolve with unmapped badge code";

    CHECK(cache.resolve("login_status", "Online") == nullptr);
}

TEST_CASE("resolve_multiple_mappings_in_same_domain", tags) {
    auto lg(make_logger(test_suite));

    BadgeCache cache(nullptr);
    cache.populate_for_testing(
        {make_def("login_online",  "#22c55e", "#ffffff"),
         make_def("login_recent",  "#eab308", "#ffffff"),
         make_def("login_old",     "#6b7280", "#ffffff")},
        {make_mapping("login_status", "Online",  "login_online"),
         make_mapping("login_status", "Recent",  "login_recent"),
         make_mapping("login_status", "Old",     "login_old")});

    BOOST_LOG_SEV(lg, info) << "Testing multiple mappings in the same domain";

    const auto* r_online = cache.resolve("login_status", "Online");
    REQUIRE(r_online != nullptr);
    CHECK(r_online->code == "login_online");
    CHECK(r_online->background_colour == "#22c55e");

    const auto* r_recent = cache.resolve("login_status", "Recent");
    REQUIRE(r_recent != nullptr);
    CHECK(r_recent->code == "login_recent");

    const auto* r_old = cache.resolve("login_status", "Old");
    REQUIRE(r_old != nullptr);
    CHECK(r_old->code == "login_old");

    CHECK(cache.resolve("login_status", "Never") == nullptr);
}
