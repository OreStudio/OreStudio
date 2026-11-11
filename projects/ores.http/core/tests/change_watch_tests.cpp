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
#include "ores.http.core/routes/change_routes.hpp"
#include "ores.http.core/stream/change_hub.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <unordered_set>
#include <vector>

using ores::http::server::routes::change_routes;
using namespace ores::http::stream;

namespace {

const std::string watch_system_tenant = "ffffffff-ffff-ffff-ffff-ffffffffffff";

std::shared_ptr<watch_set> watching(std::unordered_set<std::string> collections) {
    auto set = std::make_shared<watch_set>();
    set->replace(std::move(collections));
    return set;
}

void publish(change_hub& hub, const std::string& collection) {
    hub.publish("acme", "", collection, "event: entity-changed\ndata: " + collection + "\n\n");
}

std::string read_first(change_stream_source& source) {
    std::string out;
    boost::asio::io_context io;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> {
            std::vector<char> buffer(4096);
            const auto taken = co_await source.read(std::span(buffer.data(), buffer.size()));
            out.assign(buffer.data(), taken);
        },
        boost::asio::detached);
    io.run();
    return out;
}

}

TEST_CASE("a_watching_stream_hears_only_what_it_watches", "[change_watch]") {
    change_hub hub(watch_system_tenant);
    const auto set = watching({"iam.book"});
    auto stream = hub.subscribe({.tenant = "acme", .watching = set});
    auto everything = hub.subscribe({.tenant = "acme"});

    publish(hub, "iam.book");
    publish(hub, "iam.trade");

    REQUIRE(stream->size() == 1);
    CHECK(stream->try_pop()->find("iam.book") != std::string::npos);
    CHECK(everything->size() == 2);
}

TEST_CASE("a_watch_set_changes_under_an_open_stream", "[change_watch]") {
    change_hub hub(watch_system_tenant);
    const auto set = watching({});
    auto stream = hub.subscribe({.tenant = "acme", .watching = set});

    publish(hub, "iam.book");
    CHECK(stream->size() == 0);

    set->replace({"iam.book"});
    publish(hub, "iam.book");
    CHECK(stream->size() == 1);
}

TEST_CASE("a_resumed_watching_stream_is_given_only_what_it_watches", "[change_watch]") {
    change_hub hub(watch_system_tenant);
    auto first = hub.subscribe({.tenant = "acme"});
    publish(hub, "iam.book");
    const auto seen = first->try_pop();
    REQUIRE(seen);
    const auto id = seen->substr(4, seen->find('\n') - 4);
    publish(hub, "iam.trade");
    publish(hub, "iam.book");

    auto resumed = hub.subscribe({.tenant = "acme", .watching = watching({"iam.book"})},
                                 change_hub::read_id(id));
    REQUIRE(resumed->size() == 1);
    CHECK(resumed->try_pop()->find("iam.book") != std::string::npos);
}

TEST_CASE("a_session_watch_set_lives_as_long_as_its_stream", "[change_watch]") {
    watch_registry registry;
    CHECK_FALSE(registry.watch("s1", {"iam.book"}));
    auto set = registry.open("s1");
    CHECK(registry.open("s1") == set);
    CHECK(registry.watch("s1", {"iam.book"}));
    CHECK(set->contains("iam.book"));
    set.reset();
    CHECK_FALSE(registry.watch("s1", {"iam.book"}));
}

TEST_CASE("a_watch_body_is_one_the_browser_sends", "[change_watch]") {
    const auto two = change_routes::read_watches(
        R"({"watches":[{"component":"iam","entity":"book"},{"component":"trading","entity":"trade"}]})");
    REQUIRE(two);
    CHECK(*two == std::unordered_set<std::string>{"iam.book", "trading.trade"});
    CHECK(change_routes::read_watches("") == std::unordered_set<std::string>{});
    CHECK(change_routes::read_watches("{}") == std::unordered_set<std::string>{});
    CHECK_FALSE(
        change_routes::read_watches(R"({"watches":[{"component":"Iam","entity":"book"}]})"));
    CHECK_FALSE(change_routes::read_watches(R"({"watches":[{"component":"iam"}]})"));
    CHECK_FALSE(change_routes::read_watches(R"({"watches":"iam.book"})"));
    std::string many = R"({"watches":[)";
    for (int i = 0; i < 51; ++i)
        many += std::string(i ? "," : "") + R"({"component":"iam","entity":"book"})";
    CHECK_FALSE(change_routes::read_watches(many + "]}"));
}

TEST_CASE("a_stream_opens_with_its_opening_message", "[change_watch]") {
    change_hub hub(watch_system_tenant);
    change_stream_source source(hub.subscribe({.tenant = "acme"}),
                                std::chrono::milliseconds(10),
                                "event: connected\ndata: {}\n\n");
    CHECK(read_first(source) == "event: connected\ndata: {}\n\n");
}

TEST_CASE("the_change_routes_serve_the_browser_stream_and_its_watch_set", "[change_watch]") {
    auto router = std::make_shared<ores::http::net::router>();
    change_routes routes(std::make_shared<change_hub>(watch_system_tenant),
                         std::chrono::milliseconds(10));
    routes.register_routes(router, std::make_shared<ores::http::openapi::endpoint_registry>());
    const auto has = [&](const std::string& pattern) {
        return std::ranges::any_of(router->routes(), [&](const auto& r) {
            return r.pattern == pattern && r.requires_auth;
        });
    };
    CHECK(has("/api/v1/changes"));
    CHECK(has("/api/events"));
    CHECK(has("/api/events/watch"));
}
