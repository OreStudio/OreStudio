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
#include "ores.http.api/domain/session_store.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

using namespace ores::http::domain;
using namespace ores::logging;
using namespace std::chrono_literals;

namespace {

/**
 * @brief A clock the test moves by hand, so expiry is not a race with a sleep.
 */
class test_clock final {
public:
    [[nodiscard]] std::chrono::system_clock::time_point now() const {
        return epoch_ + std::chrono::duration_cast<std::chrono::system_clock::duration>(elapsed_);
    }

    void advance(std::chrono::seconds by) {
        elapsed_ += by;
    }

private:
    std::chrono::system_clock::time_point epoch_{};
    std::chrono::seconds elapsed_{0};
};

session_record a_record(std::string token = "the-token") {
    session_record record;
    record.token = std::move(token);
    record.claims.subject = "00000000-0000-0000-0000-000000000001";
    record.claims.tenant_id = "00000000-0000-0000-0000-000000000002";
    record.claims.party_id = "00000000-0000-0000-0000-000000000003";
    record.claims.roles = {"reader"};
    return record;
}

}

TEST_CASE("a_session_is_held_against_an_identifier_and_gives_it_back", "[session_store]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing what the store keeps and what it hands out";

    test_clock clock;
    session_store store(1h, [&clock] { return clock.now(); });

    const auto identifier = store.open(a_record());

    REQUIRE(store.size() == 1);
    const auto found = store.touch(identifier);
    REQUIRE(found.has_value());
    REQUIRE(found->token == "the-token");
    REQUIRE(found->claims.subject == "00000000-0000-0000-0000-000000000001");
    REQUIRE(found->claims.tenant_id == "00000000-0000-0000-0000-000000000002");
    REQUIRE(found->claims.roles == std::vector<std::string>{"reader"});
}

TEST_CASE("the_store_never_holds_the_identifier_it_hands_out", "[session_store]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a dump of the store is not a set of credentials";

    test_clock clock;
    session_store store(1h, [&clock] { return clock.now(); });

    const auto identifier = store.open(a_record());

    REQUIRE(session_store::key_of(identifier) != identifier);
    REQUIRE(session_store::key_of(identifier).size() == 64);
    // The lookup works from the identifier, and only from it.
    REQUIRE(!store.find("not-the-identifier").has_value());
}

TEST_CASE("a_session_that_is_left_alone_expires", "[session_store]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a quiet session is evicted";

    test_clock clock;
    session_store store(30s, [&clock] { return clock.now(); });

    const auto identifier = store.open(a_record());

    // Thirty seconds pass in silence, and the session's expiry was thirty seconds
    // after its last use, which was when it was opened.
    clock.advance(31s);
    REQUIRE(!store.touch(identifier).has_value());
    // A lookup that found it expired does not leave it behind.
    REQUIRE(store.size() == 0);
}

TEST_CASE("using_a_session_keeps_it_alive", "[session_store]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the expiry slides with use";

    test_clock clock;
    session_store store(30s, [&clock] { return clock.now(); });

    const auto identifier = store.open(a_record());
    for (auto use = 0; use < 5; ++use) {
        clock.advance(20s);
        REQUIRE(store.touch(identifier).has_value());
    }

    // Eighty seconds in, and the session is alive because it was used.
    clock.advance(20s);
    REQUIRE(store.touch(identifier).has_value());
}

TEST_CASE("closing_a_session_ends_only_that_one", "[session_store]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that signing out does not sign anybody else out";

    test_clock clock;
    session_store store(1h, [&clock] { return clock.now(); });

    const auto first = store.open(a_record("first"));
    const auto second = store.open(a_record("second"));

    REQUIRE(store.close(first));
    REQUIRE(!store.touch(first).has_value());
    REQUIRE(store.touch(second).has_value());
    REQUIRE(!store.close(first));
}

TEST_CASE("a_replaced_identifier_keeps_the_session_and_drops_the_old_one", "[session_store]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a change of privilege mints a new identifier";

    test_clock clock;
    session_store store(1h, [&clock] { return clock.now(); });

    const auto before = store.open(a_record());
    const auto after = store.rotate(before);

    REQUIRE(after.has_value());
    REQUIRE(*after != before);
    // Whoever held the old identifier holds nothing.
    REQUIRE(!store.touch(before).has_value());

    const auto found = store.touch(*after);
    REQUIRE(found.has_value());
    REQUIRE(found->token == "the-token");
}

TEST_CASE("what_has_expired_is_evicted_and_what_has_not_is_kept", "[session_store]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the sweep a timer would run";

    test_clock clock;
    session_store store(30s, [&clock] { return clock.now(); });

    const auto old_one = store.open(a_record("old"));
    clock.advance(20s);
    const auto fresh = store.open(a_record("fresh"));
    clock.advance(15s);

    // The first expired at thirty seconds; the second lives until fifty.
    REQUIRE(store.evict_expired() == 1);
    REQUIRE(store.size() == 1);
    REQUIRE(store.touch(fresh).has_value());
    REQUIRE(!store.touch(old_one).has_value());
}
