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
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/counterparty_scope.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.trading.api/domain/trade_economic_digest.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <string_view>
#include <vector>

namespace {

using ores::trading::domain::trade;
using ores::trading::domain::trade_economic_digest;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

ores::utility::uuid::tenant_id tenant_of(std::string_view text) {
    return ores::utility::uuid::tenant_id::from_string(text).value();
}

boost::uuids::uuid uuid_of(std::string_view text) {
    return boost::uuids::string_generator()(text.begin(), text.end());
}

trade make_trade() {
    trade t;
    t.id = uuid_of("00000000-0000-0000-0000-000000000101");
    t.party_id = uuid_of("00000000-0000-0000-0000-000000000102");
    t.counterparty_id = uuid_of("00000000-0000-0000-0000-000000000103");
    t.trade_type = "Swap";
    t.counterparty_scope = ores::trading::domain::counterparty_scope::external;
    t.booking_nature = ores::trading::domain::booking_nature::actual;
    t.entry_channel = ores::trading::domain::entry_channel::manual;
    t.external_version = 3;
    t.economic_digest = "0123456789abcdef";
    t.modified_by = "system";
    t.performed_by = "system";
    t.change_reason_code = "system.new";
    t.change_commentary = "Test data";
    t.recorded_at = std::chrono::system_clock::time_point{};
    return t;
}

}

using namespace ores::logging;

TEST_CASE("trade_economic_digest_is_stable_for_the_same_inputs", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_trade();
    const std::vector<std::string> components{"digest-a", "digest-b", "digest-c"};
    const auto first = trade_economic_digest(sut, components);
    const auto second = trade_economic_digest(sut, components);
    BOOST_LOG_SEV(lg, info) << "Digest: " << first;

    CHECK(first == second);
    CHECK(first.size() == 64);
    CHECK(first != trade_economic_digest(trade{}, {}));
}

TEST_CASE("trade_economic_digest_changes_when_a_component_digest_changes", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_trade();
    const std::vector<std::string> baseline{"digest-a", "digest-b", "digest-c"};
    auto amended = baseline;
    amended[1] = "digest-z";
    BOOST_LOG_SEV(lg, info) << "Baseline: " << trade_economic_digest(sut, baseline);
    BOOST_LOG_SEV(lg, info) << "Amended:  " << trade_economic_digest(sut, amended);

    CHECK(trade_economic_digest(sut, baseline) != trade_economic_digest(sut, amended));
}

TEST_CASE("trade_economic_digest_changes_when_components_are_reordered", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_trade();
    const std::vector<std::string> ascending{"digest-a", "digest-b"};
    const std::vector<std::string> descending{"digest-b", "digest-a"};
    BOOST_LOG_SEV(lg, info) << "Ascending:  " << trade_economic_digest(sut, ascending);
    BOOST_LOG_SEV(lg, info) << "Descending: " << trade_economic_digest(sut, descending);

    CHECK(trade_economic_digest(sut, ascending) != trade_economic_digest(sut, descending));
}

TEST_CASE("trade_economic_digest_changes_when_a_component_is_added_or_removed", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_trade();
    const std::vector<std::string> baseline{"digest-a", "digest-b"};
    const std::vector<std::string> removed{"digest-a"};
    const std::vector<std::string> added{"digest-a", "digest-b", "digest-c"};
    BOOST_LOG_SEV(lg, info) << "Baseline: " << trade_economic_digest(sut, baseline);
    BOOST_LOG_SEV(lg, info) << "Removed:  " << trade_economic_digest(sut, removed);
    BOOST_LOG_SEV(lg, info) << "Added:    " << trade_economic_digest(sut, added);

    CHECK(trade_economic_digest(sut, baseline) != trade_economic_digest(sut, removed));
    CHECK(trade_economic_digest(sut, baseline) != trade_economic_digest(sut, added));
}

TEST_CASE("trade_economic_digest_distinguishes_an_empty_component_list", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_trade();
    const auto empty = trade_economic_digest(sut, {});
    const auto single = trade_economic_digest(sut, {"digest-a"});
    BOOST_LOG_SEV(lg, info) << "Empty:  " << empty;
    BOOST_LOG_SEV(lg, info) << "Single: " << single;

    CHECK(empty.size() == 64);
    CHECK(empty != single);
    CHECK(empty == trade_economic_digest(sut, {}));
}

/*
 * The load-bearing mirror of the digest's own exclusion case: the trade's
 * audit and identity fields must not reach this digest either, because it
 * reuses economic_digest for the trade's own fields.
 */
TEST_CASE("trade_economic_digest_ignores_the_trade_audit_and_identity_tail", tags) {
    auto lg(make_logger(test_suite));

    const auto baseline = make_trade();
    const std::vector<std::string> components{"digest-a", "digest-b"};
    const auto expected = trade_economic_digest(baseline, components);

    auto changed = baseline;
    changed.version = 9;
    changed.tenant_id = tenant_of("00000000-0000-0000-0000-0000000000bb");
    changed.id = uuid_of("00000000-0000-0000-0000-000000000104");
    changed.party_id = uuid_of("00000000-0000-0000-0000-000000000105");
    changed.counterparty_id = uuid_of("00000000-0000-0000-0000-000000000106");
    changed.external_version = 7;
    changed.economic_digest = "fedcba9876543210";
    changed.modified_by = "someone.else";
    changed.performed_by = "someone.else";
    changed.change_reason_code = "system.amend";
    changed.change_commentary = "a change nobody agreed to";
    changed.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Baseline:      " << expected;
    BOOST_LOG_SEV(lg, info) << "Audit changed: " << trade_economic_digest(changed, components);

    CHECK(trade_economic_digest(changed, components) == expected);
}

TEST_CASE("trade_economic_digest_moves_with_a_trade_classification", tags) {
    auto lg(make_logger(test_suite));

    const auto baseline = make_trade();
    const std::vector<std::string> components{"digest-a"};

    auto amended = baseline;
    amended.counterparty_scope = ores::trading::domain::counterparty_scope::intra_entity;

    BOOST_LOG_SEV(lg, info) << "Baseline: " << trade_economic_digest(baseline, components);
    BOOST_LOG_SEV(lg, info) << "Amended:  " << trade_economic_digest(amended, components);

    CHECK(trade_economic_digest(baseline, components) !=
          trade_economic_digest(amended, components));
}

/*
 * The frames carry a length, so two component lists that concatenate the same
 * bytes must still fold to different digests; a fold that only appended the
 * digests would collide here.
 */
TEST_CASE("trade_economic_digest_does_not_collide_on_ambiguous_concatenation", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_trade();
    const std::vector<std::string> left{"ab", "c"};
    const std::vector<std::string> right{"a", "bc"};
    BOOST_LOG_SEV(lg, info) << "Left:  " << trade_economic_digest(sut, left);
    BOOST_LOG_SEV(lg, info) << "Right: " << trade_economic_digest(sut, right);

    CHECK(left[0] + left[1] == right[0] + right[1]);
    CHECK(trade_economic_digest(sut, left) != trade_economic_digest(sut, right));
}

/*
 * Pins the component count's boundary: the count is framed like every other
 * value, so a long component digest cannot merge with the count and make a
 * list fold as a different one.
 */
TEST_CASE("trade_economic_digest_pins_the_component_count_boundary", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_trade();
    const std::string long_digest(64, 'a');
    const std::vector<std::string> one{long_digest};
    const std::vector<std::string> two{long_digest, "digest-b"};
    BOOST_LOG_SEV(lg, info) << "One: " << trade_economic_digest(sut, one);
    BOOST_LOG_SEV(lg, info) << "Two: " << trade_economic_digest(sut, two);

    CHECK(trade_economic_digest(sut, one) != trade_economic_digest(sut, two));
    CHECK(trade_economic_digest(sut, one).size() == 64);
}
