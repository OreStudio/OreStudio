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
#include "ores.trading.api/domain/bond_amortization_data.hpp"
#include "ores.trading.api/domain/bond_fixed_leg_data.hpp"
#include "ores.trading.api/domain/bond_float_data.hpp"
#include "ores.trading.api/domain/bond_instrument.hpp"
#include "ores.trading.api/domain/bond_issue.hpp"
#include "ores.trading.api/domain/economic_digest.hpp"
#include "ores.trading.api/domain/trade.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace {

using ores::trading::domain::bond_amortization_data;
using ores::trading::domain::bond_fixed_leg_data;
using ores::trading::domain::bond_float_data;
using ores::trading::domain::bond_instrument;
using ores::trading::domain::bond_issue;
using ores::trading::domain::economic_digest;
using ores::trading::domain::trade;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

/*
 * Two text fields that sit next to each other in the canonical form. The
 * pair (ab, c) concatenates to the same three bytes as the pair (a, bc), so
 * a digest that only concatenated values would collide on them.
 */
struct ambiguous_pair {
    std::string first;
    std::string second;
};

ores::utility::decimal::decimal decimal_of(std::string_view text) {
    return ores::utility::decimal::decimal::from_string(text).value();
}

ores::utility::uuid::tenant_id tenant_of(std::string_view text) {
    return ores::utility::uuid::tenant_id::from_string(text).value();
}

boost::uuids::uuid uuid_of(std::string_view text) {
    return boost::uuids::string_generator()(text.begin(), text.end());
}

bond_instrument make_bond_instrument(std::string_view notional) {
    bond_instrument v;
    v.identity.version = 1;
    v.identity.tenant_id = tenant_of("00000000-0000-0000-0000-0000000000ff");
    v.identity.trade_type_code = "Bond";
    v.identity.party_id = uuid_of("00000000-0000-0000-0000-000000000001");
    v.identity.trade_id = uuid_of("00000000-0000-0000-0000-000000000002");
    v.identity.trade_activity_id = uuid_of("00000000-0000-0000-0000-000000000003");
    v.issue_id = uuid_of("00000000-0000-0000-0000-000000000004");
    v.notional = decimal_of(notional);
    v.audit.modified_by = "system";
    v.audit.performed_by = "system";
    v.audit.change_reason_code = "system.new";
    v.audit.change_commentary = "Test data";
    v.audit.recorded_at = std::chrono::system_clock::time_point{};
    return v;
}

}

using namespace ores::logging;

TEST_CASE("economic_digest_is_stable_for_the_same_value", tags) {
    auto lg(make_logger(test_suite));

    const auto sut = make_bond_instrument("1000000.25");
    const auto first = economic_digest(sut);
    const auto second = economic_digest(sut);
    BOOST_LOG_SEV(lg, info) << "Digest: " << first;

    CHECK(first == second);
    CHECK(first.size() == 64);
    CHECK(first != economic_digest(bond_instrument{}));
}

TEST_CASE("economic_digest_changes_when_an_economic_field_changes", tags) {
    auto lg(make_logger(test_suite));

    const auto baseline = make_bond_instrument("1000000.25");
    const auto amended = make_bond_instrument("1000000.26");
    BOOST_LOG_SEV(lg, info) << "Baseline: " << economic_digest(baseline);
    BOOST_LOG_SEV(lg, info) << "Amended:  " << economic_digest(amended);

    CHECK(economic_digest(baseline) != economic_digest(amended));

    auto absent = baseline;
    absent.notional = std::nullopt;
    CHECK(economic_digest(absent) != economic_digest(baseline));
}

TEST_CASE("economic_digest_changes_when_an_economic_text_changes", tags) {
    auto lg(make_logger(test_suite));

    bond_amortization_data baseline;
    baseline.type = "FixedAmount";

    bond_amortization_data amended;
    amended.type = "Annuity";

    BOOST_LOG_SEV(lg, info) << "Baseline: " << economic_digest(baseline);
    BOOST_LOG_SEV(lg, info) << "Amended:  " << economic_digest(amended);

    CHECK(economic_digest(baseline) != economic_digest(amended));
}

/*
 * The load-bearing assertion: a change to an identity or audit field is not
 * a change the customer agreed to, so it must leave the digest alone. A
 * digest that moved here would move the trade's external version for no
 * reason.
 */
TEST_CASE("economic_digest_ignores_identity_and_audit_fields", tags) {
    auto lg(make_logger(test_suite));

    const auto baseline = make_bond_instrument("1000000.25");
    const auto expected = economic_digest(baseline);

    auto changed = baseline;
    changed.identity.version = 42;
    changed.identity.tenant_id = tenant_of("00000000-0000-0000-0000-0000000000aa");
    changed.identity.trade_type_code = "ForwardBond";
    changed.identity.party_id = uuid_of("00000000-0000-0000-0000-000000000011");
    changed.identity.trade_id = uuid_of("00000000-0000-0000-0000-000000000022");
    changed.identity.trade_activity_id = uuid_of("00000000-0000-0000-0000-000000000033");
    changed.issue_id = uuid_of("00000000-0000-0000-0000-000000000044");
    changed.audit.modified_by = "someone.else";
    changed.audit.performed_by = "someone.else";
    changed.audit.change_reason_code = "system.amend";
    changed.audit.change_commentary = "a change nobody agreed to";
    changed.audit.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Baseline:      " << expected;
    BOOST_LOG_SEV(lg, info) << "Audit changed: " << economic_digest(changed);

    CHECK(economic_digest(changed) == expected);
}

TEST_CASE("economic_digest_of_a_trade_ignores_its_audit_tail", tags) {
    auto lg(make_logger(test_suite));

    trade baseline;
    baseline.id = uuid_of("00000000-0000-0000-0000-000000000101");
    baseline.party_id = uuid_of("00000000-0000-0000-0000-000000000102");
    baseline.counterparty_id = uuid_of("00000000-0000-0000-0000-000000000103");
    baseline.trade_type = "Swap";
    baseline.counterparty_scope = ores::trading::domain::counterparty_scope::external;
    baseline.booking_nature = ores::trading::domain::booking_nature::actual;
    baseline.entry_channel = ores::trading::domain::entry_channel::manual;
    const auto expected = economic_digest(baseline);

    auto changed = baseline;
    changed.version = 9;
    changed.tenant_id = tenant_of("00000000-0000-0000-0000-0000000000bb");
    changed.id = uuid_of("00000000-0000-0000-0000-000000000104");
    changed.party_id = uuid_of("00000000-0000-0000-0000-000000000105");
    changed.counterparty_id = uuid_of("00000000-0000-0000-0000-000000000106");
    changed.external_version = 7;
    changed.economic_digest = "0123456789abcdef";
    changed.modified_by = "someone.else";
    changed.performed_by = "someone.else";
    changed.change_reason_code = "system.amend";
    changed.change_commentary = "a change nobody agreed to";
    changed.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Baseline:      " << expected;
    BOOST_LOG_SEV(lg, info) << "Audit changed: " << economic_digest(changed);

    CHECK(economic_digest(changed) == expected);
}

TEST_CASE("economic_digest_of_a_trade_moves_with_a_classification", tags) {
    auto lg(make_logger(test_suite));

    trade baseline;
    baseline.trade_type = "Swap";

    auto amended = baseline;
    amended.counterparty_scope = ores::trading::domain::counterparty_scope::intra_entity;

    BOOST_LOG_SEV(lg, info) << "Baseline: " << economic_digest(baseline);
    BOOST_LOG_SEV(lg, info) << "Amended:  " << economic_digest(amended);

    CHECK(economic_digest(baseline) != economic_digest(amended));
}

TEST_CASE("economic_digest_treats_a_sequence_as_ordered_elements", tags) {
    auto lg(make_logger(test_suite));

    bond_fixed_leg_data ascending;
    ascending.rates = {bond_float_data{0.01, std::string("2026-01-01")},
                       bond_float_data{0.02, std::string("2026-07-01")}};

    bond_fixed_leg_data descending;
    descending.rates = {bond_float_data{0.02, std::string("2026-07-01")},
                        bond_float_data{0.01, std::string("2026-01-01")}};

    BOOST_LOG_SEV(lg, info) << "Ascending:  " << economic_digest(ascending);
    BOOST_LOG_SEV(lg, info) << "Descending: " << economic_digest(descending);

    CHECK(economic_digest(ascending) != economic_digest(descending));
}

TEST_CASE("economic_digest_does_not_collide_on_ambiguous_concatenation", tags) {
    auto lg(make_logger(test_suite));

    const ambiguous_pair left{"ab", "c"};
    const ambiguous_pair right{"a", "bc"};

    BOOST_LOG_SEV(lg, info) << "Left:  " << economic_digest(left);
    BOOST_LOG_SEV(lg, info) << "Right: " << economic_digest(right);

    CHECK(left.first + left.second == right.first + right.second);
    CHECK(economic_digest(left) != economic_digest(right));
}

/*
 * The mirror of the exclusion case: an external identifier is not a foreign
 * key. Re-pointing a trade from one security to another is a change the
 * customer agreed to, so the digest must move. The issue's own surrogate key
 * is still identity, so re-keying it must not.
 */
TEST_CASE("economic_digest_moves_with_an_external_identifier", tags) {
    auto lg(make_logger(test_suite));

    bond_issue baseline;
    baseline.issue_id = uuid_of("00000000-0000-0000-0000-000000000201");
    baseline.security_id = "GB0002634946";
    baseline.issuer = "HM Treasury";
    baseline.face_value = decimal_of("1000");
    baseline.issue_date = std::chrono::year_month_day{std::chrono::year{2026},
                                                      std::chrono::month{1},
                                                      std::chrono::day{15}};

    auto re_pointed = baseline;
    re_pointed.security_id = "GB0002634947";

    auto re_keyed = baseline;
    re_keyed.issue_id = uuid_of("00000000-0000-0000-0000-000000000202");

    BOOST_LOG_SEV(lg, info) << "Baseline:   " << economic_digest(baseline);
    BOOST_LOG_SEV(lg, info) << "Re-pointed: " << economic_digest(re_pointed);
    BOOST_LOG_SEV(lg, info) << "Re-keyed:   " << economic_digest(re_keyed);

    CHECK(economic_digest(re_pointed) != economic_digest(baseline));
    CHECK(economic_digest(re_keyed) == economic_digest(baseline));
}
