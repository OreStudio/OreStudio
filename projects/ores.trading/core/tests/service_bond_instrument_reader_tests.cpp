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
#include "ores.testing/database_helper.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_repository.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_repository.hpp"
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.trading.core/repository/bond_option_repository.hpp"
#include "ores.trading.core/service/bond_instrument_reader.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[repository]");

using ores::testing::database_helper;
using ores::trading::domain::bond_instrument;
using ores::trading::domain::bond_issue;
using ores::trading::domain::bond_issue_call_date;
using ores::trading::domain::bond_issue_conversion_target;
using ores::trading::domain::bond_option;
using ores::trading::repository::bond_instrument_repository;
using ores::trading::repository::bond_issue_call_date_repository;
using ores::trading::repository::bond_issue_conversion_target_repository;
using ores::trading::repository::bond_issue_repository;
using ores::trading::repository::bond_option_repository;
using ores::trading::service::bond_instrument_reader;

struct stamps final {
    boost::uuids::uuid party_id;
    ores::utility::uuid::tenant_id tenant_id;
    std::string user;
};

template <typename T>
void stamp(T& v, const stamps& s) {
    v.tenant_id = s.tenant_id;
    v.modified_by = s.user;
    v.performed_by = s.user;
    v.change_reason_code = "system.test";
    v.change_commentary = "Bond instrument reader test";
}

stamps make_stamps(database_helper& h) {
    return stamps{.party_id = boost::uuids::random_generator()(),
                  .tenant_id = h.tenant_id(),
                  .user = h.db_user()};
}

auto make_context(database_helper& h, const stamps& s) {
    return h.context().with_party(s.tenant_id, s.party_id, {s.party_id}, s.user);
}

bond_issue make_issue(const stamps& s, const std::string& security_id) {
    bond_issue r;
    stamp(r, s);
    r.issue_id = boost::uuids::random_generator()();
    r.security_id = security_id;
    r.issuer = "ACME";
    r.currency = "EUR";
    r.face_value = 1000.0;
    r.coupon_rate = 5.0;
    r.coupon_frequency_code = "Annual";
    r.day_count_code = "ACT/365";
    r.issue_date = "2024-01-15";
    r.maturity_date = "2034-01-15";
    r.description = "Test issue";
    return r;
}

bond_instrument make_instrument(const stamps& s,
                                const boost::uuids::uuid& issue_id,
                                const std::string& trade_type_code) {
    bond_instrument r;
    r.identity.instrument_id = boost::uuids::random_generator()();
    r.identity.tenant_id = s.tenant_id;
    r.identity.party_id = s.party_id;
    r.identity.trade_type_code = trade_type_code;
    r.issue_id = issue_id;
    r.audit.modified_by = s.user;
    r.audit.performed_by = s.user;
    r.audit.change_reason_code = "system.test";
    r.audit.change_commentary = "Bond instrument reader test";
    return r;
}

bond_issue_call_date make_call_date(const stamps& s,
                                    const boost::uuids::uuid& issue_id,
                                    int sequence_number,
                                    const std::string& call_date) {
    bond_issue_call_date r;
    stamp(r, s);
    r.issue_id = issue_id;
    r.sequence_number = sequence_number;
    r.call_date = call_date;
    return r;
}

bond_issue_conversion_target make_conversion_target(const stamps& s,
                                                    const boost::uuids::uuid& issue_id,
                                                    int sequence_number,
                                                    const std::string& underlying_id) {
    bond_issue_conversion_target r;
    stamp(r, s);
    r.issue_id = issue_id;
    r.sequence_number = sequence_number;
    r.underlying_id = underlying_id;
    r.conversion_ratio = 1.0;
    return r;
}

bond_option make_option(const stamps& s, const boost::uuids::uuid& instrument_id) {
    bond_option r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.option_type = "Call";
    return r;
}

}

TEST_CASE("read_instruments_rebuilds_children_in_ordinal_order", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000001");
    bond_issue_repository().write(ctx, issue);
    const auto instr = make_instrument(s, issue.issue_id, "Bond");
    bond_instrument_repository().write(ctx, instr);

    // Written out of ordinal order, so the order read back is the query's
    // doing rather than the insert order's.
    for (const auto& r : {make_call_date(s, issue.issue_id, 3, "2030-01-15"),
                          make_call_date(s, issue.issue_id, 1, "2028-01-15"),
                          make_call_date(s, issue.issue_id, 2, "2029-01-15")})
        bond_issue_call_date_repository().write(ctx, r);
    for (const auto& r : {make_conversion_target(s, issue.issue_id, 2, "UND-2"),
                          make_conversion_target(s, issue.issue_id, 1, "UND-1")})
        bond_issue_conversion_target_repository().write(ctx, r);

    const auto id = boost::uuids::to_string(instr.identity.instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({id});

    REQUIRE(instruments.size() == 1);
    const auto& data = instruments.at(id);
    CHECK(data.issue.security_id == "XS0000000001");
    CHECK(data.issue.issue_id == issue.issue_id);

    REQUIRE(data.call_dates.size() == 3);
    CHECK(data.call_dates[0].call_date == "2028-01-15");
    CHECK(data.call_dates[1].call_date == "2029-01-15");
    CHECK(data.call_dates[2].call_date == "2030-01-15");

    REQUIRE(data.conversion_targets.size() == 2);
    CHECK(data.conversion_targets[0].underlying_id == "UND-1");
    CHECK(data.conversion_targets[1].underlying_id == "UND-2");
}

TEST_CASE("read_instruments_gives_two_instruments_the_one_issue", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    // The issue is keyed by security id, so every instrument of one ISIN
    // reads the same row rather than one per instrument.
    const auto issue = make_issue(s, "XS0000000002");
    bond_issue_repository().write(ctx, issue);
    const auto first = make_instrument(s, issue.issue_id, "Bond");
    const auto second = make_instrument(s, issue.issue_id, "BondOption");
    bond_instrument_repository().write(ctx, first);
    bond_instrument_repository().write(ctx, second);
    bond_issue_call_date_repository().write(
        ctx, make_call_date(s, issue.issue_id, 1, "2028-01-15"));

    const auto first_id = boost::uuids::to_string(first.identity.instrument_id);
    const auto second_id = boost::uuids::to_string(second.identity.instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({first_id, second_id});

    REQUIRE(instruments.size() == 2);
    CHECK(instruments.at(first_id).issue.issue_id == issue.issue_id);
    CHECK(instruments.at(second_id).issue.issue_id == issue.issue_id);
    CHECK(instruments.at(first_id).call_dates.size() == 1);
    CHECK(instruments.at(second_id).call_dates.size() == 1);
}

TEST_CASE("read_instruments_omits_an_instrument_with_no_header_row", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000003");
    bond_issue_repository().write(ctx, issue);
    const auto written = make_instrument(s, issue.issue_id, "Bond");
    bond_instrument_repository().write(ctx, written);

    const auto written_id = boost::uuids::to_string(written.identity.instrument_id);
    const auto unwritten_id = boost::uuids::to_string(boost::uuids::random_generator()());

    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({written_id, unwritten_id});

    REQUIRE(instruments.size() == 1);
    CHECK(instruments.contains(written_id));
    CHECK(!instruments.contains(unwritten_id));
}

TEST_CASE("read_instruments_reads_a_fact_row_only_for_its_type_code", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000004");
    bond_issue_repository().write(ctx, issue);
    const auto option = make_instrument(s, issue.issue_id, "BondOption");
    const auto plain = make_instrument(s, issue.issue_id, "Bond");
    bond_instrument_repository().write(ctx, option);
    bond_instrument_repository().write(ctx, plain);
    bond_option_repository().write(
        ctx, make_option(s, option.identity.instrument_id));

    const auto option_id = boost::uuids::to_string(option.identity.instrument_id);
    const auto plain_id = boost::uuids::to_string(plain.identity.instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({option_id, plain_id});

    REQUIRE(instruments.size() == 2);

    // The option row exists for one instrument only, and a plain bond has
    // no fact table to read from at all.
    const auto& with_option = instruments.at(option_id);
    REQUIRE(with_option.option.has_value());
    CHECK(with_option.option->option_type == "Call");
    CHECK(!instruments.at(plain_id).option.has_value());
}
