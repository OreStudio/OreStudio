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
#include "ores.trading.core/repository/bond_forward_repository.hpp"
#include "ores.trading.core/repository/bond_future_delivery_basket_repository.hpp"
#include "ores.trading.core/repository/bond_instrument_repository.hpp"
#include "ores.trading.core/repository/bond_issue_call_date_repository.hpp"
#include "ores.trading.core/repository/bond_issue_conversion_target_repository.hpp"
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.trading.core/repository/bond_leg_amortization_repository.hpp"
#include "ores.trading.core/repository/bond_leg_amount_repository.hpp"
#include "ores.trading.core/repository/bond_leg_rate_repository.hpp"
#include "ores.trading.core/repository/bond_leg_repository.hpp"
#include "ores.trading.core/repository/bond_option_repository.hpp"
#include "ores.trading.core/repository/bond_trs_repository.hpp"
#include "ores.trading.core/repository/instrument_option_exercise_fee_repository.hpp"
#include "ores.trading.core/repository/instrument_option_payment_date_repository.hpp"
#include "ores.trading.core/repository/instrument_option_premium_repository.hpp"
#include "ores.trading.core/repository/instrument_option_repository.hpp"
#include "ores.trading.core/repository/instrument_schedule_date_repository.hpp"
#include "ores.trading.core/repository/instrument_schedule_repository.hpp"
#include "ores.trading.core/repository/instrument_strike_repository.hpp"
#include "ores.trading.core/service/bond_instrument_reader.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[repository]");

using ores::testing::database_helper;
using ores::trading::domain::bond_forward;
using ores::trading::domain::bond_future_delivery_basket;
using ores::trading::domain::bond_instrument;
using ores::trading::domain::bond_issue;
using ores::trading::domain::bond_issue_call_date;
using ores::trading::domain::bond_issue_conversion_target;
using ores::trading::domain::bond_leg;
using ores::trading::domain::bond_leg_amortization;
using ores::trading::domain::bond_leg_amount;
using ores::trading::domain::bond_leg_rate;
using ores::trading::domain::bond_option;
using ores::trading::domain::bond_trs;
using ores::trading::domain::instrument_option;
using ores::trading::domain::instrument_option_exercise_fee;
using ores::trading::domain::instrument_option_payment_date;
using ores::trading::domain::instrument_option_premium;
using ores::trading::domain::instrument_schedule;
using ores::trading::domain::instrument_schedule_date;
using ores::trading::domain::instrument_strike;
using ores::trading::repository::bond_forward_repository;
using ores::trading::repository::bond_future_delivery_basket_repository;
using ores::trading::repository::bond_instrument_repository;
using ores::trading::repository::bond_issue_call_date_repository;
using ores::trading::repository::bond_issue_conversion_target_repository;
using ores::trading::repository::bond_issue_repository;
using ores::trading::repository::bond_leg_amortization_repository;
using ores::trading::repository::bond_leg_amount_repository;
using ores::trading::repository::bond_leg_rate_repository;
using ores::trading::repository::bond_leg_repository;
using ores::trading::repository::bond_option_repository;
using ores::trading::repository::bond_trs_repository;
using ores::trading::repository::instrument_option_exercise_fee_repository;
using ores::trading::repository::instrument_option_payment_date_repository;
using ores::trading::repository::instrument_option_premium_repository;
using ores::trading::repository::instrument_option_repository;
using ores::trading::repository::instrument_schedule_date_repository;
using ores::trading::repository::instrument_schedule_repository;
using ores::trading::repository::instrument_strike_repository;
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

instrument_option make_instrument_option(const stamps& s,
                                         const boost::uuids::uuid& instrument_id) {
    instrument_option r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.long_short = "Long";
    return r;
}

instrument_option_premium make_premium(const stamps& s,
                                       const boost::uuids::uuid& instrument_id,
                                       int sequence_number) {
    instrument_option_premium r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.sequence_number = sequence_number;
    r.amount = 1000.0;
    r.currency = "EUR";
    r.pay_date = "2024-01-20";
    return r;
}

instrument_option_exercise_fee make_exercise_fee(const stamps& s,
                                                 const boost::uuids::uuid& instrument_id,
                                                 int sequence_number) {
    instrument_option_exercise_fee r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.sequence_number = sequence_number;
    r.amount = 25.0;
    return r;
}

instrument_option_payment_date make_payment_date(const stamps& s,
                                                 const boost::uuids::uuid& instrument_id,
                                                 int sequence_number,
                                                 const std::string& payment_date) {
    instrument_option_payment_date r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.sequence_number = sequence_number;
    r.payment_date = payment_date;
    return r;
}

instrument_strike make_strike(const stamps& s, const boost::uuids::uuid& instrument_id) {
    instrument_strike r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.price_value = 101.5;
    r.price_currency = "EUR";
    return r;
}

bond_forward make_forward(const stamps& s, const boost::uuids::uuid& instrument_id) {
    bond_forward r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    return r;
}

bond_future_delivery_basket make_delivery_basket(const stamps& s,
                                                 const boost::uuids::uuid& instrument_id,
                                                 int sequence_number,
                                                 const std::string& delivery_basket_id) {
    bond_future_delivery_basket r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.sequence_number = sequence_number;
    r.delivery_basket_id = delivery_basket_id;
    return r;
}

bond_trs make_trs(const stamps& s, const boost::uuids::uuid& instrument_id) {
    bond_trs r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.return_type = "TotalReturn";
    r.funding_leg_type = "Fixed";
    r.funding_rate = 3.0;
    r.funding_index = "EURIBOR-6M";
    return r;
}

bond_leg make_leg(const stamps& s,
                  const boost::uuids::uuid& instrument_id,
                  const std::string& leg_role,
                  int leg_number) {
    bond_leg r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.leg_role = leg_role;
    r.leg_number = leg_number;
    r.payer = true;
    r.leg_type = "Fixed";
    r.currency = "EUR";
    r.day_counter = "ACT/365";
    return r;
}

bond_leg_amount make_amount(const stamps& s,
                            const boost::uuids::uuid& instrument_id,
                            const std::string& leg_role,
                            int leg_number,
                            const std::string& amount_role,
                            int sequence_number,
                            double value) {
    bond_leg_amount r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.leg_role = leg_role;
    r.leg_number = leg_number;
    r.amount_role = amount_role;
    r.sequence_number = sequence_number;
    r.value = value;
    return r;
}

bond_leg_rate make_rate(const stamps& s,
                        const boost::uuids::uuid& instrument_id,
                        const std::string& leg_role,
                        int leg_number,
                        const std::string& rate_kind) {
    bond_leg_rate r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.leg_role = leg_role;
    r.leg_number = leg_number;
    r.rate_kind = rate_kind;
    return r;
}

bond_leg_amortization make_amortization(const stamps& s,
                                        const boost::uuids::uuid& instrument_id,
                                        const std::string& leg_role,
                                        int leg_number,
                                        int sequence_number) {
    bond_leg_amortization r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.leg_role = leg_role;
    r.leg_number = leg_number;
    r.sequence_number = sequence_number;
    r.amortization_type = "FixedAmount";
    r.value = 100.0;
    return r;
}

instrument_schedule make_schedule(const stamps& s,
                                  const boost::uuids::uuid& instrument_id,
                                  const std::string& owner_role,
                                  int owner_number,
                                  const std::string& schedule_role,
                                  int sequence_number,
                                  const std::string& schedule_kind) {
    instrument_schedule r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.owner_role = owner_role;
    r.owner_number = owner_number;
    r.schedule_role = schedule_role;
    r.sequence_number = sequence_number;
    r.schedule_kind = schedule_kind;
    return r;
}

instrument_schedule_date make_schedule_date(const stamps& s,
                                            const boost::uuids::uuid& instrument_id,
                                            const std::string& owner_role,
                                            int owner_number,
                                            const std::string& schedule_role,
                                            int schedule_sequence_number,
                                            int sequence_number,
                                            const std::string& schedule_date) {
    instrument_schedule_date r;
    stamp(r, s);
    r.instrument_id = instrument_id;
    r.owner_role = owner_role;
    r.owner_number = owner_number;
    r.schedule_role = schedule_role;
    r.schedule_sequence_number = schedule_sequence_number;
    r.sequence_number = sequence_number;
    r.schedule_date = schedule_date;
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

TEST_CASE("read_instruments_rebuilds_a_fixed_leg_from_its_rows", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000005");
    bond_issue_repository().write(ctx, issue);
    const auto instr = make_instrument(s, issue.issue_id, "Bond");
    bond_instrument_repository().write(ctx, instr);
    const auto instrument_id = instr.identity.instrument_id;

    auto leg = make_leg(s, instrument_id, "bond", 1);
    leg.last_period_day_counter = "ACT/360";
    leg.notional_payment_lag = 2;
    leg.settlement_fx_index = "EUR/USD";
    leg.settlement_fixing_date = "2024-04-15";
    bond_leg_repository().write(ctx, leg);

    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "notional", 1, 1000.0));
    // Written in reverse ordinal order, so the order read back is the
    // query's doing rather than the insert order's.
    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "rate", 2, 5.0));
    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "rate", 1, 4.0));

    bond_leg_rate_repository().write(ctx, make_rate(s, instrument_id, "bond", 1, "fixed"));

    auto amortization = make_amortization(s, instrument_id, "bond", 1, 1);
    amortization.end_date = "2030-01-15";
    bond_leg_amortization_repository().write(ctx, amortization);

    auto rules = make_schedule(s, instrument_id, "bond", 1, "schedule", 1, "rules");
    rules.start_date = "2024-01-15";
    rules.tenor = "1Y";
    rules.calendar = "TARGET";
    rules.convention = "ModifiedFollowing";
    instrument_schedule_repository().write(ctx, rules);

    auto payment_dates = make_schedule(s, instrument_id, "bond", 1, "payment_dates", 1, "dates");
    instrument_schedule_repository().write(ctx, payment_dates);
    instrument_schedule_date_repository().write(
        ctx, make_schedule_date(s, instrument_id, "bond", 1, "payment_dates", 1, 1, "2024-07-15"));
    instrument_schedule_date_repository().write(
        ctx, make_schedule_date(s, instrument_id, "bond", 1, "payment_dates", 1, 2, "2025-01-15"));

    const auto id = boost::uuids::to_string(instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({id});

    REQUIRE(instruments.size() == 1);
    const auto& data = instruments.at(id);
    REQUIRE(data.bond_legs.size() == 1);
    const auto& rebuilt = data.bond_legs.front();
    CHECK(rebuilt.payer == true);
    CHECK(rebuilt.leg_type == "Fixed");
    CHECK(rebuilt.currency == "EUR");
    CHECK(rebuilt.day_counter == "ACT/365");
    CHECK(rebuilt.last_period_day_counter == "ACT/360");
    CHECK(rebuilt.notional_payment_lag == 2);

    REQUIRE(rebuilt.settlement.has_value());
    CHECK(rebuilt.settlement->fx_index == "EUR/USD");
    CHECK(rebuilt.settlement->fixing_date == "2024-04-15");

    REQUIRE(rebuilt.notionals.size() == 1);
    CHECK(rebuilt.notionals[0].value == 1000.0);

    REQUIRE(rebuilt.rate.has_value());
    REQUIRE(rebuilt.rate->fixed.has_value());
    CHECK(!rebuilt.rate->floating.has_value());
    REQUIRE(rebuilt.rate->fixed->rates.size() == 2);
    CHECK(rebuilt.rate->fixed->rates[0].value == 4.0);
    CHECK(rebuilt.rate->fixed->rates[1].value == 5.0);

    REQUIRE(rebuilt.amortizations.size() == 1);
    CHECK(rebuilt.amortizations[0].type == "FixedAmount");
    CHECK(rebuilt.amortizations[0].value == 100.0);
    CHECK(rebuilt.amortizations[0].end_date == "2030-01-15");

    REQUIRE(rebuilt.schedule.rules.size() == 1);
    CHECK(rebuilt.schedule.rules[0].start_date == "2024-01-15");
    CHECK(rebuilt.schedule.rules[0].tenor == "1Y");
    CHECK(rebuilt.schedule.rules[0].calendar == "TARGET");
    CHECK(rebuilt.schedule.dates.empty());

    REQUIRE(rebuilt.payment_dates.size() == 2);
    CHECK(rebuilt.payment_dates[0] == "2024-07-15");
    CHECK(rebuilt.payment_dates[1] == "2025-01-15");
}

TEST_CASE("read_instruments_rebuilds_a_floating_leg_and_its_schedules", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000006");
    bond_issue_repository().write(ctx, issue);
    const auto instr = make_instrument(s, issue.issue_id, "Bond");
    bond_instrument_repository().write(ctx, instr);
    const auto instrument_id = instr.identity.instrument_id;

    bond_leg_repository().write(ctx, make_leg(s, instrument_id, "bond", 1));

    auto rate = make_rate(s, instrument_id, "bond", 1, "floating");
    rate.index = "EURIBOR-6M";
    rate.is_in_arrears = false;
    rate.fixing_days = 2;
    rate.front_stub_short_index = "EURIBOR-3M";
    rate.front_stub_long_index = "EURIBOR-6M";
    rate.front_stub_rounding_precision = 5;
    rate.observation_shift = true;
    bond_leg_rate_repository().write(ctx, rate);

    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "notional", 1, 1000000.0));
    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "spread", 2, 0.75));
    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "spread", 1, 0.5));
    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "cap", 1, 6.0));
    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "floor", 1, 1.0));
    bond_leg_amount_repository().write(
        ctx, make_amount(s, instrument_id, "bond", 1, "gearing", 1, 1.5));

    auto fixing = make_schedule(s, instrument_id, "bond", 1, "fixing_schedule", 1, "dates");
    fixing.convention = "Following";
    instrument_schedule_repository().write(ctx, fixing);
    instrument_schedule_date_repository().write(
        ctx,
        make_schedule_date(s, instrument_id, "bond", 1, "fixing_schedule", 1, 1, "2024-04-15"));
    instrument_schedule_date_repository().write(
        ctx,
        make_schedule_date(s, instrument_id, "bond", 1, "fixing_schedule", 1, 2, "2024-10-15"));

    auto reset = make_schedule(s, instrument_id, "bond", 1, "reset_schedule", 1, "rules");
    reset.calendar = "TARGET";
    reset.convention = "ModifiedFollowing";
    reset.tenor = "6M";
    instrument_schedule_repository().write(ctx, reset);

    const auto id = boost::uuids::to_string(instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({id});

    REQUIRE(instruments.size() == 1);
    REQUIRE(instruments.at(id).bond_legs.size() == 1);
    const auto& rebuilt = instruments.at(id).bond_legs.front();
    REQUIRE(rebuilt.rate.has_value());
    REQUIRE(rebuilt.rate->floating.has_value());
    CHECK(!rebuilt.rate->fixed.has_value());

    const auto& floating = *rebuilt.rate->floating;
    CHECK(floating.index == "EURIBOR-6M");
    CHECK(floating.is_in_arrears == false);
    CHECK(floating.fixing_days == 2);
    CHECK(floating.observation_shift == true);
    CHECK(floating.spreads.size() == 2);
    CHECK(floating.spreads[0].value == 0.5);
    CHECK(floating.spreads[1].value == 0.75);
    CHECK(floating.caps.size() == 1);
    CHECK(floating.floors.size() == 1);
    CHECK(floating.gearings.size() == 1);

    REQUIRE(floating.front_stub_interpolation.has_value());
    CHECK(floating.front_stub_interpolation->short_index == "EURIBOR-3M");
    CHECK(floating.front_stub_interpolation->long_index == "EURIBOR-6M");
    CHECK(floating.front_stub_interpolation->rounding_precision == 5);
    CHECK(!floating.back_stub_interpolation.has_value());

    REQUIRE(floating.fixing_schedule.dates.size() == 1);
    CHECK(floating.fixing_schedule.dates[0].convention == "Following");
    CHECK(floating.fixing_schedule.dates[0].dates.size() == 2);
    CHECK(floating.fixing_schedule.dates[0].dates[0] == "2024-04-15");
    CHECK(floating.fixing_schedule.dates[0].dates[1] == "2024-10-15");

    REQUIRE(floating.reset_schedule.rules.size() == 1);
    CHECK(floating.reset_schedule.rules[0].tenor == "6M");
    CHECK(floating.reset_schedule.rules[0].calendar == "TARGET");
}

TEST_CASE("read_instruments_keeps_each_leg_in_its_own_role", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000007");
    bond_issue_repository().write(ctx, issue);
    const auto first_instrument = make_instrument(s, issue.issue_id, "Bond");
    const auto second_instrument = make_instrument(s, issue.issue_id, "BondTRS");
    bond_instrument_repository().write(ctx, first_instrument);
    bond_instrument_repository().write(ctx, second_instrument);

    const auto first_id_uuid = first_instrument.identity.instrument_id;
    const auto second_id_uuid = second_instrument.identity.instrument_id;

    // The second leg is written first, so the order read back is the
    // leg's own order rather than the insert order's.
    auto second_leg = make_leg(s, first_id_uuid, "bond", 2);
    second_leg.leg_type = "Floating";
    bond_leg_repository().write(ctx, second_leg);
    bond_leg_repository().write(ctx, make_leg(s, first_id_uuid, "bond", 1));

    auto funding = make_leg(s, second_id_uuid, "trs_funding", 1);
    funding.leg_type = "Floating";
    bond_leg_repository().write(ctx, funding);

    const auto first_id = boost::uuids::to_string(first_id_uuid);
    const auto second_id = boost::uuids::to_string(second_id_uuid);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({first_id, second_id});

    REQUIRE(instruments.size() == 2);

    // One instrument's legs must not reach the other's container.
    const auto& first = instruments.at(first_id);
    REQUIRE(first.bond_legs.size() == 2);
    CHECK(first.bond_legs[0].leg_type == "Fixed");
    CHECK(first.bond_legs[1].leg_type == "Floating");
    CHECK(!first.trs_funding_leg.leg_type.has_value());

    const auto& second = instruments.at(second_id);
    CHECK(second.bond_legs.empty());
    CHECK(second.trs_funding_leg.leg_type == "Floating");
    CHECK(!second.repo_leg.leg_type.has_value());
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
    auto fact = make_option(s, option.identity.instrument_id);
    fact.redemption = "Bullet";
    fact.price_type = "Price";
    fact.knocks_out = "Up";
    bond_option_repository().write(ctx, fact);

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

    // Three members of the option block sit beside it rather than inside
    // it, so they ride on the fact row and not the shared option table.
    CHECK(with_option.option_redemption == "Bullet");
    CHECK(with_option.option_price_type == "Price");
    CHECK(with_option.option_knocks_out == "Up");
}

TEST_CASE("read_instruments_rebuilds_the_option_block_and_its_children", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000010");
    bond_issue_repository().write(ctx, issue);
    const auto instr = make_instrument(s, issue.issue_id, "BondOption");
    bond_instrument_repository().write(ctx, instr);
    const auto instrument_id = instr.identity.instrument_id;

    auto block = make_instrument_option(s, instrument_id);
    block.option_type = "Call";
    block.style = "European";
    block.notice_period = "5D";
    block.automatic_exercise = "false";
    block.has_exercise_data = true;
    block.exercise_date = "2025-01-15";
    block.exercise_price = 101.0;
    block.has_payment_data = true;
    block.payment_lag = 2;
    block.payment_calendar = "TARGET";
    block.payment_convention = "Following";
    block.payment_relative_to = "Exercise";
    block.has_settlement_data = true;
    block.settlement_pay_currency = "EUR";
    block.settlement_fx_index = "EUR/USD";
    instrument_option_repository().write(ctx, block);

    const auto second = make_premium(s, instrument_id, 2);
    const auto first = make_premium(s, instrument_id, 1);
    instrument_option_premium_repository().write(ctx, second);
    instrument_option_premium_repository().write(ctx, first);

    auto fee = make_exercise_fee(s, instrument_id, 1);
    fee.type = "Percentage";
    fee.currency = "EUR";
    instrument_option_exercise_fee_repository().write(ctx, fee);

    instrument_option_payment_date_repository().write(
        ctx, make_payment_date(s, instrument_id, 1, "2025-01-20"));
    instrument_option_payment_date_repository().write(
        ctx, make_payment_date(s, instrument_id, 2, "2025-07-20"));

    auto dates = make_schedule(s, instrument_id, "option", 1, "exercise_dates", 1, "dates");
    instrument_schedule_repository().write(ctx, dates);
    instrument_schedule_date_repository().write(
        ctx, make_schedule_date(s, instrument_id, "option", 1, "exercise_dates", 1, 1, "2025-01-15"));
    instrument_schedule_date_repository().write(
        ctx, make_schedule_date(s, instrument_id, "option", 1, "exercise_dates", 1, 2, "2025-07-15"));

    auto schedule = make_schedule(s, instrument_id, "option", 1, "exercise_schedule", 1, "rules");
    schedule.calendar = "TARGET";
    schedule.tenor = "6M";
    instrument_schedule_repository().write(ctx, schedule);

    const auto id = boost::uuids::to_string(instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({id});

    REQUIRE(instruments.size() == 1);
    const auto& rebuilt = instruments.at(id);
    REQUIRE(rebuilt.option_data.has_value());

    const auto& option = *rebuilt.option_data;
    CHECK(option.long_short == "Long");
    CHECK(option.option_type == "Call");
    CHECK(option.style == "European");
    CHECK(option.notice_period == "5D");
    CHECK(option.automatic_exercise == "false");

    REQUIRE(option.exercise_data.has_value());
    CHECK(option.exercise_data->date == "2025-01-15");
    CHECK(option.exercise_data->price == 101.0);

    REQUIRE(option.payment_data.has_value());
    CHECK(option.payment_data->dates.size() == 2);
    CHECK(option.payment_data->dates[0] == "2025-01-20");
    CHECK(option.payment_data->dates[1] == "2025-07-20");
    REQUIRE(option.payment_data->rules.has_value());
    CHECK(option.payment_data->rules->lag == 2);
    CHECK(option.payment_data->rules->calendar == "TARGET");
    CHECK(option.payment_data->rules->convention == "Following");
    CHECK(option.payment_data->rules->relative_to == "Exercise");

    REQUIRE(option.settlement_data.has_value());
    CHECK(option.settlement_data->pay_currency == "EUR");
    CHECK(option.settlement_data->fx_index == "EUR/USD");

    // The premiums come back in ordinal order, not insert order.
    REQUIRE(option.premiums.size() == 2);
    CHECK(option.premiums[0].pay_date == "2024-01-20");
    CHECK(option.premiums[1].pay_date == "2024-01-20");

    REQUIRE(option.exercise_fees.size() == 1);
    CHECK(option.exercise_fees[0].amount == 25.0);
    CHECK(option.exercise_fees[0].type == "Percentage");

    // The exercise dates are a list on the container but a schedule block
    // in the tables, so the read flattens the one into the other.
    REQUIRE(rebuilt.option_exercise_dates.size() == 2);
    CHECK(rebuilt.option_exercise_dates[0] == "2025-01-15");
    CHECK(rebuilt.option_exercise_dates[1] == "2025-07-15");

    REQUIRE(rebuilt.option_exercise_schedule.has_value());
    REQUIRE(rebuilt.option_exercise_schedule->rules.size() == 1);
    CHECK(rebuilt.option_exercise_schedule->rules[0].tenor == "6M");
}

TEST_CASE("read_instruments_rebuilds_a_strike_a_forward_and_a_delivery_basket", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000011");
    bond_issue_repository().write(ctx, issue);
    const auto instr = make_instrument(s, issue.issue_id, "BondFuture");
    bond_instrument_repository().write(ctx, instr);
    const auto instrument_id = instr.identity.instrument_id;

    instrument_strike_repository().write(ctx, make_strike(s, instrument_id));

    auto forward = make_forward(s, instrument_id);
    forward.long_in_forward = "true";
    forward.forward_maturity_date = "2026-03-15";
    forward.settlement = "Cash";
    forward.amount = 250000.0;
    forward.lock_rate = 2.5;
    forward.dv01 = 12.75;
    forward.premium_amount = "1500";
    forward.premium_date = "2024-03-15";
    bond_forward_repository().write(ctx, forward);

    // Written out of order, so the read back proves the ordinal is the
    // order rather than the insert's.
    bond_future_delivery_basket_repository().write(
        ctx, make_delivery_basket(s, instrument_id, 2, "XS0000000013"));
    bond_future_delivery_basket_repository().write(
        ctx, make_delivery_basket(s, instrument_id, 1, "XS0000000012"));

    const auto id = boost::uuids::to_string(instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({id});

    REQUIRE(instruments.size() == 1);
    const auto& rebuilt = instruments.at(id);

    REQUIRE(rebuilt.strike_data.has_value());
    CHECK(rebuilt.strike_data->price_value == 101.5);
    CHECK(rebuilt.strike_data->price_currency == "EUR");

    REQUIRE(rebuilt.forward_long_in_forward.has_value());
    CHECK(*rebuilt.forward_long_in_forward == "true");
    REQUIRE(rebuilt.forward_settlement.has_value());
    CHECK(rebuilt.forward_settlement->forward_maturity_date == "2026-03-15");
    CHECK(rebuilt.forward_settlement->settlement == "Cash");
    CHECK(rebuilt.forward_settlement->amount == 250000.0);
    CHECK(rebuilt.forward_settlement->lock_rate == 2.5);
    CHECK(rebuilt.forward_settlement->dv01 == 12.75);
    CHECK(!rebuilt.forward_settlement->forward_settlement_date.has_value());

    REQUIRE(rebuilt.forward_premium.has_value());
    CHECK(rebuilt.forward_premium->amount == "1500");
    CHECK(rebuilt.forward_premium->date == "2024-03-15");

    REQUIRE(rebuilt.future_delivery_basket.size() == 2);
    CHECK(rebuilt.future_delivery_basket[0] == "XS0000000012");
    CHECK(rebuilt.future_delivery_basket[1] == "XS0000000013");
}

TEST_CASE("read_instruments_rebuilds_the_trs_return_side", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000014");
    bond_issue_repository().write(ctx, issue);
    const auto instr = make_instrument(s, issue.issue_id, "BondTRS");
    bond_instrument_repository().write(ctx, instr);
    const auto instrument_id = instr.identity.instrument_id;

    auto trs = make_trs(s, instrument_id);
    trs.payer = "true";
    trs.price_type = "Price";
    trs.initial_price = 99.25;
    bond_trs_repository().write(ctx, trs);

    auto schedule = make_schedule(s, instrument_id, "trs", 1, "schedule", 1, "rules");
    schedule.tenor = "3M";
    schedule.calendar = "TARGET";
    instrument_schedule_repository().write(ctx, schedule);

    const auto id = boost::uuids::to_string(instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({id});

    REQUIRE(instruments.size() == 1);
    const auto& rebuilt = instruments.at(id);
    REQUIRE(rebuilt.trs.has_value());
    CHECK(rebuilt.trs->return_type == "TotalReturn");

    // The three members ride on the fact row because no other row holds
    // them, and the schedule rides on the shared schedule tables under the
    // return side's own owner role.
    REQUIRE(rebuilt.trs_payer.has_value());
    CHECK(*rebuilt.trs_payer == "true");
    CHECK(rebuilt.trs_price_type == "Price");
    REQUIRE(rebuilt.trs_initial_price.has_value());
    CHECK(*rebuilt.trs_initial_price == 99.25);

    REQUIRE(rebuilt.trs_schedule.rules.size() == 1);
    CHECK(rebuilt.trs_schedule.rules[0].tenor == "3M");
}

TEST_CASE("read_instruments_rebuilds_the_option_block_of_a_type_with_no_option_row", tags) {
    database_helper h;
    const auto s = make_stamps(h);
    auto ctx = make_context(h, s);

    const auto issue = make_issue(s, "XS0000000015");
    bond_issue_repository().write(ctx, issue);
    const auto instr = make_instrument(s, issue.issue_id, "Ascot");
    bond_instrument_repository().write(ctx, instr);
    const auto instrument_id = instr.identity.instrument_id;

    auto block = make_instrument_option(s, instrument_id);
    block.option_type = "Put";
    instrument_option_repository().write(ctx, block);

    const auto id = boost::uuids::to_string(instrument_id);
    bond_instrument_reader reader(ctx);
    const auto instruments = reader.read_instruments({id});

    REQUIRE(instruments.size() == 1);

    // An ascot states the same option element a bond option does, so the
    // block reaches the shared table even though no fact row of its own
    // type exists to name it.
    const auto& rebuilt = instruments.at(id);
    REQUIRE(rebuilt.option_data.has_value());
    CHECK(rebuilt.option_data->option_type == "Put");
    CHECK(!rebuilt.option.has_value());
    CHECK(!rebuilt.trs.has_value());
}
