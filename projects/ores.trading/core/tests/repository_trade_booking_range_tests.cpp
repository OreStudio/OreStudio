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
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.api/domain/trade_booking.hpp"
#include "ores.trading.api/generators/trade_activity_generator.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.api/messaging/trade_booking_protocol.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "trade_parent_seed.hpp"
#include <boost/uuid/uuid.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

/*
 * The booking declares its trade date and its execution time filterable, so a
 * list of bookings narrows by a period. Each case writes bookings for trades
 * of a party no other test uses, so a range answers only them.
 */

namespace {

const std::string tags("[repository][list_filter][range]");

using ores::trading::messaging::trade_bookings_filter;
using ores::trading::repository::trade_booking_repository;
using uuid = boost::uuids::uuid;
using namespace std::chrono;

struct fixture {
    ores::testing::database_helper h;
    ores::database::context ctx = ores::trading::tests::write_parent_party(h);
    uuid book = write_book();

    uuid write_book() {
        namespace gen = ores::refdata::generators;
        namespace repo = ores::refdata::repository;
        auto g = ores::testing::make_generation_context(h);

        auto currency = gen::generate_synthetic_currency(g);
        repo::currency_repository().write(ctx, {currency});

        auto portfolio = gen::generate_synthetic_portfolio(g);
        portfolio.party_id = *ctx.party_id();
        portfolio.change_reason_code = "system.test";
        repo::portfolio_repository().write(ctx, portfolio);

        auto b = gen::generate_synthetic_book(g);
        b.party_id = *ctx.party_id();
        b.parent_portfolio_id = portfolio.id;
        b.functional_currency = currency.iso_code;
        b.change_reason_code = "system.test";
        repo::book_repository().write(ctx, b);
        return b.id;
    }

    uuid write_booking(std::optional<year_month_day> date,
                       std::optional<system_clock::time_point> executed) {
        auto g = ores::testing::make_generation_context(h);

        auto trade = ores::trading::generators::generate_synthetic_trade(g);
        trade.party_id = *ctx.party_id();
        ores::trading::repository::trade_repository().write(ctx, trade);

        auto activity = ores::trading::generators::generate_synthetic_trade_activity(g);
        activity.party_id = *ctx.party_id();
        ores::trading::repository::trade_activity_repository().write(ctx, activity);

        auto booking = ores::trading::generators::generate_synthetic_trade_booking(g);
        booking.trade_id = trade.id;
        booking.trade_activity_id = activity.id;
        booking.book_id = book;
        booking.netting_set_id = std::nullopt;
        booking.counterparty_identifier_id = std::nullopt;
        booking.netting_set_identifier_id = std::nullopt;
        booking.trade_date = date;
        booking.execution_timestamp = executed;
        trade_booking_repository().write(ctx, booking);
        return trade.id;
    }

    std::vector<uuid> read(const trade_bookings_filter& f) {
        std::vector<uuid> r;
        for (const auto& b : trade_booking_repository().read_latest(ctx, 0, 1000, {}, f))
            r.push_back(b.trade_id);
        std::ranges::sort(r);
        return r;
    }
};

std::vector<uuid> sorted(std::vector<uuid> v) {
    std::ranges::sort(v);
    return v;
}

year_month_day ymd(int y, unsigned m, unsigned d) {
    return year{y} / month{m} / d;
}

system_clock::time_point at(int y, unsigned m, unsigned d, int hour, int minute = 0) {
    return sys_days{ymd(y, m, d)} + hours{hour} + minutes{minute};
}

}

TEST_CASE("a date range holds the bookings whose trade date lies in it, bounds included", tags) {
    fixture f;
    const auto first = f.write_booking(ymd(2026, 3, 1), std::nullopt);
    const auto middle = f.write_booking(ymd(2026, 3, 15), std::nullopt);
    const auto last = f.write_booking(ymd(2026, 3, 31), std::nullopt);

    trade_bookings_filter range;
    range.trade_date_from = "2026-03-15";
    range.trade_date_to = "2026-03-31";
    CHECK(f.read(range) == sorted({middle, last}));

    range = {};
    range.trade_date_to = "2026-03-01";
    CHECK(f.read(range) == std::vector<uuid>{first});

    range = {};
    range.trade_date_from = "2026-03-02";
    CHECK(f.read(range) == sorted({middle, last}));
}

TEST_CASE("a range excludes a booking that has no trade date", tags) {
    fixture f;
    const auto dated = f.write_booking(ymd(2026, 3, 10), std::nullopt);
    f.write_booking(std::nullopt, std::nullopt);

    trade_bookings_filter range;
    range.trade_date_from = "2000-01-01";
    CHECK(f.read(range) == std::vector<uuid>{dated});
}

TEST_CASE("a date as the upper bound of a timestamp range holds the whole of that day", tags) {
    fixture f;
    const auto morning = f.write_booking(std::nullopt, at(2026, 3, 5, 10));
    const auto late = f.write_booking(std::nullopt, at(2026, 3, 5, 23, 59));
    const auto next = f.write_booking(std::nullopt, at(2026, 3, 6, 0, 30));

    trade_bookings_filter range;
    range.execution_timestamp_to = "2026-03-05";
    CHECK(f.read(range) == sorted({morning, late}));

    range = {};
    range.execution_timestamp_from = "2026-03-06";
    CHECK(f.read(range) == std::vector<uuid>{next});

    range = {};
    range.execution_timestamp_from = "2026-03-05";
    range.execution_timestamp_to = "2026-03-05";
    CHECK(f.read(range) == sorted({morning, late}));
}

TEST_CASE("a UTC timestamp bound is the instant itself, included", tags) {
    fixture f;
    const auto early = f.write_booking(std::nullopt, at(2026, 3, 5, 9));
    const auto exact = f.write_booking(std::nullopt, at(2026, 3, 5, 10));
    const auto after = f.write_booking(std::nullopt, at(2026, 3, 5, 11));

    trade_bookings_filter range;
    range.execution_timestamp_from = "2026-03-05T10:00:00Z";
    CHECK(f.read(range) == sorted({exact, after}));

    range = {};
    range.execution_timestamp_to = "2026-03-05T10:00:00Z";
    CHECK(f.read(range) == sorted({early, exact}));
}

TEST_CASE("a range combines with another member by AND", tags) {
    fixture f;
    const auto in = f.write_booking(ymd(2026, 3, 10), std::nullopt);
    const auto out = f.write_booking(ymd(2026, 4, 10), std::nullopt);

    trade_bookings_filter range;
    range.trade_date_from = "2026-03-01";
    range.trade_date_to = "2026-04-30";
    range.trade_id_one_of = std::vector<uuid>{in};
    CHECK(f.read(range) == std::vector<uuid>{in});
    static_cast<void>(out);
}

TEST_CASE("a bound the repository cannot read is refused", tags) {
    fixture f;
    f.write_booking(ymd(2026, 3, 10), std::nullopt);

    trade_bookings_filter range;
    range.trade_date_from = "next week";
    CHECK_THROWS_AS(f.read(range), std::invalid_argument);

    range = {};
    range.trade_date_to = "2026-02-30";
    CHECK_THROWS_AS(f.read(range), std::invalid_argument);
}
