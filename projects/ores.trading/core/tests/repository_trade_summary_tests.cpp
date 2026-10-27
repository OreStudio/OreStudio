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
#include "ores.refdata.api/generators/counterparty_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/counterparty_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.trading.api/domain/booking_nature.hpp"
#include "ores.trading.api/generators/structure_generator.hpp"
#include "ores.trading.api/generators/structure_member_generator.hpp"
#include "ores.trading.api/generators/trade_activity_generator.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.api/generators/trade_identifier_generator.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.trading.core/repository/structure_member_repository.hpp"
#include "ores.trading.core/repository/structure_repository.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_identifier_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "test_helpers.hpp"
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
 * The trade summary is a view over the trade and its current parts. Each case
 * writes trades against a counterparty no other test uses, so a filter on it
 * answers only them.
 */

namespace {

using ores::trading::messaging::trade_summaries_filter;
using ores::trading::repository::trade_summary_repository;
using uuid = boost::uuids::uuid;
using namespace std::chrono;

struct repository_trade_summary_fixture {
    ores::testing::database_helper h;
    ores::database::context ctx = ores::trading::tests::write_parent_party(h);
    uuid counterparty = write_counterparty();
    uuid other_counterparty = write_counterparty();
    uuid book = write_book();

    uuid write_counterparty() {
        auto gen = ores::testing::make_generation_context(h);
        auto c = ores::refdata::generators::generate_synthetic_counterparty(gen);
        c.change_reason_code = "system.test";
        ores::refdata::repository::counterparty_repository().write(ctx, c);
        return c.id;
    }

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

    uuid write_activity() {
        auto gen = ores::testing::make_generation_context(h);
        auto activity = ores::trading::generators::generate_synthetic_trade_activity(gen);
        activity.party_id = *ctx.party_id();
        ores::trading::repository::trade_activity_repository().write(ctx, activity);
        return activity.id;
    }

    uuid write(const std::string& type,
               const uuid& counterparty_id,
               ores::trading::domain::booking_nature nature =
                   ores::trading::domain::booking_nature::actual) {
        auto gen = ores::testing::make_generation_context(h);
        auto t = ores::trading::generators::generate_synthetic_trade(gen);
        t.party_id = *ctx.party_id();
        t.counterparty_id = counterparty_id;
        t.counterparty_scope = ores::trading::domain::counterparty_scope::external;
        t.trade_type = type;
        t.booking_nature = nature;
        ores::trading::repository::trade_repository().write(ctx, t);
        return t.id;
    }

    void write_booking(const uuid& trade_id, const std::optional<year_month_day>& date) {
        auto gen = ores::testing::make_generation_context(h);
        auto booking = ores::trading::generators::generate_synthetic_trade_booking(gen);
        booking.trade_id = trade_id;
        booking.trade_activity_id = write_activity();
        booking.book_id = book;
        booking.netting_set_id = std::nullopt;
        booking.counterparty_identifier_id = std::nullopt;
        booking.netting_set_identifier_id = std::nullopt;
        booking.trade_date = date;
        ores::trading::repository::trade_booking_repository().write(ctx, booking);
    }

    void write_identifier(const uuid& trade_id, const std::string& type, const std::string& value) {
        auto gen = ores::testing::make_generation_context(h);
        auto id = ores::trading::generators::generate_synthetic_trade_identifier(gen);
        id.trade_id = trade_id;
        id.trade_activity_id = write_activity();
        id.id_type = type;
        id.id_value = value;
        ores::trading::repository::trade_identifier_repository().write(ctx, id);
    }

    uuid write_package(const std::vector<uuid>& members) {
        auto gen = ores::testing::make_generation_context(h);
        auto st = ores::trading::generators::generate_synthetic_structure(gen);
        st.party_id = *ctx.party_id();
        st.counterparty_id = counterparty;
        st.kind = "Package";
        st.template_code = "";
        st.parent_structure_id = boost::uuids::uuid{};
        ores::trading::repository::structure_repository().write(ctx, st);
        int sequence = 1;
        for (const auto& trade_id : members) {
            auto m = ores::trading::generators::generate_synthetic_structure_member(gen);
            m.trade_id = trade_id;
            m.structure_id = st.id;
            m.role = "leg";
            m.sequence_number = sequence++;
            ores::trading::repository::structure_member_repository().write(ctx, m);
        }
        return st.id;
    }

    std::vector<ores::trading::domain::trade_summary>
    read(const trade_summaries_filter& f, const ores::utility::domain::order& order = {}) {
        return trade_summary_repository().read_latest(ctx, 0, 1000, order, f);
    }
};

trade_summaries_filter repository_trade_summary_on(const uuid& counterparty) {
    return {.counterparty_id = std::optional<uuid>{counterparty}};
}

std::vector<uuid>
repository_trade_summary_ids(const std::vector<ores::trading::domain::trade_summary>& rows) {
    std::vector<uuid> r;
    for (const auto& s : rows)
        r.push_back(s.id);
    std::ranges::sort(r);
    return r;
}

}

TEST_CASE("a counterparty narrows the trade summary, and the total follows",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("FxForward", f.counterparty);
    f.write("Swap", f.other_counterparty);

    CHECK(repository_trade_summary_ids(f.read(repository_trade_summary_on(f.counterparty))) ==
          sorted({a, b}));
    CHECK(trade_summary_repository().get_total_trade_summary_count(
              f.ctx, repository_trade_summary_on(f.counterparty)) == 2);
}

TEST_CASE("a row of a trade names its kind and its instrument type",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto a = f.write("Swap", f.counterparty);

    const auto rows = f.read(repository_trade_summary_on(f.counterparty));
    REQUIRE(rows.size() == 1);
    CHECK(rows[0].id == a);
    CHECK(rows[0].row_kind == "trade");
    CHECK(rows[0].instrument_type == "Swap");
    CHECK(rows[0].trade_count == 1);
}

TEST_CASE("an instrument type narrows the summary", "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto a = f.write("Swap", f.counterparty);
    f.write("FxForward", f.counterparty);

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.instrument_type = "Swap";
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{a});

    filter.instrument_type_one_of = std::vector<std::string>{};
    CHECK(f.read(filter).empty());
}

TEST_CASE("a book and a trade date narrow the summary through the booking",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.counterparty);
    f.write_booking(a, year_month_day{year{2025} / 1 / 15});
    f.write_booking(b, year_month_day{year{2025} / 3 / 20});

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.book_id = std::optional<uuid>{f.book};
    CHECK(repository_trade_summary_ids(f.read(filter)) == sorted({a, b}));

    filter.trade_date_from = "2025-03-01";
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{b});

    filter.trade_date_from = std::nullopt;
    filter.trade_date_to = "2025-02-01";
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{a});
}

TEST_CASE("a trade with no booking has no book", "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    f.write("Swap", f.counterparty);

    const auto rows = f.read(repository_trade_summary_on(f.counterparty));
    REQUIRE(rows.size() == 1);
    CHECK_FALSE(rows[0].book_id.has_value());
}

TEST_CASE("the authorisation status of an actual trade with no request is not requested",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto actual = f.write("Swap", f.counterparty);
    const auto test = f.write("Swap", f.counterparty, ores::trading::domain::booking_nature::test);

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.authorisation_status = "not_requested";
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{actual});

    filter.authorisation_status = "not_required";
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{test});
}

TEST_CASE("a search matches an identifier of the trade, ignoring case, and literally",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.counterparty);
    f.write_identifier(b, "UTI", "ZX-Needle-77");

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.search = "zx-needle";
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{b});

    filter.search = "%";
    CHECK(f.read(filter).empty());

    filter.search = "";
    CHECK(f.read(filter).size() == 2);
}

TEST_CASE("a search matches the id of the trade", "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.counterparty);

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.search = boost::uuids::to_string(b);
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{b});
}

TEST_CASE("the summary pages in the service", "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    f.write("Swap", f.counterparty);
    f.write("FxForward", f.counterparty);
    f.write("CapFloor", f.counterparty);

    const auto page = trade_summary_repository().read_latest(
        f.ctx, 1, 1, {.field = "instrument_type"}, repository_trade_summary_on(f.counterparty));
    REQUIRE(page.size() == 1);
    CHECK(page[0].instrument_type == "FxForward");
}

TEST_CASE("only the listed columns sort the summary", "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto swap = f.write("Swap", f.counterparty);
    const auto forward = f.write("FxForward", f.counterparty);

    const auto up =
        f.read(repository_trade_summary_on(f.counterparty), {.field = "instrument_type"});
    REQUIRE(up.size() == 2);
    CHECK(up[0].id == forward);
    CHECK(up[1].id == swap);

    CHECK_THROWS_AS(f.read(repository_trade_summary_on(f.counterparty), {.field = "search_text"}),
                    std::invalid_argument);
}

TEST_CASE("a structure is a row of the same list, with the count of its trades",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.counterparty);
    const auto package = f.write_package({a, b});

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.row_kind = "structure";
    const auto rows = f.read(filter);

    REQUIRE(rows.size() == 1);
    CHECK(rows[0].id == package);
    CHECK(rows[0].instrument_type == "Package");
    CHECK(rows[0].trade_count == 2);
    CHECK_FALSE(rows[0].status.has_value());
    CHECK_FALSE(rows[0].book_id.has_value());

    CHECK(repository_trade_summary_ids(f.read(repository_trade_summary_on(f.counterparty))) ==
          sorted({a, b, package}));
}

TEST_CASE("a search finds a structure through an identifier of one of its trades",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto a = f.write("Swap", f.counterparty);
    const auto b = f.write("Swap", f.counterparty);
    f.write_identifier(a, "UTI", "QQ-Member-9");
    const auto package = f.write_package({a, b});

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.search = "qq-member";
    CHECK(repository_trade_summary_ids(f.read(filter)) == sorted({a, package}));
}

TEST_CASE("a filter on a column only a trade has leaves the structure out",
          "[repository][list_filter][trade_summary]") {
    repository_trade_summary_fixture f;
    const auto a = f.write("Swap", f.counterparty);
    f.write_booking(a, year_month_day{year{2025} / 1 / 15});
    f.write_package({a});

    auto filter = repository_trade_summary_on(f.counterparty);
    filter.book_id = std::optional<uuid>{f.book};
    CHECK(repository_trade_summary_ids(f.read(filter)) == std::vector<uuid>{a});
}
