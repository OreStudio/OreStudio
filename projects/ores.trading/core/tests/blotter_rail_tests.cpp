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
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.refdata.api/generators/book_generator.hpp"
#include "ores.refdata.api/generators/currency_generator.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.api/generators/portfolio_generator.hpp"
#include "ores.refdata.core/repository/book_repository.hpp"
#include "ores.refdata.core/repository/currency_repository.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.trading.api/generators/trade_activity_generator.hpp"
#include "ores.trading.api/generators/trade_booking_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.api/generators/trade_state_generator.hpp"
#include "ores.trading.api/messaging/trade_summary_protocol.hpp"
#include "ores.trading.core/repository/trade_activity_repository.hpp"
#include "ores.trading.core/repository/trade_booking_repository.hpp"
#include "ores.trading.core/repository/trade_repository.hpp"
#include "ores.trading.core/repository/trade_state_repository.hpp"
#include "ores.trading.core/repository/trade_summary_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <catch2/catch_test_macros.hpp>
#include <algorithm>
#include <cstdint>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using namespace ores::trading;

namespace {

/**
 * @brief A party with a portfolio tree and two books under it.
 *
 * The tree is a root portfolio with one child portfolio below it: the child
 * holds one book and the root holds the other directly, so a rail that counts a
 * portfolio's subtree has something to add up.
 */
struct fixture final {
    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gen = ores::testing::make_generation_context(h);
    ores::database::context ctx = h.context();
    boost::uuids::uuid party_id{};
    boost::uuids::uuid root_portfolio_id{};
    boost::uuids::uuid child_portfolio_id{};
    boost::uuids::uuid root_book_id{};
    boost::uuids::uuid child_book_id{};

    fixture() {
        namespace gen_rd = ores::refdata::generators;
        namespace repo = ores::refdata::repository;

        auto maker = ores::iam::generators::generate_synthetic_account(gen);
        maker.account_type = "service";
        maker.change_reason_code = "system.test";
        ores::iam::repository::account_repository{}.write(h.context(), maker);

        auto party = gen_rd::generate_synthetic_party(gen);
        party.change_reason_code = "system.test";
        for (const auto& e : repo::party_repository().read_latest(h.context())) {
            if (e.tenant_id == party.tenant_id) {
                party.parent_party_id = e.id;
                break;
            }
        }
        repo::party_repository().write(h.context(), party);
        party_id = party.id;
        ctx = h.context().with_party(h.tenant_id(), party.id, {party.id}, maker.username);

        const auto currency = gen_rd::generate_synthetic_currency(gen);
        repo::currency_repository().write(ctx, {currency});

        auto root = gen_rd::generate_synthetic_portfolio(gen);
        root.party_id = party.id;
        root.change_reason_code = "system.test";
        repo::portfolio_repository().write(ctx, root);
        root_portfolio_id = root.id;

        auto child = gen_rd::generate_synthetic_portfolio(gen);
        child.party_id = party.id;
        child.parent_portfolio_id = root_portfolio_id;
        child.change_reason_code = "system.test";
        repo::portfolio_repository().write(ctx, child);
        child_portfolio_id = child.id;

        auto root_book = gen_rd::generate_synthetic_book(gen);
        root_book.party_id = party.id;
        root_book.parent_portfolio_id = root_portfolio_id;
        root_book.functional_currency = currency.iso_code;
        root_book.change_reason_code = "system.test";
        repo::book_repository().write(ctx, root_book);
        root_book_id = root_book.id;

        auto child_book = gen_rd::generate_synthetic_book(gen);
        child_book.party_id = party.id;
        child_book.parent_portfolio_id = child_portfolio_id;
        child_book.functional_currency = currency.iso_code;
        child_book.change_reason_code = "system.test";
        repo::book_repository().write(ctx, child_book);
        child_book_id = child_book.id;
    }

    /** Books @p count trades into @p book, each with a live state. */
    void book(std::size_t count, const boost::uuids::uuid& book_id) {
        for (std::size_t i = 0; i < count; ++i) {
            auto trade = generators::generate_synthetic_trade(gen);
            trade.party_id = party_id;
            trade.change_reason_code = "system.test";
            repository::trade_repository{}.write(ctx, trade);

            /*
             * A booking names the activity that booked it, so the activity is
             * written first: the store refuses a booking whose activity it does
             * not hold.
             */
            auto activity = generators::generate_synthetic_trade_activity(gen);
            activity.party_id = party_id;
            activity.change_reason_code = "system.test";
            repository::trade_activity_repository{}.write(ctx, activity);

            auto booking = generators::generate_synthetic_trade_booking(gen);
            booking.trade_id = trade.id;
            booking.trade_activity_id = activity.id;
            booking.book_id = book_id;
            booking.trade_date = std::chrono::year_month_day{
                std::chrono::year{2026}, std::chrono::October, std::chrono::day{1}};
            booking.change_reason_code = "system.test";
            repository::trade_booking_repository{}.write(ctx, booking);

            auto state = generators::generate_synthetic_trade_state(gen);
            state.trade_id = trade.id;
            state.trade_activity_id = activity.id;
            state.change_reason_code = "system.test";
            repository::trade_state_repository{}.write(ctx, state);
        }
    }
};

/** The count a rail states for one node, or nothing when it states none. */
std::optional<std::uint64_t>
count_of(const std::vector<std::pair<std::string, std::uint64_t>>& counts,
         const std::string& node_id) {
    for (const auto& [id, count] : counts) {
        if (id == node_id)
            return count;
    }
    return std::nullopt;
}

}

TEST_CASE("a_blotter_rail_counts_the_rows_on_each_book", "[blotter][rail]") {
    fixture f;
    f.book(3, f.root_book_id);
    f.book(2, f.child_book_id);

    repository::trade_summary_repository summaries;
    const auto counts = summaries.count_blotter_rail(f.ctx, "book", std::nullopt);

    CHECK(count_of(counts, boost::uuids::to_string(f.root_book_id)) == 3);
    CHECK(count_of(counts, boost::uuids::to_string(f.child_book_id)) == 2);
}

TEST_CASE("a_blotter_portfolio_counts_the_rows_below_it_as_well_as_its_own", "[blotter][rail]") {
    fixture f;
    f.book(3, f.root_book_id);
    f.book(2, f.child_book_id);

    repository::trade_summary_repository summaries;
    const auto counts = summaries.count_blotter_rail(f.ctx, "portfolio", std::nullopt);

    /*
     * The child holds only its own book, and the root holds its own book and
     * everything below the child: a person reading the tree takes a portfolio's
     * figure to mean the subtree, not the rows placed on it.
     */
    CHECK(count_of(counts, boost::uuids::to_string(f.child_portfolio_id)) == 2);
    CHECK(count_of(counts, boost::uuids::to_string(f.root_portfolio_id)) == 5);
}

TEST_CASE("a_blotter_rail_honours_the_filter_the_page_honours", "[blotter][rail]") {
    fixture f;
    f.book(3, f.root_book_id);
    f.book(2, f.child_book_id);

    messaging::trade_summaries_filter filter;
    filter.book_id = f.child_book_id;

    repository::trade_summary_repository summaries;
    const auto counts = summaries.count_blotter_rail(f.ctx, "book", filter);

    CHECK(count_of(counts, boost::uuids::to_string(f.child_book_id)) == 2);
    CHECK(count_of(counts, boost::uuids::to_string(f.root_book_id)) == std::nullopt);
}

TEST_CASE("a_blotter_rail_refuses_a_dimension_it_does_not_have", "[blotter][rail]") {
    fixture f;

    repository::trade_summary_repository summaries;
    CHECK_THROWS_AS(summaries.count_blotter_rail(f.ctx, "colour", std::nullopt),
                    std::invalid_argument);
}

TEST_CASE("a_blotter_page_states_the_rows_it_was_asked_for_then_the_rest", "[blotter][page]") {
    fixture f;
    f.book(5, f.root_book_id);

    repository::trade_summary_repository summaries;

    std::vector<std::string> seen;
    std::string after;
    for (int page = 0; page < 3; ++page) {
        const auto rows = summaries.read_blotter_page(f.ctx, 2, {}, after, std::nullopt);
        for (const auto& row : rows)
            seen.push_back(boost::uuids::to_string(row.id));
        if (!rows.empty())
            after = repository::trade_summary_repository::blotter_page_key(rows.back(), "");
    }

    /*
     * Five rows in pages of two: two, two, one. Every row is stated once and
     * none is stated twice, which is what a key is for: an offset would repeat
     * or skip a row when the order places another row before it between reads.
     */
    CHECK(seen.size() == 5);
    const auto distinct = std::set<std::string>(seen.begin(), seen.end());
    CHECK(distinct.size() == 5);
}

TEST_CASE("a_blotter_page_starts_after_the_key_it_is_given", "[blotter][page]") {
    fixture f;
    f.book(4, f.root_book_id);

    repository::trade_summary_repository summaries;
    const auto first = summaries.read_blotter_page(f.ctx, 2, {}, std::string{}, std::nullopt);
    REQUIRE(first.size() == 2);

    const auto key = repository::trade_summary_repository::blotter_page_key(first.back(), "");
    const auto second = summaries.read_blotter_page(f.ctx, 2, {}, key, std::nullopt);

    REQUIRE(second.size() == 2);
    for (const auto& row : second) {
        CHECK(row.id != first[0].id);
        CHECK(row.id != first[1].id);
    }
}
