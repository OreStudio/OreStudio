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
#include "ores.trading.api/domain/trade.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.trading.api/domain/trade_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.api/domain/trade_table.hpp"
#include "ores.trading.api/domain/trade_table_io.hpp" // IWYU pragma: keep.

namespace {

using ores::trading::domain::trade;

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[domain]");

trade make_trade(const std::string& trade_type = "Swap") {
    trade t;
    t.identity.get().version = 1;
    t.identity.get().id = boost::uuids::random_generator()();
    t.identity.get().party_id = boost::uuids::random_generator()();
    t.parties.get().book_id = boost::uuids::random_generator()();
    t.parties.get().portfolio_id = boost::uuids::random_generator()();
    t.classification.get().trade_type = trade_type;
    t.classification.get().activity_type_code = "new_booking";
    t.classification.get().status_id = boost::uuids::random_generator()();
    t.lifecycle.get().trade_date = "2026-01-15";
    t.lifecycle.get().effective_date = "2026-01-20";
    t.lifecycle.get().termination_date = "2031-01-20";
    t.audit.get().modified_by = "system";
    t.audit.get().performed_by = "system";
    t.audit.get().change_reason_code = "system.new";
    t.audit.get().change_commentary = "Test data";
    t.audit.get().recorded_at = std::chrono::system_clock::now();
    return t;
}

}

using ores::trading::domain::trade;
using namespace ores::logging;

TEST_CASE("create_trade_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    trade sut;
    sut.identity.get().version = 1;
    sut.identity.get().id = boost::uuids::random_generator()();
    sut.identity.get().party_id = boost::uuids::random_generator()();
    sut.identity.get().external_id = "EXT-001";
    sut.parties.get().book_id = boost::uuids::random_generator()();
    sut.parties.get().portfolio_id = boost::uuids::random_generator()();
    sut.classification.get().trade_type = "Swap";
    sut.classification.get().netting_set_id = "NS-001";
    sut.classification.get().activity_type_code = "new_booking";
    sut.classification.get().status_id = boost::uuids::random_generator()();
    sut.lifecycle.get().trade_date = "2026-01-15";
    sut.lifecycle.get().execution_timestamp = "2026-01-15 09:30:00+00";
    sut.lifecycle.get().effective_date = "2026-01-20";
    sut.lifecycle.get().termination_date = "2031-01-20";
    sut.audit.get().modified_by = "admin";
    sut.audit.get().performed_by = "admin";
    sut.audit.get().change_reason_code = "system.new";
    sut.audit.get().change_commentary = "New trade booking";
    sut.audit.get().recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade: " << sut;

    CHECK(sut.identity.get().version == 1);
    CHECK(!sut.identity.get().id.is_nil());
    CHECK(!sut.identity.get().party_id.is_nil());
    CHECK(sut.classification.get().trade_type == "Swap");
    CHECK(sut.lifecycle.get().trade_date == "2026-01-15");
    CHECK(sut.audit.get().modified_by == "admin");
    CHECK(!sut.parties.get().successor_trade_id.has_value());
    CHECK(!sut.parties.get().counterparty_id.has_value());
}

TEST_CASE("create_trade_with_optional_fields", tags) {
    auto lg(make_logger(test_suite));

    trade sut;
    sut.identity.get().version = 2;
    sut.identity.get().id = boost::uuids::random_generator()();
    sut.identity.get().party_id = boost::uuids::random_generator()();
    sut.parties.get().book_id = boost::uuids::random_generator()();
    sut.parties.get().portfolio_id = boost::uuids::random_generator()();
    sut.parties.get().successor_trade_id = boost::uuids::random_generator()();
    sut.parties.get().counterparty_id = boost::uuids::random_generator()();
    sut.classification.get().trade_type = "FxForward";
    sut.classification.get().activity_type_code = "novation";
    sut.classification.get().status_id = boost::uuids::random_generator()();
    sut.lifecycle.get().trade_date = "2026-02-01";
    sut.lifecycle.get().effective_date = "2026-02-05";
    sut.lifecycle.get().termination_date = "2026-08-05";
    sut.audit.get().modified_by = "system";
    sut.audit.get().performed_by = "system";
    sut.audit.get().change_reason_code = "system.novation";
    sut.audit.get().change_commentary = "Novation";
    sut.audit.get().recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade with optionals: " << sut;

    CHECK(sut.parties.get().successor_trade_id.has_value());
    CHECK(!sut.parties.get().successor_trade_id->is_nil());
    CHECK(sut.parties.get().counterparty_id.has_value());
    CHECK(!sut.parties.get().counterparty_id->is_nil());
}

TEST_CASE("trade_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    trade sut;
    sut.identity.get().version = 1;
    sut.identity.get().id = boost::uuids::random_generator()();
    sut.identity.get().party_id = boost::uuids::random_generator()();
    sut.parties.get().book_id = boost::uuids::random_generator()();
    sut.parties.get().portfolio_id = boost::uuids::random_generator()();
    sut.classification.get().trade_type = "Swaption";
    sut.classification.get().activity_type_code = "new_booking";
    sut.classification.get().status_id = boost::uuids::random_generator()();
    sut.lifecycle.get().trade_date = "2026-03-01";
    sut.lifecycle.get().effective_date = "2026-03-05";
    sut.lifecycle.get().termination_date = "2031-03-05";
    sut.audit.get().modified_by = "system";
    sut.audit.get().performed_by = "system";
    sut.audit.get().change_reason_code = "system.new";
    sut.audit.get().change_commentary = "Test";
    sut.audit.get().recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade: " << sut;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    CHECK(!json_output.empty());
    CHECK(json_output.find("Swaption") != std::string::npos);
}

TEST_CASE("create_trade_with_faker", tags) {
    auto lg(make_logger(test_suite));

    trade sut;
    sut.identity.get().version = faker::number::integer(1, 10);
    sut.identity.get().id = boost::uuids::random_generator()();
    sut.identity.get().party_id = boost::uuids::random_generator()();
    sut.parties.get().book_id = boost::uuids::random_generator()();
    sut.parties.get().portfolio_id = boost::uuids::random_generator()();
    sut.classification.get().trade_type = std::string(faker::word::noun()) + "_trade";
    sut.classification.get().activity_type_code = "new_booking";
    sut.classification.get().status_id = boost::uuids::random_generator()();
    sut.lifecycle.get().trade_date = "2026-01-01";
    sut.lifecycle.get().effective_date = "2026-01-05";
    sut.lifecycle.get().termination_date = "2031-01-05";
    sut.audit.get().modified_by = std::string(faker::internet::username());
    sut.audit.get().performed_by = std::string(faker::internet::username());
    sut.audit.get().change_reason_code = "system.new";
    sut.audit.get().change_commentary = "Synthetic test data";
    sut.audit.get().recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Trade: " << sut;

    CHECK(sut.identity.get().version >= 1);
    CHECK(!sut.identity.get().id.is_nil());
    CHECK(!sut.audit.get().modified_by.empty());
    CHECK(sut.audit.get().change_reason_code == "system.new");
}

TEST_CASE("create_multiple_random_trades", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> trade_types = {"Swap", "FxForward", "CapFloor"};
    for (const auto& tt : trade_types) {
        auto sut = make_trade(tt);
        BOOST_LOG_SEV(lg, info) << "Trade: " << sut;
        CHECK(!sut.identity.get().id.is_nil());
        CHECK(sut.identity.get().version == 1);
    }
}

TEST_CASE("trade_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade> items = {make_trade("Swap")};
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("trade_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade> items;
    for (int i = 0; i < 3; ++i)
        items.push_back(make_trade("Type" + std::to_string(i)));

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
}

TEST_CASE("trade_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade> items;
    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("trade_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<trade> items;
    for (int i = 0; i < 5; ++i) {
        trade t;
        t.identity.get().version = 1;
        t.identity.get().id = boost::uuids::random_generator()();
        t.identity.get().party_id = boost::uuids::random_generator()();
        t.parties.get().book_id = boost::uuids::random_generator()();
        t.parties.get().portfolio_id = boost::uuids::random_generator()();
        t.classification.get().trade_type = std::string(faker::word::noun()) + "_" + std::to_string(i);
        t.classification.get().activity_type_code = "new_booking";
        t.classification.get().status_id = boost::uuids::random_generator()();
        t.lifecycle.get().trade_date = "2026-01-01";
        t.lifecycle.get().effective_date = "2026-01-05";
        t.lifecycle.get().termination_date = "2031-01-05";
        t.audit.get().modified_by = "system";
        t.audit.get().performed_by = "system";
        t.audit.get().change_reason_code = "system.new";
        t.audit.get().change_commentary = "Test";
        t.audit.get().recorded_at = std::chrono::system_clock::now();
        items.push_back(t);
    }

    auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
}
