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
#include "ores.trading.api/generators/activity_type_generator.hpp"
#include "ores.trading.api/generators/fpml_event_type_generator.hpp"
#include "ores.trading.api/generators/lifecycle_event_generator.hpp"
#include "ores.trading.api/generators/party_role_type_generator.hpp"
#include "ores.trading.api/generators/trade_generator.hpp"
#include "ores.trading.api/generators/trade_id_type_generator.hpp"
#include "ores.trading.api/generators/trade_identifier_generator.hpp"
#include "ores.trading.api/generators/trade_party_role_generator.hpp"
#include "ores.trading.api/generators/trade_type_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/generation/generation_context.hpp"

namespace {

const std::string_view test_suite("ores.trading.tests");
const std::string tags("[generators]");

}

using namespace ores::trading::generator;
using namespace ores::logging;
using ores::utility::generation::generation_context;

// --- trade_type ---

TEST_CASE("trade_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_trade_type(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated trade_type code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("trade_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_trade_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- activity_type ---

TEST_CASE("activity_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_activity_type(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated activity_type code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("activity_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_activity_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- fpml_event_type ---

TEST_CASE("fpml_event_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_fpml_event_type(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated fpml_event_type code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("fpml_event_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_fpml_event_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- lifecycle_event ---

TEST_CASE("lifecycle_event_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_lifecycle_event(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated lifecycle_event code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("lifecycle_event_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_lifecycle_events(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- party_role_type ---

TEST_CASE("party_role_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_party_role_type(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated party_role_type code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("party_role_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_party_role_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- trade_id_type ---

TEST_CASE("trade_id_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_trade_id_type(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated trade_id_type code: " << sut.code;

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("trade_id_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_trade_id_types(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(item.version == 1);
    }
}

// --- trade ---

TEST_CASE("trade_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_trade(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated trade id: " << sut.id;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("trade_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_trades(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(item.version == 1);
    }
}

// --- trade_identifier ---

TEST_CASE("trade_identifier_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_trade_identifier(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated trade_identifier id: " << sut.id;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.id_value.empty());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("trade_identifier_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_trade_identifiers(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(item.version == 1);
    }
}

// --- trade_party_role ---

TEST_CASE("trade_party_role_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    auto sut = generate_synthetic_trade_party_role(ctx);

    BOOST_LOG_SEV(lg, info) << "Generated trade_party_role id: " << sut.id;

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.modified_by.empty());
    CHECK(sut.change_reason_code == "system.test");
}

TEST_CASE("trade_party_role_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    generation_context ctx;
    const std::size_t count = 5;
    auto items = generate_synthetic_trade_party_roles(count, ctx);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(item.version == 1);
    }
}
