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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol_codec_roundtrip_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.trading.api/domain/bond_issue_call_date_codec.hpp"
#include "ores.trading.api/domain/bond_issue_codec.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amortization_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_amount_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_rate_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_codec.hpp"
#include "ores.trading.api/domain/bond_issue_leg_schedule_date_codec.hpp"
#include "ores.trading.api/domain/instrument_batch.hpp"
#include "ores.trading.api/domain/structure_codec.hpp"
#include "ores.trading.api/domain/structure_member_codec.hpp"
#include "ores.trading.api/domain/trade_booking_codec.hpp"
#include "ores.trading.api/domain/trade_codec.hpp"
#include "ores.trading.api/domain/trade_envelope_data.hpp"
#include "ores.trading.api/domain/trade_identifier_codec.hpp"
#include "ores.trading.api/messaging/trade_operations_protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::trading::messaging::book_trade_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::book_trade_request>();
}

TEST_CASE("ores::trading::messaging::book_trade_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::book_trade_response>();
}

TEST_CASE("ores::trading::messaging::execute_trade_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::execute_trade_request>();
}

TEST_CASE("ores::trading::messaging::execute_trade_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::execute_trade_response>();
}

TEST_CASE("ores::trading::messaging::complete_structure_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::complete_structure_request>();
}

TEST_CASE("ores::trading::messaging::complete_structure_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::complete_structure_response>();
}

TEST_CASE("ores::trading::messaging::put_bond_issue_composite_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::put_bond_issue_composite_request>();
}

TEST_CASE("ores::trading::messaging::put_bond_issue_composite_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::put_bond_issue_composite_response>();
}

TEST_CASE("ores::trading::messaging::trade_export_item", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::trade_export_item>();
}

TEST_CASE("ores::trading::messaging::export_portfolio_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::export_portfolio_request>();
}

TEST_CASE("ores::trading::messaging::export_portfolio_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::trading::messaging::export_portfolio_response>();
}

TEST_CASE("ores::trading::messaging::export_trades_to_storage_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::export_trades_to_storage_request>();
}

TEST_CASE("ores::trading::messaging::export_trades_to_storage_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::trading::messaging::export_trades_to_storage_response>();
}
