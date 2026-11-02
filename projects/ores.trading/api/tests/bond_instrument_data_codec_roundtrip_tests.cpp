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
 * Template: cpp_domain_type_codec_roundtrip_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.api/domain/ascot_codec.hpp"
#include "ores.trading.api/domain/bond_forward_premium.hpp"
#include "ores.trading.api/domain/bond_forward_settlement.hpp"
#include "ores.trading.api/domain/bond_future_codec.hpp"
#include "ores.trading.api/domain/bond_instrument_codec.hpp"
#include "ores.trading.api/domain/bond_instrument_data.hpp"
#include "ores.trading.api/domain/bond_issue_call_date_codec.hpp"
#include "ores.trading.api/domain/bond_issue_codec.hpp"
#include "ores.trading.api/domain/bond_issue_conversion_target_codec.hpp"
#include "ores.trading.api/domain/bond_leg_data.hpp"
#include "ores.trading.api/domain/bond_option_codec.hpp"
#include "ores.trading.api/domain/bond_option_data.hpp"
#include "ores.trading.api/domain/bond_repo_codec.hpp"
#include "ores.trading.api/domain/bond_schedule_data.hpp"
#include "ores.trading.api/domain/bond_strike_data.hpp"
#include "ores.trading.api/domain/bond_trs_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/sample.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <span>
#include <string>

namespace {

using bond_instrument_data_roundtrip_type = ores::trading::domain::bond_instrument_data;

const std::string bond_instrument_data_roundtrip_tags("[codec][roundtrip]");

std::span<const std::byte> bond_instrument_data_roundtrip_bytes(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

}

static_assert(ores::utility::codec::names_every_member<bond_instrument_data_roundtrip_type>(0),
              "bond_instrument_data has a member its wire shape does not name");

TEST_CASE("bond_instrument_data_codec_roundtrip_json_sample", bond_instrument_data_roundtrip_tags) {
    using namespace ores::utility::codec;
    const auto v = sample<bond_instrument_data_roundtrip_type>(0);
    CHECK(object::read_json<bond_instrument_data_roundtrip_type>(object::write_json(v)) == v);
}

TEST_CASE("bond_instrument_data_codec_roundtrip_msgpack_sample",
          bond_instrument_data_roundtrip_tags) {
    using namespace ores::utility::codec;
    const auto v = sample<bond_instrument_data_roundtrip_type>(0);
    const auto packed = object::write_msgpack(v);
    CHECK(object::read_msgpack<bond_instrument_data_roundtrip_type>(
              bond_instrument_data_roundtrip_bytes(packed)) == v);
}

TEST_CASE("bond_instrument_data_codec_roundtrip_json_default",
          bond_instrument_data_roundtrip_tags) {
    using namespace ores::utility::codec;
    const bond_instrument_data_roundtrip_type v{};
    CHECK(object::read_json<bond_instrument_data_roundtrip_type>(object::write_json(v)) == v);
}

TEST_CASE("bond_instrument_data_codec_roundtrip_msgpack_default",
          bond_instrument_data_roundtrip_tags) {
    using namespace ores::utility::codec;
    const bond_instrument_data_roundtrip_type v{};
    const auto packed = object::write_msgpack(v);
    CHECK(object::read_msgpack<bond_instrument_data_roundtrip_type>(
              bond_instrument_data_roundtrip_bytes(packed)) == v);
}
