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
#include "ores.trading.api/domain/risk_participation_agreement_instrument_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/sample.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <span>
#include <string>

namespace {

using risk_participation_agreement_instrument_roundtrip_type =
    ores::trading::domain::risk_participation_agreement_instrument;
using risk_participation_agreement_instrument_roundtrip_traits =
    ores::utility::codec::wire_traits<risk_participation_agreement_instrument_roundtrip_type>;

const std::string risk_participation_agreement_instrument_roundtrip_tags("[codec][roundtrip]");

std::span<const std::byte>
risk_participation_agreement_instrument_roundtrip_bytes(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

}

static_assert(ores::utility::codec::names_every_member<
                  risk_participation_agreement_instrument_roundtrip_type>(0),
              "risk_participation_agreement_instrument has a member its wire shape does not name");

TEST_CASE("risk_participation_agreement_instrument_codec_roundtrip_json_sample",
          risk_participation_agreement_instrument_roundtrip_tags) {
    const auto v =
        ores::utility::codec::sample<risk_participation_agreement_instrument_roundtrip_type>(0);
    CHECK(risk_participation_agreement_instrument_roundtrip_traits::read_json(
              risk_participation_agreement_instrument_roundtrip_traits::write_json(v)) == v);
}

TEST_CASE("risk_participation_agreement_instrument_codec_roundtrip_msgpack_sample",
          risk_participation_agreement_instrument_roundtrip_tags) {
    const auto v =
        ores::utility::codec::sample<risk_participation_agreement_instrument_roundtrip_type>(0);
    const auto packed = risk_participation_agreement_instrument_roundtrip_traits::write_msgpack(v);
    CHECK(risk_participation_agreement_instrument_roundtrip_traits::read_msgpack(
              risk_participation_agreement_instrument_roundtrip_bytes(packed)) == v);
}

TEST_CASE("risk_participation_agreement_instrument_codec_roundtrip_json_default",
          risk_participation_agreement_instrument_roundtrip_tags) {
    const risk_participation_agreement_instrument_roundtrip_type v{};
    CHECK(risk_participation_agreement_instrument_roundtrip_traits::read_json(
              risk_participation_agreement_instrument_roundtrip_traits::write_json(v)) == v);
}

TEST_CASE("risk_participation_agreement_instrument_codec_roundtrip_msgpack_default",
          risk_participation_agreement_instrument_roundtrip_tags) {
    const risk_participation_agreement_instrument_roundtrip_type v{};
    const auto packed = risk_participation_agreement_instrument_roundtrip_traits::write_msgpack(v);
    CHECK(risk_participation_agreement_instrument_roundtrip_traits::read_msgpack(
              risk_participation_agreement_instrument_roundtrip_bytes(packed)) == v);
}
