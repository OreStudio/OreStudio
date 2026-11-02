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
#include "ores.refdata.api/domain/cds_convention_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/sample.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <span>
#include <string>

namespace {

using cds_convention_roundtrip_type = ores::refdata::domain::cds_convention;
using cds_convention_roundtrip_traits =
    ores::utility::codec::wire_traits<cds_convention_roundtrip_type>;

const std::string cds_convention_roundtrip_tags("[codec][roundtrip]");

std::span<const std::byte> cds_convention_roundtrip_bytes(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

}

static_assert(ores::utility::codec::names_every_member<cds_convention_roundtrip_type>(0),
              "cds_convention has a member its wire shape does not name");

TEST_CASE("cds_convention_codec_roundtrip_json_sample", cds_convention_roundtrip_tags) {
    const auto v = ores::utility::codec::sample<cds_convention_roundtrip_type>(0);
    CHECK(cds_convention_roundtrip_traits::read_json(
              cds_convention_roundtrip_traits::write_json(v)) == v);
}

TEST_CASE("cds_convention_codec_roundtrip_msgpack_sample", cds_convention_roundtrip_tags) {
    const auto v = ores::utility::codec::sample<cds_convention_roundtrip_type>(0);
    const auto packed = cds_convention_roundtrip_traits::write_msgpack(v);
    CHECK(cds_convention_roundtrip_traits::read_msgpack(cds_convention_roundtrip_bytes(packed)) ==
          v);
}

TEST_CASE("cds_convention_codec_roundtrip_json_default", cds_convention_roundtrip_tags) {
    const cds_convention_roundtrip_type v{};
    CHECK(cds_convention_roundtrip_traits::read_json(
              cds_convention_roundtrip_traits::write_json(v)) == v);
}

TEST_CASE("cds_convention_codec_roundtrip_msgpack_default", cds_convention_roundtrip_tags) {
    const cds_convention_roundtrip_type v{};
    const auto packed = cds_convention_roundtrip_traits::write_msgpack(v);
    CHECK(cds_convention_roundtrip_traits::read_msgpack(cds_convention_roundtrip_bytes(packed)) ==
          v);
}
