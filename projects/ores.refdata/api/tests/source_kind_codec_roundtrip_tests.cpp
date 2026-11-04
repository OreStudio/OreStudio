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
#include "ores.refdata.api/domain/source_kind_codec.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/sample.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <span>
#include <string>

namespace {

using source_kind_roundtrip_type = ores::refdata::domain::source_kind;
using source_kind_roundtrip_traits = ores::utility::codec::wire_traits<source_kind_roundtrip_type>;

const std::string source_kind_roundtrip_tags("[codec][roundtrip]");

std::span<const std::byte> source_kind_roundtrip_bytes(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

}

static_assert(ores::utility::codec::names_every_member<source_kind_roundtrip_type>(0),
              "source_kind has a member its wire shape does not name");

TEST_CASE("source_kind_codec_roundtrip_json_sample", source_kind_roundtrip_tags) {
    const auto v = ores::utility::codec::sample<source_kind_roundtrip_type>(0);
    CHECK(source_kind_roundtrip_traits::read_json(source_kind_roundtrip_traits::write_json(v)) ==
          v);
}

TEST_CASE("source_kind_codec_roundtrip_msgpack_sample", source_kind_roundtrip_tags) {
    const auto v = ores::utility::codec::sample<source_kind_roundtrip_type>(0);
    const auto packed = source_kind_roundtrip_traits::write_msgpack(v);
    CHECK(source_kind_roundtrip_traits::read_msgpack(source_kind_roundtrip_bytes(packed)) == v);
}

TEST_CASE("source_kind_codec_roundtrip_json_default", source_kind_roundtrip_tags) {
    const source_kind_roundtrip_type v{};
    CHECK(source_kind_roundtrip_traits::read_json(source_kind_roundtrip_traits::write_json(v)) ==
          v);
}

TEST_CASE("source_kind_codec_roundtrip_msgpack_default", source_kind_roundtrip_tags) {
    const source_kind_roundtrip_type v{};
    const auto packed = source_kind_roundtrip_traits::write_msgpack(v);
    CHECK(source_kind_roundtrip_traits::read_msgpack(source_kind_roundtrip_bytes(packed)) == v);
}
