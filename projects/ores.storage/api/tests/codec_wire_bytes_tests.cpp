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
#include "ores.storage.api/messaging/objects_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The generated codec writes the bytes reflect-cpp wrote, in both
 * formats, and reads them back to the same bytes.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(json == ores::utility::codec::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (same_bytes_as_reflect_cpp(T{}), ...);
}

}

TEST_CASE("every storage message's default has reflect-cpp's bytes", codec_tags) {
    using namespace ores::storage::messaging;
    every_default_has_the_same_bytes<put_objects_request,
                                     put_objects_response,
                                     get_objects_request,
                                     get_objects_response,
                                     delete_objects_request,
                                     delete_objects_response,
                                     list_objects_request,
                                     object_summary,
                                     list_objects_response>();
}

TEST_CASE("filled storage messages have reflect-cpp's bytes", codec_tags) {
    using namespace ores::storage::messaging;
    put_objects_request put;
    put.bucket = "reports";
    put.key = "runs/2026/\"quoted\"\n.tar.gz";
    put.content = "aGVsbG8gd29ybGQ=";
    put.content_encoding = "base64";
    same_bytes_as_reflect_cpp(put);

    get_objects_response got;
    got.success = false;
    got.message = "partial";
    got.found = true;
    got.size_bytes = 4294967296ULL;
    got.content = "aGVsbG8=";
    same_bytes_as_reflect_cpp(got);

    delete_objects_response removed;
    removed.success = true;
    removed.removed = true;
    same_bytes_as_reflect_cpp(removed);

    list_objects_request list;
    list.bucket = "reports";
    list.prefix = "runs/";
    list.offset = 200;
    list.limit = 50;
    same_bytes_as_reflect_cpp(list);

    list_objects_response listing;
    listing.success = true;
    listing.objects = {{"runs/a", 12}, {"runs/b", 0}};
    listing.total_available_count = 2;
    same_bytes_as_reflect_cpp(listing);
}
