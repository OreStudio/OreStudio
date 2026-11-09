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
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <catch2/catch_test_macros.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The codec reads back what it wrote, in both formats.
 */
template <typename T>
void survives_the_wire(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (survives_the_wire(T{}), ...);
}

}

TEST_CASE("every storage message's default has its wire form", codec_tags) {
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

TEST_CASE("filled storage messages survive the wire", codec_tags) {
    using namespace ores::storage::messaging;
    put_objects_request put;
    put.bucket = "reports";
    put.key = "runs/2026/\"quoted\"\n.tar.gz";
    put.content = "aGVsbG8gd29ybGQ=";
    put.content_encoding = "base64";
    survives_the_wire(put);

    get_objects_response got;
    got.success = false;
    got.message = "partial";
    got.found = true;
    got.size_bytes = 4294967296ULL;
    got.content = "aGVsbG8=";
    survives_the_wire(got);

    delete_objects_response removed;
    removed.success = true;
    removed.removed = true;
    survives_the_wire(removed);

    list_objects_request list;
    list.bucket = "reports";
    list.prefix = "runs/";
    list.offset = 200;
    list.limit = 50;
    survives_the_wire(list);

    list_objects_response listing;
    listing.success = true;
    listing.objects = {{"runs/a", 12}, {"runs/b", 0}};
    listing.total_available_count = 2;
    survives_the_wire(listing);
}
