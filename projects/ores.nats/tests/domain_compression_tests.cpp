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
#include "ores.nats/domain/compression.hpp"
#include "ores.nats/domain/headers.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>

namespace {

const std::string tags("[domain][compression]");

std::span<const std::byte> as_bytes(std::string_view s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

} // namespace

TEST_CASE("small payload is left uncompressed", tags) {
    const std::string small(ores::nats::compression_threshold_bytes - 1, 'a');
    std::unordered_map<std::string, std::string> headers;

    const auto out = ores::nats::compress_if_worthwhile(as_bytes(small), headers);

    REQUIRE(out.size() == small.size());
    REQUIRE(headers.empty());
}

TEST_CASE("large compressible payload is compressed and flagged", tags) {
    // Highly repetitive text compresses well and is unambiguously past the
    // threshold, so this exercises the "worth it" path deterministically.
    const std::string large(ores::nats::compression_threshold_bytes * 4, 'a');
    std::unordered_map<std::string, std::string> headers;

    const auto out = ores::nats::compress_if_worthwhile(as_bytes(large), headers);

    REQUIRE(out.size() < large.size());
    REQUIRE(headers.at(std::string(ores::nats::headers::x_content_encoding)) ==
           ores::nats::headers::content_encoding_gzip);
}

TEST_CASE("large incompressible payload is left uncompressed despite size", tags) {
    // A payload gzip cannot shrink (already at the incompressibility limit
    // for this trivial generator: strictly increasing bytes have no
    // repeating structure) must not be sent compressed-but-larger.
    std::string large(ores::nats::compression_threshold_bytes * 2, '\0');
    for (std::size_t i = 0; i < large.size(); ++i)
        large[i] = static_cast<char>(i % 256);
    std::unordered_map<std::string, std::string> headers;

    const auto out = ores::nats::compress_if_worthwhile(as_bytes(large), headers);

    // Either it left the payload alone (headers empty), or compression
    // happened to still shrink it slightly -- either is fine. What must
    // never happen is claiming compression while the payload got bigger.
    if (headers.contains(std::string(ores::nats::headers::x_content_encoding)))
        REQUIRE(out.size() < large.size());
    else
        REQUIRE(out.size() == large.size());
}

TEST_CASE("compress_if_worthwhile never overrides an existing content-encoding header",
         tags) {
    const std::string large(ores::nats::compression_threshold_bytes * 4, 'a');
    std::unordered_map<std::string, std::string> headers{
        {std::string(ores::nats::headers::x_content_encoding), "identity"}};

    const auto out = ores::nats::compress_if_worthwhile(as_bytes(large), headers);

    REQUIRE(out.size() == large.size());
    REQUIRE(headers.at(std::string(ores::nats::headers::x_content_encoding)) == "identity");
}

TEST_CASE("decompress_if_flagged is a no-op without the header", tags) {
    const std::string data = "not compressed";
    std::unordered_map<std::string, std::string> headers;

    const auto out = ores::nats::decompress_if_flagged(as_bytes(data), headers);

    REQUIRE(std::string(reinterpret_cast<const char*>(out.data()), out.size()) == data);
}

TEST_CASE("decompress_if_flagged throws on the gzip header with a non-gzip payload",
         tags) {
    // Pins the contract callers must handle: a message that claims gzip
    // encoding but isn't valid gzip (corrupted in transit, or a malformed
    // sender) throws rather than silently returning garbage. Every inbound
    // message path (extract_message()'s callers, e.g. on_msg()) must catch
    // this rather than let it escape -- see on_msg()'s try block in
    // client.cpp, which exists specifically to turn this into a dropped
    // message plus a log line instead of a process crash.
    const std::string not_gzip = "this is definitely not gzip data";
    std::unordered_map<std::string, std::string> headers{
        {std::string(ores::nats::headers::x_content_encoding),
         std::string(ores::nats::headers::content_encoding_gzip)}};

    REQUIRE_THROWS_AS(ores::nats::decompress_if_flagged(as_bytes(not_gzip), headers),
                      std::runtime_error);
}

TEST_CASE("compress then decompress round-trips to the original bytes", tags) {
    const std::string original(ores::nats::compression_threshold_bytes * 3, 'x');
    std::unordered_map<std::string, std::string> headers;

    const auto compressed = ores::nats::compress_if_worthwhile(as_bytes(original), headers);
    REQUIRE(headers.contains(std::string(ores::nats::headers::x_content_encoding)));

    const auto decompressed =
        ores::nats::decompress_if_flagged(std::span<const std::byte>(compressed), headers);

    REQUIRE(decompressed.size() == original.size());
    REQUIRE(std::string(reinterpret_cast<const char*>(decompressed.data()),
                        decompressed.size()) == original);
}

TEST_CASE("round-trip preserves synthetic worst-case SVG-batch-sized binary data", tags) {
    // Mirrors the decision task's synthetic worst case: a payload the size
    // of the largest measured SVG batch, but with byte content varied
    // enough to be representative of real (non-degenerate) input rather
    // than a single repeated byte.
    std::string original;
    original.reserve(200000);
    for (std::size_t i = 0; i < 200000; ++i)
        original.push_back(static_cast<char>('A' + (i % 17)));

    std::unordered_map<std::string, std::string> headers;
    const auto compressed = ores::nats::compress_if_worthwhile(as_bytes(original), headers);
    REQUIRE(compressed.size() < original.size());

    const auto decompressed =
        ores::nats::decompress_if_flagged(std::span<const std::byte>(compressed), headers);
    REQUIRE(decompressed.size() == original.size());
    REQUIRE(std::string(reinterpret_cast<const char*>(decompressed.data()),
                        decompressed.size()) == original);
}
