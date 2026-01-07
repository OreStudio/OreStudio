/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.assets/messaging/assets_protocol.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include "ores.telemetry/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.assets.tests");
const std::string tags("[assets_protocol]");

}

using namespace ores::assets::messaging;
using namespace ores::assets::domain;
using namespace ores::comms::messaging;
using namespace ores::telemetry::log;

// get_images_request tests

TEST_CASE("get_images_request_serialize_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_request serialization with empty list";

    get_images_request request;
    auto serialized = request.serialize();

    // Should contain at least the count (4 bytes)
    REQUIRE(serialized.size() >= 4);
}

TEST_CASE("get_images_request_roundtrip_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_request roundtrip with empty list";

    get_images_request original;
    auto serialized = original.serialize();
    auto result = get_images_request::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->image_ids.empty());
}

TEST_CASE("get_images_request_roundtrip_single_id", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_request roundtrip with single ID";

    get_images_request original;
    original.image_ids.push_back("img-12345");

    auto serialized = original.serialize();
    auto result = get_images_request::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->image_ids.size() == 1);
    REQUIRE(result->image_ids[0] == "img-12345");
}

TEST_CASE("get_images_request_roundtrip_multiple_ids", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_request roundtrip with multiple IDs";

    get_images_request original;
    original.image_ids = {"img-001", "img-002", "img-003", "img-004", "img-005"};

    auto serialized = original.serialize();
    auto result = get_images_request::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->image_ids.size() == 5);
    REQUIRE(result->image_ids[0] == "img-001");
    REQUIRE(result->image_ids[4] == "img-005");
}

TEST_CASE("get_images_request_stream_operator", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_request stream operator";

    get_images_request request;
    request.image_ids = {"img-001", "img-002"};

    std::ostringstream oss;
    oss << request;

    REQUIRE_FALSE(oss.str().empty());
    REQUIRE(oss.str().find("img-001") != std::string::npos);
}

// get_images_response tests

TEST_CASE("get_images_response_serialize_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_response serialization with empty list";

    get_images_response response;
    auto serialized = response.serialize();

    // Should contain at least the count (4 bytes)
    REQUIRE(serialized.size() >= 4);
}

TEST_CASE("get_images_response_roundtrip_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_response roundtrip with empty list";

    get_images_response original;
    auto serialized = original.serialize();
    auto result = get_images_response::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->images.empty());
}

TEST_CASE("get_images_response_roundtrip_single_image", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_response roundtrip with single image";

    get_images_response original;
    original.images.push_back({
        .version = 1,
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag",
        .svg_data = "<svg>...</svg>",
        .recorded_by = "admin",
        .recorded_at = "2025-01-01T00:00:00Z"
    });

    auto serialized = original.serialize();
    auto result = get_images_response::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->images.size() == 1);
    REQUIRE(result->images[0].version == 1);
    REQUIRE(result->images[0].image_id == "img-001");
    REQUIRE(result->images[0].key == "us");
    REQUIRE(result->images[0].description == "United States flag");
    REQUIRE(result->images[0].svg_data == "<svg>...</svg>");
    REQUIRE(result->images[0].recorded_by == "admin");
    REQUIRE(result->images[0].recorded_at == "2025-01-01T00:00:00Z");
}

TEST_CASE("get_images_response_roundtrip_multiple_images", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_response roundtrip with multiple images";

    get_images_response original;
    original.images.push_back({
        .version = 1,
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag",
        .svg_data = "<svg id='us'>...</svg>",
        .recorded_by = "admin",
        .recorded_at = "2025-01-01T00:00:00Z"
    });
    original.images.push_back({
        .version = 2,
        .image_id = "img-002",
        .key = "gb",
        .description = "United Kingdom flag",
        .svg_data = "<svg id='gb'>...</svg>",
        .recorded_by = "user1",
        .recorded_at = "2025-01-02T12:00:00Z"
    });

    auto serialized = original.serialize();
    auto result = get_images_response::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->images.size() == 2);
    REQUIRE(result->images[0].key == "us");
    REQUIRE(result->images[0].version == 1);
    REQUIRE(result->images[1].key == "gb");
    REQUIRE(result->images[1].version == 2);
}

TEST_CASE("get_images_response_roundtrip_with_unicode", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_response roundtrip with unicode content";

    get_images_response original;
    original.images.push_back({
        .version = 1,
        .image_id = "img-unicode",
        .key = "jp",
        .description = "Japanese flag - \xe6\x97\xa5\xe6\x9c\xac",
        .svg_data = "<svg><!-- \xe6\x97\xa5\xe6\x9c\xac --></svg>",
        .recorded_by = "\xe7\xab\xa0\xe5\xa4\xaa\xe9\x83\x8e",
        .recorded_at = "2025-01-01T00:00:00Z"
    });

    auto serialized = original.serialize();
    auto result = get_images_response::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->images.size() == 1);
    REQUIRE(result->images[0].description == "Japanese flag - \xe6\x97\xa5\xe6\x9c\xac");
    REQUIRE(result->images[0].recorded_by == "\xe7\xab\xa0\xe5\xa4\xaa\xe9\x83\x8e");
}

TEST_CASE("get_images_response_stream_operator", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_response stream operator";

    get_images_response response;
    response.images.push_back({
        .version = 1,
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag",
        .svg_data = "<svg>...</svg>",
        .recorded_by = "admin",
        .recorded_at = "2025-01-01T00:00:00Z"
    });

    std::ostringstream oss;
    oss << response;

    REQUIRE_FALSE(oss.str().empty());
    REQUIRE(oss.str().find("img-001") != std::string::npos);
}

// Error handling tests

TEST_CASE("get_images_request_deserialize_truncated_data", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_request deserialization with truncated data";

    get_images_request original;
    original.image_ids = {"img-001"};

    auto serialized = original.serialize();
    // Truncate to just the count
    serialized.resize(4);

    auto result = get_images_request::deserialize(serialized);
    REQUIRE_FALSE(result.has_value());
}

TEST_CASE("get_images_response_deserialize_truncated_data", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing get_images_response deserialization with truncated data";

    get_images_response original;
    original.images.push_back({
        .version = 1,
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag",
        .svg_data = "<svg>...</svg>",
        .recorded_by = "admin",
        .recorded_at = "2025-01-01T00:00:00Z"
    });

    auto serialized = original.serialize();
    // Truncate to just the count + version
    serialized.resize(8);

    auto result = get_images_response::deserialize(serialized);
    REQUIRE_FALSE(result.has_value());
}

// list_images_request tests

TEST_CASE("list_images_request_serialize_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_request serialization";

    list_images_request request;
    auto serialized = request.serialize();

    REQUIRE(serialized.empty());
}

TEST_CASE("list_images_request_deserialize_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_request deserialization";

    std::span<const std::byte> empty_data;
    auto result = list_images_request::deserialize(empty_data);

    REQUIRE(result.has_value());
}

TEST_CASE("list_images_request_deserialize_non_empty_fails", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_request deserialization with non-empty data";

    std::vector<std::byte> data{std::byte{0x01}};
    auto result = list_images_request::deserialize(data);

    REQUIRE_FALSE(result.has_value());
    REQUIRE(result.error() == error_code::invalid_request);
}

TEST_CASE("list_images_request_roundtrip", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_request roundtrip";

    list_images_request request;
    auto serialized = request.serialize();
    auto result = list_images_request::deserialize(serialized);

    REQUIRE(result.has_value());
}

TEST_CASE("list_images_request_stream_operator", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_request stream operator";

    list_images_request request;
    std::ostringstream oss;
    oss << request;

    REQUIRE_FALSE(oss.str().empty());
}

// list_images_response tests

TEST_CASE("list_images_response_serialize_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_response serialization with empty list";

    list_images_response response;
    auto serialized = response.serialize();

    // Should contain at least the count (4 bytes)
    REQUIRE(serialized.size() >= 4);
}

TEST_CASE("list_images_response_roundtrip_empty", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_response roundtrip with empty list";

    list_images_response original;
    auto serialized = original.serialize();
    auto result = list_images_response::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->images.empty());
}

TEST_CASE("list_images_response_roundtrip_single_item", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_response roundtrip with single item";

    list_images_response original;
    original.images.push_back({
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag"
    });

    auto serialized = original.serialize();
    auto result = list_images_response::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->images.size() == 1);
    REQUIRE(result->images[0].image_id == "img-001");
    REQUIRE(result->images[0].key == "us");
    REQUIRE(result->images[0].description == "United States flag");
}

TEST_CASE("list_images_response_roundtrip_multiple_items", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_response roundtrip with multiple items";

    list_images_response original;
    original.images.push_back({
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag"
    });
    original.images.push_back({
        .image_id = "img-002",
        .key = "gb",
        .description = "United Kingdom flag"
    });
    original.images.push_back({
        .image_id = "img-003",
        .key = "eu",
        .description = "European Union flag"
    });

    auto serialized = original.serialize();
    auto result = list_images_response::deserialize(serialized);

    REQUIRE(result.has_value());
    REQUIRE(result->images.size() == 3);
    REQUIRE(result->images[0].key == "us");
    REQUIRE(result->images[1].key == "gb");
    REQUIRE(result->images[2].key == "eu");
}

TEST_CASE("list_images_response_stream_operator", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing list_images_response stream operator";

    list_images_response response;
    response.images.push_back({
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag"
    });

    std::ostringstream oss;
    oss << response;

    REQUIRE_FALSE(oss.str().empty());
    REQUIRE(oss.str().find("img-001") != std::string::npos);
}

TEST_CASE("image_info_stream_operator", tags) {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing image_info stream operator";

    image_info info{
        .image_id = "img-001",
        .key = "us",
        .description = "United States flag"
    };

    std::ostringstream oss;
    oss << info;

    REQUIRE_FALSE(oss.str().empty());
    REQUIRE(oss.str().find("img-001") != std::string::npos);
}
