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
#include "image_bytes.hpp"
#include "ores.assets.core/validation/image_upload_validator.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <string>
#include <vector>

namespace {

const std::string tags("[validation]");

}

using namespace ores::assets::validation;
using ores::assets::tests::jpeg_bytes;
using ores::assets::tests::png_bytes;
using ores::assets::tests::webp_bytes;

TEST_CASE("image_upload_policy_states_the_rule", tags) {
    const auto rules = image_upload_validator::policy();

    CHECK(rules.formats == std::vector<std::string>{"image/png", "image/jpeg", "image/webp"});
    CHECK(rules.max_size_bytes == 2 * 1024 * 1024);
    CHECK(rules.min_width == 128);
    CHECK(rules.min_height == 128);
}

TEST_CASE("image_upload_validator_accepts_every_stated_format", tags) {
    const auto rules = image_upload_validator::policy();

    for (const auto& format : rules.formats) {
        const auto bytes = format == "image/png"  ? png_bytes(rules.min_width, rules.min_height) :
                           format == "image/jpeg" ? jpeg_bytes(rules.min_width, rules.min_height) :
                                                    webp_bytes(rules.min_width, rules.min_height);
        const auto refusal = image_upload_validator::validate(format, bytes);
        INFO("format: " << format);
        CHECK_FALSE(refusal.has_value());
    }
}

TEST_CASE("image_upload_validator_refuses_an_unstated_media_type", tags) {
    const auto refusal = image_upload_validator::validate("image/svg+xml", png_bytes(256, 256));

    REQUIRE(refusal.has_value());
    CHECK(refusal->code == "unsupported_media_type");
    CHECK(refusal->field == "mime_type");
}

TEST_CASE("image_upload_validator_refuses_an_oversized_image", tags) {
    const auto rules = image_upload_validator::policy();
    auto bytes = png_bytes(rules.min_width, rules.min_height);
    bytes.resize(rules.max_size_bytes + 1, 0);

    const auto refusal = image_upload_validator::validate("image/png", bytes);

    REQUIRE(refusal.has_value());
    CHECK(refusal->code == "image_too_large");
    CHECK(refusal->field == "data");
    // The message states the bound the policy states, not a second copy of it.
    CHECK(refusal->message.find("2 MB") != std::string::npos);
}

TEST_CASE("image_upload_validator_refuses_an_image_below_the_minimum_size", tags) {
    const auto rules = image_upload_validator::policy();
    const auto refusal = image_upload_validator::validate(
        "image/png", png_bytes(rules.min_width - 1, rules.min_height - 1));

    REQUIRE(refusal.has_value());
    CHECK(refusal->code == "image_too_small");
    CHECK(refusal->field == "data");
    // The message states the bound the policy states, not a second copy of it.
    CHECK(refusal->message.find("128 by 128") != std::string::npos);
}

TEST_CASE("image_upload_validator_refuses_bytes_that_are_not_the_stated_type", tags) {
    const auto refusal = image_upload_validator::validate("image/png", jpeg_bytes(256, 256));

    REQUIRE(refusal.has_value());
    CHECK(refusal->code == "invalid_image");
    CHECK(refusal->field == "data");
}

TEST_CASE("image_upload_validator_refuses_bytes_that_carry_no_image", tags) {
    const std::vector<std::uint8_t> bytes(64, 0);

    const auto refusal = image_upload_validator::validate("image/png", bytes);

    REQUIRE(refusal.has_value());
    CHECK(refusal->code == "invalid_image");
    CHECK(refusal->field == "data");
}

TEST_CASE("image_upload_validator_refuses_empty_bytes", tags) {
    const auto refusal =
        image_upload_validator::validate("image/webp", std::vector<std::uint8_t>{});

    REQUIRE(refusal.has_value());
    CHECK(refusal->code == "invalid_image");
}
