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
#include "ores.assets.core/repository/image_repository.hpp"
#include "ores.assets.core/service/image_operations_service.hpp"
#include "ores.assets.core/validation/image_upload_validator.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string tags("[service]");

using ores::assets::messaging::upload_image_request;

upload_image_request make_upload(std::string mime_type, std::vector<std::uint8_t> bytes) {
    upload_image_request request;
    request.mime_type = std::move(mime_type);
    request.data = ores::utility::convert::base64_converter::convert(bytes);
    return request;
}

}

using namespace ores::assets::service;
using ores::assets::repository::image_repository;
using ores::assets::tests::png_bytes;
using ores::testing::scoped_database_helper;
using ores::utility::domain::outcome;

TEST_CASE("upload_image_stores_the_bytes_and_answers_the_id", tags) {
    scoped_database_helper h;
    image_operations_service svc(h.context());
    const auto bytes = png_bytes(128, 128);

    const auto response = svc.upload_image(make_upload("image/png", bytes));

    REQUIRE(response.result.outcome == outcome::ok);
    REQUIRE_FALSE(response.image_id.empty());

    image_repository repo;
    const auto stored = repo.read_latest(h.context(), response.image_id);
    REQUIRE(stored.size() == 1);
    CHECK(boost::uuids::to_string(stored[0].id) == response.image_id);
    CHECK(stored[0].mime_type == "image/png");
    CHECK(stored[0].data == bytes);
}

TEST_CASE("upload_image_refuses_bytes_that_are_not_the_stated_type", tags) {
    scoped_database_helper h;
    image_operations_service svc(h.context());
    const auto bytes = png_bytes(128, 128);

    const auto response = svc.upload_image(make_upload("image/jpeg", bytes));

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "invalid_image");
    CHECK(response.image_id.empty());
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "data");
}

TEST_CASE("upload_image_refuses_an_image_below_the_minimum_size", tags) {
    scoped_database_helper h;
    image_operations_service svc(h.context());

    const auto response = svc.upload_image(make_upload("image/png", png_bytes(64, 64)));

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "image_too_small");
    CHECK(response.image_id.empty());
}

TEST_CASE("upload_image_refuses_an_unstated_media_type", tags) {
    scoped_database_helper h;
    image_operations_service svc(h.context());

    const auto response = svc.upload_image(make_upload("image/svg+xml", png_bytes(128, 128)));

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "unsupported_media_type");
    REQUIRE(response.result.fields.size() == 1);
    CHECK(response.result.fields[0].field == "mime_type");
}

TEST_CASE("upload_image_refuses_an_empty_body", tags) {
    scoped_database_helper h;
    image_operations_service svc(h.context());
    upload_image_request request;
    request.mime_type = "image/png";

    const auto response = svc.upload_image(request);

    CHECK(response.result.outcome == outcome::invalid);
    CHECK(response.result.code == "invalid_image");
    CHECK(response.image_id.empty());
}

TEST_CASE("get_image_upload_policy_answers_the_validator_policy", tags) {
    scoped_database_helper h;
    image_operations_service svc(h.context());

    const auto response =
        svc.get_image_upload_policy(ores::assets::messaging::get_image_upload_policy_request{});

    const auto rules = ores::assets::validation::image_upload_validator::policy();
    CHECK(response.result.outcome == outcome::ok);
    CHECK(response.formats == rules.formats);
    CHECK(response.max_size_bytes == static_cast<int>(rules.max_size_bytes));
    CHECK(response.min_width == static_cast<int>(rules.min_width));
    CHECK(response.min_height == static_cast<int>(rules.min_height));
}
