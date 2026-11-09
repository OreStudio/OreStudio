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
#include "ores.assets.api/domain/image_codec.hpp"
#include "ores.assets.api/domain/image_tag_codec.hpp"
#include "ores.assets.api/domain/tag_codec.hpp"
#include "ores.assets.api/generators/image_generator.hpp"
#include "ores.assets.api/generators/image_tag_generator.hpp"
#include "ores.assets.api/generators/tag_generator.hpp"
#include "ores.assets.api/messaging/image_operations_protocol.hpp"
#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.assets.api/messaging/image_tag_protocol.hpp"
#include "ores.assets.api/messaging/publish_operation_protocol.hpp"
#include "ores.assets.api/messaging/tag_protocol.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace {

const std::string tags("[codec]");

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

ores::utility::generation::generation_context a_context() {
    return ores::utility::generation::generation_context(20261101);
}

boost::uuids::uuid an_id() {
    return boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
}

std::chrono::system_clock::time_point an_instant() {
    return std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
}

ores::utility::domain::result ok() {
    return {ores::utility::domain::outcome::ok, "", "", {}};
}

ores::utility::domain::result denied() {
    return {ores::utility::domain::outcome::denied, "denied", "not \"allowed\"", {}};
}

}

using namespace ores::assets::messaging;

TEST_CASE("the assets domain types survive the wire", tags) {
    auto ctx = a_context();
    auto image = ores::assets::generators::generate_synthetic_image(ctx);
    image.data = {0, 1, 127, 128, 255};
    survives_the_wire(image);
    survives_the_wire(ores::assets::generators::generate_synthetic_tag(ctx));
    survives_the_wire(ores::assets::generators::generate_synthetic_image_tag(ctx));
}

TEST_CASE("the image messages survive the wire", tags) {
    auto ctx = a_context();
    const auto image = ores::assets::generators::generate_synthetic_image(ctx);

    image_change change;
    change.write.id = an_id();
    change.write.code = "gb";
    change.write.description = "Flag of the \"UK\"";
    change.write.mime_type = "image/svg+xml";
    change.precondition = {ores::utility::domain::precondition_kind::must_match_version, 2};
    survives_the_wire(change);

    image_event event;
    event.event_id = an_id();
    event.key.code = "gb";
    event.action = "written";
    event.occurred_at = an_instant();
    survives_the_wire(event);

    images_filter filter;
    filter.search = "flag";
    survives_the_wire(filter);

    list_images_request list;
    list.order = {"code", true};
    list.filter = filter;
    survives_the_wire(list);

    list_images_response listed;
    listed.result = ok();
    listed.images = {image};
    survives_the_wire(listed);

    get_many_images_response many;
    many.result = ok();
    many.entries = {image_lookup{image_key{"gb"}, image},
                    image_lookup{image_key{"zz"}, std::nullopt}};
    survives_the_wire(many);

    put_image_request put;
    put.change = change;
    put.intent = {"system.test", "a test"};
    survives_the_wire(put);

    delete_many_images_request removals;
    removals.removals = {
        image_removal{image_key{"gb"}, ores::utility::domain::removal_precondition}};
    survives_the_wire(removals);

    get_image_response missing;
    missing.result = denied();
    survives_the_wire(missing);
}

TEST_CASE("the image operation messages survive the wire", tags) {
    upload_image_request upload;
    upload.mime_type = "image/png";
    upload.data = "iVBORw0KGgo=";
    survives_the_wire(upload);

    get_image_upload_policy_response policy;
    policy.result = ok();
    policy.formats = {"png", "svg"};
    policy.max_size_bytes = 1048576;
    policy.min_width = 16;
    policy.min_height = -1;
    survives_the_wire(policy);

    survives_the_wire(ensure_image_request{"gb"});
    survives_the_wire(publish_images_from_dq_request{});
}

TEST_CASE("the tag and image tag messages survive the wire", tags) {
    auto ctx = a_context();
    const auto tag = ores::assets::generators::generate_synthetic_tag(ctx);
    const auto image_tag = ores::assets::generators::generate_synthetic_image_tag(ctx);

    tag_lookup found{tag_key{"flag"}, tag};
    survives_the_wire(found);
    survives_the_wire(tag_lookup{tag_key{"none"}, std::nullopt});

    tags_filter filter;
    filter.id_one_of = std::vector{an_id()};
    survives_the_wire(filter);

    image_tag_change change;
    change.write = {an_id(), an_id(), "admin", an_instant()};
    change.precondition = {ores::utility::domain::precondition_kind::any, std::nullopt};
    survives_the_wire(change);

    list_by_image_id_image_tags_request by_image;
    by_image.image_id = an_id();
    by_image.order = {"tag_id", false};
    survives_the_wire(by_image);

    put_many_image_tags_response put;
    put.result = ok();
    put.image_tags = {image_tag};
    survives_the_wire(put);
}
