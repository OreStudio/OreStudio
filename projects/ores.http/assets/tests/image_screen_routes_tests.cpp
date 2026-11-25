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
#include "ores.assets.core/validation/image_upload_validator.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/image_answer.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/assets/image_screen_routes.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::assets::image_screen_routes;
using ores::nats::service::nats_client;

namespace {

http_request image_request(std::map<std::string, std::string> query,
                           std::map<std::string, std::string> path = {},
                           std::string body = {}) {
    http_request req;
    for (auto& [k, v] : query)
        req.query_params[k] = v;
    for (auto& [k, v] : path)
        req.path_params[k] = v;
    req.body = std::move(body);
    return req;
}

http_response image_call(nats_client& session,
                         http_method method,
                         const std::string& pattern,
                         const http_request& req) {
    auto router = std::make_shared<ores::http::net::router>();
    image_screen_routes::register_routes(
        router, std::make_shared<ores::http::openapi::endpoint_registry>(), session);
    const auto& routes = router->routes();
    const auto found = std::ranges::find_if(
        routes, [&](const auto& r) { return r.method == method && r.pattern == pattern; });
    REQUIRE(found != routes.end());
    std::optional<http_response> answer;
    boost::asio::io_context io;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await found->handler(req); },
        boost::asio::detached);
    io.run();
    return answer.value_or(http_response{});
}

}

TEST_CASE("image_screen_routes_read_a_page_and_an_upload_as_the_bff_did", "[image_screen]") {
    const auto page = image_screen_routes::read_page(image_request({{"search", "  flag  "}}));
    REQUIRE(page);
    CHECK(page->limit == 100);
    CHECK(page->search == "flag");
    CHECK_FALSE(image_screen_routes::read_page(image_request({{"limit", "501"}})));
    CHECK(image_screen_routes::read_upload(R"({"mimeType":"image/png","data":"AAAA"})"));
    CHECK_FALSE(image_screen_routes::read_upload(R"({"mimeType":"","data":"AAAA"})"));
    CHECK_FALSE(image_screen_routes::read_upload(R"({"mimeType":"image/png","data":""})"));
}

TEST_CASE("image_screen_routes_refuse_before_forwarding", "[image_screen]") {
    nats_client session;
    CHECK(image_call(session, http_method::get, "/api/images", image_request({{"limit", "0"}}))
              .status == http_status::bad_request);
    CHECK(image_call(
              session, http_method::get, "/api/images/{id}", image_request({}, {{"id", "nope"}}))
              .status == http_status::not_found);
    CHECK(
        image_call(session, http_method::post, "/api/images", image_request({}, {}, "{}")).status ==
        http_status::bad_request);
}

TEST_CASE("image_screen_routes_forward_an_accepted_request", "[image_screen]") {
    nats_client session;
    CHECK(image_call(session, http_method::get, "/api/image-upload-policy", image_request({}))
              .status == http_status::bad_gateway);
}

TEST_CASE("image_answer_serves_only_an_image_type", "[image_screen]") {
    const auto svg = ores::http::net::image_answer("", "<svg/>");
    CHECK(svg.content_type == "image/svg+xml");
    CHECK(svg.headers.at("Content-Security-Policy") == "default-src 'none'; sandbox");
    CHECK(ores::http::net::image_answer("image/png", "x").content_type == "image/png");
    CHECK(ores::http::net::image_answer("text/html", "x").content_type ==
          "application/octet-stream");
    CHECK(ores::http::net::image_answer("image/svg+xml", "x").content_type == "image/svg+xml");
    CHECK(ores::http::net::image_answer("image/png\r\nX-Evil: 1", "x").content_type ==
          "application/octet-stream");
    CHECK(ores::http::net::image_answer("image/", "x").content_type == "application/octet-stream");
}

TEST_CASE("image_screen_routes_map_each_flag_and_fall_back_to_the_country", "[image_screen]") {
    const auto id_of = [](const char* text) {
        return boost::uuids::string_generator()(text);
    };
    const auto gb = id_of("00000000-0000-0000-0000-0000000000a1");
    const auto gbp = id_of("00000000-0000-0000-0000-0000000000a2");
    const auto boe = id_of("00000000-0000-0000-0000-0000000000a3");
    const auto none = id_of("00000000-0000-0000-0000-0000000000a4");

    ores::refdata::domain::currency pound;
    pound.iso_code = "GBP";
    pound.image_id = gbp;
    ores::refdata::domain::currency unflagged;
    unflagged.iso_code = "XAU";
    ores::refdata::domain::country uk;
    uk.alpha2_code = "GB";
    uk.image_id = gb;
    ores::refdata::domain::calendar own;
    own.code = "BOE";
    own.country_code = "GB";
    own.image_id = boe;
    ores::refdata::domain::calendar borrowed;
    borrowed.code = "GBLO";
    borrowed.country_code = "GB";
    ores::refdata::domain::calendar orphan;
    orphan.code = "ZZZZ";
    orphan.country_code = "ZZ";
    ores::refdata::domain::business_centre london;
    london.code = "GBLO";
    london.country_alpha2_code = "GB";
    ores::assets::domain::image placeholder;
    placeholder.id = none;
    placeholder.code = "xx";
    ores::assets::domain::image other;
    other.code = "xxl";

    const auto map = image_screen_routes::image_map_of(
        {pound, unflagged}, {uk}, {own, borrowed, orphan}, {london}, {other, placeholder});
    CHECK(map.currencies ==
          std::map<std::string, std::string>{{"GBP", boost::uuids::to_string(gbp)}});
    CHECK(map.countries == std::map<std::string, std::string>{{"GB", boost::uuids::to_string(gb)}});
    CHECK(map.calendars ==
          std::map<std::string, std::string>{{"BOE", boost::uuids::to_string(boe)},
                                             {"GBLO", boost::uuids::to_string(gb)}});
    CHECK(map.businessCentres ==
          std::map<std::string, std::string>{{"GBLO", boost::uuids::to_string(gb)}});
    CHECK(map.noFlag == boost::uuids::to_string(none));

    const auto empty = image_screen_routes::image_map_of({}, {}, {}, {}, {other});
    CHECK_FALSE(empty.noFlag.has_value());
    CHECK(ores::utility::codec::json::write(empty) ==
          R"({"currencies":{},"countries":{},"calendars":{},"businessCentres":{},"noFlag":null})");
}

namespace {

/** An upload body as the picker sends it: a media type and the bytes in base64. */
std::string upload_body_of(const std::string& mime_type, const std::vector<std::uint8_t>& bytes) {
    return R"({"mimeType":")" + mime_type + R"(","data":")" +
           ores::utility::convert::base64_converter::convert(bytes) + R"("})";
}

bool states(const std::string& text, const std::string& member) {
    return text.find(member) != std::string::npos;
}

}

TEST_CASE("an_upload_over_the_limit_is_refused_before_it_is_sent", "[image_screen]") {
    const auto limit = ores::assets::validation::image_upload_validator::policy().max_size_bytes;
    // An image the validator refuses for its size, big enough that the transport
    // the deployment ran with would have refused it instead.
    const std::vector<std::uint8_t> too_large(limit + limit / 4, 0x89);

    nats_client session;
    const auto answer = image_call(session,
                                   http_method::post,
                                   "/api/images",
                                   image_request({}, {}, upload_body_of("image/png", too_large)));

    // The session is not connected, so a forward would fail: an answer at all
    // proves the image was judged before it was sent.
    CHECK(answer.status == http_status::ok);
    CHECK(states(answer.body, R"("outcome":"invalid")"));
    CHECK(states(answer.body, R"("code":"image_too_large")"));
    CHECK(states(answer.body, "The image is larger than the 2 MB limit."));
    CHECK(states(answer.body, R"("imageId":"")"));
}

TEST_CASE("an_upload_the_validator_cannot_read_is_refused_before_it_is_sent", "[image_screen]") {
    nats_client session;
    const auto answer = image_call(
        session,
        http_method::post,
        "/api/images",
        image_request({}, {}, upload_body_of("image/png", std::vector<std::uint8_t>{1, 2, 3, 4})));
    CHECK(answer.status == http_status::ok);
    CHECK(states(answer.body, R"("code":"invalid_image")"));
    CHECK(states(answer.body, "is not a readable image of media type image/png"));
}

TEST_CASE("an_upload_of_a_media_type_the_policy_refuses_is_refused_before_it_is_sent",
          "[image_screen]") {
    nats_client session;
    const auto answer = image_call(
        session,
        http_method::post,
        "/api/images",
        image_request({}, {}, upload_body_of("image/gif", std::vector<std::uint8_t>(4000, 0x21))));
    CHECK(answer.status == http_status::ok);
    CHECK(states(answer.body, R"("code":"unsupported_media_type")"));
    CHECK(states(answer.body, "The image media type must be one of"));
}

TEST_CASE("the_upload_body_carries_the_largest_image_the_validator_accepts", "[image_screen]") {
    // The body holds the bytes base64-encoded, and base64 grows them by a third,
    // so the body limit must clear the inflated largest image with room for the
    // envelope. This is the arithmetic the transport's own cap is set from.
    const auto limit = ores::assets::validation::image_upload_validator::policy().max_size_bytes;
    const std::vector<std::uint8_t> largest(limit, 0x89);
    const auto encoded = ores::utility::convert::base64_converter::convert(largest);
    CHECK(encoded.size() > limit);
    CHECK(encoded.size() + 100 < image_screen_routes::upload_body_bytes);
}
