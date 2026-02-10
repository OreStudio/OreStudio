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
#include "ores.iam/service/session_converter.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/asio/ip/address.hpp>

namespace {

const std::string tags("[session_converter]");

using namespace ores::iam::service;
using namespace ores::iam::domain;
using ores::utility::uuid::tenant_id;

session make_test_session() {
    session s;
    s.id = boost::uuids::random_generator()();
    s.account_id = boost::uuids::random_generator()();
    s.tenant_id = tenant_id::system();
    s.start_time = std::chrono::system_clock::now();
    s.end_time = std::nullopt;
    s.client_ip = boost::asio::ip::make_address("192.168.1.1");
    s.client_identifier = "test-client";
    s.client_version_major = 1;
    s.client_version_minor = 2;
    s.bytes_sent = 1024;
    s.bytes_received = 2048;
    s.country_code = "GB";
    s.protocol = session_protocol::binary;
    s.username = "testuser";
    return s;
}

}

TEST_CASE("to_session_data_preserves_all_fields", tags) {
    auto s = make_test_session();
    auto d = session_converter::to_session_data(s);

    CHECK(d->id == s.id);
    CHECK(d->account_id == s.account_id);
    CHECK(d->tenant_id == s.tenant_id);
    CHECK(d->start_time == s.start_time);
    CHECK(d->client_ip == s.client_ip);
    CHECK(d->client_identifier == s.client_identifier);
    CHECK(d->client_version_major == s.client_version_major);
    CHECK(d->client_version_minor == s.client_version_minor);
    CHECK(d->bytes_sent == s.bytes_sent);
    CHECK(d->bytes_received == s.bytes_received);
    CHECK(d->country_code == s.country_code);
    CHECK(d->username == s.username);
}

TEST_CASE("from_session_data_preserves_all_fields", tags) {
    auto s = make_test_session();
    auto d = session_converter::to_session_data(s);
    auto roundtrip = session_converter::from_session_data(*d);

    CHECK(roundtrip.id == s.id);
    CHECK(roundtrip.account_id == s.account_id);
    CHECK(roundtrip.tenant_id == s.tenant_id);
    CHECK(roundtrip.start_time == s.start_time);
    CHECK(roundtrip.client_ip == s.client_ip);
    CHECK(roundtrip.client_identifier == s.client_identifier);
    CHECK(roundtrip.client_version_major == s.client_version_major);
    CHECK(roundtrip.client_version_minor == s.client_version_minor);
    CHECK(roundtrip.bytes_sent == s.bytes_sent);
    CHECK(roundtrip.bytes_received == s.bytes_received);
    CHECK(roundtrip.country_code == s.country_code);
    CHECK(roundtrip.protocol == s.protocol);
    CHECK(roundtrip.username == s.username);
}

TEST_CASE("to_session_data_converts_binary_protocol", tags) {
    auto s = make_test_session();
    s.protocol = session_protocol::binary;
    auto d = session_converter::to_session_data(s);

    CHECK(d->protocol == ores::comms::service::session_protocol::binary);
}

TEST_CASE("to_session_data_converts_http_protocol", tags) {
    auto s = make_test_session();
    s.protocol = session_protocol::http;
    auto d = session_converter::to_session_data(s);

    CHECK(d->protocol == ores::comms::service::session_protocol::http);
}

TEST_CASE("update_from_session_data_updates_mutable_fields_only", tags) {
    auto s = make_test_session();
    auto original_id = s.id;
    auto original_ip = s.client_ip;

    ores::comms::service::session_data d;
    d.end_time = std::make_optional(std::chrono::system_clock::now());
    d.bytes_sent = 9999;
    d.bytes_received = 8888;

    session_converter::update_from_session_data(s, d);

    // Mutable fields updated
    CHECK(s.end_time == d.end_time);
    CHECK(s.bytes_sent == 9999);
    CHECK(s.bytes_received == 8888);

    // Immutable fields preserved
    CHECK(s.id == original_id);
    CHECK(s.client_ip == original_ip);
}
