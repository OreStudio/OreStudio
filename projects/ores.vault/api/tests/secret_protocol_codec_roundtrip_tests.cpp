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
 * Template: cpp_protocol_codec_roundtrip_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.vault.api/domain/secret_codec.hpp"
#include "ores.vault.api/messaging/secret_protocol.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::vault::messaging::secret_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_key>();
}

TEST_CASE("ores::vault::messaging::secret_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_write>();
}

TEST_CASE("ores::vault::messaging::secret_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_change>();
}

TEST_CASE("ores::vault::messaging::secret_removal", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_removal>();
}

TEST_CASE("ores::vault::messaging::secret_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_lookup>();
}

TEST_CASE("ores::vault::messaging::secrets_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secrets_filter>();
}

TEST_CASE("ores::vault::messaging::secret_event", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_event>();
}

TEST_CASE("ores::vault::messaging::secret_version_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_version_key>();
}

TEST_CASE("ores::vault::messaging::secret_versions_filter", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::secret_versions_filter>();
}

TEST_CASE("ores::vault::messaging::list_secrets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::list_secrets_request>();
}

TEST_CASE("ores::vault::messaging::list_secrets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::list_secrets_response>();
}

TEST_CASE("ores::vault::messaging::get_secret_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::get_secret_request>();
}

TEST_CASE("ores::vault::messaging::get_secret_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::get_secret_response>();
}

TEST_CASE("ores::vault::messaging::get_many_secrets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::get_many_secrets_request>();
}

TEST_CASE("ores::vault::messaging::get_many_secrets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::get_many_secrets_response>();
}

TEST_CASE("ores::vault::messaging::put_secret_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::put_secret_request>();
}

TEST_CASE("ores::vault::messaging::put_secret_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::put_secret_response>();
}

TEST_CASE("ores::vault::messaging::put_many_secrets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::put_many_secrets_request>();
}

TEST_CASE("ores::vault::messaging::put_many_secrets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::put_many_secrets_response>();
}

TEST_CASE("ores::vault::messaging::delete_secret_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::delete_secret_request>();
}

TEST_CASE("ores::vault::messaging::delete_secret_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::delete_secret_response>();
}

TEST_CASE("ores::vault::messaging::delete_many_secrets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::delete_many_secrets_request>();
}

TEST_CASE("ores::vault::messaging::delete_many_secrets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::delete_many_secrets_response>();
}

TEST_CASE("ores::vault::messaging::list_secret_versions_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::list_secret_versions_request>();
}

TEST_CASE("ores::vault::messaging::list_secret_versions_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::list_secret_versions_response>();
}

TEST_CASE("ores::vault::messaging::get_secret_version_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::get_secret_version_request>();
}

TEST_CASE("ores::vault::messaging::get_secret_version_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::get_secret_version_response>();
}
