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
#include "ores.vault.api/messaging/secret_operations_protocol.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::vault::messaging::write_secret_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::write_secret_request>();
}

TEST_CASE("ores::vault::messaging::write_secret_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::write_secret_response>();
}

TEST_CASE("ores::vault::messaging::describe_secret_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::describe_secret_request>();
}

TEST_CASE("ores::vault::messaging::describe_secret_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::describe_secret_response>();
}

TEST_CASE("ores::vault::messaging::resolve_secret_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::resolve_secret_request>();
}

TEST_CASE("ores::vault::messaging::resolve_secret_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::resolve_secret_response>();
}

TEST_CASE("ores::vault::messaging::remove_secret_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::remove_secret_request>();
}

TEST_CASE("ores::vault::messaging::remove_secret_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::remove_secret_response>();
}

TEST_CASE("ores::vault::messaging::rotate_secrets_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::rotate_secrets_request>();
}

TEST_CASE("ores::vault::messaging::rotate_secrets_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::rotate_secrets_response>();
}

TEST_CASE("ores::vault::messaging::vault_public_key_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::vault_public_key_request>();
}

TEST_CASE("ores::vault::messaging::vault_public_key_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::vault::messaging::vault_public_key_response>();
}
