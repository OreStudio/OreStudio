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
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_environment_override.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[ores.testing.nats_options]");

}

TEST_CASE("make_nats_options reads every setting from the environment", tags) {
    const ores::testing::scoped_environment_override env({
        {"ORES_NATS_URL", "nats://broker.test:4222"},
        {"ORES_NATS_SUBJECT_PREFIX", "ores.festive"},
        {"ORES_NATS_TLS_CA", "/keys/ca.pem"},
        {"ORES_NATS_TLS_CERT", "/keys/client.pem"},
        {"ORES_NATS_TLS_KEY", "/keys/client.key"},
    });

    const auto opts = ores::testing::make_nats_options();

    CHECK(opts.url == "nats://broker.test:4222");
    CHECK(opts.subject_prefix == "ores.festive");
    CHECK(opts.tls_ca_cert == "/keys/ca.pem");
    CHECK(opts.tls_client_cert == "/keys/client.pem");
    CHECK(opts.tls_client_key == "/keys/client.key");
}

TEST_CASE("make_nats_options falls back to the local broker", tags) {
    const ores::testing::scoped_environment_override env({}, {"ORES_NATS_URL"});

    const auto opts = ores::testing::make_nats_options();

    CHECK(opts.url == "nats://localhost:4222");
    CHECK(opts.subject_prefix.empty());
    CHECK(opts.tls_ca_cert.empty());
}
