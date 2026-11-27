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
#include "ores.vault.service/app/provisioning.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <stdexcept>
#include <string>

using namespace ores::vault::service::app;
using Catch::Matchers::ContainsSubstring;

namespace {

constexpr std::string_view document = R"({
  "consumers": [
    {"name": "iam_service@system.ores",
     "actor": "iam_service@system.ores",
     "public_key": "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8="}
  ],
  "secrets": [
    {"name": "db.iam_service.password",
     "owner": "iam_service@system.ores",
     "value": "the password the deployment generated"}
  ]
})";

}

TEST_CASE("a provisioning document reads its consumers and its secrets", "[provisioning]") {
    const auto read = provision_document::read(document);

    REQUIRE(read.consumers.size() == 1);
    REQUIRE(read.consumers[0].name == "iam_service@system.ores");
    REQUIRE(read.consumers[0].actor == "iam_service@system.ores");
    REQUIRE(read.consumers[0].public_key == "AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=");

    REQUIRE(read.secrets.size() == 1);
    REQUIRE(read.secrets[0].name == "db.iam_service.password");
    REQUIRE(read.secrets[0].owner == "iam_service@system.ores");
    REQUIRE(read.secrets[0].value == "the password the deployment generated");
}

TEST_CASE("a document that names no consumers reads as one with no consumers", "[provisioning]") {
    const auto read =
        provision_document::read(R"({"secrets": [{"name": "n", "owner": "o", "value": "v"}]})");

    REQUIRE(read.consumers.empty());
    REQUIRE(read.secrets.size() == 1);
    REQUIRE(read.secrets[0].name == "n");
}

TEST_CASE("an entry that leaves a field out is refused, naming the field", "[provisioning]") {
    REQUIRE_THROWS_WITH(provision_document::read(R"({"consumers": [{"name": "n", "actor": "a"}]})"),
                        ContainsSubstring("no public_key"));

    REQUIRE_THROWS_WITH(provision_document::read(R"({"secrets": [{"name": "n"}]})"),
                        ContainsSubstring("no owner"));
}

TEST_CASE("a document that is not JSON is refused", "[provisioning]") {
    REQUIRE_THROWS_WITH(provision_document::read("db.iam_service.password = hunter2"),
                        ContainsSubstring("does not parse"));
}

TEST_CASE("a document that is not an object is refused", "[provisioning]") {
    REQUIRE_THROWS_WITH(provision_document::read("[]"), ContainsSubstring("is not an object"));
}
