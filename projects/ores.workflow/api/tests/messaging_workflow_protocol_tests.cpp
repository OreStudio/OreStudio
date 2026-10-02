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
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <cstddef>
#include <filesystem>
#include <fstream>
#include <rfl/msgpack.hpp>
#include <span>
#include <sstream>
#include <string>
#include <vector>

namespace {

const std::string tags("[messaging]");

/**
 * @brief Reads a hex fixture the TypeScript client wrote, as wire bytes.
 *
 * The build states the fixture directory, because the test runs from the
 * output directory and its own source path is relative to the build tree.
 */
std::vector<std::byte> read_hex_fixture(const std::string& name) {
    const auto path = std::filesystem::path(ORES_WORKFLOW_API_TEST_FIXTURES) / name;
    std::ifstream in(path);
    REQUIRE(in.good());
    std::stringstream text;
    text << in.rdbuf();

    std::vector<std::byte> bytes;
    std::string hex;
    for (const char c : text.str())
        if (std::isxdigit(static_cast<unsigned char>(c)))
            hex.push_back(c);
    REQUIRE(hex.size() % 2 == 0);
    for (std::size_t i = 0; i < hex.size(); i += 2)
        bytes.push_back(static_cast<std::byte>(std::stoi(hex.substr(i, 2), nullptr, 16)));
    return bytes;
}

}

using ores::workflow::messaging::list_workflow_instance_summaries_request;

/*
 * The fixture is what ores.web's roster sends, written by its
 * workflow-boundary test. A request the client encodes without a field this
 * struct requires fails here, which is how a hand-written client once failed
 * every request at run time with "Invalid request payload".
 */
TEST_CASE("the instances list request the web client sends decodes", tags) {
    const auto bytes = read_hex_fixture("list_workflow_instance_summaries_request.msgpack.hex");

    const auto decoded = rfl::msgpack::read<list_workflow_instance_summaries_request>(
        std::span<const std::byte>(bytes));
    REQUIRE(decoded);

    CHECK(decoded->limit == 1000);
    CHECK(decoded->status_filter.empty());
    CHECK(decoded->type_filter == "provision_tenant_workflow");
    CHECK(decoded->target_kind_filter == "tenant");
    CHECK(decoded->target_id_filter.empty());
}
