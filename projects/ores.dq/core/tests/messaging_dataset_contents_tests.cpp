/** -*- mode: c++-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * @file
 * @brief The dataset contents verb, driven through the handler.
 *
 * The verb answers what a dataset has staged: the tables its artefact type
 * names, then one page of one table's rows. Four facts pin it: the list states
 * the family's tables with their own columns, a page carries the columns with
 * the rows, a table outside the family is refused rather than read, and the verb
 * asks for the read and not the write.
 */
#include "ores.dq.api/messaging/dataset_contents_protocol.hpp"
#include "ores.dq.core/messaging/dataset_contents_handler.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.testing/handler_test_support.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string contents_subject("ores.test.dq.dataset_contents.");
const std::string contents_method("dataset_contents");

/**
 * @brief The contents handler on a subject of its own, over the test tenant.
 */
struct dataset_contents_fixture {
    ores::testing::scoped_database_helper db;
    ores::nats::service::client nats;
    ores::testing::handler_test_keys keys;
    std::vector<ores::nats::service::subscription> subs;

    dataset_contents_fixture()
        : nats(ores::testing::make_nats_options()) {
        nats.connect();
        REQUIRE(nats.is_connected());
        subs.push_back(nats.queue_subscribe(
            contents_subject + contents_method, "ores.test", [this](ores::nats::message m) {
                ores::dq::messaging::dataset_contents_handler(
                    nats, db.context(), std::optional(keys.verifier()))
                    .dataset_contents(std::move(m));
            }));
    }

    ores::dq::messaging::dataset_contents_response contents(const std::string& dataset_code,
                                                            const std::string& table_name = "",
                                                            int limit = 50,
                                                            int offset = 0) {
        const auto token =
            keys.token_without_party(db.tenant_id().to_string(), {"dq::datasets:read"});
        ores::dq::messaging::dataset_contents_request req;
        req.dataset_code = dataset_code;
        req.table_name = table_name;
        req.limit = limit;
        req.offset = offset;
        const auto reply =
            ores::testing::request(nats, contents_subject + contents_method, token, req);
        const auto decoded =
            ores::testing::decode_reply<ores::dq::messaging::dataset_contents_response>(reply);
        REQUIRE(decoded);
        return *decoded;
    }
};

} // namespace

TEST_CASE("the contents list the tables a dataset stages into", "[messaging][contents]") {
    dataset_contents_fixture f;
    const auto resp = f.contents("refdata.conventions");

    REQUIRE(resp.success);
    CHECK(resp.error_message.empty());
    REQUIRE_FALSE(resp.tables.empty());

    // A family, not a row: the conventions stage into many tables.
    CHECK(resp.tables.size() > 1);
    for (const auto& table : resp.tables) {
        CHECK(table.table_name.starts_with("ores_"));
        CHECK(table.display_order > 0);
        // The two columns every family shares are stated by the request, so the
        // list does not repeat them.
        CHECK(table.columns.find("dataset_id") == std::string::npos);
        CHECK(table.columns.find("tenant_id") == std::string::npos);
    }
    // The rows are read only when a table is named.
    CHECK(resp.rows.empty());
}

TEST_CASE("a page of one table carries the columns with its rows", "[messaging][contents]") {
    dataset_contents_fixture f;
    const auto tables = f.contents("refdata.conventions");
    REQUIRE(tables.success);
    REQUIRE_FALSE(tables.tables.empty());
    const auto& table = tables.tables.front();

    const auto resp = f.contents("refdata.conventions", table.table_name, 2, 0);
    REQUIRE(resp.success);
    CHECK(resp.error_message.empty());
    REQUIRE_FALSE(resp.rows.empty());

    // The page is JSON the screen reads: the columns it carries, the rows, and
    // how many the table holds.
    CHECK(resp.rows.find("\"columns\"") != std::string::npos);
    CHECK(resp.rows.find("\"rows\"") != std::string::npos);
    CHECK(resp.rows.find("\"total\"") != std::string::npos);
    CHECK(resp.rows.find("dataset_id") == std::string::npos);
    CHECK(resp.rows.find("tenant_id") == std::string::npos);
}

TEST_CASE("a table outside the dataset family is refused", "[messaging][contents]") {
    dataset_contents_fixture f;
    const auto resp = f.contents("refdata.conventions", "ores_dq_currencies_artefact_tbl", 2, 0);

    CHECK_FALSE(resp.success);
    CHECK(resp.error_message.find("ores_dq_currencies_artefact_tbl") != std::string::npos);
    CHECK(resp.rows.empty());
}

TEST_CASE("a dataset that stages into no table is refused", "[messaging][contents]") {
    dataset_contents_fixture f;
    const auto resp = f.contents("no.such.dataset");

    CHECK_FALSE(resp.success);
    CHECK(resp.error_message.find("no.such.dataset") != std::string::npos);
    CHECK(resp.tables.empty());
}
