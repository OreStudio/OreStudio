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
#include "ores.compute.api/domain/batch.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/batch_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.api/domain/batch_table.hpp"

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[domain]");

}

using ores::compute::domain::batch;
using namespace ores::compute::domain;
using namespace ores::logging;

TEST_CASE("create_batch_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    batch sut;
    sut.version = 1;
    sut.modified_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.external_ref = "RISK-RUN-2026-03-22";
    sut.status = "open";

    BOOST_LOG_SEV(lg, info) << "Batch: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "admin");
    CHECK(sut.external_ref == "RISK-RUN-2026-03-22");
    CHECK(sut.status == "open");
}

TEST_CASE("create_batch_in_processing_state", tags) {
    auto lg(make_logger(test_suite));

    batch sut;
    sut.version = 2;
    sut.modified_by = "scheduler";
    sut.id = boost::uuids::random_generator()();
    sut.external_ref = "RISK-RUN-2026-03-21";
    sut.status = "processing";

    BOOST_LOG_SEV(lg, info) << "Batch: " << sut;

    CHECK(sut.status == "processing");
}

TEST_CASE("create_batch_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440010");

    batch sut;
    sut.version = 1;
    sut.modified_by = "system";
    sut.id = specific_id;
    sut.external_ref = "BATCH-001";
    sut.status = "closed";

    BOOST_LOG_SEV(lg, info) << "Batch: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.external_ref == "BATCH-001");
}

TEST_CASE("batch_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    batch sut;
    sut.version = 1;
    sut.modified_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.external_ref = "TEST-BATCH-001";
    sut.status = "open";

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Batch JSON: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("TEST-BATCH-001") != std::string::npos);
}

TEST_CASE("batch_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    batch b;
    b.version = 1;
    b.modified_by = "admin";
    b.id = boost::uuids::random_generator()();
    b.external_ref = "RISK-RUN-2026-03-22";
    b.status = "open";

    std::vector<batch> batches = {b};
    auto table = convert_to_table(batches);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("RISK-RUN-2026-03-22") != std::string::npos);
}

TEST_CASE("batch_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> statuses = {"open", "processing", "closed"};
    std::vector<batch> batches;
    for (int i = 0; i < 3; ++i) {
        batch b;
        b.version = i + 1;
        b.modified_by = "system";
        b.id = boost::uuids::random_generator()();
        b.external_ref = "BATCH-00" + std::to_string(i);
        b.status = statuses[i];
        batches.push_back(b);
    }

    auto table = convert_to_table(batches);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("BATCH-000") != std::string::npos);
    CHECK(table.find("BATCH-001") != std::string::npos);
    CHECK(table.find("BATCH-002") != std::string::npos);
}

TEST_CASE("batch_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<batch> batches;
    auto table = convert_to_table(batches);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("create_batch_with_faker", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> statuses = {"open", "processing", "assimilating", "closed"};

    batch sut;
    sut.version = faker::number::integer(1, 10);
    sut.modified_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.external_ref = std::string(faker::word::noun()) + "-" +
        std::to_string(faker::number::integer(1000, 9999));
    sut.status = statuses[faker::number::integer(0, 3)];

    BOOST_LOG_SEV(lg, info) << "Batch: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.external_ref.empty());
    CHECK(!sut.status.empty());
}

TEST_CASE("batch_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    const std::vector<std::string> statuses = {"open", "processing", "assimilating", "closed"};
    std::vector<batch> batches;
    for (int i = 0; i < 5; ++i) {
        batch b;
        b.version = faker::number::integer(1, 10);
        b.modified_by = std::string(faker::internet::username());
        b.id = boost::uuids::random_generator()();
        b.external_ref = std::string(faker::word::noun()) + "-" +
            std::to_string(faker::number::integer(1000, 9999));
        b.status = statuses[faker::number::integer(0, 3)];
        batches.push_back(b);
    }

    auto table = convert_to_table(batches);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& b : batches) {
        CHECK(table.find(b.external_ref) != std::string::npos);
    }
}
