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
#include "ores.compute.api/domain/host.hpp"

#include <sstream>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.compute.api/domain/host_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.api/domain/host_table.hpp"

namespace {

const std::string_view test_suite("ores.compute.tests");
const std::string tags("[domain]");

}

using ores::compute::domain::host;
using namespace ores::compute::domain;
using namespace ores::logging;

TEST_CASE("create_host_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    host sut;
    sut.version = 1;
    sut.modified_by = "admin";
    sut.id = boost::uuids::random_generator()();
    sut.external_id = "compute-node-01";
    sut.location = "us-east-1";
    sut.cpu_count = 32;
    sut.ram_mb = 65536;
    sut.gpu_type = "A100";
    sut.last_rpc_time = std::chrono::system_clock::now();
    sut.credit_total = 1234.5;

    BOOST_LOG_SEV(lg, info) << "Host: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.modified_by == "admin");
    CHECK(sut.external_id == "compute-node-01");
    CHECK(sut.location == "us-east-1");
    CHECK(sut.cpu_count == 32);
    CHECK(sut.ram_mb == 65536);
    CHECK(sut.gpu_type == "A100");
    CHECK(sut.credit_total == 1234.5);
}

TEST_CASE("create_host_without_gpu", tags) {
    auto lg(make_logger(test_suite));

    host sut;
    sut.version = 1;
    sut.modified_by = "system";
    sut.id = boost::uuids::random_generator()();
    sut.external_id = "cpu-node-05";
    sut.location = "eu-west-2";
    sut.cpu_count = 16;
    sut.ram_mb = 32768;
    sut.gpu_type = "";
    sut.last_rpc_time = std::chrono::system_clock::now();
    sut.credit_total = 0.0;

    BOOST_LOG_SEV(lg, info) << "Host: " << sut;

    CHECK(sut.cpu_count == 16);
    CHECK(sut.gpu_type.empty());
    CHECK(sut.credit_total == 0.0);
}

TEST_CASE("create_host_with_specific_uuid", tags) {
    auto lg(make_logger(test_suite));

    boost::uuids::string_generator uuid_gen;
    const auto specific_id = uuid_gen("550e8400-e29b-41d4-a716-446655440020");

    host sut;
    sut.version = 2;
    sut.modified_by = "updater";
    sut.id = specific_id;
    sut.external_id = "worker-node-99";
    sut.location = "ap-southeast-1";
    sut.cpu_count = 64;
    sut.ram_mb = 131072;
    sut.gpu_type = "H100";
    sut.last_rpc_time = std::chrono::system_clock::now();
    sut.credit_total = 9999.99;

    BOOST_LOG_SEV(lg, info) << "Host: " << sut;

    CHECK(sut.version == 2);
    CHECK(sut.external_id == "worker-node-99");
}

TEST_CASE("host_insertion_operator", tags) {
    auto lg(make_logger(test_suite));

    host sut;
    sut.version = 1;
    sut.modified_by = "developer";
    sut.id = boost::uuids::random_generator()();
    sut.external_id = "test-node-01";
    sut.location = "local";
    sut.cpu_count = 8;
    sut.ram_mb = 16384;
    sut.gpu_type = "";
    sut.last_rpc_time = std::chrono::system_clock::now();
    sut.credit_total = 0.0;

    std::ostringstream os;
    os << sut;
    const std::string json_output = os.str();

    BOOST_LOG_SEV(lg, info) << "Host JSON: " << json_output;

    CHECK(!json_output.empty());
    CHECK(json_output.find("test-node-01") != std::string::npos);
}

TEST_CASE("host_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    host h;
    h.version = 1;
    h.modified_by = "admin";
    h.id = boost::uuids::random_generator()();
    h.external_id = "compute-node-01";
    h.location = "us-east-1";
    h.cpu_count = 32;
    h.ram_mb = 65536;
    h.gpu_type = "A100";
    h.last_rpc_time = std::chrono::system_clock::now();
    h.credit_total = 1234.5;

    std::vector<host> hosts = {h};
    auto table = convert_to_table(hosts);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("compute-node-01") != std::string::npos);
}

TEST_CASE("host_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<host> hosts;
    for (int i = 0; i < 3; ++i) {
        host h;
        h.version = i + 1;
        h.modified_by = "system";
        h.id = boost::uuids::random_generator()();
        h.external_id = "node-" + std::to_string(i);
        h.location = "dc-" + std::to_string(i);
        h.cpu_count = 8 * (i + 1);
        h.ram_mb = 8192 * (i + 1);
        h.gpu_type = "";
        h.last_rpc_time = std::chrono::system_clock::now();
        h.credit_total = static_cast<double>(i * 100);
        hosts.push_back(h);
    }

    auto table = convert_to_table(hosts);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("node-0") != std::string::npos);
    CHECK(table.find("node-1") != std::string::npos);
    CHECK(table.find("node-2") != std::string::npos);
}

TEST_CASE("host_convert_empty_vector_to_table", tags) {
    auto lg(make_logger(test_suite));

    std::vector<host> hosts;
    auto table = convert_to_table(hosts);

    BOOST_LOG_SEV(lg, info) << "Empty table output:\n" << table;

    CHECK(!table.empty()); // Table should still have headers
}

TEST_CASE("create_host_with_faker", tags) {
    auto lg(make_logger(test_suite));

    host sut;
    sut.version = faker::number::integer(1, 10);
    sut.modified_by = std::string(faker::internet::username());
    sut.id = boost::uuids::random_generator()();
    sut.external_id = std::string(faker::word::noun()) + "-" +
        std::to_string(faker::number::integer(1, 99));
    sut.location = std::string(faker::location::city());
    sut.cpu_count = faker::number::integer(4, 128);
    sut.ram_mb = faker::number::integer(8192, 131072);
    sut.gpu_type = "";
    sut.last_rpc_time = std::chrono::system_clock::now();
    sut.credit_total = static_cast<double>(faker::number::integer(0, 100000));

    BOOST_LOG_SEV(lg, info) << "Host: " << sut;

    CHECK(sut.version >= 1);
    CHECK(sut.version <= 10);
    CHECK(!sut.modified_by.empty());
    CHECK(!sut.external_id.empty());
    CHECK(sut.cpu_count >= 4);
    CHECK(sut.ram_mb >= 8192);
}

TEST_CASE("host_table_with_faker_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<host> hosts;
    for (int i = 0; i < 5; ++i) {
        host h;
        h.version = faker::number::integer(1, 10);
        h.modified_by = std::string(faker::internet::username());
        h.id = boost::uuids::random_generator()();
        h.external_id = std::string(faker::word::noun()) + "-" +
            std::to_string(faker::number::integer(1, 99));
        h.location = std::string(faker::location::city());
        h.cpu_count = faker::number::integer(4, 128);
        h.ram_mb = faker::number::integer(8192, 131072);
        h.gpu_type = "";
        h.last_rpc_time = std::chrono::system_clock::now();
        h.credit_total = static_cast<double>(faker::number::integer(0, 100000));
        hosts.push_back(h);
    }

    auto table = convert_to_table(hosts);

    BOOST_LOG_SEV(lg, info) << "Faker table output:\n" << table;

    CHECK(!table.empty());
    for (const auto& h : hosts) {
        CHECK(table.find(h.external_id) != std::string::npos);
    }
}
