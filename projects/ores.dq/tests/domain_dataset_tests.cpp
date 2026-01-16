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
#include "ores.dq/domain/dataset.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/dataset_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/dataset_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::dataset;
using namespace ores::logging;

TEST_CASE("create_dataset_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    dataset sut;
    sut.version = 1;
    sut.id = boost::uuids::random_generator()();
    sut.catalog_name = "ISO Standards";
    sut.subject_area_name = "Currencies";
    sut.domain_name = "Reference Data";
    sut.coding_scheme_code = "ISO-4217";
    sut.origin_code = "external";
    sut.nature_code = "raw";
    sut.treatment_code = "live";
    sut.name = "Currency Codes";
    sut.description = "ISO 4217 currency codes dataset";
    sut.source_system_id = "ISO";
    sut.business_context = "Reference currency data";
    sut.lineage_depth = 0;
    sut.as_of_date = std::chrono::system_clock::now();
    sut.ingestion_timestamp = std::chrono::system_clock::now();
    sut.recorded_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Dataset: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.catalog_name == "ISO Standards");
    CHECK(sut.subject_area_name == "Currencies");
    CHECK(sut.domain_name == "Reference Data");
    CHECK(sut.coding_scheme_code == "ISO-4217");
    CHECK(sut.origin_code == "external");
    CHECK(sut.nature_code == "raw");
    CHECK(sut.treatment_code == "live");
    CHECK(sut.name == "Currency Codes");
    CHECK(sut.lineage_depth == 0);
}

TEST_CASE("dataset_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    dataset ds;
    ds.version = 1;
    ds.id = boost::uuids::random_generator()();
    ds.subject_area_name = "Countries";
    ds.domain_name = "Reference Data";
    ds.origin_code = "external";
    ds.nature_code = "raw";
    ds.treatment_code = "live";
    ds.name = "Country Codes";
    ds.description = "ISO 3166 country codes";
    ds.source_system_id = "ISO";
    ds.business_context = "Reference country data";
    ds.as_of_date = std::chrono::system_clock::now();
    ds.ingestion_timestamp = std::chrono::system_clock::now();
    ds.recorded_by = "system";

    std::vector<dataset> datasets = {ds};
    auto table = convert_to_table(datasets);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("Country Codes") != std::string::npos);
}

TEST_CASE("dataset_with_lineage", tags) {
    auto lg(make_logger(test_suite));

    auto source_id = boost::uuids::random_generator()();

    dataset derived;
    derived.version = 1;
    derived.id = boost::uuids::random_generator()();
    derived.subject_area_name = "Currencies";
    derived.domain_name = "Reference Data";
    derived.origin_code = "internal";
    derived.nature_code = "derived";
    derived.treatment_code = "live";
    derived.name = "Enriched Currency Data";
    derived.description = "Currency data with additional attributes";
    derived.source_system_id = "DataPlatform";
    derived.business_context = "Enhanced currency reference";
    derived.upstream_derivation_id = source_id;
    derived.lineage_depth = 1;
    derived.as_of_date = std::chrono::system_clock::now();
    derived.ingestion_timestamp = std::chrono::system_clock::now();
    derived.recorded_by = "etl_process";

    BOOST_LOG_SEV(lg, info) << "Derived dataset: " << derived;

    CHECK(derived.upstream_derivation_id == source_id);
    CHECK(derived.lineage_depth == 1);
    CHECK(derived.origin_code == "internal");
    CHECK(derived.nature_code == "derived");
}
