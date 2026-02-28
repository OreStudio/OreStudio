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
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/random_generator.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/messaging/data_organization_protocol.hpp"
#include "ores.dq/messaging/dataset_protocol.hpp"
#include "ores.dq/messaging/coding_scheme_protocol.hpp"
#include "ores.dq/messaging/dimension_protocol.hpp"
#include "ores.dq/messaging/change_management_protocol.hpp"
#include "ores.dq/messaging/dataset_bundle_protocol.hpp"
#include "ores.dq/messaging/dataset_bundle_member_protocol.hpp"
#include "ores.dq/messaging/dataset_dependency_protocol.hpp"
#include "ores.dq/messaging/publication_protocol.hpp"
#include "ores.dq/messaging/lei_entity_summary_protocol.hpp"
#include "ores.dq/messaging/publish_bundle_protocol.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[messaging]");

}

using namespace ores::logging;
using namespace ores::dq::messaging;
using namespace ores::dq::domain;

// ============================================================================
// Data Organization Protocol Tests
// ============================================================================

TEST_CASE("get_catalogs_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_catalogs_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_catalogs_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_catalog_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_catalog_request e; e.catalogs.emplace_back();
    e.catalogs[0].version = 1;
    e.catalogs[0].name = "ISO Standards";
    e.catalogs[0].description = "International standards catalogs";
    e.catalogs[0].owner = "Standards Body";
    e.catalogs[0].modified_by = "admin";
    e.catalogs[0].change_commentary = "Initial creation";
    e.catalogs[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_catalog_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.catalogs[0].version == e.catalogs[0].version);
    CHECK(a.catalogs[0].name == e.catalogs[0].name);
    CHECK(a.catalogs[0].description == e.catalogs[0].description);
    CHECK(a.catalogs[0].owner == e.catalogs[0].owner);
}

TEST_CASE("get_data_domains_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_data_domains_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_data_domains_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_data_domain_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_data_domain_request e; e.domains.emplace_back();
    e.domains[0].version = 1;
    e.domains[0].name = "Reference Data";
    e.domains[0].description = "Static reference data";
    e.domains[0].modified_by = "admin";
    e.domains[0].change_commentary = "Initial creation";
    e.domains[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_data_domain_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.domains[0].version == e.domains[0].version);
    CHECK(a.domains[0].name == e.domains[0].name);
    CHECK(a.domains[0].description == e.domains[0].description);
}

TEST_CASE("get_subject_areas_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_subject_areas_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_subject_areas_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("get_subject_areas_by_domain_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_subject_areas_by_domain_request e;
    e.domain_name = "Reference Data";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_subject_areas_by_domain_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.domain_name == e.domain_name);
}

TEST_CASE("save_subject_area_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_subject_area_request e; e.subject_areas.emplace_back();
    e.subject_areas[0].version = 1;
    e.subject_areas[0].name = "Currencies";
    e.subject_areas[0].domain_name = "Reference Data";
    e.subject_areas[0].description = "Currency reference data";
    e.subject_areas[0].modified_by = "admin";
    e.subject_areas[0].change_commentary = "Initial creation";
    e.subject_areas[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_subject_area_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.subject_areas[0].version == e.subject_areas[0].version);
    CHECK(a.subject_areas[0].name == e.subject_areas[0].name);
    CHECK(a.subject_areas[0].domain_name == e.subject_areas[0].domain_name);
}

// ============================================================================
// Dataset Protocol Tests
// ============================================================================

TEST_CASE("get_datasets_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_datasets_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_datasets_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_dataset_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_dataset_request e; e.datasets.emplace_back();
    e.datasets[0].version = 1;
    e.datasets[0].id = boost::uuids::random_generator()();
    e.datasets[0].catalog_name = "ISO Standards";
    e.datasets[0].subject_area_name = "Currencies";
    e.datasets[0].domain_name = "Reference Data";
    e.datasets[0].coding_scheme_code = "ISO_4217";
    e.datasets[0].origin_code = "Source";
    e.datasets[0].nature_code = "Actual";
    e.datasets[0].treatment_code = "Raw";
    e.datasets[0].methodology_id = boost::uuids::random_generator()();
    e.datasets[0].name = "Currency List";
    e.datasets[0].description = "List of all ISO 4217 currencies";
    e.datasets[0].source_system_id = "ISO_FEED";
    e.datasets[0].business_context = "Reference data for FX trading";
    e.datasets[0].upstream_derivation_id = std::nullopt;
    e.datasets[0].lineage_depth = 0;
    e.datasets[0].as_of_date = std::chrono::system_clock::now();
    e.datasets[0].ingestion_timestamp = std::chrono::system_clock::now();
    e.datasets[0].license_info = "Public domain";
    e.datasets[0].modified_by = "admin";
    e.datasets[0].change_commentary = "Initial creation";
    e.datasets[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_dataset_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.datasets[0].version == e.datasets[0].version);
    CHECK(a.datasets[0].id == e.datasets[0].id);
    CHECK(a.datasets[0].catalog_name == e.datasets[0].catalog_name);
    CHECK(a.datasets[0].name == e.datasets[0].name);
}

TEST_CASE("get_methodologies_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_methodologies_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_methodologies_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_methodology_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_methodology_request e; e.methodologies.emplace_back();
    e.methodologies[0].version = 1;
    e.methodologies[0].id = boost::uuids::random_generator()();
    e.methodologies[0].name = "Standard ETL";
    e.methodologies[0].description = "Standard extract-transform-load process";
    e.methodologies[0].logic_reference = "https://docs.example.com/etl";
    e.methodologies[0].implementation_details = "Python script using pandas";
    e.methodologies[0].modified_by = "admin";
    e.methodologies[0].change_commentary = "Initial creation";
    e.methodologies[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_methodology_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.methodologies[0].version == e.methodologies[0].version);
    CHECK(a.methodologies[0].id == e.methodologies[0].id);
    CHECK(a.methodologies[0].name == e.methodologies[0].name);
}

// ============================================================================
// Coding Scheme Protocol Tests
// ============================================================================

TEST_CASE("get_coding_schemes_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_coding_schemes_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_coding_schemes_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_coding_scheme_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_coding_scheme_request e; e.schemes.emplace_back();
    e.schemes[0].version = 1;
    e.schemes[0].code = "ISO_4217";
    e.schemes[0].name = "ISO 4217 Currency Codes";
    e.schemes[0].authority_type = "official";
    e.schemes[0].subject_area_name = "Currencies";
    e.schemes[0].domain_name = "Reference Data";
    e.schemes[0].uri = "https://www.iso.org/iso-4217-currency-codes.html";
    e.schemes[0].description = "Standard currency codes";
    e.schemes[0].modified_by = "admin";
    e.schemes[0].change_commentary = "Initial creation";
    e.schemes[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_coding_scheme_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.schemes[0].version == e.schemes[0].version);
    CHECK(a.schemes[0].code == e.schemes[0].code);
    CHECK(a.schemes[0].name == e.schemes[0].name);
}

TEST_CASE("get_coding_scheme_authority_types_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_coding_scheme_authority_types_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_coding_scheme_authority_types_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_coding_scheme_authority_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_coding_scheme_authority_type_request e; e.authority_types.emplace_back();
    e.authority_types[0].version = 1;
    e.authority_types[0].code = "official";
    e.authority_types[0].name = "Official Standards";
    e.authority_types[0].description = "Standards from official bodies like ISO";
    e.authority_types[0].modified_by = "admin";
    e.authority_types[0].change_commentary = "Initial creation";
    e.authority_types[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_coding_scheme_authority_type_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.authority_types[0].version == e.authority_types[0].version);
    CHECK(a.authority_types[0].code == e.authority_types[0].code);
    CHECK(a.authority_types[0].name == e.authority_types[0].name);
}

// ============================================================================
// Dimension Protocol Tests
// ============================================================================

TEST_CASE("get_nature_dimensions_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_nature_dimensions_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_nature_dimensions_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_nature_dimension_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_nature_dimension_request e; e.dimensions.emplace_back();
    e.dimensions[0].version = 1;
    e.dimensions[0].code = "Actual";
    e.dimensions[0].name = "Actual Data";
    e.dimensions[0].description = "Real production data from live systems";
    e.dimensions[0].modified_by = "admin";
    e.dimensions[0].change_commentary = "Initial creation";
    e.dimensions[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_nature_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dimensions[0].version == e.dimensions[0].version);
    CHECK(a.dimensions[0].code == e.dimensions[0].code);
    CHECK(a.dimensions[0].name == e.dimensions[0].name);
}

TEST_CASE("get_origin_dimensions_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_origin_dimensions_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_origin_dimensions_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_origin_dimension_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_origin_dimension_request e; e.dimensions.emplace_back();
    e.dimensions[0].version = 1;
    e.dimensions[0].code = "Source";
    e.dimensions[0].name = "Source Data";
    e.dimensions[0].description = "Primary/authoritative source data";
    e.dimensions[0].modified_by = "admin";
    e.dimensions[0].change_commentary = "Initial creation";
    e.dimensions[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_origin_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dimensions[0].version == e.dimensions[0].version);
    CHECK(a.dimensions[0].code == e.dimensions[0].code);
    CHECK(a.dimensions[0].name == e.dimensions[0].name);
}

TEST_CASE("get_treatment_dimensions_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_treatment_dimensions_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_treatment_dimensions_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("save_treatment_dimension_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_treatment_dimension_request e; e.dimensions.emplace_back();
    e.dimensions[0].version = 1;
    e.dimensions[0].code = "Raw";
    e.dimensions[0].name = "Raw Data";
    e.dimensions[0].description = "Unprocessed data as received from source";
    e.dimensions[0].modified_by = "admin";
    e.dimensions[0].change_commentary = "Initial creation";
    e.dimensions[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_treatment_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dimensions[0].version == e.dimensions[0].version);
    CHECK(a.dimensions[0].code == e.dimensions[0].code);
    CHECK(a.dimensions[0].name == e.dimensions[0].name);
}

// ============================================================================
// Data Organization Protocol Tests (delete/history)
// ============================================================================

TEST_CASE("delete_catalog_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_catalog_request e;
    e.names = {"ISO Standards", "FpML Catalog"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_catalog_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.names == e.names);
}

TEST_CASE("delete_catalog_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_catalog_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_catalog_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_catalog_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_catalog_history_request e;
    e.name = "ISO Standards";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_catalog_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.name == e.name);
}

TEST_CASE("get_catalog_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_catalog_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_catalog_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("delete_data_domain_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_data_domain_request e;
    e.names = {"Reference Data", "Market Data"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_data_domain_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.names == e.names);
}

TEST_CASE("delete_data_domain_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_data_domain_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_data_domain_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_data_domain_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_data_domain_history_request e;
    e.name = "Reference Data";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_data_domain_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.name == e.name);
}

TEST_CASE("get_data_domain_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_data_domain_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_data_domain_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("delete_subject_area_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_subject_area_request e;
    subject_area_key k1;
    k1.name = "Currencies";
    k1.domain_name = "Reference Data";
    e.keys.push_back(k1);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_subject_area_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.keys.size() == 1);
    CHECK(a.keys[0].name == k1.name);
    CHECK(a.keys[0].domain_name == k1.domain_name);
}

TEST_CASE("delete_subject_area_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_subject_area_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_subject_area_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_subject_area_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_subject_area_history_request e;
    e.key.name = "Currencies";
    e.key.domain_name = "Reference Data";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_subject_area_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.key.name == e.key.name);
    CHECK(a.key.domain_name == e.key.domain_name);
}

TEST_CASE("get_subject_area_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_subject_area_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_subject_area_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Dataset Protocol Tests (delete/history)
// ============================================================================

TEST_CASE("delete_dataset_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_dataset_request e;
    e.ids = {boost::uuids::random_generator()(), boost::uuids::random_generator()()};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_dataset_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.ids == e.ids);
}

TEST_CASE("delete_dataset_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_dataset_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_dataset_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_dataset_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_history_request e;
    e.id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.id == e.id);
}

TEST_CASE("get_dataset_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("delete_methodology_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_methodology_request e;
    e.ids = {boost::uuids::random_generator()(), boost::uuids::random_generator()()};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_methodology_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.ids == e.ids);
}

TEST_CASE("delete_methodology_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_methodology_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_methodology_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_methodology_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_methodology_history_request e;
    e.id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_methodology_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.id == e.id);
}

TEST_CASE("get_methodology_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_methodology_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_methodology_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Coding Scheme Protocol Tests (delete/history)
// ============================================================================

TEST_CASE("delete_coding_scheme_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_coding_scheme_request e;
    e.codes = {"ISO_4217", "ISO_3166"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_coding_scheme_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.codes == e.codes);
}

TEST_CASE("delete_coding_scheme_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_coding_scheme_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_coding_scheme_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_coding_scheme_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_coding_scheme_history_request e;
    e.code = "ISO_4217";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_coding_scheme_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.code == e.code);
}

TEST_CASE("get_coding_scheme_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_coding_scheme_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_coding_scheme_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("delete_coding_scheme_authority_type_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_coding_scheme_authority_type_request e;
    e.codes = {"official", "industry"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_coding_scheme_authority_type_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.codes == e.codes);
}

TEST_CASE("delete_coding_scheme_authority_type_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_coding_scheme_authority_type_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_coding_scheme_authority_type_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_coding_scheme_authority_type_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_coding_scheme_authority_type_history_request e;
    e.code = "official";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_coding_scheme_authority_type_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.code == e.code);
}

TEST_CASE("get_coding_scheme_authority_type_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_coding_scheme_authority_type_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_coding_scheme_authority_type_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Dimension Protocol Tests (delete/history)
// ============================================================================

TEST_CASE("delete_nature_dimension_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_nature_dimension_request e;
    e.codes = {"Actual", "Simulated"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_nature_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.codes == e.codes);
}

TEST_CASE("delete_nature_dimension_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_nature_dimension_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_nature_dimension_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_nature_dimension_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_nature_dimension_history_request e;
    e.code = "Actual";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_nature_dimension_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.code == e.code);
}

TEST_CASE("get_nature_dimension_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_nature_dimension_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_nature_dimension_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("delete_origin_dimension_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_origin_dimension_request e;
    e.codes = {"Source", "Derived"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_origin_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.codes == e.codes);
}

TEST_CASE("delete_origin_dimension_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_origin_dimension_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_origin_dimension_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_origin_dimension_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_origin_dimension_history_request e;
    e.code = "Source";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_origin_dimension_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.code == e.code);
}

TEST_CASE("get_origin_dimension_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_origin_dimension_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_origin_dimension_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("delete_treatment_dimension_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_treatment_dimension_request e;
    e.codes = {"Raw", "Cleansed"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_treatment_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.codes == e.codes);
}

TEST_CASE("delete_treatment_dimension_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_treatment_dimension_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_treatment_dimension_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_treatment_dimension_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_treatment_dimension_history_request e;
    e.code = "Raw";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_treatment_dimension_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.code == e.code);
}

TEST_CASE("get_treatment_dimension_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_treatment_dimension_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_treatment_dimension_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Change Management Protocol Tests
// ============================================================================

TEST_CASE("get_change_reasons_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reasons_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reasons_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("get_change_reasons_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reasons_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reasons_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.reasons.size() == e.reasons.size());
}

TEST_CASE("save_change_reason_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_change_reason_request e; e.reasons.emplace_back();
    e.reasons[0].version = 1;
    e.reasons[0].code = "static_data.front_office_error";
    e.reasons[0].description = "Correcting front office mistake";
    e.reasons[0].category_code = "static_data";
    e.reasons[0].applies_to_amend = true;
    e.reasons[0].applies_to_delete = false;
    e.reasons[0].requires_commentary = true;
    e.reasons[0].display_order = 10;
    e.reasons[0].modified_by = "admin";
    e.reasons[0].change_commentary = "Initial creation";
    e.reasons[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_change_reason_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.reasons[0].version == e.reasons[0].version);
    CHECK(a.reasons[0].code == e.reasons[0].code);
    CHECK(a.reasons[0].description == e.reasons[0].description);
    CHECK(a.reasons[0].category_code == e.reasons[0].category_code);
}

TEST_CASE("save_change_reason_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_change_reason_response e;
    e.success = true;
    e.message = "Saved successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_change_reason_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);

    CHECK(a.message == e.message);
}

TEST_CASE("delete_change_reason_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_change_reason_request e;
    e.codes = {"static_data.front_office_error", "static_data.back_office_error"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_change_reason_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.codes == e.codes);
}

TEST_CASE("delete_change_reason_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_change_reason_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_change_reason_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_change_reason_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reason_history_request e;
    e.code = "static_data.front_office_error";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reason_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.code == e.code);
}

TEST_CASE("get_change_reason_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reason_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reason_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

TEST_CASE("get_change_reasons_by_category_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reasons_by_category_request e;
    e.category_code = "static_data";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reasons_by_category_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.category_code == e.category_code);
}

TEST_CASE("get_change_reasons_by_category_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reasons_by_category_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reasons_by_category_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.reasons.size() == e.reasons.size());
}

TEST_CASE("get_change_reason_categories_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reason_categories_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reason_categories_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("get_change_reason_categories_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reason_categories_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reason_categories_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.categories.size() == e.categories.size());
}

TEST_CASE("save_change_reason_category_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_change_reason_category_request e; e.categories.emplace_back();
    e.categories[0].version = 1;
    e.categories[0].code = "static_data";
    e.categories[0].description = "Static reference data changes";
    e.categories[0].modified_by = "admin";
    e.categories[0].change_commentary = "Initial creation";
    e.categories[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_change_reason_category_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.categories[0].version == e.categories[0].version);
    CHECK(a.categories[0].code == e.categories[0].code);
    CHECK(a.categories[0].description == e.categories[0].description);
}

TEST_CASE("save_change_reason_category_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_change_reason_category_response e;
    e.success = true;
    e.message = "Saved successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_change_reason_category_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);

    CHECK(a.message == e.message);
}

TEST_CASE("delete_change_reason_category_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_change_reason_category_request e;
    e.codes = {"static_data", "trade"};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_change_reason_category_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.codes == e.codes);
}

TEST_CASE("delete_change_reason_category_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_change_reason_category_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_change_reason_category_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_change_reason_category_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reason_category_history_request e;
    e.code = "static_data";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reason_category_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.code == e.code);
}

TEST_CASE("get_change_reason_category_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_change_reason_category_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_change_reason_category_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Dataset Bundle Protocol Tests
// ============================================================================

TEST_CASE("get_dataset_bundles_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundles_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundles_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("get_dataset_bundles_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundles_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundles_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.bundles.size() == e.bundles.size());
}

TEST_CASE("save_dataset_bundle_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_dataset_bundle_request e; e.bundles.emplace_back();
    e.bundles[0].version = 1;
    e.bundles[0].id = boost::uuids::random_generator()();
    e.bundles[0].code = "slovaris";
    e.bundles[0].name = "Slovaris Bundle";
    e.bundles[0].description = "Synthetic reference data for testing";
    e.bundles[0].modified_by = "admin";
    e.bundles[0].change_commentary = "Initial creation";
    e.bundles[0].recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_dataset_bundle_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.bundles[0].version == e.bundles[0].version);
    CHECK(a.bundles[0].id == e.bundles[0].id);
    CHECK(a.bundles[0].code == e.bundles[0].code);
    CHECK(a.bundles[0].name == e.bundles[0].name);
}

TEST_CASE("save_dataset_bundle_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    save_dataset_bundle_response e;
    e.success = true;
    e.message = "Saved successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_dataset_bundle_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);

    CHECK(a.message == e.message);
}

TEST_CASE("delete_dataset_bundle_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_dataset_bundle_request e;
    e.ids = {boost::uuids::random_generator()(), boost::uuids::random_generator()()};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_dataset_bundle_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.ids == e.ids);
}

TEST_CASE("delete_dataset_bundle_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_dataset_bundle_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_dataset_bundle_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

TEST_CASE("get_dataset_bundle_history_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundle_history_request e;
    e.id = boost::uuids::random_generator()();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundle_history_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.id == e.id);
}

TEST_CASE("get_dataset_bundle_history_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundle_history_response e;
    e.success = true;
    e.message = "Found 0 versions";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundle_history_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
    CHECK(a.versions.size() == e.versions.size());
}

// ============================================================================
// Dataset Bundle Member Protocol Tests
// ============================================================================

TEST_CASE("get_dataset_bundle_members_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundle_members_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundle_members_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("get_dataset_bundle_members_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundle_members_response e;

    dataset_bundle_member m1;
    m1.version = 1;
    m1.bundle_code = "base";
    m1.dataset_code = "iso.countries";
    m1.display_order = 10;
    m1.optional = false;
    m1.modified_by = "admin";
    m1.change_reason_code = "system.new_record";
    m1.change_commentary = "Initial";
    m1.recorded_at = std::chrono::system_clock::now();
    e.members.push_back(m1);

    dataset_bundle_member m2;
    m2.version = 1;
    m2.bundle_code = "base";
    m2.dataset_code = "gleif.lei_parties.small";
    m2.display_order = 203;
    m2.optional = true;
    m2.modified_by = "admin";
    m2.change_reason_code = "system.new_record";
    m2.change_commentary = "Optional LEI dataset";
    m2.recorded_at = std::chrono::system_clock::now();
    e.members.push_back(m2);

    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundle_members_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.members.size() == 2);
    CHECK(a.members[0].dataset_code == "iso.countries");
    CHECK(a.members[0].optional == false);
    CHECK(a.members[1].dataset_code == "gleif.lei_parties.small");
    CHECK(a.members[1].optional == true);
}

TEST_CASE("get_dataset_bundle_members_by_bundle_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundle_members_by_bundle_request e;
    e.bundle_code = "slovaris";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundle_members_by_bundle_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.bundle_code == e.bundle_code);
}

TEST_CASE("get_dataset_bundle_members_by_bundle_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_bundle_members_by_bundle_response e;

    dataset_bundle_member m1;
    m1.version = 1;
    m1.bundle_code = "base";
    m1.dataset_code = "fpml.business_center";
    m1.display_order = 105;
    m1.optional = false;
    m1.modified_by = "admin";
    m1.change_reason_code = "system.new_record";
    m1.change_commentary = "Seed data";
    m1.recorded_at = std::chrono::system_clock::now();
    e.members.push_back(m1);

    dataset_bundle_member m2;
    m2.version = 1;
    m2.bundle_code = "base";
    m2.dataset_code = "gleif.lei_counterparties.small";
    m2.display_order = 202;
    m2.optional = true;
    m2.modified_by = "admin";
    m2.change_reason_code = "system.new_record";
    m2.change_commentary = "Optional counterparty dataset";
    m2.recorded_at = std::chrono::system_clock::now();
    e.members.push_back(m2);

    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_bundle_members_by_bundle_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.members.size() == 2);
    CHECK(a.members[0].dataset_code == "fpml.business_center");
    CHECK(a.members[0].optional == false);
    CHECK(a.members[1].dataset_code == "gleif.lei_counterparties.small");
    CHECK(a.members[1].optional == true);
}

TEST_CASE("delete_dataset_bundle_member_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_dataset_bundle_member_request e;
    dataset_bundle_member_key k1;
    k1.bundle_code = "slovaris";
    k1.dataset_code = "slovaris.countries";
    e.keys.push_back(k1);
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_dataset_bundle_member_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    REQUIRE(a.keys.size() == 1);
    CHECK(a.keys[0].bundle_code == k1.bundle_code);
    CHECK(a.keys[0].dataset_code == k1.dataset_code);
}

TEST_CASE("delete_dataset_bundle_member_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    delete_dataset_bundle_member_response e;
    e.success = true;
    e.message = "Deleted successfully";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = delete_dataset_bundle_member_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;
    CHECK(a.success == e.success);
    CHECK(a.message == e.message);
}

// ============================================================================
// Dataset Dependency Protocol Tests
// ============================================================================

TEST_CASE("get_dataset_dependencies_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_dependencies_request e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_dependencies_request::deserialize(serialized);

    REQUIRE(r.has_value());
    BOOST_LOG_SEV(lg, info) << "Actual: " << r.value();
}

TEST_CASE("get_dataset_dependencies_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_dependencies_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_dependencies_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dependencies.size() == e.dependencies.size());
}

TEST_CASE("get_dataset_dependencies_by_dataset_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_dependencies_by_dataset_request e;
    e.dataset_code = "iso.countries";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_dependencies_by_dataset_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dataset_code == e.dataset_code);
}

TEST_CASE("get_dataset_dependencies_by_dataset_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_dataset_dependencies_by_dataset_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_dataset_dependencies_by_dataset_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dependencies.size() == e.dependencies.size());
}

// ============================================================================
// Publication Protocol Tests
// ============================================================================

TEST_CASE("publish_datasets_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    publish_datasets_request e;
    e.dataset_ids = {boost::uuids::random_generator()(), boost::uuids::random_generator()()};
    e.mode = publication_mode::upsert;
    e.published_by = "admin";
    e.resolve_dependencies = true;
    e.atomic = false;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = publish_datasets_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dataset_ids == e.dataset_ids);
    CHECK(a.mode == e.mode);
    CHECK(a.published_by == e.published_by);
    CHECK(a.resolve_dependencies == e.resolve_dependencies);
    CHECK(a.atomic == e.atomic);
}

TEST_CASE("publish_datasets_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    publish_datasets_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = publish_datasets_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.results.size() == e.results.size());
}

TEST_CASE("get_publications_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_publications_request e;
    e.dataset_id = boost::uuids::random_generator()();
    e.limit = 50;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_publications_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dataset_id == e.dataset_id);
    CHECK(a.limit == e.limit);
}

TEST_CASE("get_publications_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_publications_response e;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_publications_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.publications.size() == e.publications.size());
}

TEST_CASE("resolve_dependencies_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    resolve_dependencies_request e;
    e.dataset_ids = {boost::uuids::random_generator()(), boost::uuids::random_generator()()};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = resolve_dependencies_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dataset_ids == e.dataset_ids);
}

TEST_CASE("resolve_dependencies_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    resolve_dependencies_response e;
    e.requested_ids = {boost::uuids::random_generator()()};
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = resolve_dependencies_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.datasets.size() == e.datasets.size());
    CHECK(a.requested_ids == e.requested_ids);
}

// ============================================================================
// LEI Entity Summary Protocol Tests
// ============================================================================

TEST_CASE("get_lei_entities_summary_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_lei_entities_summary_request e;
    e.search_filter = "Goldman";
    e.country_filter = "US";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_lei_entities_summary_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.search_filter == e.search_filter);
    CHECK(a.country_filter == e.country_filter);
}

TEST_CASE("get_lei_entities_summary_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    get_lei_entities_summary_response e;
    e.success = true;
    e.error_message = "";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = get_lei_entities_summary_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
    CHECK(a.entities.size() == e.entities.size());
}

// ============================================================================
// Publish Bundle Protocol Tests
// ============================================================================

TEST_CASE("publish_bundle_request_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    publish_bundle_request e;
    e.bundle_code = "slovaris";
    e.mode = publication_mode::replace_all;
    e.published_by = "admin";
    e.atomic = true;
    e.params_json = R"({"lei_parties": {"root_lei": "5493001KJTIIGC8Y1R12"}})";
    e.target_tenant_id = "tenant-001";
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = publish_bundle_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.bundle_code == e.bundle_code);
    CHECK(a.mode == e.mode);
    CHECK(a.published_by == e.published_by);
    CHECK(a.atomic == e.atomic);
    CHECK(a.params_json == e.params_json);
    CHECK(a.target_tenant_id == e.target_tenant_id);
}

TEST_CASE("publish_bundle_response_serialize_deserialize", tags) {
    auto lg(make_logger(test_suite));

    publish_bundle_response e;
    e.success = true;
    e.error_message = "";
    e.datasets_processed = 5;
    e.datasets_succeeded = 4;
    e.datasets_failed = 1;
    e.datasets_skipped = 0;
    e.total_records_inserted = 1000;
    e.total_records_updated = 200;
    e.total_records_skipped = 50;
    e.total_records_deleted = 10;
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = publish_bundle_response::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.success == e.success);
    CHECK(a.error_message == e.error_message);
    CHECK(a.datasets_processed == e.datasets_processed);
    CHECK(a.datasets_succeeded == e.datasets_succeeded);
    CHECK(a.datasets_failed == e.datasets_failed);
    CHECK(a.datasets_skipped == e.datasets_skipped);
    CHECK(a.total_records_inserted == e.total_records_inserted);
    CHECK(a.total_records_updated == e.total_records_updated);
    CHECK(a.total_records_skipped == e.total_records_skipped);
    CHECK(a.total_records_deleted == e.total_records_deleted);
    CHECK(a.dataset_results.size() == e.dataset_results.size());
}
