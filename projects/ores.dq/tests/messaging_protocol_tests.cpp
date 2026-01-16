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

    save_catalog_request e;
    e.catalog.version = 1;
    e.catalog.name = "ISO Standards";
    e.catalog.description = "International standards catalogs";
    e.catalog.owner = "Standards Body";
    e.catalog.recorded_by = "admin";
    e.catalog.change_commentary = "Initial creation";
    e.catalog.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_catalog_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.catalog.version == e.catalog.version);
    CHECK(a.catalog.name == e.catalog.name);
    CHECK(a.catalog.description == e.catalog.description);
    CHECK(a.catalog.owner == e.catalog.owner);
    CHECK(a.catalog.recorded_by == e.catalog.recorded_by);
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

    save_data_domain_request e;
    e.domain.version = 1;
    e.domain.name = "Reference Data";
    e.domain.description = "Static reference data";
    e.domain.recorded_by = "admin";
    e.domain.change_commentary = "Initial creation";
    e.domain.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_data_domain_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.domain.version == e.domain.version);
    CHECK(a.domain.name == e.domain.name);
    CHECK(a.domain.description == e.domain.description);
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

    save_subject_area_request e;
    e.subject_area.version = 1;
    e.subject_area.name = "Currencies";
    e.subject_area.domain_name = "Reference Data";
    e.subject_area.description = "Currency reference data";
    e.subject_area.recorded_by = "admin";
    e.subject_area.change_commentary = "Initial creation";
    e.subject_area.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_subject_area_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.subject_area.version == e.subject_area.version);
    CHECK(a.subject_area.name == e.subject_area.name);
    CHECK(a.subject_area.domain_name == e.subject_area.domain_name);
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

    save_dataset_request e;
    e.dataset.version = 1;
    e.dataset.id = boost::uuids::random_generator()();
    e.dataset.catalog_name = "ISO Standards";
    e.dataset.subject_area_name = "Currencies";
    e.dataset.domain_name = "Reference Data";
    e.dataset.coding_scheme_code = "ISO_4217";
    e.dataset.origin_code = "Source";
    e.dataset.nature_code = "Actual";
    e.dataset.treatment_code = "Raw";
    e.dataset.methodology_id = boost::uuids::random_generator()();
    e.dataset.name = "Currency List";
    e.dataset.description = "List of all ISO 4217 currencies";
    e.dataset.source_system_id = "ISO_FEED";
    e.dataset.business_context = "Reference data for FX trading";
    e.dataset.upstream_derivation_id = std::nullopt;
    e.dataset.lineage_depth = 0;
    e.dataset.as_of_date = std::chrono::system_clock::now();
    e.dataset.ingestion_timestamp = std::chrono::system_clock::now();
    e.dataset.license_info = "Public domain";
    e.dataset.recorded_by = "admin";
    e.dataset.change_commentary = "Initial creation";
    e.dataset.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_dataset_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dataset.version == e.dataset.version);
    CHECK(a.dataset.id == e.dataset.id);
    CHECK(a.dataset.catalog_name == e.dataset.catalog_name);
    CHECK(a.dataset.name == e.dataset.name);
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

    save_methodology_request e;
    e.methodology.version = 1;
    e.methodology.id = boost::uuids::random_generator()();
    e.methodology.name = "Standard ETL";
    e.methodology.description = "Standard extract-transform-load process";
    e.methodology.logic_reference = "https://docs.example.com/etl";
    e.methodology.implementation_details = "Python script using pandas";
    e.methodology.recorded_by = "admin";
    e.methodology.change_commentary = "Initial creation";
    e.methodology.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_methodology_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.methodology.version == e.methodology.version);
    CHECK(a.methodology.id == e.methodology.id);
    CHECK(a.methodology.name == e.methodology.name);
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

    save_coding_scheme_request e;
    e.scheme.version = 1;
    e.scheme.code = "ISO_4217";
    e.scheme.name = "ISO 4217 Currency Codes";
    e.scheme.authority_type = "official";
    e.scheme.subject_area_name = "Currencies";
    e.scheme.domain_name = "Reference Data";
    e.scheme.uri = "https://www.iso.org/iso-4217-currency-codes.html";
    e.scheme.description = "Standard currency codes";
    e.scheme.recorded_by = "admin";
    e.scheme.change_commentary = "Initial creation";
    e.scheme.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_coding_scheme_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.scheme.version == e.scheme.version);
    CHECK(a.scheme.code == e.scheme.code);
    CHECK(a.scheme.name == e.scheme.name);
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

    save_coding_scheme_authority_type_request e;
    e.authority_type.version = 1;
    e.authority_type.code = "official";
    e.authority_type.name = "Official Standards";
    e.authority_type.description = "Standards from official bodies like ISO";
    e.authority_type.recorded_by = "admin";
    e.authority_type.change_commentary = "Initial creation";
    e.authority_type.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_coding_scheme_authority_type_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.authority_type.version == e.authority_type.version);
    CHECK(a.authority_type.code == e.authority_type.code);
    CHECK(a.authority_type.name == e.authority_type.name);
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

    save_nature_dimension_request e;
    e.dimension.version = 1;
    e.dimension.code = "Actual";
    e.dimension.name = "Actual Data";
    e.dimension.description = "Real production data from live systems";
    e.dimension.recorded_by = "admin";
    e.dimension.change_commentary = "Initial creation";
    e.dimension.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_nature_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dimension.version == e.dimension.version);
    CHECK(a.dimension.code == e.dimension.code);
    CHECK(a.dimension.name == e.dimension.name);
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

    save_origin_dimension_request e;
    e.dimension.version = 1;
    e.dimension.code = "Source";
    e.dimension.name = "Source Data";
    e.dimension.description = "Primary/authoritative source data";
    e.dimension.recorded_by = "admin";
    e.dimension.change_commentary = "Initial creation";
    e.dimension.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_origin_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dimension.version == e.dimension.version);
    CHECK(a.dimension.code == e.dimension.code);
    CHECK(a.dimension.name == e.dimension.name);
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

    save_treatment_dimension_request e;
    e.dimension.version = 1;
    e.dimension.code = "Raw";
    e.dimension.name = "Raw Data";
    e.dimension.description = "Unprocessed data as received from source";
    e.dimension.recorded_by = "admin";
    e.dimension.change_commentary = "Initial creation";
    e.dimension.recorded_at = std::chrono::system_clock::now();
    BOOST_LOG_SEV(lg, info) << "Expected: " << e;

    const auto serialized = e.serialize();
    const auto r = save_treatment_dimension_request::deserialize(serialized);

    REQUIRE(r.has_value());
    const auto& a = r.value();
    BOOST_LOG_SEV(lg, info) << "Actual: " << a;

    CHECK(a.dimension.version == e.dimension.version);
    CHECK(a.dimension.code == e.dimension.code);
    CHECK(a.dimension.name == e.dimension.name);
}
