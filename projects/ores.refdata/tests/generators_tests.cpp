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
#include "ores.refdata/generators/party_generator.hpp"
#include "ores.refdata/generators/counterparty_generator.hpp"
#include "ores.refdata/generators/contact_type_generator.hpp"
#include "ores.refdata/generators/party_type_generator.hpp"
#include "ores.refdata/generators/party_status_generator.hpp"
#include "ores.refdata/generators/party_id_scheme_generator.hpp"
#include "ores.refdata/generators/party_identifier_generator.hpp"
#include "ores.refdata/generators/counterparty_identifier_generator.hpp"
#include "ores.refdata/generators/party_contact_information_generator.hpp"
#include "ores.refdata/generators/counterparty_contact_information_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.refdata.tests");
const std::string tags("[generators]");

}

using namespace ores::refdata::generators;
using namespace ores::logging;

// --- party ---

TEST_CASE("party_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_party();

    BOOST_LOG_SEV(lg, info) << "Generated party: " << sut.full_name;

    CHECK(sut.version == 1);
    CHECK(sut.tenant_id == "system");
    CHECK(!sut.id.is_nil());
    CHECK(!sut.full_name.empty());
    CHECK(!sut.short_code.empty());
    CHECK(sut.party_type == "Corporate");
    CHECK(sut.business_center_code == "WRLD");
    CHECK(sut.status == "Active");
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("party_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_parties(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.full_name.empty());
    }
}

// --- counterparty ---

TEST_CASE("counterparty_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_counterparty();

    CHECK(sut.version == 1);
    CHECK(sut.tenant_id == "system");
    CHECK(!sut.id.is_nil());
    CHECK(!sut.full_name.empty());
    CHECK(!sut.short_code.empty());
    CHECK(sut.party_type == "Bank");
    CHECK(sut.business_center_code == "WRLD");
    CHECK(sut.status == "Active");
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("counterparty_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_counterparties(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.full_name.empty());
    }
}

// --- contact_type ---

TEST_CASE("contact_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_contact_type();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(sut.display_order >= 1);
    CHECK(sut.display_order <= 100);
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("contact_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_contact_types(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- party_type ---

TEST_CASE("party_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_party_type();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(sut.display_order >= 1);
    CHECK(sut.display_order <= 100);
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("party_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_party_types(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- party_status ---

TEST_CASE("party_status_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_party_status();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(sut.display_order >= 1);
    CHECK(sut.display_order <= 100);
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("party_status_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_party_statuses(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- party_id_scheme ---

TEST_CASE("party_id_scheme_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_party_id_scheme();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.coding_scheme_code.empty());
    CHECK(sut.display_order >= 1);
    CHECK(sut.display_order <= 100);
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("party_id_scheme_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_party_id_schemes(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- party_identifier ---

TEST_CASE("party_identifier_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_party_identifier();

    CHECK(sut.version == 1);
    CHECK(sut.tenant_id == "system");
    CHECK(!sut.id.is_nil());
    CHECK(!sut.party_id.is_nil());
    CHECK(!sut.id_scheme.empty());
    CHECK(sut.id_value.size() >= 20);
    CHECK(sut.description == "Test identifier");
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("party_identifier_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_party_identifiers(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.id_value.empty());
    }
}

// --- counterparty_identifier ---

TEST_CASE("counterparty_identifier_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_counterparty_identifier();

    CHECK(sut.version == 1);
    CHECK(sut.tenant_id == "system");
    CHECK(!sut.id.is_nil());
    CHECK(!sut.counterparty_id.is_nil());
    CHECK(!sut.id_scheme.empty());
    CHECK(sut.id_value.size() >= 20);
    CHECK(sut.description == "Test identifier");
    CHECK(!sut.recorded_by.empty());
    CHECK(!sut.performed_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("counterparty_identifier_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_counterparty_identifiers(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.id_value.empty());
    }
}

// --- party_contact_information ---

TEST_CASE("party_contact_information_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_party_contact_information();

    CHECK(sut.version == 1);
    CHECK(sut.tenant_id == "system");
    CHECK(!sut.id.is_nil());
    CHECK(!sut.party_id.is_nil());
    CHECK(!sut.contact_type.empty());
    CHECK(sut.city == "London");
    CHECK(sut.country_code.empty());
    CHECK(!sut.street_line_1.empty());
    CHECK(!sut.postal_code.empty());
    CHECK(!sut.phone.empty());
    CHECK(!sut.email.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("party_contact_information_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_party_contact_informations(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.city.empty());
    }
}

// --- counterparty_contact_information ---

TEST_CASE("counterparty_contact_information_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_counterparty_contact_information();

    CHECK(sut.version == 1);
    CHECK(sut.tenant_id == "system");
    CHECK(!sut.id.is_nil());
    CHECK(!sut.counterparty_id.is_nil());
    CHECK(!sut.contact_type.empty());
    CHECK(sut.city == "New York");
    CHECK(sut.country_code.empty());
    CHECK(!sut.street_line_1.empty());
    CHECK(!sut.postal_code.empty());
    CHECK(!sut.phone.empty());
    CHECK(!sut.email.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_reason_code == "system.new");
}

TEST_CASE("counterparty_contact_information_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_counterparty_contact_informations(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.city.empty());
    }
}
