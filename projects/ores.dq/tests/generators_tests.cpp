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
#include "ores.dq/generators/catalog_generator.hpp"
#include "ores.dq/generators/change_reason_category_generator.hpp"
#include "ores.dq/generators/change_reason_generator.hpp"
#include "ores.dq/generators/coding_scheme_authority_type_generator.hpp"
#include "ores.dq/generators/coding_scheme_generator.hpp"
#include "ores.dq/generators/data_domain_generator.hpp"
#include "ores.dq/generators/dataset_generator.hpp"
#include "ores.dq/generators/methodology_generator.hpp"
#include "ores.dq/generators/nature_dimension_generator.hpp"
#include "ores.dq/generators/origin_dimension_generator.hpp"
#include "ores.dq/generators/subject_area_generator.hpp"
#include "ores.dq/generators/treatment_dimension_generator.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[generators]");

}

using namespace ores::dq::generators;
using namespace ores::logging;

// --- catalog ---

TEST_CASE("catalog_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_catalog();

    BOOST_LOG_SEV(lg, info) << "Generated catalog: " << sut.name;

    CHECK(sut.version == 1);
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("catalog_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_catalogs(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.name.empty());
    }
}

// --- change_reason_category ---

TEST_CASE("change_reason_category_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_change_reason_category();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("change_reason_category_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_change_reason_categories(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- change_reason ---

TEST_CASE("change_reason_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_change_reason();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.category_code.empty());
    CHECK(sut.display_order >= 1);
    CHECK(sut.display_order <= 100);
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("change_reason_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_change_reasons(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(!item.category_code.empty());
    }
}

// --- coding_scheme_authority_type ---

TEST_CASE("coding_scheme_authority_type_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_coding_scheme_authority_type();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("coding_scheme_authority_type_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_coding_scheme_authority_types(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- coding_scheme ---

TEST_CASE("coding_scheme_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_coding_scheme();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.authority_type.empty());
    CHECK(!sut.subject_area_name.empty());
    CHECK(!sut.domain_name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("coding_scheme_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_coding_schemes(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
        CHECK(!item.name.empty());
    }
}

// --- data_domain ---

TEST_CASE("data_domain_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_data_domain();

    CHECK(sut.version == 1);
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("data_domain_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_data_domains(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.name.empty());
    }
}

// --- dataset ---

TEST_CASE("dataset_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_dataset();

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.subject_area_name.empty());
    CHECK(!sut.domain_name.empty());
    CHECK(!sut.origin_code.empty());
    CHECK(!sut.nature_code.empty());
    CHECK(!sut.treatment_code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.source_system_id.empty());
    CHECK(!sut.business_context.empty());
    CHECK(sut.lineage_depth >= 0);
    CHECK(sut.lineage_depth <= 5);
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("dataset_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_datasets(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.name.empty());
    }
}

// --- methodology ---

TEST_CASE("methodology_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_methodology();

    CHECK(sut.version == 1);
    CHECK(!sut.id.is_nil());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("methodology_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_methodologies(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.id.is_nil());
        CHECK(!item.name.empty());
    }
}

// --- origin_dimension ---

TEST_CASE("origin_dimension_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_origin_dimension();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("origin_dimension_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_origin_dimensions(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- nature_dimension ---

TEST_CASE("nature_dimension_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_nature_dimension();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("nature_dimension_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_nature_dimensions(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}

// --- subject_area ---

TEST_CASE("subject_area_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_subject_area();

    CHECK(sut.version == 1);
    CHECK(!sut.name.empty());
    CHECK(!sut.domain_name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("subject_area_generator_with_domain_name_uses_provided_name", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_subject_area("Test Domain");

    CHECK(!sut.name.empty());
    CHECK(sut.domain_name == "Test Domain");
}

TEST_CASE("subject_area_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_subject_areas(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.name.empty());
        CHECK(!item.domain_name.empty());
    }
}

// --- treatment_dimension ---

TEST_CASE("treatment_dimension_generator_produces_valid_instance", tags) {
    auto lg(make_logger(test_suite));
    auto sut = generate_synthetic_treatment_dimension();

    CHECK(sut.version == 1);
    CHECK(!sut.code.empty());
    CHECK(!sut.name.empty());
    CHECK(!sut.description.empty());
    CHECK(!sut.recorded_by.empty());
    CHECK(sut.change_commentary == "Synthetic test data");
}

TEST_CASE("treatment_dimension_generator_produces_multiple_instances", tags) {
    auto lg(make_logger(test_suite));
    const std::size_t count = 5;
    auto items = generate_synthetic_treatment_dimensions(count);

    CHECK(items.size() == count);
    for (const auto& item : items) {
        CHECK(!item.code.empty());
    }
}
