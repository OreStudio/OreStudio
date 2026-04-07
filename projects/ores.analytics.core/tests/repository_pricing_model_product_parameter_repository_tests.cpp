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
#include "ores.analytics.core/repository/pricing_model_product_parameter_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_model_product_parameter.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_model_product_parameter_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/generators/pricing_engine_type_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_config_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_product_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_product_parameter_generator.hpp"
#include "ores.analytics.core/repository/pricing_engine_type_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"

namespace {

const std::string_view test_suite("ores.analytics.tests");
const std::string tags("[repository]");

}

using namespace ores::analytics::generators;
using ores::analytics::domain::pricing_model_product_parameter;
using ores::analytics::repository::pricing_model_product_parameter_repository;
using ores::analytics::repository::pricing_model_config_repository;
using ores::analytics::repository::pricing_model_product_repository;
using ores::analytics::repository::pricing_engine_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

namespace {

/**
 * @brief Writes the prerequisite engine types, config, and product.
 * Returns the config_id and product_id for use in tests.
 */
std::pair<boost::uuids::uuid, boost::uuids::uuid> write_prerequisites(
    scoped_database_helper& h,
    ores::utility::generation::generation_context& ctx) {
    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto products = generate_fictional_pricing_model_products(1, configs[0].id, ctx);
    pricing_model_product_repository prod_repo;
    prod_repo.write(h.context(), products[0]);

    return {configs[0].id, products[0].id};
}

}

TEST_CASE("write_single_pricing_model_product_parameter", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto params = generate_fictional_pricing_model_product_parameters(
        1, config_id, product_id, ctx);
    REQUIRE(!params.empty());
    BOOST_LOG_SEV(lg, debug) << "Parameter: " << params[0];

    pricing_model_product_parameter_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), params[0]));
}

TEST_CASE("write_multiple_pricing_model_product_parameters", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto params = generate_fictional_pricing_model_product_parameters(
        5, config_id, product_id, ctx);
    BOOST_LOG_SEV(lg, debug) << "Parameters: " << params;

    pricing_model_product_parameter_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), params));
}

TEST_CASE("read_latest_pricing_model_product_parameters_for_config", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto written = generate_fictional_pricing_model_product_parameters(
        5, config_id, product_id, ctx);
    pricing_model_product_parameter_repository repo;
    repo.write(h.context(), written);

    const auto config_id_str = boost::uuids::to_string(config_id);
    auto read = repo.read_latest(h.context(), config_id_str);
    BOOST_LOG_SEV(lg, debug) << "Read parameters: " << read;

    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_pricing_model_product_parameters_for_product", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto written = generate_fictional_pricing_model_product_parameters(
        5, config_id, product_id, ctx);
    pricing_model_product_parameter_repository repo;
    repo.write(h.context(), written);

    const auto product_id_str = boost::uuids::to_string(product_id);
    auto read = repo.read_latest_for_product(h.context(), product_id_str);
    BOOST_LOG_SEV(lg, debug) << "Read parameters for product: " << read;

    // product-scoped parameters (model + engine scope) should be returned
    CHECK(!read.empty());
}

TEST_CASE("read_latest_pricing_model_product_parameter_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto params = generate_fictional_pricing_model_product_parameters(
        1, config_id, product_id, ctx);
    REQUIRE(!params.empty());
    auto p = params[0];
    const auto param_id_str = boost::uuids::to_string(p.id);
    BOOST_LOG_SEV(lg, debug) << "Parameter id: " << param_id_str;

    pricing_model_product_parameter_repository repo;
    repo.write(h.context(), p);

    auto read = repo.read_latest_by_id(h.context(), param_id_str);
    BOOST_LOG_SEV(lg, debug) << "Read parameters: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].id == p.id);
}

TEST_CASE("read_all_pricing_model_product_parameter_history", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto params = generate_fictional_pricing_model_product_parameters(
        1, config_id, product_id, ctx);
    REQUIRE(!params.empty());
    auto p = params[0];
    const auto param_id_str = boost::uuids::to_string(p.id);
    BOOST_LOG_SEV(lg, debug) << "Parameter: " << p;

    pricing_model_product_parameter_repository repo;
    repo.write(h.context(), p);

    p.parameter_value = p.parameter_value + "_v2";
    repo.write(h.context(), p);

    auto history = repo.read_all(h.context(), param_id_str);
    BOOST_LOG_SEV(lg, debug) << "History: " << history;

    CHECK(history.size() >= 2);
}

TEST_CASE("remove_pricing_model_product_parameter", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto params = generate_fictional_pricing_model_product_parameters(
        1, config_id, product_id, ctx);
    REQUIRE(!params.empty());
    auto p = params[0];
    const auto param_id_str = boost::uuids::to_string(p.id);

    pricing_model_product_parameter_repository repo;
    repo.write(h.context(), p);

    CHECK_NOTHROW(repo.remove(h.context(), param_id_str));

    auto read = repo.read_latest_by_id(h.context(), param_id_str);
    CHECK(read.empty());
}

TEST_CASE("remove_pricing_model_product_parameters_for_config", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto [config_id, product_id] = write_prerequisites(h, ctx);

    auto params = generate_fictional_pricing_model_product_parameters(
        5, config_id, product_id, ctx);
    pricing_model_product_parameter_repository repo;
    repo.write(h.context(), params);

    const auto config_id_str = boost::uuids::to_string(config_id);
    CHECK_NOTHROW(repo.remove_for_config(h.context(), config_id_str));

    auto read = repo.read_latest(h.context(), config_id_str);
    CHECK(read.empty());
}
