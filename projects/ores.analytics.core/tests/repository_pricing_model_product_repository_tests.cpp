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
#include "ores.analytics.core/repository/pricing_model_product_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_model_product.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_model_product_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/generators/pricing_model_config_generator.hpp"
#include "ores.analytics.api/generators/pricing_model_product_generator.hpp"
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"
#include "ores.analytics.core/repository/pricing_engine_type_repository.hpp"
#include "ores.analytics.api/generators/pricing_engine_type_generator.hpp"

namespace {

const std::string_view test_suite("ores.analytics.tests");
const std::string tags("[repository]");

}

using namespace ores::analytics::generators;
using ores::analytics::domain::pricing_model_product;
using ores::analytics::repository::pricing_model_product_repository;
using ores::analytics::repository::pricing_model_config_repository;
using ores::analytics::repository::pricing_engine_type_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_pricing_model_product", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    // prerequisites: engine types and config must exist (soft FKs)
    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto products = generate_fictional_pricing_model_products(1, configs[0].id, ctx);
    REQUIRE(!products.empty());
    BOOST_LOG_SEV(lg, debug) << "Pricing model product: " << products[0];

    pricing_model_product_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), products[0]));
}

TEST_CASE("write_multiple_pricing_model_products", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto products = generate_fictional_pricing_model_products(3, configs[0].id, ctx);
    BOOST_LOG_SEV(lg, debug) << "Pricing model products: " << products;

    pricing_model_product_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), products));
}

TEST_CASE("read_latest_pricing_model_products_for_config", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto written = generate_fictional_pricing_model_products(3, configs[0].id, ctx);
    pricing_model_product_repository repo;
    repo.write(h.context(), written);

    const auto config_id_str = boost::uuids::to_string(configs[0].id);
    auto read = repo.read_latest(h.context(), config_id_str);
    BOOST_LOG_SEV(lg, debug) << "Read products: " << read;

    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_pricing_model_product_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto products = generate_fictional_pricing_model_products(1, configs[0].id, ctx);
    REQUIRE(!products.empty());
    auto p = products[0];
    const auto product_id_str = boost::uuids::to_string(p.id);
    BOOST_LOG_SEV(lg, debug) << "Product id: " << product_id_str;

    pricing_model_product_repository repo;
    repo.write(h.context(), p);

    auto read = repo.read_latest_by_id(h.context(), product_id_str);
    BOOST_LOG_SEV(lg, debug) << "Read products: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].id == p.id);
}

TEST_CASE("read_all_pricing_model_product_history", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto products = generate_fictional_pricing_model_products(1, configs[0].id, ctx);
    REQUIRE(!products.empty());
    auto p = products[0];
    const auto product_id_str = boost::uuids::to_string(p.id);
    BOOST_LOG_SEV(lg, debug) << "Product: " << p;

    pricing_model_product_repository repo;
    repo.write(h.context(), p);

    p.model = p.model + "_v2";
    repo.write(h.context(), p);

    auto history = repo.read_all(h.context(), product_id_str);
    BOOST_LOG_SEV(lg, debug) << "History: " << history;

    CHECK(history.size() >= 2);
}

TEST_CASE("remove_pricing_model_product", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto products = generate_fictional_pricing_model_products(1, configs[0].id, ctx);
    REQUIRE(!products.empty());
    auto p = products[0];
    const auto product_id_str = boost::uuids::to_string(p.id);

    pricing_model_product_repository repo;
    repo.write(h.context(), p);

    CHECK_NOTHROW(repo.remove(h.context(), product_id_str));

    auto read = repo.read_latest_by_id(h.context(), product_id_str);
    CHECK(read.empty());
}

TEST_CASE("remove_pricing_model_products_for_config", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);

    auto engine_types = generate_fictional_pricing_engine_types(5, ctx);
    pricing_engine_type_repository et_repo;
    et_repo.write(h.context(), engine_types);

    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    pricing_model_config_repository cfg_repo;
    cfg_repo.write(h.context(), configs[0]);

    auto products = generate_fictional_pricing_model_products(3, configs[0].id, ctx);
    pricing_model_product_repository repo;
    repo.write(h.context(), products);

    const auto config_id_str = boost::uuids::to_string(configs[0].id);
    CHECK_NOTHROW(repo.remove_for_config(h.context(), config_id_str));

    auto read = repo.read_latest(h.context(), config_id_str);
    CHECK(read.empty());
}
