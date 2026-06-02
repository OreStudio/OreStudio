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
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"

#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_model_config.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/domain/pricing_model_config_json_io.hpp" // IWYU pragma: keep.
#include "ores.analytics.api/generators/pricing_model_config_generator.hpp"

namespace {

const std::string_view test_suite("ores.analytics.tests");
const std::string tags("[repository]");

}

using namespace ores::analytics::generators;
using ores::analytics::domain::pricing_model_config;
using ores::analytics::repository::pricing_model_config_repository;
using ores::testing::scoped_database_helper;
using namespace ores::logging;

TEST_CASE("write_single_pricing_model_config", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    auto cfg = configs[0];
    BOOST_LOG_SEV(lg, debug) << "Pricing model config: " << cfg;

    pricing_model_config_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), cfg));
}

TEST_CASE("write_multiple_pricing_model_configs", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto configs = generate_fictional_pricing_model_configs(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Pricing model configs: " << configs;

    pricing_model_config_repository repo;
    CHECK_NOTHROW(repo.write(h.context(), configs));
}

TEST_CASE("read_latest_pricing_model_configs", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto written = generate_fictional_pricing_model_configs(3, ctx);
    BOOST_LOG_SEV(lg, debug) << "Written configs: " << written;

    pricing_model_config_repository repo;
    repo.write(h.context(), written);

    auto read = repo.read_latest(h.context());
    BOOST_LOG_SEV(lg, debug) << "Read configs: " << read;

    CHECK(read.size() >= written.size());
}

TEST_CASE("read_latest_pricing_model_config_by_name", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    auto cfg = configs[0];
    const auto original_description = cfg.description;
    BOOST_LOG_SEV(lg, debug) << "Pricing model config: " << cfg;

    pricing_model_config_repository repo;
    repo.write(h.context(), cfg);

    cfg.description = original_description + " v2";
    repo.write(h.context(), cfg);

    auto read = repo.read_latest_by_name(h.context(), cfg.name);
    BOOST_LOG_SEV(lg, debug) << "Read configs: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].name == cfg.name);
    CHECK(read[0].description == original_description + " v2");
}

TEST_CASE("read_latest_pricing_model_config_by_id", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    auto cfg = configs[0];
    const auto id_str = boost::uuids::to_string(cfg.id);
    BOOST_LOG_SEV(lg, debug) << "Pricing model config id: " << id_str;

    pricing_model_config_repository repo;
    repo.write(h.context(), cfg);

    auto read = repo.read_latest(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "Read configs: " << read;

    REQUIRE(read.size() == 1);
    CHECK(read[0].id == cfg.id);
}

TEST_CASE("read_nonexistent_pricing_model_config", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    pricing_model_config_repository repo;

    const std::string nonexistent_name = "NONEXISTENT_CONFIG_12345";
    BOOST_LOG_SEV(lg, debug) << "Non-existent name: " << nonexistent_name;

    auto read = repo.read_latest_by_name(h.context(), nonexistent_name);
    BOOST_LOG_SEV(lg, debug) << "Read configs: " << read;

    CHECK(read.size() == 0);
}

TEST_CASE("read_all_pricing_model_config_history", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    auto cfg = configs[0];
    const auto id_str = boost::uuids::to_string(cfg.id);
    BOOST_LOG_SEV(lg, debug) << "Pricing model config: " << cfg;

    pricing_model_config_repository repo;
    repo.write(h.context(), cfg);

    cfg.description = cfg.description + " v2";
    repo.write(h.context(), cfg);

    auto history = repo.read_all(h.context(), id_str);
    BOOST_LOG_SEV(lg, debug) << "History: " << history;

    CHECK(history.size() >= 2);
}

TEST_CASE("remove_pricing_model_config", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    auto ctx = ores::testing::make_generation_context(h);
    auto configs = generate_fictional_pricing_model_configs(1, ctx);
    REQUIRE(!configs.empty());
    auto cfg = configs[0];
    const auto id_str = boost::uuids::to_string(cfg.id);
    BOOST_LOG_SEV(lg, debug) << "Pricing model config: " << cfg;

    pricing_model_config_repository repo;
    repo.write(h.context(), cfg);

    CHECK_NOTHROW(repo.remove(h.context(), id_str));

    auto read = repo.read_latest(h.context(), id_str);
    CHECK(read.empty());
}
