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
#include "ores.platform/environment/environment.hpp"
#include "ores.platform/environment/fake_environment_provider.hpp"
#include "ores.platform/environment/real_environment_provider.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <cstdlib>
#include <optional>
#include <stdexcept>
#include <string>
#include <unordered_map>

namespace {

const std::string tags("[environment]");

using ores::platform::environment::environment;
using ores::platform::environment::environment_provider;
using ores::platform::environment::fake_environment_provider;
using ores::platform::environment::real_environment_provider;

/**
 * @brief Swaps in a fake provider for the duration of one test case.
 *
 * Restores the previous provider in the destructor, so a failing
 * REQUIRE does not leak the fake into the next test case.
 */
class scoped_fake_provider final {
public:
    explicit scoped_fake_provider(std::unordered_map<std::string, std::string> values)
        : fake_(std::move(values)) {
        previous_ = environment::swap_provider(&fake_);
    }

    scoped_fake_provider(const scoped_fake_provider&) = delete;
    scoped_fake_provider& operator=(const scoped_fake_provider&) = delete;

    ~scoped_fake_provider() {
        environment::swap_provider(previous_);
    }

private:
    fake_environment_provider fake_;
    environment_provider* previous_;
};

}

TEST_CASE("get_value_returns_installed_provider_value", tags) {
    const scoped_fake_provider guard({{"ORES_PLATFORM_TEST_KEY", "installed-value"}});

    CHECK(environment::get_value("ORES_PLATFORM_TEST_KEY") ==
          std::optional<std::string>("installed-value"));
}

TEST_CASE("get_value_returns_empty_optional_for_absent_variable", tags) {
    const scoped_fake_provider guard({{"ORES_PLATFORM_TEST_PRESENT", "present"}});

    CHECK_FALSE(environment::get_value("ORES_PLATFORM_TEST_ABSENT").has_value());
}

TEST_CASE("get_value_or_default_returns_default_for_absent_variable", tags) {
    const scoped_fake_provider guard({});

    CHECK(environment::get_value_or_default("ORES_PLATFORM_TEST_ABSENT", "fallback") == "fallback");
}

TEST_CASE("get_value_or_default_prefers_present_value_over_default", tags) {
    const scoped_fake_provider guard({{"ORES_PLATFORM_TEST_KEY", "present"}});

    CHECK(environment::get_value_or_default("ORES_PLATFORM_TEST_KEY", "fallback") == "present");
}

TEST_CASE("get_int_value_or_default_parses_decimal_value", tags) {
    const scoped_fake_provider guard({{"ORES_PLATFORM_TEST_INT", "42"}});

    CHECK(environment::get_int_value_or_default("ORES_PLATFORM_TEST_INT", 7) == 42);
}

TEST_CASE("get_int_value_or_default_returns_default_for_absent_variable", tags) {
    const scoped_fake_provider guard({});

    CHECK(environment::get_int_value_or_default("ORES_PLATFORM_TEST_ABSENT", 7) == 7);
}

TEST_CASE("get_int_value_or_default_returns_default_for_unparseable_value", tags) {
    const scoped_fake_provider guard({{"ORES_PLATFORM_TEST_INT", "not-a-number"}});

    CHECK(environment::get_int_value_or_default("ORES_PLATFORM_TEST_INT", 7) == 7);
}

TEST_CASE("get_value_or_throw_returns_present_value", tags) {
    const scoped_fake_provider guard({{"ORES_PLATFORM_TEST_KEY", "required-value"}});

    CHECK(environment::get_value_or_throw("ORES_PLATFORM_TEST_KEY") == "required-value");
}

TEST_CASE("get_value_or_throw_throws_for_absent_variable", tags) {
    const scoped_fake_provider guard({});

    CHECK_THROWS_AS(environment::get_value_or_throw("ORES_PLATFORM_TEST_ABSENT"),
                    std::runtime_error);
}

TEST_CASE("get_value_or_throw_names_the_missing_variable", tags) {
    const scoped_fake_provider guard({});

    CHECK_THROWS_WITH(environment::get_value_or_throw("ORES_PLATFORM_TEST_ABSENT"),
                      "Required environment variable not set: ORES_PLATFORM_TEST_ABSENT");
}

TEST_CASE("set_value_writes_through_the_installed_provider", tags) {
    const scoped_fake_provider guard({});

    environment::set_value("ORES_PLATFORM_TEST_KEY", "written");

    CHECK(environment::get_value("ORES_PLATFORM_TEST_KEY") ==
          std::optional<std::string>("written"));
}

TEST_CASE("unset_value_removes_the_variable_from_the_installed_provider", tags) {
    const scoped_fake_provider guard({{"ORES_PLATFORM_TEST_KEY", "present"}});

    environment::unset_value("ORES_PLATFORM_TEST_KEY");

    CHECK_FALSE(environment::get_value("ORES_PLATFORM_TEST_KEY").has_value());
}

TEST_CASE("swap_provider_returns_the_previous_provider", tags) {
    fake_environment_provider first({{"ORES_PLATFORM_TEST_KEY", "first"}});
    fake_environment_provider second({{"ORES_PLATFORM_TEST_KEY", "second"}});

    auto* originally_installed = environment::swap_provider(&first);
    CHECK(environment::get_value("ORES_PLATFORM_TEST_KEY") == std::optional<std::string>("first"));

    auto* swapped_out = environment::swap_provider(&second);
    CHECK(swapped_out == &first);
    CHECK(environment::get_value("ORES_PLATFORM_TEST_KEY") == std::optional<std::string>("second"));

    environment::swap_provider(originally_installed);
}

TEST_CASE("default_provider_reads_the_process_environment", tags) {
    const char* path_from_c_runtime = std::getenv("PATH");
    REQUIRE(path_from_c_runtime != nullptr);

    CHECK(environment::get_value("PATH") == std::optional<std::string>(path_from_c_runtime));
}

TEST_CASE("real_environment_provider_round_trips_a_variable", tags) {
    real_environment_provider provider;
    const std::string name("ORES_PLATFORM_TEST_REAL_ROUND_TRIP");
    const std::string value("real-value");

    provider.set(name, value);
    CHECK(provider.get(name) == std::optional<std::string>(value));

    provider.unset(name);
    CHECK_FALSE(provider.get(name).has_value());
    CHECK(std::getenv(name.c_str()) == nullptr);
}

TEST_CASE("real_environment_provider_get_returns_empty_optional_for_absent_variable", tags) {
    real_environment_provider provider;

    CHECK_FALSE(provider.get("ORES_PLATFORM_TEST_NEVER_SET").has_value());
}

TEST_CASE("real_environment_provider_set_overwrites_an_existing_value", tags) {
    real_environment_provider provider;
    const std::string name("ORES_PLATFORM_TEST_OVERWRITE");

    provider.set(name, "first");
    provider.set(name, "second");

    CHECK(provider.get(name) == std::optional<std::string>("second"));

    provider.unset(name);
}

#ifndef _WIN32
TEST_CASE("real_environment_provider_set_is_visible_to_std_getenv", tags) {
    real_environment_provider provider;
    const std::string name("ORES_PLATFORM_TEST_VISIBLE_TO_GETENV");

    provider.set(name, "visible");
    const char* from_c_runtime = std::getenv(name.c_str());
    REQUIRE(from_c_runtime != nullptr);
    CHECK(std::string(from_c_runtime) == "visible");

    provider.unset(name);
}
#endif
