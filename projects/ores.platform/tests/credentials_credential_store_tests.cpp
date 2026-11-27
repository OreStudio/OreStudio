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
#include "ores.platform/credentials/credential_store.hpp"
#include "ores.platform/environment/environment.hpp"
#include "ores.platform/filesystem/scoped_temp_directory.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

using ores::platform::credentials::credential_store;
using ores::platform::environment::environment;
using ores::platform::filesystem::scoped_temp_directory;

constexpr std::size_t key_len = 32;

std::vector<unsigned char> make_key(unsigned char seed) {
    std::vector<unsigned char> key(key_len);
    for (std::size_t i = 0; i < key.size(); ++i)
        key[i] = static_cast<unsigned char>(seed + i);
    return key;
}

/// Points the store at a directory of the test's own, and puts it back after.
struct stated_directory {
    scoped_temp_directory directory;

    stated_directory() {
        environment::set_value(std::string(credential_store::directory_variable),
                               directory.path().string());
    }
    ~stated_directory() {
        environment::unset_value(std::string(credential_store::directory_variable));
    }
    stated_directory(const stated_directory&) = delete;
    stated_directory& operator=(const stated_directory&) = delete;
};

}

TEST_CASE("a_placed_credential_reads_back", "[platform][credentials]") {
    stated_directory store;

    credential_store::write("vault.kek", make_key(0));

    CHECK(credential_store::read("vault.kek") == make_key(0));
    CHECK(credential_store::read("vault.kek", key_len) == make_key(0));
}

TEST_CASE("an_absent_credential_is_refused", "[platform][credentials]") {
    stated_directory store;

    CHECK_THROWS_AS(credential_store::read("nothing.here"), std::runtime_error);
}

TEST_CASE("a_credential_of_the_wrong_length_is_refused", "[platform][credentials]") {
    stated_directory store;
    credential_store::write("short.key", make_key(1));

    CHECK_THROWS_AS(credential_store::read("short.key", key_len + 1), std::runtime_error);
}

TEST_CASE("a_host_with_no_store_refuses_rather_than_guesses", "[platform][credentials]") {
    environment::unset_value(std::string(credential_store::directory_variable));

#if defined(_WIN32) || defined(__APPLE__)
    // These hosts keep a store of their own, so the refusal is only about the
    // name, not about the host having nowhere to look.
    CHECK_THROWS_AS(credential_store::read("a.credential.nobody.placed"), std::runtime_error);
#else
    CHECK_FALSE(credential_store::store_directory().has_value());
    CHECK_THROWS_AS(credential_store::read("vault.kek"), std::runtime_error);
    CHECK_THROWS_AS(credential_store::write("vault.kek", make_key(0)), std::runtime_error);
#endif
}
