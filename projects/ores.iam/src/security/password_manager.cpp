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
#include "ores.iam/security/password_manager.hpp"

#include <cmath>
#include <vector>
#include <sstream>
#include <stdexcept>
#include <cstring>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/kdf.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.platform/environment/environment.hpp"

namespace ores::iam::security {

using namespace ores::logging;
using ores::utility::converter::base64_converter;
using ores::platform::environment::environment;

std::uint64_t password_manager::get_n_parameter() {
    static const bool use_fast =
        environment::get_value("ORES_TEST_PASSWORD_FAST").has_value();
    if (use_fast) {
        return TEST_N;
    }
    return PRODUCTION_N;
}

std::string password_manager::create_password_hash(const std::string& password) {
    if (password.empty()) {
        BOOST_LOG_SEV(lg(), error) << "Password is empty";
        throw std::invalid_argument("Password cannot be empty");
    }

    std::vector<unsigned char> salt(SALT_LEN);
    if (RAND_bytes(salt.data(), SALT_LEN) != 1) {
        BOOST_LOG_SEV(lg(), error) << "Failed to generate random salt";
        throw std::runtime_error("Failed to generate random salt");
    }

    const auto N = get_n_parameter();
    std::vector<unsigned char> hash(HASH_LEN);
    if (EVP_PBE_scrypt(password.c_str(), password.length(), salt.data(), SALT_LEN,
                       N, DEFAULT_r, DEFAULT_p, 0, hash.data(), HASH_LEN) != 1) {
        BOOST_LOG_SEV(lg(), error) << "scrypt hashing failed";
        throw std::runtime_error("scrypt hashing failed");
    }

    std::stringstream ss;
    ss << "$scrypt$ln=" << std::log2(N) << ",r=" << DEFAULT_r
       << ",p=" << DEFAULT_p << "$"
       << base64_converter::convert(salt)
       << "$" << base64_converter::convert(hash);

    return ss.str();
}

bool password_manager::
verify_password_hash(const std::string& password, const std::string& hash_str) {
    if (password.empty() || hash_str.empty()) {
        BOOST_LOG_SEV(lg(), warn) << "Empty password or hash string";
        return false;
    }

    std::stringstream ss(hash_str);
    std::string segment;
    std::vector<std::string> segments;
    while (std::getline(ss, segment, '$')) {
        segments.push_back(segment);
    }

    if (segments.size() != 5 || segments[1] != "scrypt") {
        BOOST_LOG_SEV(lg(), warn) << "Invalid hash format";
        return false;
    }

    // Parse scrypt parameters from segments[2] (e.g., "ln=14,r=8,p=1")
    std::uint64_t N = 0;
    std::uint32_t r = 0, p = 0;
    std::stringstream params(segments[2]);
    std::string param;
    while (std::getline(params, param, ',')) {
        size_t eq_pos = param.find('=');
        if (eq_pos == std::string::npos) {
            BOOST_LOG_SEV(lg(), warn) << "Invalid parameter format in hash";
            return false;
        }
        std::string key = param.substr(0, eq_pos);
        std::string value = param.substr(eq_pos + 1);
        try {
            if (key == "ln") {
                int ln = std::stoi(value);
                if (ln < 1 || ln > 31) { // Reasonable bounds for ln
                    BOOST_LOG_SEV(lg(), warn) << "Invalid ln value: " << ln;
                    return false;
                }
                N = 1ULL << ln;
            } else if (key == "r") {
                r = std::stoul(value);
            } else if (key == "p") {
                p = std::stoul(value);
            }
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to parse parameter: " << param;
            return false;
        }
    }

    if (N == 0 || r == 0 || p == 0) {
        BOOST_LOG_SEV(lg(), warn) << "Missing or invalid scrypt parameters";
        return false;
    }

    // Decode salt and expected hash
    std::vector<unsigned char> salt, expected_hash;
    try {
        salt = base64_converter::convert(segments[3]);
        expected_hash = base64_converter::convert(segments[4]);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Base64 decode failed: " << e.what();
        return false;
    }

    if (expected_hash.size() != HASH_LEN) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid hash length: " << expected_hash.size();
        return false;
    }

    // Compute hash with provided password and parsed parameters
    std::vector<unsigned char> actual_hash(HASH_LEN);
    if (EVP_PBE_scrypt(password.c_str(), password.length(), salt.data(), salt.size(),
                       N, r, p, 0, actual_hash.data(), HASH_LEN) != 1) {
        BOOST_LOG_SEV(lg(), warn) << "scrypt verification failed";
        return false;
    }

    return CRYPTO_memcmp(expected_hash.data(), actual_hash.data(), HASH_LEN) == 0;
}

}
