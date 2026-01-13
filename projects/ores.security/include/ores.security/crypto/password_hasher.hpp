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
#ifndef ORES_SECURITY_CRYPTO_PASSWORD_HASHER_HPP
#define ORES_SECURITY_CRYPTO_PASSWORD_HASHER_HPP

#include <string>
#include "ores.logging/make_logger.hpp"

namespace ores::security::crypto {

/**
 * @brief Manages password hashing and verification using the scrypt algorithm.
 *
 * The password_hasher class provides static methods to securely hash passwords
 * and verify them against stored hashes. It uses the scrypt key derivation
 * function from OpenSSL to generate and validate password hashes, ensuring
 * strong security through configurable CPU/memory cost parameters.
 */
class password_hasher {
private:
    inline static std::string_view logger_name =
        "ores.security.crypto.password_hasher";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief Helper to Base64 encode.
     */
    static std::string base64_encode(const std::vector<unsigned char>& data);

    /**
     * @brief Helper to Base64 decode
     */
    static std::vector<unsigned char> base64_decode(const std::string& encoded);

    // scrypt parameters - OWASP recommendations for production
    static constexpr std::uint64_t PRODUCTION_N = 1 << 14; /// CPU/memory cost
    static constexpr std::uint32_t DEFAULT_r = 8;          /// Block size
    static constexpr std::uint32_t DEFAULT_p = 1;          /// Parallelization
    static constexpr std::size_t SALT_LEN = 16;
    static constexpr std::size_t HASH_LEN = 64;

    // Fast scrypt parameters for testing (16x faster)
    static constexpr std::uint64_t TEST_N = 1 << 10;

    /**
     * @brief Gets the N parameter, checking for test mode.
     *
     * If ORES_TEST_PASSWORD_FAST environment variable is set, returns
     * TEST_N for faster test execution. Otherwise returns PRODUCTION_N.
     */
    static std::uint64_t get_n_parameter();

public:
    /**
     * @brief Creates a password hash from the given password.
     *
     * Generates a secure hash of the provided password using the scrypt
     * algorithm with predefined parameters (CPU/memory cost, block size, and
     * parallelisation). The hash is formatted as a string containing the
     * algorithm identifier, parameters, salt, and hash, all Base64-encoded.
     *
     * @param password The plaintext password to hash.
     * @return A string containing the formatted hash (e.g.,
     * "$scrypt$ln=14,r=8,p=1$<salt>$<hash>").
     *
     * @throws std::invalid_argument If the password is empty.
     * @throws std::runtime_error If hash generation fails (e.g., due to random
     * salt generation or scrypt errors).
     */
    static std::string hash(const std::string &password);

    /**
     * @brief Verifies a password against a stored hash.
     *
     * Checks if the provided password matches the given hash by recomputing the
     * hash with the same salt and scrypt parameters extracted from the hash
     * string. Uses constant-time comparison to prevent timing attacks.
     *
     * @param password The plaintext password to verify.
     * @param hash The stored hash string to verify against (e.g.,
     * "$scrypt$ln=14,r=8,p=1$<salt>$<hash>").
     * @return True if the password matches the hash, false otherwise.
     */
    static bool verify(const std::string& password, const std::string &hash);
};

}

#endif
