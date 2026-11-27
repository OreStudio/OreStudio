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
#ifndef ORES_SECURITY_CRYPTO_SEALED_BOX_HPP
#define ORES_SECURITY_CRYPTO_SEALED_BOX_HPP

#include "ores.logging/logger.hpp"
#include "ores.security/export.hpp"
#include <cstddef>
#include <string>
#include <vector>

namespace ores::security::crypto {

/**
 * @brief Seals a value to a recipient's public key, which only that
 * recipient's private key opens.
 *
 * The two sides share no secret beforehand and need no round trip to agree on
 * one: the sender generates a key pair for the single value, agrees a key with
 * the recipient's public key, and puts the generated public half in the value
 * it sends. This is what lets a service seal an answer to a caller it has never
 * spoken to, and what lets a caller seal a request to a service without holding
 * its key.
 *
 * The private half never leaves its holder. The public half is not a secret and
 * is meant to be registered, published or carried in configuration.
 *
 * A sealed value is one Base64 string holding, in this order, the sender's
 * 32-byte public key, a 12-byte nonce, a 16-byte tag and the ciphertext. The
 * header is therefore 60 bytes, and both public keys are bound into the
 * derivation and the authentication, so a value whose header was altered does
 * not open.
 */
class ORES_SECURITY_EXPORT sealed_box {
private:
    inline static std::string_view logger_name = "ores.security.crypto.sealed_box";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /// The length of a public key, a private key and the shared secret.
    static constexpr std::size_t KEY_LEN = 32;

    /// The nonce length, which is the one GCM takes best.
    static constexpr std::size_t NONCE_LEN = 12;

    /// The authentication tag length.
    static constexpr std::size_t TAG_LEN = 16;

    /// A key pair, as the raw halves. The private half is a credential; the
    /// public half is not.
    struct ORES_SECURITY_EXPORT key_pair {
        std::vector<unsigned char> private_key;
        std::vector<unsigned char> public_key;
    };

    /**
     * @brief Makes a key pair for a recipient.
     *
     * @return The pair, whose private half the caller keeps as a credential and
     * whose public half the caller publishes or registers.
     *
     * @throws std::runtime_error If the platform cannot generate the key.
     */
    [[nodiscard]] static key_pair generate();

    /**
     * @brief Derives the public half of a private key.
     *
     * @param private_key The 32-byte private key.
     * @return The 32-byte public key.
     *
     * @throws std::invalid_argument If the key is not KEY_LEN bytes.
     * @throws std::runtime_error If the platform rejects the key.
     */
    [[nodiscard]] static std::vector<unsigned char>
    public_key_of(const std::vector<unsigned char>& private_key);

    /**
     * @brief Seals a plaintext to a recipient's public key.
     *
     * @param recipient_public_key The recipient's 32-byte public key.
     * @param plaintext The value. It may be empty, which still seals.
     * @param context What this value belongs to: the subject, the name and the
     * version a delivery names, so that a captured reply replayed to another
     * consumer and another row does not open.
     * @return One Base64 value carrying the sender's public key, the nonce, the
     * tag and the ciphertext.
     *
     * @throws std::invalid_argument If the recipient's key is not KEY_LEN
     * bytes.
     * @throws std::runtime_error If the platform fails to agree, derive or
     * encrypt.
     */
    [[nodiscard]] static std::string seal(const std::vector<unsigned char>& recipient_public_key,
                                          const std::string& plaintext,
                                          const std::string& context);

    /**
     * @brief Opens a value sealed to this private key.
     *
     * @param private_key The recipient's 32-byte private key.
     * @param sealed A value produced by seal(), for the public half.
     * @param context The same context the value was sealed with. A reply
     * replayed under any other context does not authenticate.
     * @return The plaintext.
     *
     * @throws std::invalid_argument If the key is not KEY_LEN bytes.
     * @throws std::runtime_error If the value is too short to hold its own
     * header, or if it does not authenticate under this key. An altered value
     * never opens, and never answers a different plaintext.
     */
    [[nodiscard]] static std::string open(const std::vector<unsigned char>& private_key,
                                          const std::string& sealed,
                                          const std::string& context);
};

}

#endif
