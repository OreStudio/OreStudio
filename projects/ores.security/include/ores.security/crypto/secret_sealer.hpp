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
#ifndef ORES_SECURITY_CRYPTO_SECRET_SEALER_HPP
#define ORES_SECURITY_CRYPTO_SECRET_SEALER_HPP

#include "ores.logging/logger.hpp"
#include "ores.security/export.hpp"
#include <cstddef>
#include <string>
#include <vector>

namespace ores::security::crypto {

/**
 * @brief Seals a secret under a symmetric key, and opens it again.
 *
 * The sealer holds one AES-256-GCM key and turns a plaintext into one opaque
 * Base64 string: a random nonce, the authentication tag and the ciphertext, in
 * that order. The caller does not supply the nonce, so the same plaintext
 * never seals twice, and the caller cannot reuse a nonce by accident.
 *
 * The key is the caller's to keep, and it must live outside the database the
 * sealed secret is stored in: a key stored beside what it protects protects
 * nothing.
 *
 * Nothing in this class decides who may seal or open a secret, which row a
 * secret belongs to or which key version sealed it. Those are the store's
 * concerns, and this is the primitive beneath them.
 */
class ORES_SECURITY_EXPORT secret_sealer {
private:
    inline static std::string_view logger_name = "ores.security.crypto.secret_sealer";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    std::vector<unsigned char> key_;

public:
    /// The key length AES-256 takes.
    static constexpr std::size_t KEY_LEN = 32;

    /// The nonce length, which is the one GCM takes best.
    static constexpr std::size_t NONCE_LEN = 12;

    /// The authentication tag length.
    static constexpr std::size_t TAG_LEN = 16;

    /**
     * @brief Builds a sealer around one key.
     *
     * @param key The 32-byte key. It is copied, because the key outlives the
     * call that read it and the caller must be free to wipe its own copy.
     *
     * @throws std::invalid_argument If the key is not KEY_LEN bytes.
     */
    explicit secret_sealer(std::vector<unsigned char> key);

    /**
     * @brief Seals a plaintext under the key, bound to a context.
     *
     * @param plaintext The secret. It may be empty, which still seals.
     * @param context What this value belongs to: the tenant, the name, the
     * owning consumer and the key version of the row that holds it. It is
     * authenticated and not encrypted, so a ciphertext copied into another row
     * does not open under that row's context.
     * @return The nonce, the tag and the ciphertext, Base64-encoded.
     *
     * @throws std::runtime_error If the platform cannot produce a nonce or the
     * cipher fails.
     */
    [[nodiscard]] std::string seal(const std::string& plaintext, const std::string& context) const;

    /**
     * @brief Opens a sealed secret under the key, with the context it was
     * sealed under.
     *
     * @param sealed A value this sealer, or another sealer with the same key,
     * produced.
     * @param context The same context the value was sealed with. A copy of a
     * value under any other context does not authenticate.
     * @return The plaintext.
     *
     * @throws std::runtime_error If the value is too short to hold a nonce and
     * a tag, or if the tag does not authenticate under this key and context. An
     * altered value never opens, and never answers a different plaintext.
     */
    [[nodiscard]] std::string open(const std::string& sealed, const std::string& context) const;
};

}

#endif
