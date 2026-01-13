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
#ifndef ORES_SECURITY_CRYPTO_ENCRYPTION_HPP
#define ORES_SECURITY_CRYPTO_ENCRYPTION_HPP

#include <string>
#include <vector>

namespace ores::security::crypto {

/**
 * @brief AES-256-GCM encryption service.
 *
 * Provides secure encryption and decryption using AES-256-GCM with a key
 * derived from a password using PBKDF2.
 *
 * The encrypted format is: base64(salt || iv || tag || ciphertext)
 * - salt: 16 bytes for PBKDF2 key derivation
 * - iv: 12 bytes for AES-GCM
 * - tag: 16 bytes authentication tag
 * - ciphertext: variable length encrypted data
 */
class encryption final {
public:
    static constexpr size_t SALT_LEN = 16;
    static constexpr size_t IV_LEN = 12;
    static constexpr size_t TAG_LEN = 16;
    static constexpr size_t KEY_LEN = 32;  // AES-256
    static constexpr int PBKDF2_ITERATIONS = 600000;  // OWASP recommendation

    /**
     * @brief Encrypt plaintext using a password.
     * @param plaintext The text to encrypt.
     * @param password The password used to derive the encryption key.
     * @return Base64-encoded encrypted data.
     */
    static std::string encrypt(const std::string& plaintext,
                               const std::string& password);

    /**
     * @brief Decrypt ciphertext using a password.
     * @param encrypted_data Base64-encoded encrypted data.
     * @param password The password used to derive the decryption key.
     * @return Decrypted plaintext.
     * @throws std::runtime_error if decryption fails.
     */
    static std::string decrypt(const std::string& encrypted_data,
                               const std::string& password);

    /**
     * @brief Verify if a password can decrypt the given data.
     * @param encrypted_data Base64-encoded encrypted data.
     * @param password The password to verify.
     * @return true if the password is correct.
     */
    static bool verify_password(const std::string& encrypted_data,
                                const std::string& password);

private:
    static std::vector<unsigned char> derive_key(
        const std::string& password,
        const std::vector<unsigned char>& salt);
};

}

#endif
