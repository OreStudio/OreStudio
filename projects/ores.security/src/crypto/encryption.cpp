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
#include "ores.security/crypto/encryption.hpp"

#include <memory>
#include <stdexcept>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include "ores.utility/convert/base64_converter.hpp"

namespace ores::security::crypto {

using ores::utility::converter::base64_converter;

std::vector<unsigned char> encryption::derive_key(
    const std::string& password,
    const std::vector<unsigned char>& salt) {

    std::vector<unsigned char> key(KEY_LEN);

    if (PKCS5_PBKDF2_HMAC(password.c_str(), static_cast<int>(password.length()),
                          salt.data(), static_cast<int>(salt.size()),
                          PBKDF2_ITERATIONS, EVP_sha256(),
                          static_cast<int>(KEY_LEN), key.data()) != 1) {
        throw std::runtime_error("PBKDF2 key derivation failed");
    }

    return key;
}

std::string encryption::encrypt(const std::string& plaintext,
                                 const std::string& password) {
    if (plaintext.empty()) {
        return "";
    }

    // Generate random salt and IV
    std::vector<unsigned char> salt(SALT_LEN);
    std::vector<unsigned char> iv(IV_LEN);

    if (RAND_bytes(salt.data(), SALT_LEN) != 1 ||
        RAND_bytes(iv.data(), IV_LEN) != 1) {
        throw std::runtime_error("Failed to generate random bytes");
    }

    // Derive encryption key
    auto key = derive_key(password, salt);

    // Create cipher context with RAII
    std::unique_ptr<EVP_CIPHER_CTX, decltype(&EVP_CIPHER_CTX_free)> ctx(
        EVP_CIPHER_CTX_new(), &EVP_CIPHER_CTX_free);
    if (!ctx) {
        throw std::runtime_error("Failed to create cipher context");
    }

    // Initialize encryption
    if (EVP_EncryptInit_ex(ctx.get(), EVP_aes_256_gcm(), nullptr,
                           key.data(), iv.data()) != 1) {
        throw std::runtime_error("Failed to initialize encryption");
    }

    // Encrypt
    std::vector<unsigned char> ciphertext(plaintext.size() + EVP_MAX_BLOCK_LENGTH);
    int len = 0;
    int ciphertext_len = 0;

    if (EVP_EncryptUpdate(ctx.get(), ciphertext.data(), &len,
                          reinterpret_cast<const unsigned char*>(plaintext.data()),
                          static_cast<int>(plaintext.size())) != 1) {
        throw std::runtime_error("Encryption failed");
    }
    ciphertext_len = len;

    if (EVP_EncryptFinal_ex(ctx.get(), ciphertext.data() + len, &len) != 1) {
        throw std::runtime_error("Final encryption failed");
    }
    ciphertext_len += len;
    ciphertext.resize(static_cast<std::size_t>(ciphertext_len));

    // Get authentication tag
    std::vector<unsigned char> tag(TAG_LEN);
    if (EVP_CIPHER_CTX_ctrl(ctx.get(), EVP_CTRL_GCM_GET_TAG, TAG_LEN, tag.data()) != 1) {
        throw std::runtime_error("Failed to get authentication tag");
    }

    // Combine: salt || iv || tag || ciphertext
    std::vector<unsigned char> combined;
    combined.reserve(salt.size() + iv.size() + tag.size() + ciphertext.size());
    combined.insert(combined.end(), salt.begin(), salt.end());
    combined.insert(combined.end(), iv.begin(), iv.end());
    combined.insert(combined.end(), tag.begin(), tag.end());
    combined.insert(combined.end(), ciphertext.begin(), ciphertext.end());

    return base64_converter::convert(combined);
}

std::string encryption::decrypt(const std::string& encrypted_data,
                                 const std::string& password) {
    if (encrypted_data.empty()) {
        return "";
    }

    // Decode from base64
    auto combined = base64_converter::convert(encrypted_data);

    const size_t min_len = SALT_LEN + IV_LEN + TAG_LEN;
    if (combined.size() < min_len) {
        throw std::runtime_error("Invalid encrypted data: too short");
    }

    // Extract components
    std::vector<unsigned char> salt(combined.begin(),
                                     combined.begin() + SALT_LEN);
    std::vector<unsigned char> iv(combined.begin() + SALT_LEN,
                                   combined.begin() + SALT_LEN + IV_LEN);
    std::vector<unsigned char> tag(combined.begin() + SALT_LEN + IV_LEN,
                                    combined.begin() + SALT_LEN + IV_LEN + TAG_LEN);
    std::vector<unsigned char> ciphertext(combined.begin() + min_len,
                                           combined.end());

    // Derive decryption key
    auto key = derive_key(password, salt);

    // Create cipher context with RAII
    std::unique_ptr<EVP_CIPHER_CTX, decltype(&EVP_CIPHER_CTX_free)> ctx(
        EVP_CIPHER_CTX_new(), &EVP_CIPHER_CTX_free);
    if (!ctx) {
        throw std::runtime_error("Failed to create cipher context");
    }

    // Initialize decryption
    if (EVP_DecryptInit_ex(ctx.get(), EVP_aes_256_gcm(), nullptr,
                           key.data(), iv.data()) != 1) {
        throw std::runtime_error("Failed to initialize decryption");
    }

    // Set expected tag
    if (EVP_CIPHER_CTX_ctrl(ctx.get(), EVP_CTRL_GCM_SET_TAG, TAG_LEN, tag.data()) != 1) {
        throw std::runtime_error("Failed to set authentication tag");
    }

    // Decrypt
    std::vector<unsigned char> plaintext(ciphertext.size() + EVP_MAX_BLOCK_LENGTH);
    int len = 0;
    int plaintext_len = 0;

    if (EVP_DecryptUpdate(ctx.get(), plaintext.data(), &len,
                          ciphertext.data(),
                          static_cast<int>(ciphertext.size())) != 1) {
        throw std::runtime_error("Decryption failed");
    }
    plaintext_len = len;

    // Verify tag and finalize
    if (EVP_DecryptFinal_ex(ctx.get(), plaintext.data() + len, &len) != 1) {
        throw std::runtime_error("Authentication failed: invalid password or corrupted data");
    }
    plaintext_len += len;
    plaintext.resize(static_cast<std::size_t>(plaintext_len));

    return {plaintext.begin(), plaintext.end()};
}

bool encryption::verify_password(const std::string& encrypted_data,
                                  const std::string& password) {
    if (encrypted_data.empty()) {
        return true;  // Empty data can be "decrypted" with any password
    }

    try {
        decrypt(encrypted_data, password);
        return true;
    } catch (const std::exception&) {
        return false;
    }
}

}
