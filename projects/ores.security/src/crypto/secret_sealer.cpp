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
#include "ores.security/crypto/secret_sealer.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include <algorithm>
#include <memory>
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::security::crypto {

using namespace ores::logging;
using ores::utility::convert::base64_converter;

namespace {

/// The nonce and the tag, which every sealed value starts with.
constexpr std::size_t header_len = secret_sealer::NONCE_LEN + secret_sealer::TAG_LEN;

/**
 * Owns the cipher context, so that every failing step leaves through an
 * exception without leaking it.
 */
struct cipher_context {
    EVP_CIPHER_CTX* handle = EVP_CIPHER_CTX_new();

    cipher_context() = default;
    cipher_context(const cipher_context&) = delete;
    cipher_context& operator=(const cipher_context&) = delete;

    ~cipher_context() {
        if (handle != nullptr) {
            EVP_CIPHER_CTX_free(handle);
        }
    }
};

/// A GCM operation never produces more than the plaintext plus one block.
constexpr std::size_t output_slack = EVP_MAX_BLOCK_LENGTH;

}

secret_sealer::secret_sealer(std::vector<unsigned char> key)
    : key_(std::move(key)) {
    if (key_.size() != KEY_LEN) {
        ORES_LOG_SEV(lg(), error) << "Refusing a key of " << key_.size() << " bytes; AES-256 takes "
                                  << KEY_LEN;
        throw std::invalid_argument("The sealing key must be 32 bytes");
    }
}

std::string secret_sealer::seal(const std::string& plaintext, const std::string& context) const {
    std::vector<unsigned char> sealed(header_len + plaintext.size() + output_slack);

    // The nonce sits at the head of the value it belongs to, so that opening
    // needs nothing but the value and the key.
    auto* nonce = sealed.data();
    auto* tag = sealed.data() + NONCE_LEN;
    auto* body = sealed.data() + header_len;

    if (RAND_bytes(nonce, static_cast<int>(NONCE_LEN)) != 1) {
        ORES_LOG_SEV(lg(), error) << "Failed to generate a nonce";
        throw std::runtime_error("Failed to generate a nonce");
    }

    cipher_context ctx;
    if (ctx.handle == nullptr) {
        ORES_LOG_SEV(lg(), error) << "Failed to allocate a cipher context";
        throw std::runtime_error("Failed to allocate a cipher context");
    }

    if (EVP_EncryptInit_ex(ctx.handle, EVP_aes_256_gcm(), nullptr, nullptr, nullptr) != 1) {
        throw std::runtime_error("Failed to initialise AES-256-GCM");
    }

    if (EVP_CIPHER_CTX_ctrl(
            ctx.handle, EVP_CTRL_GCM_SET_IVLEN, static_cast<int>(NONCE_LEN), nullptr) != 1) {
        throw std::runtime_error("Failed to set the nonce length");
    }

    if (EVP_EncryptInit_ex(ctx.handle, nullptr, nullptr, key_.data(), nonce) != 1) {
        throw std::runtime_error("Failed to set the sealing key");
    }

    // The context is authenticated and not encrypted: it is what binds the
    // ciphertext to its row, so a copy moved elsewhere fails to open.
    int context_written = 0;
    if (EVP_EncryptUpdate(ctx.handle,
                          nullptr,
                          &context_written,
                          reinterpret_cast<const unsigned char*>(context.data()),
                          static_cast<int>(context.size())) != 1) {
        throw std::runtime_error("Failed to set the sealing context");
    }

    int written = 0;
    if (EVP_EncryptUpdate(ctx.handle,
                          body,
                          &written,
                          reinterpret_cast<const unsigned char*>(plaintext.data()),
                          static_cast<int>(plaintext.size())) != 1) {
        throw std::runtime_error("Failed to seal the plaintext");
    }

    int trailing = 0;
    if (EVP_EncryptFinal_ex(ctx.handle, body + written, &trailing) != 1) {
        throw std::runtime_error("Failed to finish sealing the plaintext");
    }
    written += trailing;

    // The tag is only readable while the context is alive, and only after the
    // final block, which is the step whose absence leaves a value that no key
    // opens.
    if (EVP_CIPHER_CTX_ctrl(ctx.handle, EVP_CTRL_GCM_GET_TAG, static_cast<int>(TAG_LEN), tag) !=
        1) {
        throw std::runtime_error("Failed to read the authentication tag");
    }

    sealed.resize(header_len + static_cast<std::size_t>(written));

    return base64_converter::convert(sealed);
}

std::string secret_sealer::open(const std::string& sealed, const std::string& context) const {
    const auto raw = base64_converter::convert(sealed);

    if (raw.size() < header_len) {
        ORES_LOG_SEV(lg(), warn) << "Refusing a sealed value of " << raw.size()
                                 << " bytes, which is shorter than its own header";
        throw std::runtime_error("The sealed value is too short to hold a nonce and a tag");
    }

    const auto* nonce = raw.data();
    const auto* tag = raw.data() + NONCE_LEN;
    const auto* body = raw.data() + header_len;
    const auto body_len = raw.size() - header_len;

    std::vector<unsigned char> plaintext(body_len + output_slack);

    cipher_context ctx;
    if (ctx.handle == nullptr) {
        throw std::runtime_error("Failed to allocate a cipher context");
    }

    if (EVP_DecryptInit_ex(ctx.handle, EVP_aes_256_gcm(), nullptr, nullptr, nullptr) != 1) {
        throw std::runtime_error("Failed to initialise AES-256-GCM");
    }

    if (EVP_CIPHER_CTX_ctrl(
            ctx.handle, EVP_CTRL_GCM_SET_IVLEN, static_cast<int>(NONCE_LEN), nullptr) != 1) {
        throw std::runtime_error("Failed to set the nonce length");
    }

    if (EVP_DecryptInit_ex(ctx.handle, nullptr, nullptr, key_.data(), nonce) != 1) {
        throw std::runtime_error("Failed to set the sealing key");
    }

    // The context is authenticated, as it was on the way in, and it is set
    // before the ciphertext the cipher must hold it against.
    int context_written = 0;
    if (EVP_DecryptUpdate(ctx.handle,
                          nullptr,
                          &context_written,
                          reinterpret_cast<const unsigned char*>(context.data()),
                          static_cast<int>(context.size())) != 1) {
        throw std::runtime_error("Failed to set the sealing context");
    }

    int written = 0;
    if (EVP_DecryptUpdate(
            ctx.handle, plaintext.data(), &written, body, static_cast<int>(body_len)) != 1) {
        throw std::runtime_error("Failed to open the sealed value");
    }

    // The control takes the tag as mutable, so the copy is the one the cipher
    // reads. It is not part of the plaintext and never escapes this call.
    std::vector<unsigned char> expected_tag(tag, tag + TAG_LEN);
    if (EVP_CIPHER_CTX_ctrl(
            ctx.handle, EVP_CTRL_GCM_SET_TAG, static_cast<int>(TAG_LEN), expected_tag.data()) !=
        1) {
        throw std::runtime_error("Failed to set the authentication tag");
    }

    int trailing = 0;
    if (EVP_DecryptFinal_ex(ctx.handle, plaintext.data() + written, &trailing) != 1) {
        ORES_LOG_SEV(lg(), warn) << "A sealed value did not authenticate under this key";
        throw std::runtime_error("The sealed value did not authenticate under this key");
    }
    written += trailing;

    plaintext.resize(static_cast<std::size_t>(written));

    return std::string(plaintext.begin(), plaintext.end());
}

}
