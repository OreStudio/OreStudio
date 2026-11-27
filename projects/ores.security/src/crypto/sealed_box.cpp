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
#include "ores.security/crypto/sealed_box.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include <algorithm>
#include <openssl/crypto.h>
#include <openssl/evp.h>
#include <openssl/kdf.h>
#include <openssl/rand.h>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::security::crypto {

using namespace ores::logging;
using ores::utility::convert::base64_converter;

namespace {

[[nodiscard]] auto& box_lg() {
    static auto instance = make_logger("ores.security.crypto.sealed_box");
    return instance;
}

/// The sender's public key, which leads every sealed value.
constexpr std::size_t ephemeral_len = sealed_box::KEY_LEN;

/// The whole header: the sender's public key, the nonce and the tag.
constexpr std::size_t box_header_len = ephemeral_len + sealed_box::NONCE_LEN + sealed_box::TAG_LEN;

/// A GCM operation never produces more than the plaintext plus one block.
constexpr std::size_t box_slack = EVP_MAX_BLOCK_LENGTH;

/// The derivation's domain separation, so the key agreed is good for this
/// purpose and no other.
constexpr std::string_view box_info = "ores.sealed_box.v1";

/// Owns an EVP_PKEY, so that a failing step leaves through an exception without
/// leaking it.
struct pkey_handle {
    EVP_PKEY* handle = nullptr;

    pkey_handle() = default;
    pkey_handle(const pkey_handle&) = delete;
    pkey_handle& operator=(const pkey_handle&) = delete;

    ~pkey_handle() {
        if (handle != nullptr) {
            EVP_PKEY_free(handle);
        }
    }
};

/// Owns an EVP_PKEY_CTX, for the same reason.
struct pkey_context {
    EVP_PKEY_CTX* handle = nullptr;

    explicit pkey_context(EVP_PKEY* key)
        : handle(EVP_PKEY_CTX_new(key, nullptr)) {}
    explicit pkey_context(int type)
        : handle(EVP_PKEY_CTX_new_id(type, nullptr)) {}
    pkey_context(const pkey_context&) = delete;
    pkey_context& operator=(const pkey_context&) = delete;

    ~pkey_context() {
        if (handle != nullptr) {
            EVP_PKEY_CTX_free(handle);
        }
    }
};

/// Owns a cipher context.
struct gcm_context {
    EVP_CIPHER_CTX* handle = EVP_CIPHER_CTX_new();

    gcm_context() = default;
    gcm_context(const gcm_context&) = delete;
    gcm_context& operator=(const gcm_context&) = delete;

    ~gcm_context() {
        if (handle != nullptr) {
            EVP_CIPHER_CTX_free(handle);
        }
    }
};

/// Wipes a buffer when it leaves scope, so that a shared secret and a derived
/// key do not outlive the call that made them.
struct secret_buffer {
    std::vector<unsigned char> bytes;

    explicit secret_buffer(std::size_t size)
        : bytes(size) {}
    secret_buffer(const secret_buffer&) = delete;
    secret_buffer& operator=(const secret_buffer&) = delete;

    ~secret_buffer() {
        if (!bytes.empty()) {
            OPENSSL_cleanse(bytes.data(), bytes.size());
        }
    }

    [[nodiscard]] unsigned char* data() {
        return bytes.data();
    }
    [[nodiscard]] const unsigned char* data() const {
        return bytes.data();
    }
    [[nodiscard]] std::size_t size() const {
        return bytes.size();
    }
};

[[noreturn]] void refuse(const std::string& step) {
    ORES_LOG_SEV(box_lg(), error) << "sealed box: " << step;
    throw std::runtime_error("sealed box: " + step);
}

/// Rejects a key of the wrong length before any cipher call runs.
void check_key_length(const std::vector<unsigned char>& key, const char* what) {
    if (key.size() != sealed_box::KEY_LEN) {
        ORES_LOG_SEV(box_lg(), error) << "Refusing a " << what << " of " << key.size()
                                      << " bytes; X25519 takes " << sealed_box::KEY_LEN;
        throw std::invalid_argument("A sealed box key must be 32 bytes");
    }
}

/// The public half of a private key, as raw bytes.
[[nodiscard]] std::vector<unsigned char>
raw_public_key_of(const std::vector<unsigned char>& private_key) {
    pkey_handle pair;
    pair.handle = EVP_PKEY_new_raw_private_key(
        EVP_PKEY_X25519, nullptr, private_key.data(), private_key.size());
    if (pair.handle == nullptr) {
        refuse("the platform rejected the private key");
    }

    std::vector<unsigned char> public_key(sealed_box::KEY_LEN);
    std::size_t len = public_key.size();
    if (EVP_PKEY_get_raw_public_key(pair.handle, public_key.data(), &len) != 1 ||
        len != sealed_box::KEY_LEN) {
        refuse("the platform cannot derive a public key from this private key");
    }
    return public_key;
}

/// Agrees the shared secret between a private key and the peer's public key,
/// into a buffer the caller owns and wipes.
void agree(const std::vector<unsigned char>& private_key,
           const std::vector<unsigned char>& peer_public_key,
           secret_buffer& shared) {
    pkey_handle mine;
    mine.handle = EVP_PKEY_new_raw_private_key(
        EVP_PKEY_X25519, nullptr, private_key.data(), private_key.size());
    if (mine.handle == nullptr) {
        refuse("the platform rejected the private key");
    }

    pkey_handle peer;
    peer.handle = EVP_PKEY_new_raw_public_key(
        EVP_PKEY_X25519, nullptr, peer_public_key.data(), peer_public_key.size());
    if (peer.handle == nullptr) {
        refuse("the platform rejected the peer's public key");
    }

    pkey_context ctx(mine.handle);
    if (ctx.handle == nullptr) {
        refuse("cannot allocate a key agreement context");
    }
    if (EVP_PKEY_derive_init(ctx.handle) <= 0) {
        refuse("cannot initialise the key agreement");
    }
    if (EVP_PKEY_derive_set_peer(ctx.handle, peer.handle) <= 0) {
        refuse("cannot set the peer's public key");
    }

    std::size_t len = shared.size();
    if (EVP_PKEY_derive(ctx.handle, shared.data(), &len) <= 0 || len != shared.size()) {
        refuse("the key agreement failed");
    }
}

/// Derives the encryption key from the shared secret, bound to both public
/// keys through the salt.
void derive_key(const secret_buffer& shared,
                const std::vector<unsigned char>& salt,
                unsigned char* out,
                std::size_t out_len) {
    pkey_context hkdf(EVP_PKEY_HKDF);
    if (hkdf.handle == nullptr) {
        refuse("cannot allocate a derivation context");
    }
    if (EVP_PKEY_derive_init(hkdf.handle) <= 0) {
        refuse("cannot initialise the derivation");
    }
    if (EVP_PKEY_CTX_set_hkdf_md(hkdf.handle, EVP_sha256()) <= 0) {
        refuse("cannot set the derivation's digest");
    }
    if (EVP_PKEY_CTX_set1_hkdf_salt(hkdf.handle, salt.data(), static_cast<int>(salt.size())) <= 0) {
        refuse("cannot set the derivation's salt");
    }
    if (EVP_PKEY_CTX_set1_hkdf_key(hkdf.handle, shared.data(), static_cast<int>(shared.size())) <=
        0) {
        refuse("cannot set the derivation's input");
    }
    if (EVP_PKEY_CTX_add1_hkdf_info(hkdf.handle,
                                    reinterpret_cast<const unsigned char*>(box_info.data()),
                                    static_cast<int>(box_info.size())) <= 0) {
        refuse("cannot set the derivation's information");
    }

    std::size_t len = out_len;
    if (EVP_PKEY_derive(hkdf.handle, out, &len) <= 0 || len != out_len) {
        refuse("the derivation failed");
    }
}

/// The salt and the additional data: the sender's public key, the recipient's
/// and the context the caller states, in that order. Both sides can rebuild it,
/// a value whose header was altered cannot, and a reply replayed under another
/// subject, name or version cannot.
[[nodiscard]] std::vector<unsigned char> bound_to_keys(const unsigned char* sender,
                                                       const std::vector<unsigned char>& recipient,
                                                       const std::string& context) {
    std::vector<unsigned char> bound(sender, sender + ephemeral_len);
    bound.insert(bound.end(), recipient.begin(), recipient.end());
    bound.insert(bound.end(), context.begin(), context.end());
    return bound;
}

}

sealed_box::key_pair sealed_box::generate() {
    pkey_context ctx(EVP_PKEY_X25519);
    if (ctx.handle == nullptr) {
        refuse("cannot allocate a key generation context");
    }
    if (EVP_PKEY_keygen_init(ctx.handle) <= 0) {
        refuse("cannot initialise the key generation");
    }

    pkey_handle pair;
    if (EVP_PKEY_keygen(ctx.handle, &pair.handle) <= 0 || pair.handle == nullptr) {
        refuse("the key generation failed");
    }

    key_pair result;
    result.private_key.resize(KEY_LEN);
    result.public_key.resize(KEY_LEN);

    std::size_t len = result.private_key.size();
    if (EVP_PKEY_get_raw_private_key(pair.handle, result.private_key.data(), &len) != 1 ||
        len != KEY_LEN) {
        refuse("the platform cannot read the generated private key");
    }
    len = result.public_key.size();
    if (EVP_PKEY_get_raw_public_key(pair.handle, result.public_key.data(), &len) != 1 ||
        len != KEY_LEN) {
        refuse("the platform cannot read the generated public key");
    }
    return result;
}

std::vector<unsigned char>
sealed_box::public_key_of(const std::vector<unsigned char>& private_key) {
    check_key_length(private_key, "private key");
    return raw_public_key_of(private_key);
}

std::string sealed_box::seal(const std::vector<unsigned char>& recipient_public_key,
                             const std::string& plaintext,
                             const std::string& context) {
    check_key_length(recipient_public_key, "public key");

    const auto ephemeral = generate();
    const auto bound = bound_to_keys(ephemeral.public_key.data(), recipient_public_key, context);

    secret_buffer shared(KEY_LEN);
    agree(ephemeral.private_key, recipient_public_key, shared);
    secret_buffer key(KEY_LEN);
    derive_key(shared, bound, key.data(), key.size());

    std::vector<unsigned char> sealed(box_header_len + plaintext.size() + box_slack);
    auto* sender = sealed.data();
    auto* nonce = sealed.data() + ephemeral_len;
    auto* tag = sealed.data() + ephemeral_len + NONCE_LEN;
    auto* body = sealed.data() + box_header_len;

    std::copy(ephemeral.public_key.begin(), ephemeral.public_key.end(), sender);

    if (RAND_bytes(nonce, static_cast<int>(NONCE_LEN)) != 1) {
        refuse("cannot generate a nonce");
    }

    gcm_context ctx;
    if (ctx.handle == nullptr) {
        refuse("cannot allocate a cipher context");
    }
    if (EVP_EncryptInit_ex(ctx.handle, EVP_aes_256_gcm(), nullptr, nullptr, nullptr) != 1) {
        refuse("cannot initialise AES-256-GCM");
    }
    if (EVP_CIPHER_CTX_ctrl(
            ctx.handle, EVP_CTRL_GCM_SET_IVLEN, static_cast<int>(NONCE_LEN), nullptr) != 1) {
        refuse("cannot set the nonce length");
    }
    if (EVP_EncryptInit_ex(ctx.handle, nullptr, nullptr, key.data(), nonce) != 1) {
        refuse("cannot set the derived key");
    }

    int written = 0;
    if (EVP_EncryptUpdate(
            ctx.handle, nullptr, &written, bound.data(), static_cast<int>(bound.size())) != 1) {
        refuse("cannot set the additional data");
    }
    if (EVP_EncryptUpdate(ctx.handle,
                          body,
                          &written,
                          reinterpret_cast<const unsigned char*>(plaintext.data()),
                          static_cast<int>(plaintext.size())) != 1) {
        refuse("cannot seal the plaintext");
    }

    int trailing = 0;
    if (EVP_EncryptFinal_ex(ctx.handle, body + written, &trailing) != 1) {
        refuse("cannot finish sealing the plaintext");
    }
    written += trailing;

    // The tag is only readable while the context is alive.
    if (EVP_CIPHER_CTX_ctrl(ctx.handle, EVP_CTRL_GCM_GET_TAG, static_cast<int>(TAG_LEN), tag) !=
        1) {
        refuse("cannot read the authentication tag");
    }

    sealed.resize(box_header_len + static_cast<std::size_t>(written));

    return base64_converter::convert(sealed);
}

std::string sealed_box::open(const std::vector<unsigned char>& private_key,
                             const std::string& sealed,
                             const std::string& context) {
    check_key_length(private_key, "private key");

    const auto raw = base64_converter::convert(sealed);
    if (raw.size() < box_header_len) {
        ORES_LOG_SEV(box_lg(), warn) << "Refusing a sealed value of " << raw.size()
                                     << " bytes, which is shorter than its own header";
        throw std::runtime_error("The sealed value is too short to hold its own header");
    }

    const unsigned char* sender = raw.data();
    const unsigned char* nonce = raw.data() + ephemeral_len;
    const unsigned char* tag = raw.data() + ephemeral_len + NONCE_LEN;
    const unsigned char* body = raw.data() + box_header_len;
    const auto body_len = raw.size() - box_header_len;

    const std::vector<unsigned char> sender_public_key(sender, sender + ephemeral_len);
    const auto recipient_public_key = raw_public_key_of(private_key);
    const auto bound = bound_to_keys(sender, recipient_public_key, context);

    secret_buffer shared(KEY_LEN);
    agree(private_key, sender_public_key, shared);
    secret_buffer key(KEY_LEN);
    derive_key(shared, bound, key.data(), key.size());

    std::vector<unsigned char> plaintext(body_len + box_slack);

    gcm_context ctx;
    if (ctx.handle == nullptr) {
        refuse("cannot allocate a cipher context");
    }
    if (EVP_DecryptInit_ex(ctx.handle, EVP_aes_256_gcm(), nullptr, nullptr, nullptr) != 1) {
        refuse("cannot initialise AES-256-GCM");
    }
    if (EVP_CIPHER_CTX_ctrl(
            ctx.handle, EVP_CTRL_GCM_SET_IVLEN, static_cast<int>(NONCE_LEN), nullptr) != 1) {
        refuse("cannot set the nonce length");
    }
    if (EVP_DecryptInit_ex(ctx.handle, nullptr, nullptr, key.data(), nonce) != 1) {
        refuse("cannot set the derived key");
    }

    int written = 0;
    if (EVP_DecryptUpdate(
            ctx.handle, nullptr, &written, bound.data(), static_cast<int>(bound.size())) != 1) {
        refuse("cannot set the additional data");
    }
    if (EVP_DecryptUpdate(
            ctx.handle, plaintext.data(), &written, body, static_cast<int>(body_len)) != 1) {
        refuse("cannot open the sealed value");
    }

    // The control takes the tag as mutable, so the copy is the one the cipher
    // reads.
    std::vector<unsigned char> expected_tag(tag, tag + TAG_LEN);
    if (EVP_CIPHER_CTX_ctrl(
            ctx.handle, EVP_CTRL_GCM_SET_TAG, static_cast<int>(TAG_LEN), expected_tag.data()) !=
        1) {
        refuse("cannot set the authentication tag");
    }

    int trailing = 0;
    if (EVP_DecryptFinal_ex(ctx.handle, plaintext.data() + written, &trailing) != 1) {
        ORES_LOG_SEV(box_lg(), warn) << "A sealed value did not authenticate under this key";
        throw std::runtime_error("The sealed value did not authenticate under this key");
    }
    written += trailing;

    plaintext.resize(static_cast<std::size_t>(written));

    return std::string(plaintext.begin(), plaintext.end());
}

}
