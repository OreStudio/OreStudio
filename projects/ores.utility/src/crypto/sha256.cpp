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
#include "ores.utility/crypto/sha256.hpp"
#include <array>
#include <fstream>
#include <iomanip>
#include <openssl/evp.h>
#include <sstream>
#include <stdexcept>

namespace ores::utility::crypto {

namespace {

std::string hex_encode(const unsigned char* digest, unsigned int len) {
    std::ostringstream oss;
    oss << std::hex << std::setfill('0');
    for (unsigned int i = 0; i < len; ++i)
        oss << std::setw(2) << static_cast<unsigned int>(digest[i]);
    return oss.str();
}

class digest_context {
public:
    digest_context() : ctx_(EVP_MD_CTX_new()) {
        if (!ctx_)
            throw std::runtime_error("SHA256: failed to create digest context");
        if (EVP_DigestInit_ex(ctx_, EVP_sha256(), nullptr) != 1) {
            EVP_MD_CTX_free(ctx_);
            throw std::runtime_error("SHA256: failed to initialise digest");
        }
    }

    ~digest_context() {
        if (ctx_)
            EVP_MD_CTX_free(ctx_);
    }

    digest_context(const digest_context&) = delete;
    digest_context& operator=(const digest_context&) = delete;

    void update(const void* data, std::size_t len) {
        if (EVP_DigestUpdate(ctx_, data, len) != 1)
            throw std::runtime_error("SHA256: failed to update digest");
    }

    std::string finalise() {
        unsigned char digest[EVP_MAX_MD_SIZE];
        unsigned int digest_len = 0;
        if (EVP_DigestFinal_ex(ctx_, digest, &digest_len) != 1)
            throw std::runtime_error("SHA256: failed to finalise digest");
        return hex_encode(digest, digest_len);
    }

private:
    EVP_MD_CTX* ctx_;
};

}

std::string sha256::hex_digest(std::string_view data) {
    digest_context ctx;
    ctx.update(data.data(), data.size());
    return ctx.finalise();
}

std::string sha256::hex_digest_of_file(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    if (!in)
        throw std::runtime_error("SHA256: failed to open file: " + path.string());

    digest_context ctx;
    std::array<char, 1 << 16> buffer;
    while (in.read(buffer.data(), buffer.size()) || in.gcount() > 0)
        ctx.update(buffer.data(), static_cast<std::size_t>(in.gcount()));

    return ctx.finalise();
}

}
