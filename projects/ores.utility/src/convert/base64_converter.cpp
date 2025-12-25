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
#include "ores.utility/convert/base64_converter.hpp"

#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/kdf.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>

namespace ores::utility::converter {

std::string base64_converter::convert(const std::vector<unsigned char>& data) {
    BIO *bio = nullptr, *b64 = nullptr;
    BUF_MEM *bufferPtr = nullptr;

    try {
        b64 = BIO_new(BIO_f_base64());
        if (!b64) {
            throw std::runtime_error("Base64 encode: BIO creation failed");
        }
        bio = BIO_new(BIO_s_mem());
        if (!bio) {
            BIO_free_all(b64);
            throw std::runtime_error("Base64 encode: Memory BIO creation failed");
        }
        bio = BIO_push(b64, bio);
        BIO_set_flags(bio, BIO_FLAGS_BASE64_NO_NL);
        BIO_write(bio, data.data(), data.size());
        BIO_flush(bio);
        BIO_get_mem_ptr(bio, &bufferPtr);
        std::string encoded(bufferPtr->data, bufferPtr->length);
        BIO_free_all(bio);
        return encoded;
    } catch (...) {
        BIO_free_all(bio);
        throw;
    }
}

std::vector<unsigned char>
base64_converter::convert(const std::string& encoded) {
    BIO *bio = nullptr, *b64 = nullptr;

    try {
        if (encoded.empty()) {
            throw std::runtime_error("Base64 decode: Empty input");
        }

        b64 = BIO_new(BIO_f_base64());
        if (!b64) {
            throw std::runtime_error("Base64 decode: BIO creation failed");
        }
        bio = BIO_new_mem_buf(encoded.c_str(), -1);
        if (!bio) {
            BIO_free_all(b64);
            throw std::runtime_error("Base64 decode: Memory BIO creation failed");
        }
        bio = BIO_push(b64, bio);
        BIO_set_flags(bio, BIO_FLAGS_BASE64_NO_NL);

        std::vector<unsigned char> decoded(encoded.length());
        int decoded_len = BIO_read(bio, decoded.data(), decoded.size());
        if (decoded_len < 0) {
            BIO_free_all(bio);
            throw std::runtime_error("Base64 decode failed");
        }
        decoded.resize(decoded_len);
        BIO_free_all(bio);
        return decoded;
    } catch (...) {
        BIO_free_all(bio);
        throw;
    }
}


}
