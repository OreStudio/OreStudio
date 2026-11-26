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
#include "ores.platform/credentials/credential_store.hpp"
#include "ores.platform/environment/environment.hpp"
#include <cstdlib>
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>

#if defined(_WIN32)
#    include <wincrypt.h>
#    include <windows.h>
#else
#    include <sys/stat.h>
#endif

namespace ores::platform::credentials {

using ores::platform::environment::environment;

namespace {

/// The directory a launcher stated, when it stated one.
[[nodiscard]] std::optional<std::filesystem::path> stated_directory() {
    const auto stated = environment::get_value(std::string(credential_store::directory_variable));
    if (!stated.has_value() || stated->empty()) {
        return std::nullopt;
    }
    return std::filesystem::path(*stated);
}

[[nodiscard]] std::vector<unsigned char> read_file(const std::filesystem::path& path) {
    std::ifstream in(path, std::ios::binary);
    if (!in) {
        throw std::runtime_error("No credential at " + path.string() +
                                 "; the deployment places it and this host has none");
    }
    const std::string bytes{std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
    return std::vector<unsigned char>(bytes.begin(), bytes.end());
}

void write_file(const std::filesystem::path& path, std::span<const unsigned char> bytes) {
    std::error_code ec;
    std::filesystem::create_directories(path.parent_path(), ec);
    std::ofstream out(path, std::ios::binary | std::ios::trunc);
    if (!out) {
        throw std::runtime_error("Nowhere to place a credential at " + path.string());
    }
    out.write(reinterpret_cast<const char*>(bytes.data()),
              static_cast<std::streamsize>(bytes.size()));
}

void check_length(const std::filesystem::path& path,
                  const std::vector<unsigned char>& bytes,
                  std::size_t expected_length) {
    if (bytes.size() != expected_length) {
        throw std::runtime_error("The credential at " + path.string() + " holds " +
                                 std::to_string(bytes.size()) + " bytes and the cipher takes " +
                                 std::to_string(expected_length));
    }
}

#if defined(_WIN32)

/// Decrypts a DPAPI blob for the user that encrypted it.
[[nodiscard]] std::vector<unsigned char> unprotect(const std::vector<unsigned char>& blob) {
    DATA_BLOB in{static_cast<DWORD>(blob.size()), const_cast<BYTE*>(blob.data())};
    DATA_BLOB out{};
    if (::CryptUnprotectData(&in, nullptr, nullptr, nullptr, nullptr, 0, &out) == FALSE) {
        throw std::runtime_error("Windows could not decrypt the credential for this user");
    }
    std::vector<unsigned char> bytes(out.pbData, out.pbData + out.cbData);
    ::LocalFree(out.pbData);
    return bytes;
}

/// Encrypts a credential for the user that reads it.
[[nodiscard]] std::vector<unsigned char> protect(const std::vector<unsigned char>& bytes) {
    DATA_BLOB in{static_cast<DWORD>(bytes.size()), const_cast<BYTE*>(bytes.data())};
    DATA_BLOB out{};
    if (::CryptProtectData(&in, L"ORES credential", nullptr, nullptr, nullptr, 0, &out) == FALSE) {
        throw std::runtime_error("Windows could not encrypt the credential for this user");
    }
    std::vector<unsigned char> blob(out.pbData, out.pbData + out.cbData);
    ::LocalFree(out.pbData);
    return blob;
}

#endif

}

std::optional<std::filesystem::path> credential_store::store_directory() {
#if defined(_WIN32)
    const auto local = environment::get_value("LOCALAPPDATA");
    if (!local.has_value() || local->empty()) {
        return std::nullopt;
    }
    return std::filesystem::path(*local) / "ORES" / "credentials";
#elif defined(__APPLE__)
    const auto home = environment::get_value("HOME");
    if (!home.has_value() || home->empty()) {
        return std::nullopt;
    }
    return std::filesystem::path(*home) / "Library" / "Application Support" / "ORES" /
           "credentials";
#else
    // A Linux host has no store of its own: the manager places the credentials
    // and states the directory, or there are none.
    return std::nullopt;
#endif
}

namespace {

/// One credential, with the path it came from so a refusal can name it.
struct located_credential {
    std::filesystem::path path;
    std::vector<unsigned char> bytes;
};

[[nodiscard]] located_credential read_located(std::string_view name) {
    if (const auto directory = stated_directory(); directory.has_value()) {
        const auto path = *directory / std::string(name);
        return located_credential{.path = path, .bytes = read_file(path)};
    }

    const auto store = credential_store::store_directory();
    if (!store.has_value()) {
        throw std::runtime_error("No credential store on this host: no " +
                                 std::string(credential_store::directory_variable) +
                                 " is set and the platform keeps none, so the credential '" +
                                 std::string(name) + "' cannot be read");
    }

    const auto path = *store / std::string(name);
    const auto bytes = read_file(path);
#if defined(_WIN32)
    return located_credential{.path = path, .bytes = unprotect(bytes)};
#else
    return located_credential{.path = path, .bytes = bytes};
#endif
}

}

std::vector<unsigned char> credential_store::read(std::string_view name) {
    return read_located(name).bytes;
}

std::vector<unsigned char> credential_store::read(std::string_view name,
                                                  std::size_t expected_length) {
    auto credential = read_located(name);
    check_length(credential.path, credential.bytes, expected_length);
    return credential.bytes;
}

void credential_store::write(std::string_view name, std::span<const unsigned char> bytes) {
    const auto directory = stated_directory();
    if (directory.has_value()) {
        write_file(*directory / std::string(name), bytes);
        return;
    }

    const auto store = store_directory();
    if (!store.has_value()) {
        throw std::runtime_error(
            "No credential store on this host: no " + std::string(directory_variable) +
            " is set and the platform keeps none, so a credential cannot be placed");
    }

    const auto path = *store / std::string(name);
    std::error_code ec;
    std::filesystem::create_directories(*store, ec);
#if !defined(_WIN32)
    // Only the user may enter the directory, and only the user may read a key.
    std::filesystem::permissions(
        *store, std::filesystem::perms::owner_all, std::filesystem::perm_options::replace, ec);
#endif

#if defined(_WIN32)
    write_file(path, protect(std::vector<unsigned char>(bytes.begin(), bytes.end())));
#else
    write_file(path, bytes);
    std::filesystem::permissions(path,
                                 std::filesystem::perms::owner_read |
                                     std::filesystem::perms::owner_write,
                                 std::filesystem::perm_options::replace,
                                 ec);
#endif
}

}
