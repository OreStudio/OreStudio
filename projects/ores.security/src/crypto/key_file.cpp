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
#include "ores.security/crypto/key_file.hpp"
#include "ores.logging/logger.hpp"
#include "ores.platform/environment/environment.hpp"
#include <fstream>
#include <iterator>
#include <stdexcept>
#include <string>

namespace ores::security::crypto {

using namespace ores::logging;

namespace {

[[nodiscard]] auto& lg() {
    static auto instance = make_logger("ores.security.crypto.key_file");
    return instance;
}

}

std::optional<std::filesystem::path> key_file::credential_directory() {
    const auto stated =
        ores::platform::environment::environment::get_value(std::string(directory_variable));
    if (!stated.has_value() || stated->empty()) {
        return std::nullopt;
    }
    return std::filesystem::path(*stated);
}

std::vector<unsigned char> key_file::read(const std::filesystem::path& path,
                                          std::size_t expected_length) {
    std::ifstream in(path, std::ios::binary);
    if (!in) {
        ORES_LOG_SEV(lg(), error) << "No key at " << path;
        throw std::runtime_error("No key at " + path.string());
    }

    const std::string bytes{std::istreambuf_iterator<char>(in), std::istreambuf_iterator<char>()};
    if (bytes.size() != expected_length) {
        ORES_LOG_SEV(lg(), error) << "The key at " << path << " holds " << bytes.size()
                                  << " bytes and the cipher takes " << expected_length;
        throw std::runtime_error("The key at " + path.string() + " holds " +
                                 std::to_string(bytes.size()) + " bytes and the cipher takes " +
                                 std::to_string(expected_length));
    }

    return std::vector<unsigned char>(bytes.begin(), bytes.end());
}

}
