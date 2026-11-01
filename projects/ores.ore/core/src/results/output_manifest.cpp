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
#include "ores.ore.core/results/output_manifest.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <boost/json.hpp>
#include <stdexcept>

namespace ores::ore::results {
namespace {

namespace json = boost::json;

const output_family* family_of(const output_manifest_file& file) {
    return file.family.empty() ? nullptr : family_named(file.family);
}

}

std::string family_code_of(std::string_view file_name) {
    const auto* family = family_of_file(file_name);
    return family == nullptr ? std::string{} : family->code;
}

bool is_market_state_file(const output_manifest_file& file) {
    const auto* family = family_of(file);
    return family != nullptr && is_market_state(*family);
}

bool is_result_file(const output_manifest_file& file) {
    const auto* family = family_of(file);
    return family != nullptr && !is_market_state(*family);
}

std::string to_json(const output_manifest& manifest) {
    json::array files;
    for (const auto& file : manifest.files) {
        files.push_back(json::object{{"name", file.name},
                                     {"family", file.family},
                                     {"bytes", file.bytes},
                                     {"compressed_bytes", file.compressed_bytes},
                                     {"sha256", file.sha256},
                                     {"key", file.key}});
    }
    return json::serialize(json::object{{"result_id", manifest.result_id}, {"files", files}});
}

output_manifest manifest_from_json(std::string_view text) {
    try {
        const auto root = json::parse(text).as_object();
        output_manifest manifest;
        manifest.result_id = json::value_to<std::string>(root.at("result_id"));
        for (const auto& entry : root.at("files").as_array()) {
            const auto& file = entry.as_object();
            manifest.files.push_back(output_manifest_file{
                .name = json::value_to<std::string>(file.at("name")),
                .family = json::value_to<std::string>(file.at("family")),
                .bytes = file.at("bytes").to_number<std::int64_t>(),
                .compressed_bytes = file.at("compressed_bytes").to_number<std::int64_t>(),
                .sha256 = json::value_to<std::string>(file.at("sha256")),
                .key = json::value_to<std::string>(file.at("key"))});
        }
        return manifest;
    } catch (const std::exception& e) {
        throw std::runtime_error(std::string("The text is not an output manifest: ") + e.what());
    }
}

std::vector<output_manifest_file> located_files(const output_manifest& manifest,
                                                std::string_view manifest_key) {
    const auto slash = manifest_key.rfind('/');
    const auto directory =
        slash == std::string_view::npos ? std::string_view{} : manifest_key.substr(0, slash + 1);
    const auto own = "/" + manifest.result_id + "/";
    if (manifest.result_id.empty() || !directory.ends_with(own))
        throw std::runtime_error("The manifest at '" + std::string(manifest_key) +
                                 "' states result '" + manifest.result_id +
                                 "', which is not the result it sits under.");

    std::vector<output_manifest_file> files;
    files.reserve(manifest.files.size());
    for (auto file : manifest.files) {
        if (file.name.empty() || file.name.find('/') != std::string::npos)
            throw std::runtime_error("The manifest names a file '" + file.name +
                                     "' that is not a single name.");
        file.key = std::string(directory) + file.name + ".gz";
        files.push_back(std::move(file));
    }
    return files;
}

void verify_body(const output_manifest_file& file, std::string_view body) {
    if (static_cast<std::int64_t>(body.size()) != file.bytes)
        throw std::runtime_error("The file '" + file.name + "' holds " +
                                 std::to_string(body.size()) + " bytes where its manifest states " +
                                 std::to_string(file.bytes) + ".");
    if (ores::utility::crypto::sha256::hex_digest(body) != file.sha256)
        throw std::runtime_error("The file '" + file.name +
                                 "' does not match the digest its manifest states.");
}

}
