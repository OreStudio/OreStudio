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
#include "ores.compute.client/client/package_publisher.hpp"
#include "ores.compute.api/net/compute_storage.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <boost/json.hpp>
#include <stdexcept>

namespace ores::compute::client {

package_publisher::package_publisher(std::string http_base_url)
    : transfer_(std::move(http_base_url)) {}

package_publish_result package_publisher::publish(const std::string& app_name,
                                                  const std::string& version,
                                                  const std::string& platform_code,
                                                  const std::filesystem::path& local_file,
                                                  const std::string& ext) {
    const auto local_sha256 = ores::utility::crypto::sha256::hex_digest_of_file(local_file);
    const auto key = net::compute_storage::package_key(app_name, version, platform_code, ext);

    const auto response = transfer_.upload_returning_response(
        std::string(net::compute_storage::bucket), key, local_file);

    const auto parsed = boost::json::parse(response);
    const auto& obj = parsed.as_object();
    const auto* sha256_field = obj.if_contains("sha256");
    if (!sha256_field || !sha256_field->is_string())
        throw std::runtime_error("package_publisher: upload response missing sha256: " + response);

    const auto server_sha256 = std::string(sha256_field->as_string());
    if (server_sha256 != local_sha256) {
        throw std::runtime_error("package_publisher: SHA256 mismatch for " + local_file.string() +
                                 ": local " + local_sha256 + ", server " + server_sha256 +
                                 " -- the server did not receive the bytes we sent");
    }

    return package_publish_result{
        .package_uri = net::compute_storage::package_path(app_name, version, platform_code, ext),
        .sha256 = server_sha256};
}

}
