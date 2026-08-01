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
#ifndef ORES_COMPUTE_CLIENT_CLIENT_PACKAGE_PUBLISHER_HPP
#define ORES_COMPUTE_CLIENT_CLIENT_PACKAGE_PUBLISHER_HPP

#include "ores.compute.client/export.hpp"
#include "ores.storage/net/storage_transfer.hpp"
#include <filesystem>
#include <string>

namespace ores::compute::client {

/**
 * @brief Result of publishing a single (app, version, platform) package.
 */
struct package_publish_result {
    /**
     * @brief Storage-relative URI of the uploaded package, suitable for
     * ores_compute_app_version_platforms_tbl.package_uri.
     */
    std::string package_uri;

    /**
     * @brief Server-computed SHA256 of the uploaded bytes, suitable for
     * ores_compute_app_version_platforms_tbl.sha256.
     */
    std::string sha256;
};

/**
 * @brief Uploads a compute engine package to the canonical per-triplet
 * storage location and verifies its integrity.
 *
 * The single shared core behind both ores.shell's non-interactive publish
 * command and ores.qt's "Upload Engines" dialog -- HTTP upload plus
 * client-vs-server hash verification only; building and sending the
 * save_app_request/save_app_version_request NATS messages that register the
 * result stays with each caller, since shell and Qt each already have their
 * own request/response plumbing (ores::nats::service::authenticated_request_and_decode
 * vs. ClientManager) that isn't worth abstracting over here.
 */
class ORES_COMPUTE_CLIENT_EXPORT package_publisher {
public:
    explicit package_publisher(std::string http_base_url);

    /**
     * @brief Uploads @p local_file to
     * packages/{app_name}/{version}/{app_name}-{version}-{platform_code}[.ext]
     * -- the only key shape apps/versions/platforms are published under.
     *
     * Computes the local SHA256 before uploading, compares it against the
     * server's own SHA256 of the bytes it actually received (returned by
     * storage_routes::handle_put), and throws std::runtime_error on
     * mismatch rather than returning a result the caller might trust.
     *
     * @param app_name       App name, e.g. "ore".
     * @param version        Engine version, e.g. "1.8.16.0-3-g3b62ba248".
     * @param platform_code  vcpkg triplet code, e.g. "x64-linux".
     * @param local_file     Local path of the package archive to upload.
     * @param ext            File extension including the leading dot.
     * @throws std::runtime_error on upload failure or a hash mismatch.
     */
    package_publish_result publish(const std::string& app_name,
                                   const std::string& version,
                                   const std::string& platform_code,
                                   const std::filesystem::path& local_file,
                                   const std::string& ext = ".tar.gz");

private:
    ores::storage::net::storage_transfer transfer_;
};

}

#endif
