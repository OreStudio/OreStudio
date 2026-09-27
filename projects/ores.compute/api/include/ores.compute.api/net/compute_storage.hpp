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
#ifndef ORES_COMPUTE_API_NET_COMPUTE_STORAGE_HPP
#define ORES_COMPUTE_API_NET_COMPUTE_STORAGE_HPP

#include "ores.storage.api/net/object_keys.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include <string>
#include <string_view>

namespace ores::compute::net {

/**
 * @brief Storage keys for compute grid artifacts.
 *
 * Compute artifacts live in the platform bucket under the =compute= service,
 * one purpose per kind of artifact:
 *
 *   compute/packages/{app_version_id}[.ext]
 *       bundle keyed by app version alone.
 *   compute/packages/{app_name}-{version}/{app_name}-{version}-{platform_code}[.ext]
 *       per-triplet bundle; the id keeps the version and the name is the file,
 *       so blobs stay browseable by hand ("this is ORE 1.8.15.0 for x64-linux").
 *   compute/input/{workunit_id}[.ext]    — workunit input archive
 *   compute/config/{workunit_id}[.ext]   — workunit configuration file
 *   compute/output/{result_id}.tar.gz    — result output archive
 *
 * Use @c storage_paths::make_object_path / @c make_object_url to build
 * the full HTTP path or URL from the helpers below.
 */
struct compute_storage {
    /**
     * @brief The platform bucket every service writes to.
     */
    static constexpr std::string_view bucket =
        ores::storage::api::object_keys::ores_bucket;

    /**
     * @brief Key for an application package binary.
     *
     * @param id   App version UUID as string.
     * @param ext  File extension including the leading dot (e.g. ".tar.gz").
     */
    static std::string package_key(std::string_view id, std::string_view ext = "") {
        return ores::storage::api::object_keys::make(
            service, "packages", std::string(id) + std::string(ext));
    }

    /**
     * @brief Key for a per-platform application package binary.
     *
     * @param app_name       App name, e.g. "ore".
     * @param version        Engine version, e.g. "1.8.15.0".
     * @param platform_code  vcpkg triplet code, e.g. "x64-linux".
     * @param ext            File extension including the leading dot.
     *
     * Uses a human-readable layout so admins browsing the bucket can identify
     * blobs by sight, and the SQL seed (which writes the same path) round-trips
     * through this helper.
     */
    static std::string package_key(std::string_view app_name,
                                   std::string_view version,
                                   std::string_view platform_code,
                                   std::string_view ext) {
        const std::string id =
            std::string(app_name) + "-" + std::string(version);
        return ores::storage::api::object_keys::make(
            service, "packages", id, id + "-" + std::string(platform_code) +
                std::string(ext));
    }

    /**
     * @brief Key for a workunit input archive.
     *
     * @param workunit_id  Workunit UUID as string.
     * @param ext          File extension including the leading dot.
     */
    static std::string input_key(std::string_view workunit_id, std::string_view ext = "") {
        return ores::storage::api::object_keys::make(
            service, "input", std::string(workunit_id) + std::string(ext));
    }

    /**
     * @brief Key for a workunit configuration file.
     *
     * @param workunit_id  Workunit UUID as string.
     * @param ext          File extension including the leading dot.
     */
    static std::string config_key(std::string_view workunit_id, std::string_view ext = "") {
        return ores::storage::api::object_keys::make(
            service, "config", std::string(workunit_id) + std::string(ext));
    }

    /**
     * @brief Key for a result output archive.
     *
     * @param result_id  Result UUID as string.
     */
    static std::string output_key(std::string_view result_id) {
        return ores::storage::api::object_keys::make(
            service, "output", std::string(result_id) + ".tar.gz");
    }

    /**
     * @brief Key for a batch's shared input bundle.
     *
     * A batch uploads one input tarball that every workunit of the batch
     * references, so its key is batch-scoped rather than workunit-scoped.
     *
     * @param batch_id  Compute batch UUID as string.
     * @param ext       File extension including the leading dot.
     */
    static std::string batch_input_key(std::string_view batch_id,
                                       std::string_view ext = ".tar.gz") {
        return ores::storage::api::object_keys::make(
            service, "input", std::string(batch_id) + std::string(ext));
    }

    static std::string package_path(std::string_view id, std::string_view ext = "") {
        return ores::storage::net::storage_paths::make_object_path(bucket, package_key(id, ext));
    }

    /**
     * @brief HTTP path for a per-platform package binary.
     *
     * Canonical location for the (app_version, platform_code) bundle used
     * when populating ores_compute_app_version_platforms_tbl.package_uri so
     * clients don't have to synthesise the path inline.
     */
    static std::string package_path(std::string_view app_name,
                                    std::string_view version,
                                    std::string_view platform_code,
                                    std::string_view ext) {
        return ores::storage::net::storage_paths::make_object_path(
            bucket, package_key(app_name, version, platform_code, ext));
    }

    static std::string input_path(std::string_view workunit_id, std::string_view ext = "") {
        return ores::storage::net::storage_paths::make_object_path(bucket,
                                                                   input_key(workunit_id, ext));
    }

    static std::string config_path(std::string_view workunit_id, std::string_view ext = "") {
        return ores::storage::net::storage_paths::make_object_path(bucket,
                                                                   config_key(workunit_id, ext));
    }

    static std::string output_path(std::string_view result_id) {
        return ores::storage::net::storage_paths::make_object_path(bucket, output_key(result_id));
    }

private:
    /**
     * @brief The service segment every compute key starts with.
     */
    static constexpr std::string_view service = "compute";
};

}

#endif
